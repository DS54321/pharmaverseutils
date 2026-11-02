#
# find_nonstd_chars_uni.R
#
# Find nonstandard characters using unicode codepoints.
#
# DStreng, 01Nov2026



# Detect

# detect_unicode_chars(
#   data,
#   categories = c("Cc", "Cf")
# )



library(here)
library(lubridate)
library(Unicode) #for looking up the long names for hex codes
library(dplyr)
library(tidyverse)
library(tibble)

# to generate test dataset
source(here("R", "nonstd_ascii_dataset_generator.R"))


# Generate 100 records of dirty data
cm_test <- nonstd_ascii_dataset_generator(n=100)

 

# this is the name of the column vector that we want to clean.
col0 <- cm_test$CMCAT



# ----CONTROL CHARACTERS--------------
# detect control characters. Expects an input vector.
# stri_detect_charclass() returns a logical vector (the same length as our input vector),
# and which() gets the row numbers where TRUE.
# This returns the row numbers of your vector where control characters were found.
# id_control <- which(
#   stringi::stri_detect_charclass(
#   col0,
#   "\\p{Cc}"
# ))

# # count the obs containing control characters
# n_id_control <- length(id_control)



#---------------- 1. REPORT -------------------------------------------

#--------------------- Generalized function-------------------
find_unicode_category <- function(x, category, printsum = FALSE, printdetails = FALSE) {

# printsum, will print a short dummary of findings
# printdetails, will print one row per finding

  # Find the rows that contain the character values of interest (as specified by category)
  ids <- which(  # which() returns which rows in the vector are TRUE (category chars found)
    stringi::stri_detect_charclass(
      x,
      paste0("\\p{", category, "}") # look for the presence of characters in this category
    )
  )

  # store the results in a list object
  list(
    category = category,
    rows = ids,
    n = length(ids)
  )

  if (printsum == TRUE){
    # prints a general summary of findings
    print(
    tibble::tibble(
        category = category,
        n = length(ids),
        rows = paste(ids, collapse = ", ")
      )
    )
    }
  
  if (printdetails == TRUE) {
      print(
        detail_tbl <- tibble(
      row = ids,
      category = "Cf",
      value = col0[ids]
      ))
  }

}


#----------------------------- REVIEW------------------------------------

# initialize a storage list
result <- list()

# Get the results and store them in a list
result[['Cc']] <- find_unicode_category(col0, "Cc", printdetails = TRUE) 
result[['Cf']] <- find_unicode_category(col0, "Cf", printsum=TRUE)
result[['Zs']] <-find_unicode_category(col0, "Zs")
result[['Co']] <-find_unicode_category(col0, "Co")
result[['Cn']] <-find_unicode_category(col0, "Cn")

# Where we are looking for the following categories of characters:
# Cc  Control
# Cf  Format (zero-width, BOM, etc.)
# Zs  Non-standard spaces
# Co  Private-use characters
# Cn  Unassigned code points




# ---------------------3.a. REMOVE-----------------------------------------
# move this out to another function soon.
# for control characters, we probably want to just remove them all.

# This loops through all the character columns and replaces the control characters with null "".
remove_chars_by_category <- function(df, category) {

# replace them with ""
 df %>%
  mutate(
    across(
      where(is.character),
      ~ stringi::stri_replace_all_charclass(
          .x,
          paste0("\\p{", category, "}"),
          ""
      )
    )
  )
}


cm_cleaned_control <- remove_chars_by_category(cm_test, "Cc")


# run for several categories -- NEEDS UPDATE
# cm_cleaned_combo <- remove_chars_by_category(
#   cm_test,
#   category = c("Cc", "Cf"),
#   replacement = ""
# )




# -------------------------------3.b. REPLACE------------------------------

# for other characters, we might want to replace them.

# First define a replacement table (EXAMPLE)
replacement_table <- tibble::tribble(
  ~old_unicode, ~old_name, ~new_unicode, ~new_name, ~action,
  "U+0000", "NULL (NUL)", "", "", "Remove NULL character",
  "U+0009", "HORIZONTAL TAB (TAB)", "U+0020", "SPACE", "Replace TAB with SPACE",
  "U+000A", "LINE FEED (LF)", "U+0020", "SPACE", "Replace LINE FEED with SPACE",
  "U+000D", "CARRIAGE RETURN (CR)", "U+0020", "SPACE", "Replace CARRIAGE RETURN with SPACE",
  "U+200B", "ZERO WIDTH SPACE", "", "", "Remove ZERO WIDTH SPACE",
  "U+200C", "ZERO WIDTH NON-JOINER", "", "", "Remove ZERO WIDTH NON-JOINER",
  "U+200D", "ZERO WIDTH JOINER", "", "", "Remove ZERO WIDTH JOINER",
  "U+FEFF", "BYTE ORDER MARK (BOM)", "", "", "Remove BYTE ORDER MARK",
  "U+00A0", "NO-BREAK SPACE", "U+0020", "SPACE", "Replace NO-BREAK SPACE with SPACE",
  "U+2002", "EN SPACE", "U+0020", "SPACE", "Replace EN SPACE with SPACE",
  "U+2003", "EM SPACE", "U+0020", "SPACE", "Replace EM SPACE with SPACE",
  "U+2018", "LEFT SINGLE QUOTATION MARK", "U+0027", "APOSTROPHE", "Replace LEFT SINGLE QUOTE with APOSTROPHE",
  "U+2019", "RIGHT SINGLE QUOTATION MARK", "U+0027", "APOSTROPHE", "Replace RIGHT SINGLE QUOTE with APOSTROPHE",
  "U+201C", "LEFT DOUBLE QUOTATION MARK", "U+0022", "QUOTATION MARK", "Replace LEFT DOUBLE QUOTE with QUOTATION MARK",
  "U+201D", "RIGHT DOUBLE QUOTATION MARK", "U+0022", "QUOTATION MARK", "Replace RIGHT DOUBLE QUOTE with QUOTATION MARK",
  "U+2013", "EN DASH", "U+002D", "HYPHEN-MINUS", "Replace EN DASH with HYPHEN-MINUS",
  "U+2014", "EM DASH", "U+002D", "HYPHEN-MINUS", "Replace EM DASH with HYPHEN-MINUS",
  "U+2026", "HORIZONTAL ELLIPSIS", "U+002E", "PERIOD", "Replace ELLIPSIS with PERIOD"
)

#print(replacement_table)


replace_unicode_chars <- function(df, replacement_table, verbose = TRUE) {

  for (i in seq_len(nrow(replacement_table))) {

    old_unicode <- replacement_table$old_unicode[i]
    new_unicode <- replacement_table$new_unicode[i]

    # skip rows in the data where there is no replacement defined or old value is NULL.
    # empty search patterns are not supported, and you'll get a warning message.
    #if (is.na(old_unicode) || old_unicode == "" || old_unicode == "U+0000") {
    #next

    # ABOVE FAILS because some values actually resolve to empty using intToUTF8().
    # problem values here are like U+200B (zero width space), U+200c (zero width non-joiner), 
    # U+000A, U+FEFF (BOM) etc....

  # Skip invalid search patterns
      if (is.na(old_unicode) ||
          old_unicode == "" ||
          old_unicode == "U+0000") {

        if (verbose) {
          cat("Skipping:", old_unicode, "\n")
        }

        next
      }


    # Update to check the characters for old and new that are actually used
    old_char <- intToUtf8(
      strtoi(
        sub("^U\\+", "", old_unicode),
        base = 16L
      )
    )

     # Extra safety check
    if (identical(old_char, "")) {

      if (verbose) {
        cat(
          "Skipping empty pattern generated from:",
          old_unicode,
          "\n"
        )
      }

      next
    }

    
    # Convert replacement character
    if (is.na(new_unicode) || new_unicode == "") {

      # Remove character
      new_char <- ""

    } else {

      new_char <- intToUtf8(
        strtoi(
          sub("^U\\+", "", new_unicode),
          base = 16L
        )
      )

    }
    
    # no empty search patterns are permitted
    if (
      is.na(old_char) ||
      length(old_char) == 0 ||
      identical(old_char, "")
    ) {
      cat(
        "Skipping empty pattern generated from:",
        old_unicode,
        "\n"
      )
      next
    }


 if (verbose) {
      cat(
        "Replacing",
        old_unicode, dQuote(old_char),
        "with",
        ifelse(is.na(new_unicode), "<REMOVE>", new_unicode),
        dQuote(new_char),
        "\n"
      )
    }

    # troubleshooting
    cat(
  "\nROW:", i,
  "\nold_unicode:", old_unicode,
  "\nnew_unicode:", new_unicode,
  "\nold_char:", dQuote(old_char),
  "\nnew_char:", dQuote(new_char),
  "\nold_nchar:", nchar(old_char, type = "chars"),
  "\nnew_nchar:", nchar(new_char, type = "chars"),
  "\n"
)
    
    df <- stringi::stri_replace_all_fixed(
      df,
      old_char,
      new_char,
      vectorize_all = FALSE
    )
  }


  df
  }

# execute the replacement
col0_clean <- replace_unicode_chars(
  col0,
  replacement_table
)


warning("PROGRAMMERS NOTE:: still getting errors due to empty search patterns that are not supported! Needs updating.")




# # something in the replacement table is yielding an empty search pattern.
# # Find it!
# find_it <- replacement_table |>
#   dplyr::mutate(
#     old_char = purrr::map_chr(
#       old_unicode,
#       \(x) {
#         intToUtf8(
#           strtoi(
#             sub("^U\\+", "", x),
#             base = 16L
#           )
#         )
#       }
#     ),
#     old_char_length = nchar(old_char)
#   ) |>
#   dplyr::select(old_unicode, old_char, old_char_length)