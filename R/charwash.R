#
#   charwash.R
#
#   Identify special (non-ascii) characters in your R dataframes.
#
#   DStreng, 15July2026
#
# --------------------------------------------------


library(lubridate)
library(Unicode) #for looking up the long names for hex codes
library(here)
library(dplyr)
#library(writexl)
library(tidyverse)
library(tibble)


#here()
# --------------------------------------------------------------------------------------------------
# Step 0: Generate a test dataframe
# --------------------------------------------------------------------------------------------------

# Generate test dataset
source(here("R", "nonstd_ascii_dataset_generator.R"))

# Example: generate 100 records
cm_test <- nonstd_ascii_dataset_generator(n=100)

#print the first 10 records
#head(cm, 10)


# --------------------------------------------------------------------------------------------------
# Step 1.A.: Generate an ascii character table.
# --------------------------------------------------------------------------------------------------

# ascii control characters do have unicode names, but they are not available in the internal tables. 
# So, we include this lookup table, defined below.

source(here("R","get_ascii_table.R"))


# We will request all ascii character codes (control chars, printable chars, and extended ascii table chars)
codes <- 0:255

ascii_table <- ascii_table %>%
  select(hex_code, unicode_name, replace_char_hex_code)  # drop the char field, this causes problems in csv.


# inhibit the hex_code field from attempts at conversion (e.g. treat as literal text)
ascii_table$hex_code <- I(ascii_table$hex_code)

# output the ascii table as a .csv file, for manual editing.
write.csv(
  x = ascii_table,
  file = here("R", "ascii_table_worksheet.csv"),
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

# output as excel file 
#write_xlsx(
#  x = list(my_dataframe = ascii_table),
#  path = here("scripts", "ascii_table.xlsx"
#)


# --------------------------------------------------------------------------------------------------------------
# Step 1.B.: Manually update the ascii_table with replacement characters (if needed), and then read in the map.
# --------------------------------------------------------------------------------------------------------------


# <<<<<------------ MANUAL UPDATE BEFORE CONTINUING -------------------->>>>>

# read in the manually adjusted ascii character table mapping .csv file
ascii_table_map <- read.csv(
  file = here("R","ascii_table_worksheet_dianamap.csv"),   # WARNING! EXAMPLE ONLY FOR TESTING !!!!!!!!!!!!!!!!!!!!
  colClasses = "character",  #stops interpretation of hex codes
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

# NOTE:: Blank spaces are not stripped away, so the replace_char is used as-is (if it includes blank space 
# that will appear in the cleaned text value)

# only keep those rows for conversion (e.g., where replace_char is not empty)
ascii_table_map <- ascii_table_map %>%
  filter(!is.na(replace_char) & replace_char != "")


# Note: the value of replacement character (column in the .csv file) is recommended to be a printable ascii character itself,
# and not the hex code or decimal character code.

# --------------------------------------------------------------------------------------------------
# Step 3: Identify the bad characters and report them in an easy-to-read way.
# --------------------------------------------------------------------------------------------------

summarize_special_char_report <- function(df) {

  extract_bad_info <- function(x) {
    chars <- unlist(strsplit(x, ""))

    cps   <- utf8ToInt(x)
    bad_idx <- which(cps < 32 | cps > 126)  #identify the non-standard characters
    if (length(bad_idx) == 0) return(NULL)

    bad_chars <- chars[bad_idx]
    bad_hex   <- paste0("0x", toupper(format(as.hexmode(cps[bad_idx]))))

    bad_names <- sapply(cps[bad_idx], function(cp) {
      key <- paste0("0x", toupper(format(as.hexmode(cp))))
      if (key %in% names(control_lookup)) {  # references control_lookup, created at top
        control_lookup[[key]]
      } else {
        nm <- Unicode::u_char_name(cp)
        if (is.na(nm) || nm == "") paste0("UNKNOWN (U+", toupper(format(as.hexmode(cp))), ")") else nm
      }
    })

    data.frame(
      bad_char     = bad_chars,
      hex_code     = bad_hex,
      unicode_name = bad_names,
      stringsAsFactors = FALSE
    )
  }

  report <- list()

  for (col in names(df)) {
    if (!is.character(df[[col]])) next

    bad_rows <- sapply(df[[col]], function(x)
      grepl("[^\\x20-\\x7E]", x)
    )
# NOTE::
# "[^\\x20-\\x7E]": The regular expression pattern breaking down as:
#   \x20: Hexadecimal code for space (" "), the first standard printable ASCII character.
#   \x7E: Hexadecimal code for tilde ("~"), the last standard printable ASCII character.
#   \x20-\x7E: The range of all standard printable ASCII characters (letters, numbers, basic punctuation, space).
#   [^ ... ]: The ^ inside square brackets means NOT (negation).

    if (any(bad_rows)) {

      col_report <- do.call(
        rbind,
        lapply(which(bad_rows), function(i) {
          info <- extract_bad_info(df[[col]][i])
          if (is.null(info)) return(NULL)

          data.frame(
            column       = col,
            row          = i,
            value        = df[[col]][i],
            bad_char     = info$bad_char,
            hex_code     = info$hex_code,
            unicode_name = info$unicode_name,
            stringsAsFactors = FALSE
          )
        })
      )

      report[[col]] <- col_report
    }
  }

  return(report)
  
}


# Create your special character report.
# Input to this function is your dataframe containing character columns with potential non-printable ascii characters.
review <- summarize_special_char_report(cm_test)

warning("Is this what we want?")
warning("This outputs one multiple rows per source row, one row to identify each non-standard ascii character present.")

# Print the first 10 rows of each dataframe (review is a list of dataframes.)
head(review, n=10)


# --------------------------------------------------------------------------------------------------
# Step 4: Review each column text and perform the map (e.g. replace characters). Note that this 
#         is performed piecewise on each column, not over the entire dataframe.
# --------------------------------------------------------------------------------------------------

replace_special_chars <- function(text_vector, replace_table) {

  out <- text_vector

  for (i in seq_len(nrow(replace_table))) {

    # Convert hex_code → integer → actual character
    hex_val <- gsub("0x", "", replace_table$hex_code[i])
    int_val <- strtoi(hex_val, base = 16)
    special_char <- intToUtf8(int_val)

    # Replace inside full strings
    out <- gsub(
      pattern = special_char,
      replacement = replace_table$replace_char[i],
      x = out,
      fixed = TRUE
    )
  }

  out
}


# Perform the cleaning, one character column at a time.
# input to the function is the dataframe$column value, as well as 
# the name of the manually adjusted ascii_table_map.csv file.
cm_test$cleaned_CMCAT <- replace_special_chars(cm_test$CMCAT, ascii_table_map)


# Check to make sure that we really did remove all the non-standard ascii characters:
check_results <- summarize_special_char_report(cm_test |> select(cleaned_CMCAT))