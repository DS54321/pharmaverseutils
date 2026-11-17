#
#   find_nonstd_chars.R
#
#   Scan a dataframe's character columns for non-standard ASCII characters
#   (code points outside the standard printable ASCII range 0x20-0x7E,
#   i.e. space through tilde) and report every occurrence found.
#
#   find_nonstd_chars(df)
#     df: a dataframe to scan. Only character columns are checked; if none
#     are found, a warning is issued and an empty list is returned.
#     Returns a named list of dataframes, one per source column that
#     contains at least one non-standard character. Each dataframe has one
#     row per flagged character, with columns column, row, value, bad_char,
#     hex_code, and unicode_name.
#
#   Depends on: control_lookup (created in get_ascii_table.R) for naming
#   control characters, and Unicode::u_char_name() as a fallback for other
#   non-standard characters.
#
# EXAMPLE
# Create your special character report.
# Input to this function is your dataframe containing character columns with potential non-printable ascii characters.
# review <- find_nonstandard_chars(cm_test)
#
# Print the first 10 rows of each dataframe (review is a list of dataframes so use lapply())
# lapply(review, head, n = 10)

#--------------------------------------------------



# --------------------------------------------------------------------------------------------------
# Identify the bad characters and report them in an easy-to-read way.
# --------------------------------------------------------------------------------------------------

find_nonstd_chars <- function(df) {

  # get the information from the lookup table
  control_lookup <- control_lookup()

  extract_bad_info <- function(x) {
    chars <- unlist(strsplit(x, ""))

    cps   <- utf8ToInt(x)
    bad_idx <- which(cps < 32 | cps > 126)  #identify the non-standard characters
    if (length(bad_idx) == 0) return(NULL)

    bad_chars <- chars[bad_idx]
    bad_hex   <- paste0("0x", toupper(format(as.hexmode(cps[bad_idx]), width = 4)))

    bad_names <- sapply(cps[bad_idx], function(cp) {
      key <- paste0("0x", toupper(format(as.hexmode(cp), width = 4)))
      match_row <- control_lookup[control_lookup$hex == key, ]
      if (nrow(match_row) == 1) {  # references control_lookup, created at top
        match_row$name
      } else {
        nm <- Unicode::u_char_name(cp)
        if (is.na(nm) || nm == "") paste0("UNKNOWN (U+", toupper(format(as.hexmode(cp), width = 4)), ")") else nm
      }
    })

    data.frame(
      bad_char     = bad_chars,
      hex_code     = bad_hex,
      unicode_name = bad_names,
      stringsAsFactors = FALSE
    )
  }

  # Check: there must be at least one character column to check.
  char_cols <- names(df)[sapply(df, is.character)]
  if (length(char_cols) == 0) {
    warning("No character columns found in df; returning an empty report.")
    return(list())
  }

  report <- list()

  for (col in char_cols) {

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

  # `report` is a list of one dataframe per source column, so distinct()
  # must be applied to each dataframe individually (across all its columns)
  # rather than to the list as a whole.
  report <- report |>
    lapply(distinct)

  return(report)
  
}

