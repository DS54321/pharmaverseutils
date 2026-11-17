replace_by_codepoint_old <- function(df, replacement_table, verbose = TRUE) {

  for (i in seq_len(nrow(replacement_table))) {

    old_unicode <- replacement_table$old_unicode[i]
    new_unicode <- replacement_table$new_unicode[i]

  # Skip invalid search patterns - stringi will throw an error if pattern is empty ("") because there is nothing to match.
      if (is.na(old_unicode) ||
          old_unicode == "" ||
          old_unicode == "U+0000") {

        if (verbose) {
          cat("Skipping invalid search pattern: old_unicode = ", old_unicode, "\n")
        }

        next
      }
  

    # Also skip the BOM (U+FEFF), this causes warnings due to empty patterns.
    # This is a known issue in stringi package (#516 in github issues for stringi) - the author states
    # "The current policy it that generally the users themselves are responsible for removing BOMs from strings manually.".
    # https://github.com/gagolews/stringi/issues/516 

    # Let's just replace this with a "" then.
    if (old_unicode == "U+FEFF") {
      df <- gsub(
        "\ufeff",
        "",
        df,
        fixed = TRUE
      )

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
    
    # Post-conversion check: no empty search patterns are permitted
    if (
      is.na(old_char) ||
      length(old_char) == 0 ||
      identical(old_char, "")
    ) {
      cat(
        "Skipping empty pattern generated from old_unicode = ",
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
#     cat(
#   "\nROW:", i,
#   "\nold_unicode:", old_unicode,
#   "\nnew_unicode:", new_unicode,
#   "\nold_char:", dQuote(old_char),
#   "\nnew_char:", dQuote(new_char),
#   "\nold_nchar:", nchar(old_char, type = "chars"),
#   "\nnew_nchar:", nchar(new_char, type = "chars"),
#   "\n"
# )
    
    
    
    df <- stringi::stri_replace_all_fixed(
      df,
      old_char,
      new_char,
      vectorize_all = FALSE
    )
  }


  df
  }