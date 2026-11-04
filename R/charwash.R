#' Replace non‑standard ASCII characters in a character vector
#'
#' This function scans a character vector for non‑standard ASCII characters
#' (typically identified using an externally prepared lookup table) and replaces
#' them with user‑specified substitute characters. It can be used
#' column‑wise within a dataframe, or across the entire dataframe at once.
#'
#' @description
#' `charwash()` performs targeted replacement of characters whose code points
#' fall outside the standard printable ASCII range (0x20–0x7E). The function
#' expects a lookup table containing:
#'   * `hex_code` — the hex representation of the character to replace  
#'   * `replace_char` — the character that should be substituted
#'
#' The lookup table is typically generated using `get_ascii_table()` and then
#' manually edited to specify replacement characters. This function is part of a
#' broader workflow for identifying and cleaning non‑standard ASCII characters in
#' clinical or regulatory datasets.
#'
#' @param text_vector A character vector to be cleaned.
#' @param replace_table A dataframe containing at least two columns:
#'   * `hex_code` — hex string like `"0x00A0"`  
#'   * `replace_char` — the replacement character to use
#'
#' @return A cleaned character vector with all mapped non‑standard characters
#'   replaced.
#'
#' @examples
#' \dontrun{
#'   cleaned <- charwash(df$COMMENT, ascii_table_map)
#' }
#'
#' @author
#' D. Streng, C. Devine
#'
#' @seealso
#' * `find_nonstandard_chars()` — identify non‑standard characters in a dataframe  
#' * `replace_nonstandard_chars()` — wrapper for applying replacements to a column  
#' * `get_ascii_table()` — generate the ASCII lookup table
#'
#' @export
charwash <- function(text_vector, replace_table, replace_all = NULL) {

  # Specify replace_all OR replace_table, not both
  if (!is.null(replace_all) && !is.null(replace_table)) {
    stop("Specify either `replace_all` or `replace_table`, not both.")
  }
  if (is.null(replace_all) && is.null(replace_table)) {
    stop("Specify either `replace_all` or `replace_table`.")
  }

  # Initialize output as a copy of the input vector
  out <- text_vector

  # Replace ALL non-standard characters with the specified replacement
  if (!is.null(replace_all)) {
    out <- gsub(
      pattern     = "[^\\x20-\\x7E]",
      replacement = replace_all,
      x           = out,
      perl        = TRUE
    )
    return(out)
  }

  # Loop through each row of the replacement table
  for (i in seq_len(nrow(replace_table))) {

    # Extract the hex code and convert to an integer
    # Example: "0x00A0" → "00A0" → integer → UTF‑8 character
    hex_val <- gsub("0x", "", replace_table$hex_code[i])
    int_val <- strtoi(hex_val, base = 16)

    # Convert integer code point to actual character
    special_char <- intToUtf8(int_val)

    # Perform the replacement inside the full string
    # fixed = TRUE ensures literal matching (no regex interpretation)
    out <- gsub(
      pattern     = special_char,
      replacement = replace_table$replace_char[i],
      x           = out,
      fixed       = TRUE
    )
  }

  # Return the cleaned vector
  out
}
