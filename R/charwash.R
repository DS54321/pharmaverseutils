#
#   charwash.R
#
#   Identify and clean special (non-standard ASCII) characters found in
#   character columns of an R dataframe.
#
#   Workflow:
#     0. Generate (or supply) a dataframe that may include non-standard characters.
#     1. Build an ASCII lookup table (see get_ascii_table.R) and write it to
#        a .csv worksheet for manual review.
#     2. Manually edit that worksheet to add a replacement character (`replace_char`) for any hex
#        codes that should be replaced, then read the edited file back in
#        as `ascii_table_map`.
#     3. find_nonstandard_chars(): scan a dataframe's character
#        columns and report every non-standard character found (code
#        points outside the standard printable ASCII range 0x20-0x7E).
#     4. replace_nonstandard_chars(): using `ascii_table_map`, replace flagged
#        characters in a single character column with their mapped
#        replacements.
#
#   Depends on: get_ascii_table.R (for `control_lookup`, used to name
#   non-standard characters) and nonstd_ascii_dataset_generator.R (for the
#   example test dataset).
#
#   DStreng, 15July2026
#   Casey Devine
#
#  Modifications:
#  14Sep2026, DStreng - Updated doc block, moved "codes" to get_ascii_table.R. Revise comments.
#    Change %>% to the base R |> . Renamed summarize_special_char_report() to
#    find_nonstandard_chars() for clarity. Renamed replace_special_chars() to
#    replace_nonstandard_chars() for consistency.
#  17Sep2026, DStreng - reorganized to break out functions into separate files.
# --------------------------------------------------




# --------------------------------------------------------------------------------------------------
# Review each column text and perform the map (e.g. replace characters). Note that this 
# is performed column-wise, not over the entire dataframe.
# --------------------------------------------------------------------------------------------------
#a.k.a replace nonstandard ascii characters

charwash <- function(text_vector, replace_table) {

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


