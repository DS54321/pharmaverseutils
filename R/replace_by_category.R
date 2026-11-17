#
#  Remove unicode codepoints (characters) by unicode category.
#  Also returns a count of the replacements made.
#  Default replaces with null "".
#


# This loops through all the character columns and by default replaces the control characters with null "".
replace_by_category <- function(df, category, repl="") {

  # First, count the number of replacements that will occur
  pattern <- paste0("\\p{", category, "}")

  n_replaced <- df %>%
    summarise(
      across(
        where(is.character),
        ~ sum(
          stringi::stri_count_charclass(.x, pattern),
          na.rm = TRUE
        )
      )
    ) %>%
    unlist(use.names = FALSE) %>%
    sum()

  message(paste0("For category ", category ,", there are ", n_replaced, " characters replaced."))


# Second, action the replacement them with the value in 'repl'
 df %>%
  mutate(
    across(
      where(is.character),
      ~ stringi::stri_replace_all_charclass(
          .x,
          paste0("\\p{", category, "}"),
          repl
      )
    )
  )
}