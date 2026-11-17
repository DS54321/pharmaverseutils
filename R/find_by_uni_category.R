# Identify and report unicode codepoints within specified unicode categories.
# Expects an input vector.
# stri_detect_charclass() returns a logical vector (the same length as our input vector),
# and which() gets the row numbers where TRUE.
# 
# Returns the row numbers of your vector where control characters were found.



find_by_uni_category <- function(x, category, printsum = FALSE, printdetails = FALSE) {
# arguments:
# x, a vector of character values (like adcm$CMTRT)
# category, the 2 character abbreviation of unicode category, for character values (like Cc, Cf, Zs,...)
# printsum, logical, will print a short summary of findings
# printdetails, logical, will print one row per finding

  # Find the rows that contain the character values of interest (as specified by category)
  ids <- which(  # which() returns which rows in the vector are TRUE (category chars found)
    stringi::stri_detect_charclass(
      x,
      paste0("\\p{", category, "}") # look for the presence of characters in this category
    )
  )

  # store the results in a list object
  outlist <- list(
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

  return(outlist)

}
