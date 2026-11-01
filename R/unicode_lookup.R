#
#   unicode_lookup.R
#
#   Build a lookup table of characters and associated codepoints.
#
#--------------------------------------------------

# identify character properties

library(stringi)

check <- stri_detect_charclass(
  "Hello! My name is 
  Di@n@.",   # there's a line feed (return) in here
  "\\p{Cc}"
)

check

find_unicode_issues(
  x,
  categories = c("Cc", "Cf", "Co", "Cn")
)


#\p{Cc} is a Unicode character class (also called a Unicode property), where
# \p{...}  = "match characters with this Unicode property"
# Cc       = Unicode General Category: Control
#
# and so this means "Match any Unicode control character."
#
# Remember the double \\ is to escape R and resolves to a single \.




# Per data for clinical submissions, we should make sure our data includes characters ONLY
# from the Categories L, N, P, Zs, and S.

# So - we could flag any character outside these 5 categories.

warning("QUESTION > How efficient is the execution of this classification, for our dataframe variables?")


# To get more specific, for our text cleaning, the categories of unicode characters we care about are probably:
# Cc  Control
# Cf  Format (zero-width, BOM, etc.)
# Zs  Non-standard spaces
# Co  Private-use characters
# Cn  Unassigned code points

