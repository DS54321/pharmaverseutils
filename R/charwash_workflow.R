#
#   charwash_workflow.R
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
#     3. find_nonstd_chars(): scan a dataframe's character
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
#
# --------------------------------------------------

library(here)
library(lubridate)
library(Unicode) #for looking up the long names for hex codes
library(dplyr)
library(tidyverse)
library(tibble)

# to generate test dataset
source(here("R", "nonstd_ascii_dataset_generator.R"))

# to create the ascii table worksheet
source(here("R","get_ascii_table.R"))

# to identify the non-standard ascii characters
source(here("R","find_nonstd_chars.R"))

# get the ascii lookup table
source(here("R","control_lookup.R"))

# action the replacement characters
source(here("R","charwash.R"))


#here()
# --------------------------------------------------------------------------------------------------
# Step 0: Generate a test dataframe
# --------------------------------------------------------------------------------------------------

# Example: generate 100 records
cm_test <- nonstd_ascii_dataset_generator(n=100)

#print the first 10 records
#head(cm_test, 10)

# --------------------------------------------------------------------------------------------------
# Step 1: Generate an ascii character table worksheet.
# --------------------------------------------------------------------------------------------------

# ascii control characters do have unicode names, but they are not available in R's internal  tables. 
# So, we include this lookup table, defined below.


# function above outputs a .csv file
get_ascii_table() # default output file name is ascii_table_worksheet

# do not specify file extension in the fname for the output file.
#get_ascii_table(fname="test_ascii_table") # give an alternate name for the output file

# --------------------------------------------------------------------------------------------------------------
# Step 2: Manually update the ascii_table worksheet with replacement characters (if needed), and then read in the map.
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
ascii_table_map <- ascii_table_map |>
  filter(!is.na(replace_char) & replace_char != "")


# Note: the value of replacement character (column in the .csv file) is recommended to be a printable ascii character itself,
# and not the hex code or decimal character code.

# --------------------------------------------------------------------------------------------------
# Step 3: Identify the bad characters and report them in an easy-to-read way.
# --------------------------------------------------------------------------------------------------

# Create your special character report.
# Input to this function is your dataframe containing character columns with potential non-printable ascii characters.
review <- find_nonstd_chars(cm_test)

# Print the first 10 rows of each dataframe (review is a list of dataframes.)
lapply(review, head, n = 10)


# --------------------------------------------------------------------------------------------------
# Step 4: Review each column text and perform the map (e.g. replace characters). Note that this 
#         is performed column-wise, not over the entire dataframe.
# --------------------------------------------------------------------------------------------------


# Option A:
# Perform the cleaning, one df character column at a time.
# input to the function is the dataframe$column value, as well as 
# the name of the manually adjusted ascii_table_map.csv file.

cm_test$cleaned_CMCAT <- charwash(cm_test$CMCAT, ascii_table_map)

# Check to make sure that we really did remove all the non-standard ascii characters:
check_results <- find_nonstd_chars(cm_test |> select(cleaned_CMCAT))



# Option B:
# Update to run through all the character columns within your dataframe, using tidyverse. 
cm_test_clean2 <- cm_test |>
  mutate( 
    across(where(is.character), 
    ~ charwash(.x, replace_table = ascii_table_map))
  )

# Check to make sure that we really did remove all the non-standard ascii characters:
check_results2 <- find_nonstd_chars(cm_test_clean2 |> select(CMCAT))
