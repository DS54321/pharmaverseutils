#
#
#  nonstd_ascii_dataset_generator.R
#  (a.k.a. nasty dataset generator)
#
#  For testing identification of non-standard/ control ascii characters in your data.
#  This example dataset is based on the CDISC SDTM CM (Concomitant Medications) structure.
#
#  Diana Streng
#  16July2026
#-------------------------------------


nonstd_ascii_dataset_generator <- function(n=100) {

  # n = number of observations.

# Special characters to inject
special_chars <- c(
  "—", "–", "µ", "Ω", "β", "®", "™", "★", "†", "‡",
  "∞", "≈", "≠", "≤", "≥", "§", "¶", "•", "€", "£",
  "©", "¿", "¡", "√", "∑", "∂", "∆", "π", "σ", "✓",
  "~", "%", "\t", "\n"
)

# Base CM terms
base_CMCAT   <- c("ANALGESICS", "ANTIDIABETICS", "ANTIHISTAMINES")
base_CMTRT   <- c("Ibuprofen", "Metformin", "Benadryl")
base_CMDECOD <- c("IBUPROFEN", "METFORMIN", "DIPHENHYDRAMINE")

# Function to inject random special characters into a string
inject_special <- function(x, n = 3) {
  paste0(
    x,
    paste0(sample(special_chars, n, replace = TRUE), collapse = "")
  )
}

  set.seed(123)

  df <- data.frame(
    STUDYID = rep("TEST12345", n), # clinical study identifier
    USUBJID = sprintf("001-%03d", 1:n), # unique subject identifier (patient ID)

    CMSEQ   = as.integer(rep(1, n)), # unique sequence number (unique within USUBJID)
    CMSTDY  = sample(1:30, n, replace = TRUE), # study day of conmed start
    CMENDY  = sample(2:40, n, replace = TRUE), # study day of conmed end

    CMCAT   = sapply(sample(base_CMCAT, n, replace = TRUE),  # conmed category
                     inject_special),

    CMTRT   = sapply(sample(base_CMTRT, n, replace = TRUE),  # collected conmed name
                     inject_special),

    CMDECOD = sapply(sample(base_CMDECOD, n, replace = TRUE),  #dictionary coded term
                     inject_special),

    CMSTDTC = as.POSIXct(  # conmed start date/time, iso8601
      paste0("2024-01-", sprintf("%02d", sample(1:28, n, replace = TRUE)),
             "T", sprintf("%02d:%02d:%02d",
                          sample(0:23, n, replace = TRUE),
                          sample(0:59, n, replace = TRUE),
                          sample(0:59, n, replace = TRUE))),
      format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"
    ),

    CMENDTC = as.POSIXct( # conmed end date/time, iso8601
      paste0("2024-02-", sprintf("%02d", sample(1:28, n, replace = TRUE)),
             "T", sprintf("%02d:%02d:%02d",
                          sample(0:23, n, replace = TRUE),
                          sample(0:59, n, replace = TRUE),
                          sample(0:59, n, replace = TRUE))),
      format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"
    ),

    stringsAsFactors = FALSE
  )

  return(df)

}


# Example: generate 100 records
#cm <- nonstd_ascii_dataset_generator(n=100)

#print the first 10 records
#head(cm, 10)