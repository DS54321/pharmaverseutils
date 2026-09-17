#
#   get_ascii_table.R
#
#   Build a lookup table of ASCII + extended ASCII characters, including
#   Unicode names for control characters.
#
#   Diana Streng
#   16July2026
#
#  Modifications
#  14Sep2026, DStreng - update to the doc block. Tidy up the ascii code reference table.
#--------------------------------------------------
#
#   About ascii code ranges:
#     0 - 31    C0 control characters      (hex 0x00-0x1F)
#     32 - 126  printable ASCII characters (commonly used)
#     127       DELETE (DEL) - standalone character, not part of C0 or C1
#     128 - 159 C1 control characters      (hex 0x80-0x9F)
#     160 - 255 "extended ASCII" characters
#
#   NOTE: standard ASCII is a 7-bit encoding covering only 0-127. Codes
#   128-255 are not part of any universal ASCII standard; their meaning
#   depends on the 8-bit code page in use. The names used here for
#   128-255 reflect one specific interpretation (via Unicode::u_char_name),
#   not a universally agreed-upon "extended ASCII" table.
#
#   Control characters (C0 and C1) have Unicode names, but these are not
#   available in R's internal tables, so they are defined manually below.
#   The standard C0/C1 control sets are defined in ISO/IEC 6429.
#
#--------------------------------------------------

get_ascii_table <- function(fname = "ascii_table_worksheet") {

# We will request all ascii character codes (control chars, printable chars, and extended ascii table chars)
codes <- 0:255

  control_lookup <- control_lookup()

# getCreate an ascii lookup table with hex codes and long text names 
# for each code point in 'codes'.
ascii_table <- data.frame(

  #converts each integer code point to its corresponding character
  char = intToUtf8(codes, multiple = TRUE),

  # IMPORTANT: pad hex to 4 digits so it matches control_lookup$names
  hex_code = paste0("0x", toupper(format(as.hexmode(codes), width = 4))),

  unicode_name = sapply(codes, function(cp) {
     # sapply() loops over every value in codes (defined at top)
     # and calls the anonymous functions once per code point.
     # For each cp, compute the hex key and looks it up in the reference table. 
     # cp is the name of each code point element during iteration
    key <- paste0("0x", toupper(format(as.hexmode(cp), width = 4)))

    # Use your custom control name if available
    match_row <- control_lookup[control_lookup$hex == key, ]

    if (nrow(match_row) == 1) {  #if there is exactly one match
      return(match_row$name)
    }

    # Otherwise use Unicode name
    nm <- Unicode::u_char_name(cp)
    if (is.na(nm) || nm == "") {
      return(paste0("UNKNOWN (U+", toupper(format(as.hexmode(cp), width = 4)), ")"))
    }
   
    nm  # if prior conditions not met
  }),
  replace_char_hex_code = NA_character_,
  stringsAsFactors = FALSE
)


ascii_table <- ascii_table |>
  # drop the char field, this causes problems in csv.
  select(hex_code, unicode_name, replace_char_hex_code)  

# view
# head(ascii_table,10)

# inhibit the hex_code field from attempts at conversion 
# (e.g. treat as literal text)
ascii_table$hex_code <- I(ascii_table$hex_code)


# set the output file name
foutname <- paste0(fname,".csv")

# output the ascii table as a .csv file, for manual editing.
write.csv(
  x = ascii_table,
  file = here("R", foutname),
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

}

