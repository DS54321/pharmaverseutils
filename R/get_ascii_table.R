#
#   get_ascii_table.R
#
#   Get ascii table of characters
#
#   Diana Streng
#   16July2026
#
#--------------------------------------------------


#ascii character codes 0 -32 are control
#asci character codes 33 - 127 are printable ascii character codes (commonly used)
# ascii character codes 128 - 255 are the extended ascii character codes.

# Build ASCII + extended ASCII table with Unicode names



# ascii control characters do have unicode names, but they are not available in the internal tables. 
# So, we include this lookup table, defined below. 
# Note: the standard C0 control set is defined in ISO/IEC 6429.

warning("These C0 control_names should be checked by a human!")
c0_control_names <- c(
    '0x0000' = "NULL (NUL)",
  '0x0001' = "START OF HEADING",
  '0x0002' = "START OF TEXT",
  '0x0003' = "END OF TEXT",
  '0x0004' = "END OF TRANSMISSION",
  '0x0005' = "ENQUIRY",
  '0x0006' = "ACKNOWLEDGE",
  '0x0007' = "BELL, ALERT (BEL)",
  '0x0008' = "BACKSPACE (BS)",
  '0x0009' = "HORIZONTAL TAB (TAB)",
  '0x000A' = "LINE FEED (LF)",
  '0x000B' = "VERTICAL TAB (VT)",
  '0x000C' = "FORM FEED (FF)",
  '0x000D' = "CARRIAGE RETURN (CR)",
  '0x000E' = "SHIFT OUT",
  '0x000F' = "SHIFT IN",
  '0x0010' = "DATA LINK ESCAPE (DLE)",
  '0x0011' = "DEVICE CONTROL 1 (DC1)",
  '0x0012' = "DEVICE CONTROL 2 (DC2)",
  '0x0013' = "DEVICE CONTROL 3 (DC3)",
  '0x0014' = "DEVICE CONTROL 4 (DC4)",
  '0x0015' = "NEGATIVE ACKNOWLEDGE (NAK)",
  '0x0016' = "SYNCHRONOUS IDLE (SYN)",
  '0x0017' = "END OF TRANSMISSION BLOCK (ETB)",
  '0x0018' = "CANCEL (CAN)",
  '0x0019' = "END OF MEDIUM (EM)",
  '0x001A' = "SUBSTITUTE (SUB)",
  '0x001B' = "ESCAPE (ESC)",
  '0x001C' = "FILE SEPARATOR (FS)",
  '0x001D' = "GROUP SEPARATOR (GS)",
  '0x001E' = "RECORD SEPARATOR (RS)",
  '0x007F' = "DELETE (DEL)"
)

warning("These C1 control_names should be checked by a human!")
# This is the C1 Control Character Names table (0x80-09F)
c1_control_names <- c(
  '0x0080' = "PADDING CHARACTER (PAD)",
  '0x0081' = "HIGH OCTET PRESET (HOP)",
  '0x0082' = "BREAK PERMITTED HERE (BPH)",
  '0x0083' = "NO BREAK HERE (NBH)",
  '0x0084' = "INDEX (IND)",
  '0x0085' = "NEXT LINE (NEL)",
  '0x0086' = "START OF SELECTED AREA (SSA)",
  '0x0087' = "END OF SELECTED AREA (ESA)",
  '0x0088' = "CHARACTER TABULATION SET (HTS)",
  '0x0089' = "CHARACTER TABULATION WITH JUSTIFICATION (HTJ)",
  '0x008A' = "LINE TABULATION SET (VTS)",
  '0x008B' = "PARTIAL LINE FORWARD (PLD)",
  '0x008C' = "PARTIAL LINE BACKWARD (PLU)",
  '0x008D' = "REVERSE LINE FEED (RI)",
  '0x008E' = "SINGLE-SHIFT TWO (SS2)",
  '0x008F' = "SINGLE-SHIFT THREE (SS3)",
  '0x0090' = "DEVICE CONTROL STRING (DCS)",
  '0x0091' = "PRIVATE USE ONE (PU1)",
  '0x0092' = "PRIVATE USE TWO (PU2)",
  '0x0093' = "SET TRANSMIT STATE (STS)",
  '0x0094' = "CANCEL CHARACTER (CCH)",
  '0x0095' = "MESSAGE WAITING (MW)",
  '0x0096' = "START OF GUARDED AREA (SPA)",
  '0x0097' = "END OF GUARDED AREA (EPA)",
  '0x0098' = "START OF STRING (SOS)",
  '0x0099' = "SINGLE GRAPHIC CHARACTER INTRODUCER (SGCI)",
  '0x009A' = "SINGLE CHARACTER INTRODUCER (SCI)",
  '0x009B' = "CONTROL SEQUENCE INTRODUCER (CSI)",
  '0x009C' = "STRING TERMINATOR (ST)",
  '0x009D' = "OPERATING SYSTEM COMMAND (OSC)",
  '0x009E' = "PRIVACY MESSAGE (PM)",
  '0x009F' = "APPLICATION PROGRAM COMMAND (APC)"
)


# Create a tibble of the combined C0 and C1:

control_lookup <- tribble(
  ~hex,     ~dec, ~name,
  "0x0000",    0, "NULL (NUL)",
  "0x0001",    1, "START OF HEADING",
  "0x0002",    2, "START OF TEXT",
  "0x0003",    3, "END OF TEXT",
  "0x0004",    4, "END OF TRANSMISSION",
  "0x0005",    5, "ENQUIRY",
  "0x0006",    6, "ACKNOWLEDGE",
  "0x0007",    7, "BELL, ALERT (BEL)",
  "0x0008",    8, "BACKSPACE (BS)",
  "0x0009",    9, "HORIZONTAL TAB (TAB)",
  "0x000A",   10, "LINE FEED (LF)",
  "0x000B",   11, "VERTICAL TAB (VT)",
  "0x000C",   12, "FORM FEED (FF)",
  "0x000D",   13, "CARRIAGE RETURN (CR)",
  "0x000E",   14, "SHIFT OUT",
  "0x000F",   15, "SHIFT IN",
  "0x0010",   16, "DATA LINK ESCAPE (DLE)",
  "0x0011",   17, "DEVICE CONTROL 1 (DC1)",
  "0x0012",   18, "DEVICE CONTROL 2 (DC2)",
  "0x0013",   19, "DEVICE CONTROL 3 (DC3)",
  "0x0014",   20, "DEVICE CONTROL 4 (DC4)",
  "0x0015",   21, "NEGATIVE ACKNOWLEDGE (NAK)",
  "0x0016",   22, "SYNCHRONOUS IDLE (SYN)",
  "0x0017",   23, "END OF TRANSMISSION BLOCK (ETB)",
  "0x0018",   24, "CANCEL (CAN)",
  "0x0019",   25, "END OF MEDIUM (EM)",
  "0x001A",   26, "SUBSTITUTE (SUB)",
  "0x001B",   27, "ESCAPE (ESC)",
  "0x001C",   28, "FILE SEPARATOR (FS)",
  "0x001D",   29, "GROUP SEPARATOR (GS)",
  "0x001E",   30, "RECORD SEPARATOR (RS)",
  "0x001F",   31, "UNIT SEPARATOR (US)",   # rarely used but part of C0
  "0x007F",  127, "DELETE (DEL)",

  # C1 controls (0x80–0x9F)
  "0x0080", 128, "PADDING CHARACTER (PAD)",
  "0x0081", 129, "HIGH OCTET PRESET (HOP)",
  "0x0082", 130, "BREAK PERMITTED HERE (BPH)",
  "0x0083", 131, "NO BREAK HERE (NBH)",
  "0x0084", 132, "INDEX (IND)",
  "0x0085", 133, "NEXT LINE (NEL)",
  "0x0086", 134, "START OF SELECTED AREA (SSA)",
  "0x0087", 135, "END OF SELECTED AREA (ESA)",
  "0x0088", 136, "CHARACTER TABULATION SET (HTS)",
  "0x0089", 137, "CHARACTER TABULATION WITH JUSTIFICATION (HTJ)",
  "0x008A", 138, "LINE TABULATION SET (VTS)",
  "0x008B", 139, "PARTIAL LINE FORWARD (PLD)",
  "0x008C", 140, "PARTIAL LINE BACKWARD (PLU)",
  "0x008D", 141, "REVERSE LINE FEED (RI)",
  "0x008E", 142, "SINGLE-SHIFT TWO (SS2)",
  "0x008F", 143, "SINGLE-SHIFT THREE (SS3)",
  "0x0090", 144, "DEVICE CONTROL STRING (DCS)",
  "0x0091", 145, "PRIVATE USE ONE (PU1)",
  "0x0092", 146, "PRIVATE USE TWO (PU2)",
  "0x0093", 147, "SET TRANSMIT STATE (STS)",
  "0x0094", 148, "CANCEL CHARACTER (CCH)",
  "0x0095", 149, "MESSAGE WAITING (MW)",
  "0x0096", 150, "START OF GUARDED AREA (SPA)",
  "0x0097", 151, "END OF GUARDED AREA (EPA)",
  "0x0098", 152, "START OF STRING (SOS)",
  "0x0099", 153, "SINGLE GRAPHIC CHARACTER INTRODUCER (SGCI)",
  "0x009A", 154, "SINGLE CHARACTER INTRODUCER (SCI)",
  "0x009B", 155, "CONTROL SEQUENCE INTRODUCER (CSI)",
  "0x009C", 156, "STRING TERMINATOR (ST)",
  "0x009D", 157, "OPERATING SYSTEM COMMAND (OSC)",
  "0x009E", 158, "PRIVACY MESSAGE (PM)",
  "0x009F", 159, "APPLICATION PROGRAM COMMAND (APC)"
)




ascii_table <- data.frame(
  char     = intToUtf8(codes, multiple = TRUE),

  # IMPORTANT: pad hex to 4 digits so it matches control_names
  hex_code = paste0("0x", toupper(format(as.hexmode(codes), width = 4))),

  unicode_name = sapply(codes, function(cp) {

    key <- paste0("0x", toupper(format(as.hexmode(cp), width = 4)))

    # Use your custom control name if available
    if (key %in% names(control_lookup)){    #c0_control_names)) {
      return(control_lookup[[key]])
    }

    # Otherwise use Unicode name
    nm <- Unicode::u_char_name(cp)
    if (is.na(nm) || nm == "") {
      return(paste0("UNKNOWN (U+", toupper(format(as.hexmode(cp), width = 4)), ")"))
    }

    nm
  }),
  replace_char_hex_code = NA_character_,
  stringsAsFactors = FALSE
)

ascii_table



