#
#   control_lookup.R
#
#   Build a lookup table of C0 and C1 ASCII control character names, since
#   these are not available in R's internal tables (see get_ascii_table.R
#   for standard/extended ASCII names).
#
#   control_lookup()
#     Takes no arguments.
#     Returns a tibble with one row per control code point, with columns:
#       hex  - the code point as a 4-digit, uppercase, "0x"-prefixed hex
#              string (e.g. "0x001F"), matching the format used elsewhere
#              in this package (see get_ascii_table.R, find_nonstd_chars.R).
#       dec  - the code point as a decimal integer.
#       name - the human-readable control character name.
#
#   Used by: get_ascii_table.R and find_nonstd_chars.R, which each call
#   control_lookup() and assign the result to a local variable of the same
#   name (control_lookup <- control_lookup()) before joining against it by
#   the hex column.
#
#--------------------------------------------------

control_lookup <- function() {

# Create a tibble of the combined C0 and C1 ascii tables:

control_lookup <- tribble(
  ~hex,     ~dec, ~name,

  # C0 controls (0x00-0x1F)
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
  "0x001F",   31, "UNIT SEPARATOR (US)",

  # DEL - standalone control character, not part of C0 or C1
  "0x007F",  127, "DELETE (DEL)",

  # C1 controls (0x80-0x9F)
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

}