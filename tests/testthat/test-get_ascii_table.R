test_that("ascii_table has exactly 256 rows covering all ASCII/extended codes", {
  expect_equal(nrow(ascii_table), 256)

  expected_hex <- paste0("0x", toupper(format(as.hexmode(0:255), width = 4)))
  expect_setequal(ascii_table$hex_code, expected_hex)
  expect_length(unique(ascii_table$hex_code), 256)
})

test_that("all 65 control-character hex codes resolve to non-UNKNOWN names", {
  # control_lookup and ascii_table are built at package load time by
  # R/get_ascii_table.R (65 = 32 C0 + 1 DEL + 32 C1 control codes)
  expect_equal(nrow(control_lookup), 65)

  looked_up <- ascii_table[match(control_lookup$hex, ascii_table$hex_code), ]

  expect_false(any(is.na(looked_up$unicode_name)))
  expect_false(any(grepl("^UNKNOWN", looked_up$unicode_name)))
  expect_equal(looked_up$unicode_name, control_lookup$name)
})
