library(testthat)
library(poio)

# Can't compare po object directly using
# expect_identical(po1, po2) due to
# https://github.com/hadley/dplyr/issues/2194
# Use this instead

expect_po_equal <- function(actual, expected, check_po_revision_date = FALSE) {
  expect_identical(actual$source_type, expected$source_type)
  expect_identical(actual$file_type, expected$file_type)
  expect_identical(actual$initial_comments, expected$initial_comments)
  expect_identical(actual$initial_comments, expected$initial_comments)

  if(check_po_revision_date) {
    # PO-Revision-Date is set to the current time.
    # Fake the value, then check that separately.
    actual_po_revision_date <- with(actual$metadata, value[name == "PO-Revision-Date"])
    actual$metadata <- within(
      actual$metadata,
      value[name == "PO-Revision-Date"] <- "DUMMY VALUE"
    )
    date_format <- "%Y-%m-%d %H:%M:%S%z"
    expect_match(actual_po_revision_date, rebus.datetimes::datetime(date_format))
    expect_lt(
      as.numeric(
        difftime(
          Sys.time(),
          strptime(actual_po_revision_date, date_format),
          units = "secs"
        )
      ),
      30
    )
  }

  # Avoid tibble mess by converting data_frames
  # to data.frames before comparing
  actual_metadata <- as.data.frame(actual$metadata)
  expected_metadata <- as.data.frame(expected$metadata)
  expect_equal(actual_metadata, expected_metadata)
  actual_direct <- as.data.frame(actual$direct)
  expected_direct <- as.data.frame(expected$direct)
  expect_equal(actual_direct, expected_direct)
  actual_countable <- as.data.frame(actual$countable)
  expected_countable <- as.data.frame(expected$countable)
  expect_equal(actual_countable, expected_countable)
}

test_check("poio")

