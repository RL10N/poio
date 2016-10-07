context("Empty PO/POT files")

test_that(
  "read_pot works on an empty POT file",
  {
    pot_file <- system.file("extdata/R-empty-raw.pot", package = "poio")
    expected <- structure(
      list(
        source_type = "r",
        file_type   = "pot",
        metadata    = data.frame(
          name = c(),
          value = c()
        ),
        direct = data.frame(
          msgid  = character(),
          msgstr = character(),
          stringsAsFactors = FALSE
        ),,
        countable = data.frame(
          msgid         = character(),
          msgid_plural  = character(),
          msgstr        = character(),
          stringsAsFactors = FALSE
        )
      )
    )
    actual <- read_pot(pot_file)
    expect_identical(actual, expected)
  }
)
