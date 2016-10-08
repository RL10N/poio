context("Empty PO/POT files")

test_that(
  "read_po works on an empty POT file",
  {
    pot_file <- system.file("extdata/R-empty-raw.pot", package = "poio")
    expected <- structure(
      list(
        source_type = "r",
        file_type   = "pot",
        metadata    = data.frame(
          name = c(
            "Project-Id-Version", "Report-Msgid-Bugs-To", "POT-Creation-Date",
            "PO-Revision-Date", "Last-Translator", "Language-Team",
            "MIME-Version", "Content-Type", "Content-Transfer-Encoding"
          ),
          value = c(
            "R 3.3.1", "bugs.r-project.org", "2016-10-05 20:19",
            "YEAR-MO-DA HO:MI+ZONE", "FULL NAME <EMAIL@ADDRESS>", "LANGUAGE <LL@li.org>",
            "1.0", "text/plain; charset=CHARSET", "8bit"
          ),
          stringsAsFactors = FALSE
        ),
        direct = data.frame(
          msgid  = character(),
          msgstr = character(),
          stringsAsFactors = FALSE
        ),
        countable = data.frame(
          msgid         = character(),
          msgid_plural  = character(),
          msgstr        = character(),
          stringsAsFactors = FALSE
        )
      ),
      class = c("po", "list")
    )
    actual <- read_po(pot_file)
    expect_identical(actual, expected)
  }
)

test_that(
  "fix_metadata works on an empty pot file",
  {
    pot_file <- system.file("extdata/R-empty-raw.pot", package = "poio")
    expected <- structure(
      list(
        source_type = "r",
        file_type   = "pot",
        metadata    = data.frame(
          name = c(
            "Project-Id-Version", "Report-Msgid-Bugs-To", "POT-Creation-Date",
            "PO-Revision-Date", "Last-Translator", "Language-Team",
            "MIME-Version", "Content-Type", "Content-Transfer-Encoding"
          ),
          value = c(
            "poio 0.0-1", "https://github.com/RL10N/poio/issues", "2016-10-05 20:19",
            "DUMMY VALUE", "FULL NAME <EMAIL@ADDRESS>", "LANGUAGE <LL@li.org>",
            "1.0", "text/plain; charset=UTF-8", "8bit"
          ),
          stringsAsFactors = FALSE
        ),
        direct = data.frame(
          msgid  = character(),
          msgstr = character(),
          stringsAsFactors = FALSE
        ),
        countable = data.frame(
          msgid         = character(),
          msgid_plural  = character(),
          msgstr        = character(),
          stringsAsFactors = FALSE
        )
      ),
      class = c("po", "list")
    )
    pot <- read_po(pot_file)
    actual <- fix_metadata(pot)
    # PO-Revision-Date is set to the current time.
    # Fake the value, then check that separately.
    actual_po_revision_date <- with(actual$metadata, value[name == "PO-Revision-Date"])
    actual$metadata <- within(
      actual$metadata,
      value[name == "PO-Revision-Date"] <- "DUMMY VALUE"
    )
    expect_identical(actual, expected)

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
)
