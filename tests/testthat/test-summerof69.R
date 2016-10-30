context("Summer of 69 -  complicated POT file")

test_that(
  "read_po works on a complicated POT file",
  {
    pot_file <- system.file("extdata/R-summerof69.pot", package = "poio")
    expected <- structure(
      list(
        source_type = "r",
        file_type   = "pot",
        initial_comments = c(
          "This is a translator comment before the metadata.",
          "Other comment types aren't useful here, and should be ignored.",
          "Like the \"fuzzy\" flags comment below."
        ),
        metadata    = tibble::data_frame(
          name = c(
            "Project-Id-Version", "Report-Msgid-Bugs-To", "POT-Creation-Date",
            "PO-Revision-Date", "Last-Translator", "Language-Team",
            "MIME-Version", "Content-Type", "Content-Transfer-Encoding"
          ),
          value = c(
            "R 3.3.1", "bugs.r-project.org", "2016-10-05 20:19",
            "YEAR-MO-DA HO:MI+ZONE", "FULL NAME <EMAIL@ADDRESS>", "LANGUAGE <LL@li.org>",
            "1.0", "text/plain; charset=CHARSET", "8bit"
          )
        ),
        direct = tibble::data_frame(
          msgid  = c(
            "I got my first real six-string",
            "Bought it at the %f-and-dime",
            "Played it till my fingers bled",
            "It was the summer of '%d.",
            "Had a \\\"band\\\"\" and we tried real hard",
            "Jimmy quit and Jody got married"
          ),
          msgstr = character(6),
          is_obsolete = rep.int(c(FALSE, TRUE), c(5, 1)),
          msgctxt = list(
            character(),
            character(),
            character(),
            "Summer as in seasons, not a function that calculates sums",
            character(),
            character()
          ),
          translator_comments = list(
            c("Now commencing Bryan Adams lyrics", "Because the song has numbers in it" ),
            c("This one gets a \"c-format\" flags comment", "because it uses c-style sprintf formatting" ),
            c("I don't think that the tools package supports generating",  "source reference comments, but we should preserve them", "in case someone manually inserts them into their file" ),
            "Also uses xgettextf",
            "Testing quote escaping",
            "Obsolete messages can also have other comments"
          ),
          source_reference_comments = list(
            "some_source_file.R:123",
            character(),
            "some_source_file.R:123",
            character(),
            character(),
            character()
          ),
          flags_comments = list(
            character(),
            "c-format",
            character(),
            "c-format",
            character(),
            character()
          ),
          previous_string_comments = list(
            character(),
            character(),
            character(),
            character(),
            character(),
            character()
          )
        ),
        countable = tibble::data_frame(
          msgid         = c("Me and %d guy from school", "I should've known we'd never get far" ),
          msgid_plural  = c("Me and %d guys from school", "I should've known we'd never get fars" ),
          msgstr        = list(
            character(2),
            character(2)
          ),
          is_obsolete = c(FALSE, TRUE),
          msgctxt = list(
            character(),
            character()
          ),
          translator_comments = list(
            c("Technically the lyric says 'some' guys", "but I want a countable example" ),
            c("Countably obsolete. Apologies for bad English.", "Also note the bad number order." )
          ),
          source_reference_comments = list(
            character(),
            character()
          ),
          flags_comments = list(
            character(),
            character()
          ),
          previous_string_comments = list(
            character(),
            character()
          )
        )
      ),
      class = c("po", "list")
    )
    actual <- read_po(pot_file)
    expect_identical(actual, expected)
  }
)

test_that(
  "fix_metadata works on a complicated POT file",
  {
    pot_file <- system.file("extdata/R-summerof69.pot", package = "poio")
    expected <- structure(
      list(
        source_type = "r",
        file_type   = "pot",
        initial_comments = c(
          "This is a translator comment before the metadata.",
          "Other comment types aren't useful here, and should be ignored.",
          "Like the \"fuzzy\" flags comment below."
        ),
        metadata    = tibble::data_frame(
          name = c(
            "Project-Id-Version", "Report-Msgid-Bugs-To", "POT-Creation-Date",
            "PO-Revision-Date", "Last-Translator", "Language-Team",
            "MIME-Version", "Content-Type", "Content-Transfer-Encoding"
          ),
          value = c(
            "poio 0.0-1", "https://github.com/RL10N/poio/issues", "2016-10-05 20:19",
            "DUMMY VALUE", "FULL NAME <EMAIL@ADDRESS>", "LANGUAGE <LL@li.org>",
            "1.0", "text/plain; charset=UTF-8", "8bit"
          )
        ),
        direct = tibble::data_frame(
          msgid  = c(
            "I got my first real six-string",
            "Bought it at the %f-and-dime",
            "Played it till my fingers bled",
            "It was the summer of '%d.",
            "Had a \\\"band\\\"\" and we tried real hard",
            "Jimmy quit and Jody got married"
          ),
          msgstr = character(6),
          is_obsolete = rep.int(c(FALSE, TRUE), c(5, 1)),
          msgctxt = list(
            character(),
            character(),
            character(),
            "Summer as in seasons, not a function that calculates sums",
            character(),
            character()
          ),
          translator_comments = list(
            c("Now commencing Bryan Adams lyrics", "Because the song has numbers in it" ),
            c("This one gets a \"c-format\" flags comment", "because it uses c-style sprintf formatting" ),
            c("I don't think that the tools package supports generating",  "source reference comments, but we should preserve them", "in case someone manually inserts them into their file" ),
            "Also uses xgettextf",
            "Testing quote escaping",
            "Obsolete messages can also have other comments"
          ),
          source_reference_comments = list(
            "some_source_file.R:123",
            character(),
            "some_source_file.R:123",
            character(),
            character(),
            character()
          ),
          flags_comments = list(
            character(),
            "c-format",
            character(),
            "c-format",
            character(),
            character()
          ),
          previous_string_comments = list(
            character(),
            character(),
            character(),
            character(),
            character(),
            character()
          )
        ),
        countable = tibble::data_frame(
          msgid         = c("Me and %d guy from school", "I should've known we'd never get far" ),
          msgid_plural  = c("Me and %d guys from school", "I should've known we'd never get fars" ),
          msgstr        = list(
            character(2),
            character(2)
          ),
          is_obsolete = c(FALSE, TRUE),
          msgctxt = list(
            character(),
            character()
          ),
          translator_comments = list(
            c("Technically the lyric says 'some' guys", "but I want a countable example" ),
            c("Countably obsolete. Apologies for bad English.", "Also note the bad number order." )
          ),
          source_reference_comments = list(
            character(),
            character()
          ),
          flags_comments = list(
            character(),
            character()
          ),
          previous_string_comments = list(
            character(),
            character()
          )
        )
      ),
      class = c("po", "list")
    )
    pot <- read_po(pot_file)
    pkg <- devtools::as.package(system.file(package = "poio"))
    expect_true(is.list(pkg))
    actual <- fix_metadata(pot, pkg)
    # PO-Revision-Date is set to the current time.
    # Fake the value, then check that separately.
    actual_po_revision_date <- with(actual$metadata, value[name == "PO-Revision-Date"])
    actual$metadata <- within(
      actual$metadata,
      value[name == "PO-Revision-Date"] <- "DUMMY VALUE"
    )

    # Bug in tibble means comparing data_frames with empty list elts not possible
    # Convert to data.frame until this is fixed
    actual$direct <- as.data.frame(actual$direct)
    expected$direct <- as.data.frame(expected$direct)
    actual$countable <- as.data.frame(actual$countable)
    expected$countable <- as.data.frame(expected$countable)
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

test_that(
  "write_po works on a complicated POT file",
  {
    pot_file <- system.file("extdata/R-summerof69.pot", package = "poio")

    pot <- read_po(pot_file)
    out_file <- tempfile("actual.pot")
    write_po(pot, out_file)
    pre <- readLines(pot_file)
    # Lines 4, 23:25 ignored (unused comment, multiple blanks)
    pre <- pre[-c(4, 23:25)]

    # Replace arbitrary whitespace at start of comment line with a single space
    pre <- stringi::stri_replace_first_regex(pre, "^#([,:|~])?\\s*", "#$1 ")

    # Replace arbitrary whitespace after msgid or msgstr with a single space
    pre <- stringi::stri_replace_first_regex(pre, "msg(id(?:_plural)?|str(?:\\[[0-9+]\\])?)\\s*", "msg$1 ")

    # Move countable msg to end
    # Swap msgstr[1], msgstr[0]
    pre <- pre[c(1:40, 48:55, 41:47, 56:59, 61, 60)]

    # Add blank line at end
    pre <- c(pre, "")
    post <- readLines(out_file)
    expect_identical(post, pre)
  }
)

test_that(
  "generate_po_from_pot can convert complicated pot file to po file",
  {
    pot_file <- system.file("extdata/R-summerof69.pot", package = "poio")
    expected <- structure(
      list(
        source_type = "r",
        file_type   = "po",
        initial_comments = c(
          "This is a translator comment before the metadata.",
          "Other comment types aren't useful here, and should be ignored.",
          "Like the \"fuzzy\" flags comment below."
        ),
        metadata    = tibble::data_frame(
          name = c(
            "Project-Id-Version", "Report-Msgid-Bugs-To", "POT-Creation-Date",
            "PO-Revision-Date", "Last-Translator", "Language-Team",
            "MIME-Version", "Content-Type", "Content-Transfer-Encoding",
            "Language", "Plural-Forms"
          ),
          value = c(
            "R 3.3.1", "bugs.r-project.org", "2016-10-05 20:19",
            "YEAR-MO-DA HO:MI+ZONE", "FULL NAME <EMAIL@ADDRESS>", "LANGUAGE <LL@li.org>",
            "1.0", "text/plain; charset=CHARSET", "8bit",
            "ru", "nplurals=3; plural=(n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2);"
          )
        ),
        direct = tibble::data_frame(
          msgid  = c(
            "I got my first real six-string",
            "Bought it at the %f-and-dime",
            "Played it till my fingers bled",
            "It was the summer of '%d.",
            "Had a \\\"band\\\"\" and we tried real hard",
            "Jimmy quit and Jody got married"
          ),
          msgstr = character(6),
          is_obsolete = rep.int(c(FALSE, TRUE), c(5, 1)),
          msgctxt = list(
            character(),
            character(),
            character(),
            "Summer as in seasons, not a function that calculates sums",
            character(),
            character()
          ),
          translator_comments = list(
            c("Now commencing Bryan Adams lyrics", "Because the song has numbers in it" ),
            c("This one gets a \"c-format\" flags comment", "because it uses c-style sprintf formatting" ),
            c("I don't think that the tools package supports generating",  "source reference comments, but we should preserve them", "in case someone manually inserts them into their file" ),
            "Also uses xgettextf",
            "Testing quote escaping",
            "Obsolete messages can also have other comments"
          ),
          source_reference_comments = list(
            "some_source_file.R:123",
            character(),
            "some_source_file.R:123",
            character(),
            character(),
            character()
          ),
          flags_comments = list(
            character(),
            "c-format",
            character(),
            "c-format",
            character(),
            character()
          ),
          previous_string_comments = list(
            character(),
            character(),
            character(),
            character(),
            character(),
            character()
          )
        ),
        countable = tibble::data_frame(
          msgid         = c("Me and %d guy from school", "I should've known we'd never get far" ),
          msgid_plural  = c("Me and %d guys from school", "I should've known we'd never get fars" ),
          msgstr        = list(
            character(3),
            character(3)
          ),
          is_obsolete = c(FALSE, TRUE),
          msgctxt = list(
            character(),
            character()
          ),
          translator_comments = list(
            c("Technically the lyric says 'some' guys", "but I want a countable example" ),
            c("Countably obsolete. Apologies for bad English.", "Also note the bad number order." )
          ),
          source_reference_comments = list(
            character(),
            character()
          ),
          flags_comments = list(
            character(),
            character()
          ),
          previous_string_comments = list(
            character(),
            character()
          )
        )
      ),
      class = c("po", "list")
    )
    pot <- read_po(pot_file)
    actual <- generate_po_from_pot(pot, "ru")

    # Bug in tibble means comparing data_frames with empty list elts not possible
    # Convert to data.frame until this is fixed
    actual$direct <- as.data.frame(actual$direct)
    expected$direct <- as.data.frame(expected$direct)
    actual$countable <- as.data.frame(actual$countable)
    expected$countable <- as.data.frame(expected$countable)
    expect_identical(actual, expected)
  }
)

