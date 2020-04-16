RX <- list(
  # "capture(some letters or hyphens): capture(some stuff)\n"
  metadata                 = '^"([a-zA-Z-]+): *(.+)\\\\n"$',
  # optional(obsolete comment) msgid "capture(some stuff)"
  msgid                    = '^(#~)? *msgid *"(.+)"$',
  # optional(obsolete comment) msgid_plural "capture(some stuff)"
  msgid_plural             = '^(?:#~)? *msgid_plural *"(.+)"$',
  # optional(obsolete comment) msgstr "capture(some stuff)"
  msgstr_direct            = '^(?:#~)? *msgstr *" *(.*)"$',
  # optional(obsolete comment) msgstr[index] "capture(some stuff)"
  msgstr_countable         = '^(?:#~)? *msgstr(?:\\[([0-9]+)\\])? *"(.*)"$',
  # optional(obsolete comment) msgctxt "capture(some stuff)"
  msgctxt                  = '^(?:#~)? *msgctxt *"(.+)"$',
  # hash, not (caret or comma or colon or pipe or tilde) capture(some stuff)
  translator_comment       = "^#(?=[^,:|~]) *(.+)$",
  # hash, colon capture(some stuff)
  source_reference_comment = "^#: *(.+)$",
  # hash, comma capture(some stuff)
  flags_comment            = "^#, *(.+)$",
  # hash, pipe capture(some stuff)
  previous_string_comment  = "^#\\| *(.+)$",
  # maybe some spaces
  blank                    = "^ *$"
)

# avoid warning in R RMCD check
utils::globalVariables(".")

#' @importFrom magrittr extract
#' @importFrom stringi stri_match_first_regex
match_and_extract <- function(x, rx, drop = TRUE)
{
  x %>%
    stri_match_first_regex(rx) %>%
    extract(!is.na(.[, 1L]), -1L, drop = drop)
}

empty_direct_msgs <- function() {
  tibble(
    msgid                     = character(),
    msgstr                    = character(),
    is_obsolete               = logical(),
    msgctxt                   = list(),
    translator_comments       = list(),
    source_reference_comments = list(),
    flags_comments            = list(),
    previous_string_comments  = list()
  )
}

empty_countable_msgs <- function() {
  tibble(
    msgid                     = character(),
    msgid_plural              = character(),
    msgstr                    = list(),
    is_obsolete               = logical(),
    msgctxt                   = list(),
    translator_comments       = list(),
    source_reference_comments = list(),
    flags_comments            = list(),
    previous_string_comments  = list()
  )
}

#' Read PO and POT files
#'
#' Reads .PO and .POT translation files.
#' @param po_file A string giving a path to a PO file.
#' @return An object of class \code{\link{po}}. The \code{source_type} and
#' \code{file_type} elements are automatically determined from the file
#' name.
#' @seealso \code{\link[tools]{xgettext}}
#' @examples
#' # read_po is used for both po and pot files
#' pot_file <- system.file("extdata/R-summerof69.pot", package = "poio")
#' (pot <- read_po(pot_file))
#' @importFrom assertive.base bapply
#' @importFrom assertive.properties is_empty
#' @importFrom assertive.properties is_non_empty
#' @importFrom assertive.properties is_scalar
#' @importFrom assertive.types assert_is_a_string
#' @importFrom assertive.files assert_all_are_existing_files
#' @importFrom dplyr filter_
#' @importFrom dplyr select_
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#' @importFrom magrittr %>%
#' @importFrom magrittr extract
#' @importFrom magrittr extract2
#' @importFrom stringi stri_read_lines
#' @importFrom stringi stri_detect_regex
#' @importFrom stringi stri_match_first_regex
#' @importFrom tibble tibble
#' @importFrom tools file_ext
#' @export
read_po <- function(po_file)
{
  assert_is_a_string(po_file)
  assert_all_are_existing_files(po_file)
  warn_if_not_po_file(po_file)
  lines <- stri_read_lines(po_file, "UTF-8")

  base_file_name <- basename(po_file)
  source_type <- ifelse(
    substring(base_file_name, 1, 2) == "R-", "r", "c"
  )
  file_type <- file_ext(base_file_name)

  metadata_line_index <- which(stri_detect_regex(lines, RX$metadata))
  metadata_lines <- lines[metadata_line_index]
  metadata <- metadata_lines %>%
    stri_match_first_regex(RX$metadata)
  metadata <- tibble(
    name  = metadata[, 2],
    value = metadata[, 3]
  )

  # Translator comments before metadata
  first_metadata_line <- metadata_line_index[1]
  lines_before_metadata <- lines[seq_len(first_metadata_line - 1L)]
  initial_comments <- match_and_extract(
    lines_before_metadata,
    RX$translator_comment
  )

  # How many plural forms are there?
  n_plural_forms <- get_n_plural_forms(metadata)

  # Now consider only lines after metadata
  last_metadata_line <- metadata_line_index[length(metadata_line_index)]
  lines <- lines[-seq_len(last_metadata_line)]

  # Split on blank/whitespace lines
  blank_lines <- cumsum(stri_detect_regex(lines, RX$blank))
  line_groups <- split(lines, blank_lines)

  # And remove those empty lines
  line_groups <- line_groups %>%
    lapply(
      function(x)
        x[!stri_detect_regex(x, RX$blank)]
    )

  # Remove empty groups
  line_groups <- line_groups[
    bapply(line_groups, is_non_empty)
    ]

  # Special handling of empty case need because lapply
  # just returns list() otherwise.
  if(is_empty(line_groups))
  {
    msgs_direct <- empty_direct_msgs()
    msgs_countable <- empty_countable_msgs()
  } else # lines groups is not empty
  {
    all_msgs <- line_groups %>%
      lapply(
      function(lines)
      {
        msgid_match <- match_and_extract(lines, RX$msgid, drop = FALSE)
        # Where there is an obsolete comment, no msgids are possible
        # Currently the implementation allows for zero msgids under all
        # circumstances which is possibly too lenient, but the spec is unclear.
        if(nrow(msgid_match) != 1L)
        {
          stop(
            "There should be exactly one msgid found in each block. Are you missing a blank line?\nThe offending lines are:\n",
            paste(lines, collapse = "\n")
          )
        }
        is_obsolete <- !is.na(msgid_match[, 1L])
        msgid <- msgid_match[, 2L]

        comments <- tibble(
          translator_comments = list(
            match_and_extract(lines, RX$translator_comment)
          ),
          source_reference_comments = list(
            match_and_extract(lines, RX$source_reference_comment)
          ),
          flags_comments = list(
            match_and_extract(lines, RX$flags_comment)
          ),
          previous_string_comments = list(
            match_and_extract(lines, RX$previous_string_comment)
          )
        )
        msgctxt <- list(match_and_extract(lines, RX$msgctxt))
        msgid_plural <- match_and_extract(lines, RX$msgid_plural)
        if(is_empty(msgid_plural)) # direct msg
        {
          msgstr <- match_and_extract(lines, RX$msgstr_direct)
          list(
            msg_type = "direct",
            msgs = tibble(
              msgid       = msgid,
              msgstr      = msgstr,
              is_obsolete = is_obsolete,
              msgctxt     = msgctxt
            ) %>%
              bind_cols(comments)
          )
        } else # countable msg
        {
          msgstr_and_id <- match_and_extract(lines, RX$msgstr_countable)
          # In case of weird file with, e.g, msgstr[1] before msgstr[0], reorder
          o <- order(msgstr_and_id[, 1L])
          msgstr <- msgstr_and_id[o, 2L]
          # If badly formed with wrong number of plurals, add empty translations
          # up to no. of plural forms suggested by metadata (or ignore extras).
          length(msgstr) <- n_plural_forms
          msgstr <- list(msgstr)

          list(
            msg_type = "countable",
            msgs = tibble(
              msgid        = msgid,
              msgid_plural = msgid_plural,
              msgstr       = msgstr,
              is_obsolete  = is_obsolete,
              msgctxt      = msgctxt
            ) %>%
              bind_cols(comments)
          )
        }
      }
    )
    is_direct <- all_msgs %>%
      bapply(
        function(x)
        {
          x$msg_type == "direct"
        }
      )
    msgs_direct <- if(any(is_direct)) {
      all_msgs[is_direct] %>%
        lapply(
          function(x)
          {
            x$msgs
          }
        ) %>%
        bind_rows()
    } else {
      empty_direct_msgs()
    }
    msgs_countable <- if(any(!is_direct)) {
      all_msgs[!is_direct] %>%
        lapply(
          function(x)
          {
            x$msgs
          }
        ) %>%
        bind_rows()
    } else {
      empty_countable_msgs()
    }
  }

  po(
    source_type = source_type,
    file_type = file_type,
    initial_comments = initial_comments,
    metadata = metadata,
    direct = msgs_direct,
    countable = msgs_countable
  )
}

is_po_file <- function(po_file)
{
  stri_detect_regex(po_file, "\\.pot?$")
}

warn_if_not_po_file <- function(po_file)
{
  if(!is_po_file(po_file)) {
    warning("The file extension is neither 'po' nor 'pot'.")
  }
}
