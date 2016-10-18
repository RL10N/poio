RX <- list(
  metadata                 = '^"([a-zA-Z-]+): *(.+)\\\\n"$',
  msgid                    = '^(#~)? *msgid *"(.+)"$',
  msgid_plural             = '^(?:#~)? *msgid_plural *"(.+)"$',
  msgstr_direct            = '^(?:#~)? *msgstr *" *(.*)"$',
  msgstr_countable         = '^(?:#~)? *msgstr(?:\\[([0-9]+)\\])? *"(.*)"$',
  msgctxt                  = '^(#~)? *msgid *"(.+)"$',
  translator_comment       = "^#(?=[^,:|~]) *(.+)$",
  source_reference_comment = "^#: *(.+)$",
  flags_comment            = "^#, *(.+)$",
  previous_string_comment  = "^#\\| *(.+)$",
  blank                    = "^ *$"
)

#' @importFrom magrittr extract
#' @importFrom stringi stri_match_first_regex
match_and_extract <- function(x, rx, drop = TRUE)
{
  x %>%
    stri_match_first_regex(rx) %>%
    extract(!is.na(.[, 1L]), -1L, drop = drop)
}

#' Read PO and POT files
#'
#' Reads .PO and .POT translation files.
#' @param po_file A string giving a path to a PO file.
#' @param pot_file A string giving a path to a POT file, or \code{NULL} to
#' autogenerate it.
#' @return An object of class \code{po}, which is a list with the following
#' components:
#' \describe{
#' \item{source_type}{Either "r" or "c", depending upon whether the messages
#' originated from R-level code, or C-level code.  Determined from the file
#' name.}
#' \item{file_type}{Either "po" or "pot", depending upon whether the messages
#' originated from a PO (language-specific) or POT (master translation) file.
#' Determined from the file name.}
#' \item{initial_comments}{A character vector of comments added by the
#' translator.}
#' \item{metadata}{A \code{\link[tibble]{data_frame}} of file metadata with
#' columns "name" and "value".}
#' \item{direct}{A \code{\link[tibble]{data_frame}} of messages with a direct
#' translation, as created by \code{\link[base]{stop}},
#' \code{\link[base]{warning}}, \code{\link[base]{message}} or
#' \code{\link[base]{gettext}}; its columns are described below.}
#' \item{countable}{A data frame of messages where the translation depends upon
#' a countable value, as created by \code{\link[base]{ngettext}}; its columns are
#' described below.}
#' }
#'
#' The \code{direct} element of the \code{po} object has the following columns.
#' \describe{
#' \item{msgid}{Character. The untranslated (should be American English)
#' message.}
#' \item{msgstr}{Character. The translated message, or empty strings in the case
#' of POT files.}
#' \item{is_obsolete}{Logical. Is the message obsolete?}
#' \item{msgctxt}{List of character. Disambiguating context information to allow
#' multiple messages with the same ID.}
#' \item{translator_comments}{List of character. Comments added by the
#' translator, typically to explain unclear messages, or why translation choices
#' were made.}
#' \item{source_reference_comments}{List of character. Links to where the
#' message occured in the source, in the form "filename:line".}
#' \item{flags_comments}{List of character. Typically used to describe
#' formatting directives. R uses C-style formatting, which would imply a
#' "c-format" flag.  For example %%d denotes an integer, and %%s denotes a
#' string. "fuzzy" flags can appear when PO files are merged.}
#' \item{previous_string_comment}{List of character. When PO files are merged
#' with an updated POT file ,and a fuzzy flag is generated, the old msgid is
#' stored in a previous string comment.}
#' }
#'
#' The \code{countable} element of the \code{po} object takes the same form as
#' the \code{direct} element, with two differences.
#' \describe{
#' \item{msgid_plural}{Character. The plural form of the untranslated message.}
#' \item{msgstr}{This is now a list of character (rather than character.)}
#' }
#' @references Much of the logic for this functions was determined from reading
#' \url{http://pology.nedohodnik.net/doc/user/en_US/ch-poformat.html}
#' @seealso \code{\link[tools]{xgettext}}
#' @examples
#' # TODO
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
#' @importFrom pathological get_extension
#' @importFrom stringi stri_read_lines
#' @importFrom stringi stri_detect_regex
#' @importFrom stringi stri_match_first_regex
#' @importFrom tibble data_frame
#' @export
read_po <- function(po_file)
{
  assert_is_a_string(po_file)
  assert_all_are_existing_files(po_file)
  lines <- stri_read_lines(po_file, "UTF-8")

  base_file_name <- basename(po_file)
  source_type <- ifelse(
    substring(base_file_name, 1, 2) == "R-", "r", "c"
  )
  file_type <- unname(get_extension(base_file_name))

  metadata_line_index <- which(stri_detect_regex(lines, RX$metadata))
  metadata_lines <- lines[metadata_line_index]
  metadata <- metadata_lines %>%
    stri_match_first_regex(RX$metadata)
  metadata <- data_frame(
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
  plural_forms <- metadata %>%
    filter_(~ name == "Plural-Forms") %>%
    select_(~ value) %>%
    extract2(1)
  n_plural_forms <- if(is_scalar(plural_forms))
  {
    plural_forms %>%
      stri_match_first_regex("nplurals *= *([0-9])")[, 2] %>%
      as.integer
  } else
  {
    2L # for POT files
  }

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

      comments <- data_frame(
        translator_comments = list(match_and_extract(lines, RX$translator_comment)),
        source_reference_comments = list(match_and_extract(lines, RX$source_reference_comment)),
        flags_comments = list(match_and_extract(lines, RX$flags_comment)),
        previous_string_comments = list(match_and_extract(lines, RX$previous_string_comment))
      )
      msgctxt <- list(match_and_extract(lines, RX$msgctxt))
      msgid_plural <- match_and_extract(lines, RX$msgid_plural)
      if(is_empty(msgid_plural)) # direct msg
      {
        msgstr <- match_and_extract(lines, RX$msgstr_direct)
        list(
          msg_type = "direct",
          msgs = data_frame(
            msgid  = msgid,
            msgstr = msgstr,
            is_obsolete = is_obsolete,
            msgctxt = msgctxt
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
          msgs = data_frame(
            msgid  = msgid,
            msgid_plural = msgid_plural,
            msgstr = msgstr,
            is_obsolete = is_obsolete,
            msgctxt = msgctxt
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
  msgs_direct <- all_msgs[is_direct] %>%
    lapply(
      function(x)
      {
        x$msgs
      }
    ) %>%
    bind_rows()
  msgs_countable <- all_msgs[!is_direct] %>%
    lapply(
      function(x)
      {
        x$msgs
      }
    ) %>%
    bind_rows()

  structure(
    list(
      source_type = source_type,
      file_type = file_type,
      initial_comments = initial_comments,
      metadata = metadata,
      direct = msgs_direct,
      countable = msgs_countable
    ),
    class = c("po", "list")
  )
}

read_po_no_comments <- function(po_file)
{
  assert_is_a_string(po_file)
  assert_all_are_existing_files(po_file)
  lines <- stri_read_lines(po_file, "UTF-8")

  base_file_name <- basename(po_file)
  source_type <- ifelse(
    substring(base_file_name, 1, 2) == "R-", "r", "c"
  )
  file_type <- unname(get_extension(base_file_name))

  metadata_lines <- lines[stri_detect_regex(lines, '^"')]
  metadata <- stri_match_first_regex(metadata_lines, '^"([a-zA-Z-]+): *(.+)\\\\n"$')
  metadata <- data.frame(
    name = metadata[, 2],
    value = metadata[, 3],
    stringsAsFactors = FALSE
  )

  # Ignore first instance of msgid, since it is blank
  msgid_index <- which(stri_detect_regex(lines, "^msgid "))[-1]

  # Need to deal with countable cases separately
  msgid_plural_index <- which(stri_detect_regex(lines, "^msgid_plural"))
  msgid_singular_index <- msgid_plural_index - 1
  msgid_direct_index <- setdiff(msgid_index, msgid_singular_index)

  if(is_non_empty(msgid_direct_index))
  {
    msgid_direct_matches = stri_match_first_regex(
      lines[msgid_direct_index],
      '^msgid +"(.+)"$'
    )

    msgstr_direct_matches = stri_match_first_regex(
      lines[msgid_direct_index + 1],
      '^msgstr +"(.*)"$'
    )

    # The print method for data frames doesn't handle UTF-8 chars well
    # under Windows but the value is correct
    msgs_direct <- data.frame(
      msgid            = msgid_direct_matches[, 2],
      msgstr           = msgstr_direct_matches[, 2],
      stringsAsFactors = FALSE
    )
  } else
  {
    msgs_direct <- data.frame(
      msgid            = character(),
      msgstr           = character(),
      stringsAsFactors = FALSE
    )
  }

  if(is_non_empty(msgid_plural_index))
  {
    msgid_singular_matches = stri_match_first_regex(
      lines[msgid_singular_index],
      '^msgid +"(.+)"$'
    )

    msgid_plural_matches = stri_match_first_regex(
      lines[msgid_plural_index],
      '^msgid_plural +"(.+)"$'
    )

    msgstr_countable_index <- which(stri_detect_regex(lines, "^msgstr\\["))

    msgstr_countable_matches = stri_match_first_regex(
      lines[msgstr_countable_index],
      '^msgstr(?:\\[([0-9]+)\\])? +"(.*)"$'
    )
    msgstr_countable_grp <- cumsum(msgstr_countable_matches[, 2] == "0")

    msgs_countable <- data.frame(
      msgid            = msgid_singular_matches[, 2],
      msgid_plural     = msgid_plural_matches[, 2],
      stringsAsFactors = FALSE
    )
    msgs_countable$msgstr <- split(msgstr_countable_matches[, 3], msgstr_countable_grp)
  } else
  {
    msgs_countable <- data.frame(
      msgid            = character(),
      msgid_plural     = character(),
      msgstr           = character(),
      stringsAsFactors = FALSE
    )
  }

  structure(
    list(
      source_type = source_type,
      file_type = file_type,
      metadata = metadata,
      direct = msgs_direct,
      countable = msgs_countable
    ),
    class = c("po", "list")
  )
}
