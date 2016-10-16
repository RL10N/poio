RX <- list(
  metadata                 = '^"([a-zA-Z-]+): *(.+)\\\\n"$',
  msgid                    = '^msgid *"(.+)"$',
  msgid_plural             = '^msgid_plural *"(.+)"$',
  msgstr_direct            = '^msgstr *" *(.*)"$',
  msgstr_countable         = '^msgstr(?:\\[([0-9]+)\\])? *"(.*)"$',
  translator_comment       = "^#[^\\p{P}] *(.+)$", # hash, then not punctuation
  source_reference_comment = "^#: *(.+)$",
  flags_comment            = "^#, *(.+)$",
  previous_string_comment  = "^#\\| *(.+)$",
  obsolete_comment         = "^#~ *(.+)$",
  blank                    = "^ *$"
)

#' @importFrom magrittr extract
#' @importFrom stringi stri_match_first_regex
match_and_extract <- function(x, rx)
{
  x %>%
    stri_match_first_regex(rx) %>%
    extract(!is.na(.[, 2]), 2L)
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
#' \item{metadata}{A data frame of file metadata with columns "name" and
#' "value".}
#' \item{direct}{A data frame of messages with a direct translation, with
#' columns "msgid" and "msgstr".}
#' \item{countable}{A data frame of messages where the translation depends upon
#' a countable value (as created by \code{ngettext}), with columns "msgid",
#' "msgid_plural" and "msgstr".  The latter column contains a list of character
#' vectors.}
#' }
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

  # Remove empty groups
  line_groups <- line_groups[
    bapply(line_groups, is_non_empty)
  ]

  # And remove those empty lines
  line_groups <- line_groups %>%
    lapply(
      function(x)
        x[!stri_detect_regex(x, RX$blank)]
    )

  all_msgs <- line_groups %>%
    lapply(
    function(lines)
    {

      msgid <- match_and_extract(lines, RX$msgid)
      # Where there is an obsolete comment, no msgids are possible
      # Currently the implementation allows for zero msgids under all
      # circumstances which is possibly too lenient, but the spec is unclear.
      if(length(msgid) > 1)
      {
        stop(
          "There should be exactly one msgid found in each block. Are you missing a blank line?\nThe offending lines are:\n",
          paste(lines, collapse = "\n")
        )
      }

      comments <- data_frame(
        translator_comments = list(match_and_extract(lines, RX$translator_comment)),
        source_reference_comments = list(match_and_extract(lines, RX$source_reference_comment)),
        flags_comments = list(match_and_extract(lines, RX$flags_comment)),
        previous_string_comments = list(match_and_extract(lines, RX$previous_string_comment)),
        obsolete_comments = list(match_and_extract(lines, RX$obsolete_comment))
      )
      msgid_plural <- match_and_extract(lines, RX$msgid_plural)
      if(is_empty(msgid_plural)) # direct msg
      {
        msgstr <- match_and_extract(lines, RX$msgstr_direct)
        list(
          msg_type = "direct",
          msgs = data_frame(msgid  = msgid, msgstr = msgstr) %>%
            bind_cols(comments)
        )
      } else # countable msg
      {
        # Can't do
        # msgstr <- list(match_and_extract(lines, RX$msgstr_countable))
        # since the logic is a little more complicated
        msgstr_and_id <- lines %>%
          stri_match_first_regex(RX$msgstr_countable) %>%
          extract(!is.na(.[, 2L]), 2:3)
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
            msgstr = msgstr
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
    )
  msgs_countable <- all_msgs[!is_direct] %>%
    lapply(
      function(x)
      {
        x$msgs
      }
    )

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
