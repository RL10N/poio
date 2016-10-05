#' Read PO and POT files
#'
#' Reads .PO and .POT translation files.
#' @param po_file A string giving a path to a PO file.
#' @param pot_file A string giving a path to a POT file, or \code{NULL} to
#' autogenerate it.
#' @return An object of class \code{po}, which is a list with the following
#' components:
#' \describe{
#' \item{type}{Either "r" or "c".  Guessed from the file name.}
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
#' @importFrom assertive.properties is_non_empty
#' @importFrom stringi stri_read_lines
#' @importFrom stringi stri_detect_regex
#' @importFrom stringi stri_match_first_regex
#' @export
read_po_file <- function(po_file)
{
  lines <- stri_read_lines(po_file, "UTF-8")

  msg_type <- ifelse(
    substring(basename(po_file), 1, 2) == "R-", "r", "c"
  )

  metadata_lines <- lines[stri_detect_regex(lines, '^"')]
  metadata <- stri_match_first_regex(metadata_lines, '^"([a-zA-Z-]+): ?(.+)\\\\n"$')
  metadata <- data.frame(name = metadata[, 2], value = metadata[, 3])

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
      type = msg_type,
      metadata = metadata,
      direct = msgs_direct,
      countable = msgs_countable
    ),
    class = c("po", "list")
  )
}

#' @rdname read_po_file
#' @export
read_pot_file <- function(pot_file = NULL)
{
  if(is.null(pot_file))
  {
    pot_files <- dir("po", pattern = "\\.pot$", full.names = TRUE)
    if(is_non_empty(pot_files))
    {
      pot_file <- pot_files[1]
      message("Reading ", pot_file)
    }
  }
  read_po_file(pot_file)
}
