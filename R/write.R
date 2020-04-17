#' Write a PO file
#'
#' Writes and object of class \code{po} to a .po file.
#' @param po An object of class \code{po}.
#' @param po_file A path to the po_file to be written, or NULL to automatically
#' generate the path.
#' @param ... Passed between methods. Not currently used.
#' @return The function is mostly invoked for the side-effect of writing a PO
#' file.  The \code{po} argument is also invisibly returned, for convenience
#' when this function is used in a pipe chain.
#' @examples
#' pot_file <- system.file("extdata/R-summerof69.pot", package = "poio")
#' pot <- read_po(pot_file)
#' write_po(pot, stdout())
#' @export
write_po <- function(po, po_file = NULL, ...)
{
  UseMethod("write_po")
}

#' @importFrom stringi stri_extract_first_regex
#' @importFrom stringi stri_replace_all_fixed
#' @importFrom stringi stri_write_lines
#' @export
write_po.po <- function(po, po_file = NULL, ...)
{
  n_plurals <- get_n_plural_forms(po)
  # with(r6obj, {}) is broken. See
  # https://github.com/wch/R6/issues/111
  # Need to explicitly write po$
  lines <- c(
    initial_comments_to_lines(po$initial_comments),
    'msgid ""',
    'msgstr ""',
    metadata_to_lines(po$metadata),
    '',
    direct_msgs_to_lines(po$direct),
    countable_msgs_to_lines(po$countable, n_plurals)
  )

  if(is.null(po_file)) # auto-generate file name
  {
    po_file <- generate_po_file_name(po)
    message("Writing to ", po_file)
  }
  # Sanitize \n inside msgs
  lines <- stri_replace_all_fixed(lines, "\n", "\\n")
  # stri_write_lines is faster, but doesn't support writing to text connections
  if(inherits(po_file, "connection"))
  {
    writeLines(lines, po_file)
  } else
  {
    stri_write_lines(lines, po_file)
  }
  invisible(po)
}

enclose_text <- function(prefix = "", x, suffix = "") {
  paste0(
    rep_len(prefix, length(x)),
    x,
    rep_len(suffix, length(x))
  )
}

initial_comments_to_lines <- function(initial_comments)
{
  enclose_text("# ", initial_comments)
}

metadata_to_lines <- function(metadata)
{
  if(nrow(metadata) == 0L)
  {
    return(character())
  }
  paste0('"', metadata$name, ': ', metadata$value, '\n"')
}

#' @importFrom dplyr rowwise
#' @importFrom dplyr transmute
#' @importFrom magrittr %>%
#' @importFrom dplyr .data
direct_msgs_to_lines <- function(direct)
{
  if(nrow(direct) == 0L)
  {
    return(character())
  }
  direct %>%
    rowwise() %>%
    transmute(
      output = list(
        c(
          enclose_text("# ", .data$translator_comments),
          enclose_text("#: ", .data$source_reference_comments),
          enclose_text("#, ", .data$flags_comments),
          enclose_text("#| ", .data$previous_string_comments),
          enclose_text('msgctxt "', .data$msgctxt, '"'),
          paste0(if(.data$is_obsolete) '#~ ', 'msgid "', .data$msgid, '"'),
          paste0(if(.data$is_obsolete) '#~ ', 'msgstr "', .data$msgstr, '"'),
          ''
        )
      )
    ) %>%
    unlist(use.names = FALSE)
}

#' @importFrom dplyr rowwise
#' @importFrom dplyr transmute
#' @importFrom magrittr %>%
#' @importFrom dplyr .data
countable_msgs_to_lines <- function(countable, n_plurals)
{
  if(nrow(countable) == 0L)
  {
    return(character())
  }
  countable %>%
    rowwise() %>%
    transmute(
      output = list(
        c(
          enclose_text("# ", .data$translator_comments),
          enclose_text("#: ", .data$source_reference_comments),
          enclose_text("#, ", .data$flags_comments),
          enclose_text("#| ", .data$previous_string_comments),
          enclose_text('msgctxt "', .data$msgctxt, '"'),
          paste0(if(.data$is_obsolete) '#~ ', 'msgid "', .data$msgid, '"'),
          paste0(
            if(.data$is_obsolete) '#~ ', 'msgid_plural "',
            .data$msgid_plural, '"'
          ),
          paste0(
            if(.data$is_obsolete) '#~ ', "msgstr[",
            seq(0L, n_plurals - 1L), '] "', .data$msgstr, '"'
          ),
          ''
        )
      )
    ) %>%
    unlist(use.names = FALSE)
}

#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom magrittr extract2
#' @importFrom stringi stri_extract_first_regex
#' @importFrom dplyr .data
generate_po_file_name <- function(po)
{
  # POT files don't have a Language element in the metadata, but PO files do
  if(po$file_type == "pot")
  {
    # Use pkg name instead of language
    lang <- po$metadata %>%
      filter(.data$name == "Project-Id-Version") %>%
      extract2("value") %>%
      stri_extract_first_regex("^[a-zA-Z0-9._]+")
    file_ext <- ".pot"
  } else # po$file_type == "po"
  {
    lang <- po$metadata %>%
      filter(.data$name == "Language") %>%
      extract2("value")
    file_ext <- ".po"
  }
  file.path("po", paste0(if(po$source_type == "r") "R-", lang, file_ext))
}
