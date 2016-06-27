#' Write a PO file
#'
#' Writes and object of class \code{po} to a .po file.
#' @param po An object of class \code{po}.
#' @param po_file A path to the po_file to be written, or NULL to automatically
#' generate the path.
#' @param ... PAssed between methods. Not currently used.
#' @return The funcion is mostly invoked for the side-effect of writing a PO
#' file.  The lines that are written to file are also invisibly returned.
#' @export
write_po_file <- function(po, po_file = NULL, ...)
{
  UseMethod("write_po_file")
}

#' @importFrom stringi stri_extract_first_regex
#' @importFrom stringi stri_write_lines
#' @export
write_po_file.po <- function(po, po_file = NULL, ...)
{
  lines <- with(
    po,
    {
      c(
        'msgid ""',
        'msgstr ""',
        paste0(names(metadata), ': ', metadata, '\\n'),
        '',
        as.character(
          apply(direct, 1, function(row) c(row[1], row[2], ''))
        ),
        as.character(
          apply(countable, 1, function(row) c(row[[1]], row[[2]], row[[3]], ''))
        )
      )
    }
  )
  if(is.null(po_file))
  {
    # POT files don't have a Language element in the metadata, but PO files do
    lang <- po$metadata["Language"]
    if(is.na(lang))
    {
      # Use pkg name instead of language
      lang <- stri_extract_first_regex(
        po$metadata["Project-Id-Version"],
        "^[a-zA-Z0-9._]+"
      )
      file_ext <- ".pot"
    } else
    {
      file_ext <- ".po"
    }
    po_file <- file.path("po", paste0(if(po$type == "r") "R-", lang, file_ext))
    message("Writing to ", po_file)
  }
  stri_write_lines(lines, po_file)
  invisible(lines)
}
