#' Write a PO file
#'
#' Writes and object of class \code{po} to a .po file.
#' @param po An object of class \code{po}.
#' @param po_file A path to the po_file to be written, or NULL to automatically
#' generate the path.
#' @param ... Passed between methods. Not currently used.
#' @return The funcion is mostly invoked for the side-effect of writing a PO
#' file.  The \code{po} argument is also invisibly returned, for convenience
#' when this function is used in a pipe chain.
#' @export
write_po <- function(po, po_file = NULL, ...)
{
  UseMethod("write_po")
}

#' @importFrom stringi stri_extract_first_regex
#' @importFrom stringi stri_write_lines
#' @export
write_po.po <- function(po, po_file = NULL, ...)
{
  lines <- with(
    po,
    {
      c(
        paste0("# ", initial_comments),
        'msgid ""',
        'msgstr ""',
        paste0('"', metadata$name, ': ', metadata$value, '\\n"'),
        '',
        unlist(
          apply(
            direct,
            1,
            function(row)
            {
              c(
                paste0(
                  rep_len('# ', length(row$translator_comments)),
                  row$translator_comments
                ),
                paste0(
                  rep_len('#: ', length(row$source_reference_comments)),
                  row$source_reference_comments
                ),
                paste0(
                  rep_len('#, ', length(row$flags_comments)),
                  row$flags_comments
                ),
                paste0(
                  rep_len('#| ', length(row$previous_string_comments)),
                  row$previous_string_comments
                ),
                paste0(
                  rep_len('msgctxt "', length(row$msgctxt)),
                  row$msgctxt,
                  rep_len('"', length(row$msgctxt))
                ),
                paste0(if(row$is_obsolete) '#~ ', 'msgid "', row$msgid, '"'),
                paste0(if(row$is_obsolete) '#~ ','msgstr "', row$msgstr, '"'),
                ''
              )
            }
          ),
          use.names = FALSE
        ),
        unlist(
          apply(
            countable,
            1,
            function(row)
            {
              n_plurals <- length(row$msgstr)
              c(
                paste0(
                  rep_len('# ', length(row$translator_comments)),
                  row$translator_comments
                ),
                paste0(
                  rep_len('#: ', length(row$source_reference_comments)),
                  row$source_reference_comments
                ),
                paste0(
                  rep_len('#, ', length(row$flags_comments)),
                  row$flags_comments
                ),
                paste0(
                  rep_len('#| ', length(row$previous_string_comments)),
                  row$previous_string_comments
                ),
                paste0(if(row$is_obsolete) '#~ ', 'msgid "', row$msgid, '"'),
                paste0(
                  if(row$is_obsolete) '#~ ', 'msgid_plural "',
                  row$msgid_plural, '"'
                ),
                paste0(
                  if(row$is_obsolete) '#~ ', "msgstr[",
                  seq(0L, n_plurals - 1L), '] "', row$msgstr, '"'
                ),
                ''
              )
            }
          ),
          use.names = FALSE
        )
      )
    }
  )

  if(is.null(po_file)) # auto-generate file name
  {
    # POT files don't have a Language element in the metadata, but PO files do
    lang <- po$metadata["Language"]
    if(po$file_type == "pot")
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
