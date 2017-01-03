#' Summarize a PO object
#'
#' \code{\link[base]{summary}} method for PO objects. \code{summary} shows less
#' information than \code{print}.
#' @param x An object of class \code{po}.
#' @param ... Arguments passed to \code{\link[base]{print.data.frame}}.
#' @return The \code{x} argument in invisibly returned, but the function is
#' mostly invoked for the side-effect of printing the \code{x} argument.
#' @examples
#' pot_file <- system.file("extdata/R-summerof69.pot", package = "poio")
#' summary(pot <- read_po(pot_file))
#' @export
summary.po <- function(x, ...)
{
  n_direct <- nrow(x$direct)
  n_countable <- nrow(x$countable)
  cat(
    sprintf(
      "A %s %s translation object from %s code containing\n  %d direct %s (%d non-obsolete) and\n  %d countable %s (%d non-obsolete).\n",
      x$file_type,
      switch(x$file_type, pot = "master", po = "language-specific"),
      x$source_type,
      n_direct,
      ngettext(n_direct, "message", "messages"),
      sum(!x$direct$is_obsolete),
      n_countable,
      ngettext(n_countable, "message", "messages"),
      sum(!x$countable$is_obsolete)
    )
  )
  cat("\nSelected Metadata:\n")
  x$metadata %>%
    filter_(~ name %in% c("Project-Id-Version", "PO-Revision-Date", "Last-Translator")) %>%
    as.data.frame() %>%
    print(...)
  cat("\nNon-obsolete Direct Translations:\n")
  x$direct %>%
    filter_(~ !is_obsolete) %>%
    select_(~ msgid, ~ msgstr) %>%
    as.data.frame() %>%
    print(...)
  cat("\nNon-obsolete Countable Translations:\n")
  x$countable %>%
    filter_(~ !is_obsolete) %>%
    select_(~ msgid, ~ msgid_plural, ~ msgstr) %>%
    as.data.frame() %>%
    print(...)
  invisible(x)
}
