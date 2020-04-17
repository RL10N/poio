#' Summarize a PO object
#'
#' \code{\link[base]{summary}} method for PO objects. \code{summary} shows less
#' information than \code{print}.
#' @param object An object of class \code{po}.
#' @param ... Arguments passed to \code{\link[base]{print.data.frame}}.
#' @return The \code{object} argument in invisibly returned, but the function is
#' mostly invoked for the side-effect of printing the \code{object} argument.
#' @seealso \code{\link{print.po}}
#' @examples
#' pot_file <- system.file("extdata/R-summerof69.pot", package = "poio")
#' summary(pot <- read_po(pot_file))
#' @export
summary.po <- function(object, ...)
{
  n_direct <- nrow(object$direct)
  n_countable <- nrow(object$countable)
  cat(
    sprintf(
      "A %s %s translation object from %s code containing\n  %d direct %s (%d non-obsolete) and\n  %d countable %s (%d non-obsolete).\n",
      object$file_type,
      switch(object$file_type, pot = "master", po = "language-specific"),
      object$source_type,
      n_direct,
      ngettext(n_direct, "message", "messages"),
      sum(!object$direct$is_obsolete),
      n_countable,
      ngettext(n_countable, "message", "messages"),
      sum(!object$countable$is_obsolete)
    )
  )
  cat("\nSelected Metadata:\n")
  object$metadata %>%
    filter(.data$name %in% c("Project-Id-Version", "PO-Revision-Date", "Last-Translator")) %>%
    as.data.frame() %>%
    print(...)
  cat("\nNon-obsolete Direct Translations:\n")
  object$direct %>%
    filter(!.data$is_obsolete) %>%
    select(.data$msgid, .data$msgstr) %>%
    as.data.frame() %>%
    print(...)
  cat("\nNon-obsolete Countable Translations:\n")
  object$countable %>%
    filter(!.data$is_obsolete) %>%
    select(.data$msgid, .data$msgid_plural, .data$msgstr) %>%
    as.data.frame() %>%
    print(...)
  invisible(object)
}
