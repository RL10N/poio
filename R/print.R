#' Print a PO object
#'
#' \code{\link[base]{print}} method for PO objects.
#' @param x An object of class \code{po}.
#' @param ... Arguments passed to \code{\link[tibble]{print.tbl_df}}.
#' @return The \code{x} argument in invisibly returned, but the function is
#' mostly invoked for the side-effect of printing the \code{x} argument.
#' @examples
#' pot_file <- system.file("extdata/R-summerof69.pot", package = "poio")
#' print(pot <- read_po(pot_file))
#'
#' # Use width = Inf to print all columns in metadata, direct, and countable
#' print(pot, width = Inf)
#' @export
print.po <- function(x, ...)
{
  cat(
    sprintf(
      "A %s translation object from %s code\n  containing %d direct messages and %d countable messages.\n",
      switch(x$file_type, pot = "master", po = "language-specific"),
      x$source_type,
      nrow(x$direct),
      nrow(x$countable)
    )
  )
  cat("\nSelected Metadata:\n")
  x$metadata %>%
    filter_(~ name %in% c("Project-Id-Version", "PO-Revision-Date", "Last-Translator")) %>%
    as.data.frame() %>%
    print(...)
  cat("\nDirect Translations:\n")
  x$direct %>%
    select_(~ msgid, ~ msgstr) %>%
    as.data.frame() %>%
    print(...)
  cat("\nCountable Translations:\n")
  x$countable %>%
    select_(~ msgid, ~ msgid_plural, ~ msgstr) %>%
    as.data.frame() %>%
    print(...)
  invisible(x)
}
