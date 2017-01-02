#' Print a PO object
#'
#' \code{\link[base]{print}} method for PO objects.
#' @param x An object of class \code{po}.
#' @param ... Arguments passed to \code{\link[tibble]{print.tbl_df}}.
#' @return The \code{x} argument in invisibly returned, but the function is
#' mostly invoked for the side-effect of printing the \code{x} argument.
#' @examples
#'
#' @export
print.po <- function(x, ...)
{
  cat(
    sprintf(
      "A %s translation object containing %d direct messages and %d countable messages from %s code.\n",
      switch(x$file_type, pot = "master", po = "language-specific"),
      nrow(x$direct),
      nrow(x$countable),
      x$source_type
    )
  )
  if(length(x$initial_comments) > 0L)
  {
    cat("\nInitial comments:\n")
    cat(x$initial_comments, sep = "\n")
  }
  cat("\nMetadata:\n")
  print(x$metadata, ...)
  cat("\nDirect Translations:\n")
  print(x$direct, ...)
  cat("\nCountable Translations:\n")
  print(x$countable, ...)
  invisible(x)
}
