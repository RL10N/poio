#' Print a PO object
#'
#' \code{\link[base]{print}} method for PO objects.
#' @param x An object of class \code{po}.
#' @param ... Arguments passed to \code{\link[tibble]{print.tbl_df}}.
#' @return The \code{x} argument in invisibly returned, but the function is
#' mostly invoked for the side-effect of printing the \code{x} argument.
#' @seealso \code{\link{summary.po}}
#' @examples
#' pot_file <- system.file("extdata/R-summerof69.pot", package = "poio")
#' print(pot <- read_po(pot_file))
#'
#' # Use width = Inf to print all columns in metadata, direct, and countable
#' print(pot, width = Inf)
#' @export
print.po <- function(x, ...)
{
  types <- structure(
    list(
      "Source Type:" = paste(x$source_type, "code"),
      "File Type:"   = paste(
        x$file_type,
        switch(x$file_type, pot = "master", po = "language-specific"),
        "translation"
      )
    ),
    class = "simple.list"
  )
  print(types)
  if(length(x$initial_comments) > 0L)
  {
    cat("\nInitial comments:\n")
    print(noquote(x$initial_comments))
  }
  cat("\nMetadata:\n")
  print(x$metadata, ...)
  cat("\nDirect Translations:\n")
  print(x$direct, ...)
  cat("\nCountable Translations:\n")
  print(x$countable, ...)
  invisible(x)
}
