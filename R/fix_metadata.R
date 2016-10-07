#' @export
fix_metadata <- function(x, pkg = ".", ...)
{
  UseMethod("fix_metadata")
}

#' @export
fix_metadata.po <- function(x, pkg = ".", ...)
{
  x$metadata <- fix_metadata(x$metadata, pkg = pkg, file_type = x$file_type)
  x
}

#' @importFrom assertive.base coerce_to
#' @importFrom devtools as.package
#' @importFrom magrittr %>%
#' @export
fix_metadata.data.frame <- function(x, pkg = ".", file_type, ...)
{
  if(is.character(pkg))
  {
    pkg <- as.package(pkg)
  }
  x <- coerce_to(x, "data.frame")
  x <- x %>%
    fix_metadata_columns() %>%
    fix_metadata_rows(file_type = file_type) %>%
    fix_metadata_project_id_version(pkg) %>%
    fix_report_msgid_bugs_to(pkg) %>%
    fix_po_revision_date() %>%
    fix_mime_version() %>%
    fix_content_type() %>%
    fix_content_transfer_encoding()
  x
}

fix_metadata_columns <- function(x)
{
  required_columns <- c("name", "value")
  for(column in required_columns)
  {
    if(is.null(x[[column]]))
    {
      msg <- gettextf(
        "Adding the missing column %s to the metadata.",
        sQuote(column)
      )
      message(msg)
      x[[column]] <- character()
    }
  }
  x
}

fix_metadata_rows <- function(x, file_type = c("po", "pot"))
{
  file_type <- match.arg(file_type)
  required_rows <- c(
    "Project-Id-Version", "Report-Msgid-Bugs-To", "POT-Creation-Date",
    "PO-Revision-Date", "Last-Translator", "Language-Team",
    "MIME-Version", "Content-Type", "Content-Transfer-Encoding"
  )
  if(file_type == "po")
  {
    required_rows <- c(required_rows, "Language", "Plural-Forms")
  }
  for(row in required_rows)
  {
    if(!row %in% x[["name"]])
    {
      msg <- gettextf(
        "Adding the missing row %s to the metadata.",
        sQuote(row)
      )
      message(msg)
      x <- rbind(x, data.frame(name = row, value = character(1)))
    }
  }
  x
}

fix_metadata_project_id_version <- function(x, pkg)
{
  # Don't use with fn here since it throws an error if the fields don't exist
  # We want to use the warning mechanism in fix_field instead.
  # Notice that "package" and "version" are lowercase in the indexing since
  # devtools::as.package converts them, but uppercase in desc_fields since
  # those are the originals in the DESCRIPTION file.
  expected <- paste(pkg[["package"]], pkg[["version"]])
  fix_field(x, pkg, "Project-Id-Version", expected, c("Package", "Version"))
}

fix_report_msgid_bugs_to <- function(x, pkg)
{
  fix_field(x, pkg, "Report-Msgid-Bugs-To", pkg[["bugreports"]], "BugReports")
}

fix_po_revision_date <- function(x)
{
  fix_field(x, NA, "PO-Revision-Date", expected = format(Sys.time(), "%Y-%m-%d %H:%M:%S%z"))
}

fix_mime_version <- function(x)
{
  fix_field(x, NA, "MIME-Version", expected = "1.0")
}

fix_content_type <- function(x)
{
  fix_field(x, NA, "Content-Type", expected = "text/plain; charset=UTF-8")
}

fix_content_transfer_encoding <- function(x)
{
  fix_field(x, NA, "Content-Transfer-Encoding", expected = "8bit")
}

fix_language <- function(x, lang)
{
  fix_field(x, NA, "Language", lang)
}

fix_plural_forms <- function(x, lang)
{
  expected <- lookup_plural_forms_for_language(lang)
  fix_field(x, NA, "Plural-Forms", expected)
}

#' @importFrom assertive.base bapply
fix_field <- function(x, pkg, po_field, expected, desc_fields = character())
{
  # If user forced pkg = NULL, don't do anything
  if(!missing(pkg) && is.null(pkg))
  {
    msg <- gettextf(
      "No package data available; not fixing the %s field.",
      po_field
    )
    message(msg)
    return(x)
  }
  # If fields from the DESCRIPTION file are needed, check that they exist,
  # and warn otherwise.
  if(length(desc_fields) > 0L)
  {
    bad <- bapply(pkg[tolower(desc_fields)], is.null)
    if(any(bad))
    {
      wrn <- gettextf(
        "The package DESCRIPTION file does not have these required fields: %s.\nPlease add these fields to DESCRIPTION and re-run this function, or manually fix the %s field in your PO or POT file.",
        toString(sQuote(desc_fields[bad])),
        po_field
      )
      warning(wrn)
      return(x)
    }
  }
  # Update field, if necessary
  actual <- x[x$name == po_field, "value"]
  if(actual != expected)
  {
    msg <- gettextf(
      "Updating the %s to %s.",
      po_field,
      sQuote(expected)
    )
    message(msg)
    x[x$name == po_field, "value"] <- expected
  }
  x
}
