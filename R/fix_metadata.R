fix_metadata <- function(x, ...)
{
  UseMethod("fix_metadata")
}

fix_metadata.po <- function(x, ...)
{
  x$metadata <- fix_metadata(x$metadata)
  x
}

#' @importFrom assertive.base coerce_to
fix_metadata.data.frame <- function(x, ...)
{
  x <- coerce_to(x, "data.frame")
  x <- check_metadata_columns(x)
  x <- check_metadata_rows(x)
  x
}

check_metadata_columns <- function(x)
{
  REQUIRED_COLUMNS <- c("name", "value")
  for(column in REQUIRED_COLUMNS)
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

check_metadata_rows <- function(x)
{
  REQUIRED_ROWS <- c(
    "Project-Id-Version", "Report-Msgid-Bugs-To", "POT-Creation-Date",
    "PO-Revision-Date", "Last-Translator", "Language-Team", "Language",
    "MIME-Version", "Content-Type", "Content-Transfer-Encoding", "Plural-Forms"
  )
  for(row in REQUIRED_ROWS)
  {
    if(is.null(x[[row]]))
    {
      msg <- gettextf(
        "Adding the missing row %s to the metadata.",
        sQuote(row)
      )
      message(msg)
      x <- rbind(x, data.frame(name = row, value = character()))
    }
  }
  x
}

check_metadata_project_id_version <- function(x, pkg_desc)
{
  expected <- with(pkg_desc, paste(Package, Version))
  actual <- x[x$name == "Project-Id-Version", "value"]
  if(actual != expected)
  {
    msg <- gettextf(
      "Updating the Project-Id-Version to %s.",
      sQuote(expected)
    )
    message(msg)
    x[x$name == "Project-Id-Version", "value"] <- expected
  }
  x
}
