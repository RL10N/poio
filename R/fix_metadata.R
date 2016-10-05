fix_metadata <- function(x, ...)
{
  UseMethod("fix_metadata")
}

fix_metadata.po <- function(x, ...)
{
  x$metadata <- fix_metadata(x$metadata)
  x
}

fix_metadata.data.frame <- function(x, ...)
{

}
