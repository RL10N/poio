#' Get the number of plural forms from a PO file
#'
#' Gets the number of plural forms specified in the"Plural-Forms" metadata
#' element of a PO file.
#' @param x A \code{po} object or its metadata element.
#' @param default An integer to return if the number of plural forms cannot be
#'   determined.
#' @return An integer of the number of plural forms for the language defined in
#'   the PO file.
#' @note POT files are not language-specific and don't have a "Plural-Forms"
#'   metadata element. By convention, they are considered to have 2 plural
#'   forms, since that is how many pulral forms there are in English.
#' @export
get_n_plural_forms <- function(x, ...)
{
  UseMethod("get_n_plural_forms")
}

#' @export
get_n_plural_forms.po <- function(x, default = 2L, ...)
{
  get_n_plural_forms(x$metadata)
}

#' @importFrom assertive.properties is_empty
#' @importFrom dplyr filter_
#' @importFrom dplyr select_
#' @importFrom magrittr %>%
#' @importFrom magrittr extract2
#' @importFrom stringi stri_match_first_regex
#' @export
get_n_plural_forms.data.frame <- function(x, default = 2L, ...)
{
  plural_forms <- x %>%
    filter_(~ name == "Plural-Forms") %>%
    select_(~ value) %>%
    extract2(1)
  if(is_empty(plural_forms)) # e.g., for POT files
  {
    return(default)
  }
  if(length(plural_forms) > 1)
  {
    warning("There are multiple plural-forms fields. Using the first one.")
    plural_forms <- plural_forms[1]
  }
  n_plural_forms <- plural_forms %>%
    stri_match_first_regex("nplurals *= *([0-9])") %>%
    extract(, 2) %>%
    as.integer
  if(is.na(n_plural_forms))
  {
    warning("The plural-forms metadata field is badly formed.")
    return(default)
  }
  n_plural_forms
}
