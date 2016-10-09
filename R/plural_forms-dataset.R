#' Plural Forms Data
#'
#' This dataset contains the known values for the "Plural-Forms" metadata
#' elements in a PO file.
#'
#' @format
#' \code{plural_forms} is a data frame with 3 character columns and 142 rows:
#' \describe{
#' \item{ISO}{The ISO 639 specification of the language.  Usually this is the
#' two letter ISO 639-1 name, but some three letter ISO 639-2 names are also
#' included, for cases where no two letter name exists.}
#' \item{EnglishName}{How the language is known in English.}
#' \item{PluralFormHeader}{The GNU gettext specification of how plural forms
#' are specified in the language.}
#' }
#' @references This dataset was originally taken from
#' \url{http://localization-guide.readthedocs.org/en/latest/l10n/pluralforms.html}
#'
#' ISO 639-1 (a.k.a. ISO 629-2 alpha-2) and ISO 639-2 language codes can be
#' found in the \code{\link[ISOcodes]{ISO_639_2}} help page, or online at
#' \url{http://www.loc.gov/standards/iso639-2/php/code_list.php}
#' @docType data
#' @keywords datasets
#' @name plural_forms
#' @usage data(plural_forms)
#' @examples
#' e <- new.env()
#' data(plural_forms, package = "poio", envir = e)
#' e$plural_forms
NULL

#' @importFrom utils data
#' @importFrom stringi stri_detect_fixed
#' @importFrom stringi stri_extract_first_regex
lookup_plural_forms_for_language <- function(lang)
{
  e <- new.env()
  data(plural_forms, package = "poio", envir = e)
  is_dialect <- stri_detect_fixed(lang, "_")
  base_lang <- stri_extract_first_regex(lang, "^[a-z]{2,3}")
  if(lang %in% e$plural_forms$ISO)
  {
    if(lang == "zh")
    {
      message('For Chinese, the plural form is set to "nplurals=1; plural=0;". However, in rare cases related to personal pronouns, there are two plural forms.  If this is applicable to your content, set it to "nplurals=2; plural=(n > 1);".')
    }
    with(e$plural_forms, PluralFormHeader[ISO == lang])
  } else if(is_dialect && base_lang %in% e$plural_forms$ISO)
  {
    msg <- gettextf(
      "Using the plural forms for the generic language, %s.",
      sQuote(base_lang)
    )
    message(msg)
    with(e$plural_forms, PluralFormHeader[ISO == base_lang])
  } else
  {
    msg <- gettextf(
      "The plural form for language %s is unknown, and cannot be updated.",
      sQuote(lang)
    )
    message(msg)
    NA_character_
  }
}
