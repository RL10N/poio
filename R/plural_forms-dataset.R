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
NULL
