#' Generate a PO object from a POT object
#'
#' Generates a PO object from a POT object.
#' @param x An object of class \code{po}, as read by \code{\link{read_po}} from
#' a POT file.
#' @param lang A language code, possibly with a country code attached. See
#' \code{\link{language_codes}} for possible values.
#' @param ... Currently unused.
#' @return An object of class \code{po}, ready to be written to a PO file by
#' \code{\link{write_po}}.
#' @details The \code{file_type} element is changed from "pot" to "po", and
#' "Language" and "Plural-Forms" values are added to the metadata element.
#' @note If the plural form is unknown for the specified language, the plural
#' form is set to \code{NA}.  See \code{\link{plural_forms}} for supported
#' languages.
#' @export
generate_po_from_pot <- function(x, lang, ...)
{
  UseMethod("generate_po_from_pot")
}

#' @importFrom dplyr bind_rows
#' @importFrom magrittr %>%
#' @importFrom tibble data_frame
#' @rdname generate_po_from_pot
#' @export
generate_po_from_pot.po <- function(x, lang, ...)
{
  check_language(lang)
  x$file_type <- "po"
  plural_forms <- lookup_plural_forms_for_language(lang)
  if("Language" %in% x$metadata$name)
  {
    # Don't bother with dplyr here. The code isn't clearer.
    # x$metadata <- x$metadata %>%
    #   mutate(value = ~ ifelse(name == "Language", lang, value))
    x$metadata$value[x$metadata$name == "Language"] <- lang
  } else
  {
    x$metadata <- x$metadata %>%
      bind_rows(data_frame(name = "Language", value = lang))
  }
  if("Plural-Forms" %in% x$metadata$name)
  {
    x$metadata$value[x$metadata$name == "Plural-Forms"] <- plural_forms
  } else
  {
    x$metadata <- x$metadata %>%
      bind_rows(data_frame(name = "Plural-Forms", value = plural_forms))
  }
  x
}

