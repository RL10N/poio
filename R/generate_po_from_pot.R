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

#' @rdname generate_po_from_pot
#' @export
generate_po_from_pot.po <- function(x, lang, ...)
{
  check_language(lang)
  x$file_type <- "pot"
  plural_forms <- lookup_plural_forms_for_language(lang)
  if("Language" %in% x$metadata$name)
  {
    # Hmm. Should maybe write this using dplyr at some point.
    x$metadata$value[x$metadata$name == "Language"] <- lang
  } else
  {
    x$metadata <- rbind(
      x$metadata,
      data.frame(name = "Language", value = lang, stringsAsFactors = FALSE)
    )
  }
  if("Plural-Forms" %in% x$metadata$name)
  {
    x$metadata$value[x$metadata$name == "Plural-Forms"] <- plural_forms
  } else
  {
    x$metadata <- rbind(
      x$metadata,
      data.frame(
        name = "Plural-Forms",
        value = plural_forms,
        stringsAsFactors = FALSE
      )
    )
  }
  x
}

