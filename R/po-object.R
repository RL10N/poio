#' Create an object of class po
#'
#' Creates an object of class \code{po}, for storing package translations.
#' @param source_type Either "r" or "c", depending upon whether the messages
#' originated from R-level code, or C-level code.
#' @param file_type Either "po" or "pot", depending upon whether the messages
#' originated from a PO (language-specific) or POT (master translation) file.
#' @param initial_comments A character vector of comments added by the
#' translator.
#' @param metadata A \code{\link[tibble]{data_frame}} of file metadata with
#' columns "name" and "value".
#' @param direct A \code{\link[tibble]{data_frame}} of messages with a direct
#' translation, as created by \code{\link[base]{stop}},
#' \code{\link[base]{warning}}, \code{\link[base]{message}} or
#' \code{\link[base]{gettext}}; its columns are described below.
#' @param countable A data frame of messages where the translation depends upon
#' a countable value, as created by \code{\link[base]{ngettext}}; its columns are
#' described below.
#' @return An \code{\link[R6]{R6}} object of class \code{po}.
#' @note #' The \code{direct} element of the \code{po} object has the following columns.
#' \describe{
#' \item{msgid}{Character. The untranslated (should be American English)
#' message.}
#' \item{msgstr}{Character. The translated message, or empty strings in the case
#' of POT files.}
#' \item{is_obsolete}{Logical. Is the message obsolete?}
#' \item{msgctxt}{List of character. Disambiguating context information to allow
#' multiple messages with the same ID.}
#' \item{translator_comments}{List of character. Comments added by the
#' translator, typically to explain unclear messages, or why translation choices
#' were made.}
#' \item{source_reference_comments}{List of character. Links to where the
#' message occured in the source, in the form "filename:line".}
#' \item{flags_comments}{List of character. Typically used to describe
#' formatting directives. R uses C-style formatting, which would imply a
#' "c-format" flag.  For example %%d denotes an integer, and %%s denotes a
#' string. "fuzzy" flags can appear when PO files are merged.}
#' \item{previous_string_comments}{List of character. When PO files are merged
#' with an updated POT file ,and a fuzzy flag is generated, the old msgid is
#' stored in a previous string comment.}
#' }
#'
#' The \code{countable} element of the \code{po} object takes the same form as
#' the \code{direct} element, with two differences.
#' \describe{
#' \item{msgid_plural}{Character. The plural form of the untranslated message.}
#' \item{msgstr}{This is now a list of character (rather than character.)}
#' }
#' @references Much of the logic for this function was determined from reading
#' \url{http://pology.nedohodnik.net/doc/user/en_US/ch-poformat.html}
#' @importFrom R6 R6Class
#' @importFrom tibble data_frame
#' @importFrom tibble as_tibble
#' @export
po <- function(source_type, file_type, initial_comments, metadata, direct, countable) {
  po_factory$new(source_type, file_type, initial_comments, metadata, direct, countable)
}

po_factory <- R6::R6Class(
  "po",
  private = list(
    # data fields
    ..source_type = "r",
    ..file_type = "pot",
    ..initial_comments = character(),
    ..metadata = tibble::data_frame(name = character(), value = character()),
    ..direct = tibble::data_frame(
      msgid  = character(),
      msgstr = character(),
      is_obsolete = logical(),
      msgctxt = list(),
      translator_comments = list(),
      source_reference_comments = list(),
      flags_comments = list(),
      previous_string_comments = list()
    ),
    ..countable = tibble::data_frame(
      msgid         = character(),
      msgid_plural  = character(),
      msgstr        = list(),
      is_obsolete = logical(),
      msgctxt = list(),
      translator_comments = list(),
      source_reference_comments = list(),
      flags_comments = list(),
      previous_string_comments = list()
    ),
    # functionality not relevant outside object

    # Fix is_obsolete field
    # @param x An object, ideally a logical vector.
    # @param n Integer number of columns.
    # @return A logical vector.
    fix_is_obsolete_field = function(x, n) {
      if(is.null(x)) {
        logical(n)
      } else {
        assertive.base::coerce_to(x, "logical")
      }
    },
    # Fix comment fields
    # @param x An object, ideally a list of character vectors.
    # @param n Integer number of columns.
    # @return A list of character vectors.
    fix_comment_field = function(x, n) {
      if(is.null(x)) {
        replicate(n, character(), simplify = FALSE)
      } else {
        .xname = assertive.base::get_name_in_parent(x)
        private$as_list_of_character(x, .xname)
      }
    },
    # Convert an object into a list of character vectors
    # @param x An object.
    # @param .xname Not intended to be used directly.
    # @return A list of character vectors.
    as_list_of_character = function(x, .xname = assertive.base::get_name_in_parent(x)) {
      if(length(x) == 0L) {
        return(list())
      }
      force(.xname)
      if(is.recursive(x)) {
        x <- coerce_to(x, "list")
        .xnames <- paste0(.xname, "[[", seq_along(x), "]]")
        x <- Map(coerce_to, x, .xname = .xnames, target_class = "character")
      } else if(is.atomic(x)) {
        x <- coerce_to(x, "character", .xname = .xname)
        x <- list(x)
      } else {
        stop("symbols and externalptrs can't be turned into a list of character vectors.")
      }
      x
    }
  ),
  active = list(
    source_type = function(value) {
      if(missing(value)) {
        private$..source_type
      } else {
        value <- tolower(value)
        value <- match.arg(value, c("r", "c"))
        private$..source_type <- value
      }
    },
    file_type = function(value) {
      if(missing(value)) {
        private$..file_type
      } else {
        value <- tolower(value)
        value <- match.arg(value, c("pot", "po"))
        private$..file_type <- value
      }
    },
    initial_comments = function(value) {
      if(missing(value)) {
        private$..initial_comments
      } else {
        value <- as.character(value)
        private$..initial_comments <- value
      }
    },
    metadata = function(value) {
      if(missing(value)) {
        private$..metadata
      } else {
        value <- tibble::as_tibble(value)
        correct_cols <- c("name", "value")
        assertive.sets::assert_are_set_equal(colnames(value), correct_cols)
        value$name <- assertive.base::coerce_to(value$name, "character")
        value$value <- assertive.base::coerce_to(value$value, "character")
        # browser()
        assertive.strings::assert_all_are_non_missing_nor_empty_character(value$name)
        private$..metadata <- value
      }
    },
    direct = function(value) {
      if(missing(value)) {
        append_key(private$..direct)
      } else {
        value <- as_tibble(value)
        compulsory_cols <- c("msgid", "msgstr")
        optional_cols <- c(
          "is_obsolete", "msgctxt",
          "translator_comments", "source_reference_comments",
          "flags_comments", "previous_string_comments"
        )
        n <- nrow(value)
        assertive.sets::assert_is_superset(colnames(value), compulsory_cols)
        value$msgid <- assertive.base::coerce_to(value$msgid, "character")
        value$msgstr <- assertive.base::coerce_to(value$msgstr, "character")
        value$is_obsolete <- private$fix_is_obsolete_field(value$is_obsolete, n)
        value$msgctxt <- private$fix_comment_field(value$msgctxt, n)
        value$translator_comments <- private$fix_comment_field(
          value$translator_comments, n
        )
        value$source_reference_comments <- private$fix_comment_field(
          value$source_reference_comments, n
        )
        value$flags_comments <- private$fix_comment_field(value$flags_comments, n)
        value$previous_string_comments <- private$fix_comment_field(
          value$previous_string_comments, n
        )
        # Get fields in order. msgkey *not* included
        # In case a user tries to do po2$direct <- po1$direct
        value <- value[, c(compulsory_cols, optional_cols)]
        private$..direct <- value
      }
    },
    countable = function(value) {
      if(missing(value)) {
        append_key(private$..countable)
      } else {
        value <- as_tibble(value)
        compulsory_cols <- c("msgid", "msgid_plural", "msgstr")
        optional_cols <- c(
          "is_obsolete", "msgctxt",
          "translator_comments", "source_reference_comments",
          "flags_comments", "previous_string_comments"
        )
        n <- nrow(value)
        assertive.sets::assert_is_superset(colnames(value), compulsory_cols)
        value$msgid <- assertive.base::coerce_to(value$msgid, "character")
        value$msgid_plural <- assertive.base::coerce_to(value$msgid_plural, "character")
        value$msgstr <- private$as_list_of_character(value$msgstr)
        value$is_obsolete <- private$fix_is_obsolete_field(value$is_obsolete, n)
        value$msgctxt <- private$fix_comment_field(value$msgctxt, n)
        value$translator_comments <- private$fix_comment_field(
          value$translator_comments, n
        )
        value$source_reference_comments <- private$fix_comment_field(
          value$source_reference_comments, n
        )
        value$flags_comments <- private$fix_comment_field(value$flags_comments, n)
        value$previous_string_comments <- private$fix_comment_field(
          value$previous_string_comments, n
        )
        # Get fields in order. msgkey *not* included
        value <- value[, c(compulsory_cols, optional_cols)]
        private$..countable <- value
      }
    },
    n_plural_forms = function()
    {
      # Can't have this completely defined in here, since read_po()
      # needs to use it before the po object is created.
      get_n_plural_forms(self$metadata)
    }
  ),
  public = list(
    initialize = function(source_type, file_type, initial_comments, metadata, direct, countable) {
      if(!missing(source_type)) {
        self$source_type <- source_type
      }
      if(!missing(file_type)) {
        self$file_type <- file_type
      }
      if(!missing(initial_comments)) {
        self$initial_comments <- initial_comments
      }
      if(!missing(metadata)) {
        self$metadata <- metadata
      }
      if(!missing(direct)) {
        self$direct <- direct
      }
      if(!missing(countable)) {
        self$countable <- countable
      }
    }
  )
)

#' @importFrom digest digest
#' @importFrom dplyr mutate_
#' @importFrom magrittr %>%
append_key <- function(x) {
  # (msgid, msgctxt) pairs ought to be unique, since the whole point of msgctxt
  # is to disambiguate the rare case that there are duplicate msgids
  # xxhash32 chosen because the hash is only 8 letters
  x %>%
    mutate_(
      msgkey = ~ vapply(
        paste(msgid, msgctxt), digest, character(1), algo = "xxhash32"
      )
    )
}

