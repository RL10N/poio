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
    )
  ),
  active = list(
    source_type = function(value) {
      if(missing(value)) {
        private$..source_type
      } else {
        value <- match.arg(value, c("r", "c"))
        private$..source_type <- value
      }
    },
    file_type = function(value) {
      if(missing(value)) {
        private$..file_type
      } else {
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
        value$name <- coerce_to(value$name, "character")
        value$name <- coerce_to(value$value, "character")
        assertive.strings::assert_all_are_non_missing_nor_empty_character(value$name)
        assertive.strings::assert_all_are_non_empty_character(value$value)
        private$..metadata <- value
      }
    },
    direct = function(value) {
      if(missing(value)) {
        append_key(private$..direct)
      } else {
        value <- as_tibble(value)
        # Ignore a msgkey field
        # In case a user tries to do po2$direct <- po1$direct
        value$msgkey <- NULL
        correct_cols <- c(
          "msgid", "msgstr", "is_obsolete", "msgctxt", "translator_comments",
          "source_reference_comments", "flags_comments", "previous_string_comments"
        )
        assertive.sets::assert_are_set_equal(colnames(value), correct_cols)
        value$msgid <- assertive.base::coerce_to(value$msgid, "character")
        value$msgstr <- assertive.base::coerce_to(value$msgstr, "character")
        value$is_obsolete <- assertive.base::coerce_to(value$is_obsolete, "logical")
        value$msgctxt <- as_list_of_character(value$msgctxt)
        value$translator_comments <- as_list_of_character(value$translator_comments)
        value$source_reference_comments <- as_list_of_character(value$source_reference_comments)
        value$flags_comments <- as_list_of_character(value$flags_comments)
        value$previous_string_comments <- as_list_of_character(value$previous_string_comments)
        private$..direct <- value
      }
    },
    countable = function(value) {
      if(missing(value)) {
        append_key(private$..countable)
      } else {
        value <- as_tibble(value)
        # Ignore a msgkey field, as above
        value$msgkey <- NULL
        correct_cols <- c(
          "msgid", "msgid_plural", "msgstr", "is_obsolete", "msgctxt", "translator_comments",
          "source_reference_comments", "flags_comments", "previous_string_comments"
        )
        assertive.sets::assert_are_set_equal(colnames(value), correct_cols)
        value$msgid <- assertive.base::coerce_to(value$msgid, "character")
        value$msgid_plural <- assertive.base::coerce_to(value$msgid_plural, "character")
        value$msgstr <- as_list_of_character(value$msgstr)
        value$is_obsolete <- assertive.base::coerce_to(value$is_obsolete, "logical")
        value$msgctxt <- as_list_of_character(value$msgctxt)
        value$translator_comments <- as_list_of_character(value$translator_comments)
        value$source_reference_comments <- as_list_of_character(value$source_reference_comments)
        value$flags_comments <- as_list_of_character(value$flags_comments)
        value$previous_string_comments <- as_list_of_character(value$previous_string_comments)
        private$..countable <- value
      }
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

#' Convert an object into a list of character vectors
#'
#' Converts an object into a list of character vectors.  Not intended to be
#' called directly by the user, but needs to be exported to enable run-time
#' checking of \code{po} objects.
#' @param x An object.
#' @param .xname Not intended to be used directly.
#' @return A list of character vectors.
#' @importFrom assertive.base coerce_to
#' @importFrom assertive.base get_name_in_parent
#' @export
as_list_of_character <- function(x, .xname = get_name_in_parent(x)) {
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

