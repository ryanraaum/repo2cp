
## these are not vectorized; they always return a single value

.this_exists <- function(x) {
  !(all(is.na(x)) || all(is.null(x)) || length(x) == 0)
}

.this_is_singular <- function(x) {
  if (any(is.na(x)) || any(is.null(x))) { return(NA) }
  length(unique(x)) == 1
}

.convert_language <- function(langcode) {
  if (nchar(langcode) == 2 && langcode %in% language3to2) {
    return(langcode)
  } else if (nchar(langcode) == 2) {
    stop("unknown two letter language code")
  } else if (nchar(langcode) != 3) {
    stop("language code is neither 2 nor three letters")
  } else if (!(langcode %in% names(language3to2))) {
    stop("unconvertable three letter language code")
  } else {
    return(language3to2[[langcode]])
  }
}

## these are vectorized; they return a value for each entry in a input vector

.singular_this_or_empty_string <- function(x) {
  if(.this_exists(x)) { return(x) }
  ""
}

.vectorized_this_or_empty_string <- Vectorize(.singular_this_or_empty_string,
                                              USE.NAMES = FALSE)

.this_or_empty_string <- function(x) {
  if (length(x) > 1) { return(.vectorized_this_or_empty_string(x))}
  .singular_this_or_empty_string(x)
}

.singular_this_or_na <- function(x) {
  ifelse(.this_exists(x), x, NA)
}

.vectorized_this_or_na <- Vectorize(.singular_this_or_na, USE.NAMES=FALSE)

.this_or_na <- function(x) {
  if (length(x) > 1) { return(.vectorized_this_or_na(x)) }
  .singular_this_or_na(x)
}

.clean_doi <- function(doi_string) {
  unlist(stringr::str_extract_all(doi_string, "10\\.\\S+"))
}

## sort-of vectorized, sort-of not
##- returns a vector of text when appropriate
##- but if nodeset is empty, returns a single NULL

.xml_text_or_null <- function(x) {
  if (length(x) == 0) { return(NULL) }
  xml2::xml_text(x)
}


.edb2csl <- function(l) {
  assertthat::assert_that(.this_exists(l$item))
  res <- l$item
  creators <- intersect(csl_creator_types_clean, names(l))
  for (creator in creators) {
    res[[creator]] <- l[[creator]]
  }
  res
}

.unclean_csl_list <- function(l) {
  # l needs to be in the csl-data layout
  res <- l
  unclean_names <- clean2csl[names(l)]
  assertthat::are_equal(length(res), length(unclean_names))
  names(res) <- unclean_names

  creators <- intersect(csl_creator_types, names(res))
  for (creator in creators) {
    for (i in seq_along(res[[creator]])) {
      unclean_names <- clean2csl[names(res[[creator]][[i]])]
      assertthat::are_equal(length(res[[creator]][[i]]), length(unclean_names))
      names(res[[creator]][[i]]) <- unclean_names
    }
  }
  res
}

## exported

#' Convert citeproc-list data to citeproc-json
#'
#' @param l A list with citeproc formatted data
#'
#' @returns A JSON document
#' @export
#'
#' @examples
#' cplist2json(list(title="Hello"))
cplist2json <- function(l) {
  jsonlite::toJSON(l, auto_unbox = TRUE, pretty = TRUE)
}


#' Combine JSON elements
#'
#' @param ... Either several JSON items or one list of JSON items
#'
#' @returns A JSON array
#' @export
#'
#' @examples
#' a <- crossref2cp(cr_journal_article)
#' b <- crossref2cp(cr_journal_article)
#' cjson(a, b)
#'
#' cjson(list(a, b))
cjson <- function(...) {
  these_args <- list(...)
  if (length(these_args) == 1) {
    if (is.list(these_args[[1]])) {
      these_args <- these_args[[1]]
    }
  }
  all_are_json <- all(unlist(purrr::map(these_args, \(x) class(x) == "json")))
  these_args <- purrr::map(these_args, \(x) {stringr::str_replace_all(x, "\n", "\n  ")})
  these_args <- purrr::map(these_args, \(x) {stringr::str_replace(x, "\\{", "  \\{")})
  if (!all_are_json) {stop("can only combine `json` elements")}
  res <- paste0("[\n",
         paste(these_args, collapse=",\n"),
         "\n]")
  class(res) <- "json"
  res
}
