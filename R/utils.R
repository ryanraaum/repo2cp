
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
