
.this_exists <- function(x) {
  !(all(is.na(x)) || all(is.null(x)) || length(x) == 0)
}

.this_is_singular <- function(x) {
  length(unique(x)) == 1
}

.this_or_empty_string <- Vectorize(function(x) {
  ifelse(.this_exists(x), x, "")
})

.this_or_na <- function(x) {
  ifelse(.this_exists(x), x, NA)
}

.xml_text_or_null <- function(x) {
  if (length(x) == 0) { return(NULL) }
  xml2::xml_text(x)
}

.clean_doi <- function(doi_string) {
  unlist(stringr::str_extract_all(doi_string, "10\\..+"))
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
