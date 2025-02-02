
.crossref_type_to_citeproc <- function(cr_type) {
  assertthat::assert_that(cr_type %in% names(crossref2csl),
                          msg=glue::glue("'{cr_type}' is not a valid crossref type"))
  csl_type <- crossref2csl[[cr_type]]
  assertthat::assert_that(assertthat::noNA(csl_type),
                          msg=glue::glue("could not find csl type for crossref type '{cr_type}'"))
  csl_type
}

.crossref_process_author <- function(aulist) {
  # ensure all accessors are clean
  names(aulist) <- janitor::make_clean_names(names(aulist))
  # identify institution authors
  au_accessors <- names(aulist)
  if ("name" %in% au_accessors &&
      !("family" %in% au_accessors) &&
      !("given" %in% au_accessors)) {
    aulist$literal <- aulist$name
    aulist$name <- NULL
  }
  aulist[intersect(names(aulist), csl_creator_clean)]
}

.preprocess_author_data <- function(this_thing) {
  if (inherits(this_thing, "data.frame")) {
    author_list <- purrr::pmap(this_thing, ~c(...))
  } else if (is.list(this_thing)) {
    author_list <- this_thing
  }
  author_list
}

.crossref_author_data <- function(this_thing) {
  author_list <- .preprocess_author_data(this_thing)
  purrr::map(author_list, .crossref_process_author)
}

.all_empty <- function(l, element) {
  purrr::map(l, \(x) length(x[[element]]) == 0) |>
    unlist() |> all()
}

.crossref_process_affiliation <- function(aulist) {
  # ensure all accessors are clean
  names(aulist) <- janitor::make_clean_names(names(aulist))
  aulist[intersect(names(aulist), "affiliation")]
}

.crossref_affiliation_data <- function(this_thing) {
  author_list <- .preprocess_author_data(this_thing)
  these_results <- purrr::map(author_list, .crossref_process_affiliation)
  if (.all_empty(these_results, "affiliation")) {return(NULL)}
  these_results
}

.crossref_process_identifier <- function(aulist) {
  # ensure all accessors are clean
  names(aulist) <- janitor::make_clean_names(names(aulist))
  aulist[intersect(names(aulist), "orcid")]
}

.crossref_identifier_data <- function(this_thing) {
  author_list <- .preprocess_author_data(this_thing)
  these_results <- purrr::map(author_list, .crossref_process_identifier)
  if (.all_empty(these_results, "orcid")) {return(NULL)}
  these_results
}


.crossref_date <- function(this_cr_json) {
  this_cr_date <- list()
  if (.this_exists(this_cr_json$issued)) {
    this_cr_date <- this_cr_json$issued
  } else if (.this_exists(this_cr_json$published)) {
    this_cr_date <- this_cr_json$published
  } else if (.this_exists(this_cr_json$`published-print`)) {
    this_cr_date <- this_cr_json$`published-print`
  }
  if ("date-parts" %in% names(this_cr_date)) {
    date_parts <- this_cr_date[["date-parts"]]
    if (is.list(date_parts)) {
      date_parts <- t(matrix(date_parts[[1]]))
    }
    assertthat::assert_that(ncol(date_parts) >= 1 && ncol(date_parts) <= 3, msg = "crossref date does not have 1-3 columns")
    assertthat::assert_that(nrow(date_parts) == 1, msg = "crossref date has more than 1 row")
    this_cr_date_year <- date_parts[1,1]
    this_cr_date_month <- ifelse(ncol(date_parts) > 1, date_parts[1,2], "6") # default to June if no month given (i.e. middle of year)
    this_cr_date_day <- ifelse(ncol(date_parts) > 2, date_parts[1,3], "1") # default to first if no day given
    return(lubridate::ymd(glue::glue("{this_cr_date_year}-{this_cr_date_month}-{this_cr_date_day}")))
  } else {
    stop("unknown crossref date structure")
  }
}

crossref2cp <- function(this_json, format="list") {
  res <- list(
    item = list(
      type = .crossref_type_to_citeproc(this_json$type),
      language = .convert_language(this_json$language),
      abstract = ifelse(.this_exists(this_json$abstract),
                        this_json$abstract |> xml2::read_html() |> xml2::xml_text(),
                        NA
      ),
      DOI = this_json$DOI,
      volume = this_json$volume,
      issue = this_json$issue,
      container_title = this_json$`container-title`,
      container_title_short = this_json$`container-title-short`,
      issued = .crossref_date(this_json),
      page = this_json$page,
      title = this_json$title
    ),
    author = .crossref_author_data(this_json$author),
    affiliation = .crossref_affiliation_data(this_json$author),
    identifier = .crossref_identifier_data(this_json$author)
  )

  res
}
