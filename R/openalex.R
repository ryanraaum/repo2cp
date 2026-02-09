.oa_type_to_citeproc <- function(oa_type, oa_location_type, oa_version) {
  # first, deal with openalex specific types
  oa_types <- c("article", "preprint", "paratext", "letter", "editorial", "erratum",
                "libguides", "supplementary-materials", "review")
  if (oa_type == "article") {
    if (aidr::this_or_empty_string(oa_version) == "submittedVersion") {
      return("article")
    } else if (aidr::this_exists(oa_location_type) && oa_location_type == "journal") {
      return("article-journal")
    } else if (aidr::this_exists(oa_location_type) && oa_location_type == "conference") {
      return("paper-conference")
    } else {
      return("unknown")
    }
  } else if (oa_type == "letter" && aidr::this_or_empty_string(oa_location_type) == "journal") {
    return("article-journal")
  } else if (oa_type == "preprint") {
    return("article")
  } else if (oa_type == "paratext") {
    return("document")
  } else if (oa_type == "editorial") {
    return("article-journal")
  } else if (oa_type == "erratum") {
    return("article-journal")
  } else if (oa_type == "libguides") {
    return("document")
  } else if (oa_type == "supplementary-materials") {
    return("document")
  } else if (oa_type == "review") {
    return("article-journal")
  } else if (oa_type == "other" && aidr::this_exists(oa_location_type) && oa_location_type == "journal") {
    return("article-journal")
  }

  # then (per their documentation) if it not one of their types, it must be a crossref type,
  # so deal with it as a crossref type
  if (!(oa_type %in% names(crossref2csl))) {
    return("unknown")
  }
  csl_type <- crossref2csl[[oa_type]]
  if (is.na(csl_type)) {
    return("unknown")
  }
  csl_type
}

.oa_clean_pages <- function(first_page=NULL, last_page=NULL) {
  if (aidr::this_exists(last_page) && last_page != first_page) {
    return(glue::glue("{first_page}-{last_page}"))
  }
  first_page
}

.oa_uninvert_abstract <- function(inverted_abstract) {
  # Handle empty inverted index
  if (length(inverted_abstract) == 0) {
    return("")
  }
  words <- vector(mode="character", length=max(unlist(inverted_abstract))+1 )
  for (word in names(inverted_abstract)) {
    positions <- unlist(inverted_abstract[[word]]) + 1
    for (p in positions) {words[p] <- word}
  }
  paste(words, collapse=" ")
}

.oa_extract_authors <- function(oa_authorships) {
  # Handle empty authorships
  if (length(oa_authorships) == 0) {
    return(list())
  }
  # Stupid hack to avoid complaint about undeclared global variables in `check`
  ## These are never used; just here as code check misdirection for the column
  ## names used in the dplyr chain below
  first_name <- middle_name <- last_name <- given <- suffix <- NULL
  # on to actual body of function
  authors <- purrr::map_vec(oa_authorships, \(x) {x$author$display_name})
  author_df <- humaniformat::parse_names(authors) |>
    dplyr::mutate(first_name = aidr::this_or_empty_string(first_name)) |>
    dplyr::mutate(middle_name = aidr::this_or_empty_string(middle_name)) |>
    dplyr::mutate(last_name = aidr::this_or_empty_string(last_name)) |>
    dplyr::mutate(given = stringr::str_trim(stringr::str_c(first_name, middle_name, sep=" "))) |>
    dplyr::select(given, last_name, suffix) |>
    dplyr::rename(family = last_name)
  author_list <- apply(author_df, 1, as.list)
  author_list
}

.oa_extract_identifiers <- function(oa_authorships) {
  orcids  <- purrr::map(oa_authorships, \(x) {list(orcid=aidr::this_or_na(x$author$orcid))})
  orcids
}

.oa_clean_pmid <- function(pmid_url) {
  # Extract PMID from URL like "https://pubmed.ncbi.nlm.nih.gov/36656910"
  # Return just the ID number
  if (!aidr::this_exists(pmid_url)) {
    return(NULL)
  }
  # Extract the numeric ID at the end of the URL
  stringr::str_extract(pmid_url, "[0-9]+$")
}

.oa_clean_pmcid <- function(pmcid_url) {
  # Extract PMCID from URL like "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9970057"
  # Return with PMC prefix like "PMC9970057"
  if (!aidr::this_exists(pmcid_url)) {
    return(NULL)
  }
  # Extract PMC followed by numbers
  stringr::str_extract(pmcid_url, "PMC[0-9]+")
}

.oa_extract_affiliations <- function(oa_authorships) {
  # Handle empty authorships
  if (length(oa_authorships) == 0) {
    return(list())
  }
  # Extract raw_affiliation_strings for each author
  affiliations <- purrr::map(oa_authorships, function(authorship) {
    if (aidr::this_exists(authorship$raw_affiliation_strings)) {
      # raw_affiliation_strings is a list, unlist to get character vector
      unlist(authorship$raw_affiliation_strings)
    } else {
      NULL
    }
  })
  affiliations
}

#' Convert openalex json to citeproc csl-data
#'
#' @param this_json The json data from openalex.
#' @param format The format for return of the processed data.
#'
#' @returns A object of the selected return `format`.
#' @export
#'
#' @examples
#' \dontrun{
#' these_results <- oa_query(entity = "works", doi = "10.1371/journal.pcbi.1009061") |>
#'                   oa_request()
#' this_result <- these_results[[1]]
#'
#' openalex2cp(this_result)
#' }
openalex2cp <- function(this_json, format="citeproc-json") {
  res <- list(
    item = list(
      type = .oa_type_to_citeproc(this_json$type,
                                  this_json$primary_location$source$type,
                                  this_json$primary_location$version),
      language = this_json$language,
      doi = .clean_doi(this_json$doi),
      pmid = .oa_clean_pmid(this_json$ids$pmid),
      pmcid = .oa_clean_pmcid(this_json$ids$pmcid),
      volume = this_json$biblio$volume,
      issue = this_json$biblio$issue,
      container_title = this_json$primary_location$source$display_name,
      issued = lubridate::ymd(this_json$publication_date),
      page_first = this_json$biblio$first_page,
      page = .oa_clean_pages(this_json$biblio$first_page, this_json$biblio$last_page),
      title = this_json$title,
      abstract= ifelse(aidr::this_exists(this_json$abstract_inverted_index),
                       .oa_uninvert_abstract(this_json$abstract_inverted_index),
                       NA
      )
    ),
    author = .oa_extract_authors(this_json$authorships),
    author_affiliation = .oa_extract_affiliations(this_json$authorships),
    author_identifier = .oa_extract_identifiers(this_json$authorships)
  )

  if (format == "edb-list") { return(res) }
  stop("citeproc-json return not implemented")
}

