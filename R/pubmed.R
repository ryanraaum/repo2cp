
.pubmed_one_author_data <- function(one_xml) {
  one_au <- list(
    given = xml2::xml_find_all(one_xml, ".//ForeName") |> xml_text_or_null(),
    family = xml2::xml_find_all(one_xml, ".//LastName") |> xml_text_or_null(),
    literal = xml2::xml_find_all(one_xml, ".//CollectiveName") |> xml_text_or_null()
    # initials = xml_find_all(one_xml, ".//Initials") |> xml_text(),
    # affiliation = xml_find_all(one_xml, ".//Affiliation") |> xml_text_or_null()
  )
  one_au[lengths(one_au) != 0]
}

.pubmed_one_affiliation_data <- function(one_xml) {
  xml2::xml_find_all(one_xml, ".//Affiliation") |> xml_text_or_null()
}

#' Convert pubmed xml to citeproc csl-data
#'
#' @param this_xml The xml data from pubmed.
#' @param format The format for return of the processed data.
#'
#' @returns A object of the selected return `format`.
#' @export
#'
#' @examples
#' \dontrun{
#' this_res <- entrez_search("pubmed", term="10.1371/journal.pcbi.1009061")
#' this_pmid <- this_res$ids[1]
#' this_record <- entrez_fetch("pubmed", id=this_pmid, rettype="xml")
#' this_xml <- read_xml(this_record)
#'
#' pubmed2cp(this_xml)
#' }
pubmed2cp <- function(this_xml, format="citeproc-json") {

  this_journal_issn <-
    xml2::xml_find_all(this_xml, ".//Journal//ISSN") |>
    xml_text_or_null()
  this_journal_volume <-
    xml2::xml_find_all(this_xml, ".//Journal//Volume") |>
    xml_text_or_null()
  this_journal_issue <-
    xml2::xml_find_all(this_xml, ".//Journal//Issue") |>
    xml_text_or_null()
  this_journal_year <-
    xml2::xml_find_all(this_xml, ".//Journal//PubDate//Year") |>
    xml_text_or_null()
  this_journal_month <-
    xml2::xml_find_all(this_xml, ".//Journal//PubDate//Month") |>
    xml_text_or_null()
  this_journal_day <-
    xml2::xml_find_all(this_xml, ".//Journal//PubDate//Day") |>
    xml_text_or_null()
  this_journal_title <-
    xml2::xml_find_all(this_xml, ".//Journal//Title") |>
    xml_text_or_null()
  this_journal_abbreviation <-
    xml2::xml_find_all(this_xml, ".//Journal//ISOAbbreviation") |>
    xml_text_or_null()
  this_article_year <-
    xml2::xml_find_all(this_xml, ".//Article//ArticleDate//Year") |>
    xml_text_or_null()
  this_article_month <-
    xml2::xml_find_all(this_xml, ".//Article//ArticleDate//Month") |>
    xml_text_or_null()
  this_article_day <-
    xml2::xml_find_all(this_xml, ".//Article//ArticleDate//Day") |>
    xml_text_or_null()
  this_article_title <-
    xml2::xml_find_all(this_xml, ".//Article//ArticleTitle") |>
    xml_text_or_null()
  this_article_startpage <-
    xml2::xml_find_all(this_xml, ".//Article//Pagination//StartPage") |>
    xml_text_or_null()
  this_article_endpage <-
    xml2::xml_find_all(this_xml, ".//Article//Pagination//EndPage") |>
    xml_text_or_null()
  this_article_abstract <-
    xml2::xml_find_all(this_xml, ".//Article//Abstract//AbstractText") |>
    xml_text_or_null()
  # need to convert language from 3-letter to 2-letter?
  # - or go to 3-letter elsewhere?
  # - or just not worry about it?
  this_article_language <-
    xml2::xml_find_all(this_xml, ".//Article//Language") |>
    xml_text_or_null()
  this_article_aid_doi <-
    xml2::xml_find_all(this_xml, ".//PubmedData/ArticleIdList//ArticleId[@IdType='doi']") |>
    xml_text_or_null()
  this_article_eid_doi <-
    xml2::xml_find_all(this_xml, ".//Article//ELocationID[@EIdType='doi']") |>
    xml_text_or_null()
  this_article_pmid <-
    xml2::xml_find_all(this_xml, ".//PubmedData/ArticleIdList//ArticleId[@IdType='pubmed']") |>
    xml_text_or_null()
  this_article_pmcid <-
    xml2::xml_find_all(this_xml, ".//PubmedData/ArticleIdList//ArticleId[@IdType='pmc']") |>
    xml_text_or_null()
  this_entry_pmid <-
    xml2::xml_find_all(this_xml, ".//PMID") |>
    xml_text_or_null()

  # sometimes the abstract has several disconnected sentences (instead of being one block of text)
  if (length(this_article_abstract) > 1) {
    this_article_abstract <- paste(this_article_abstract, collapse=" ")
  }

  # complex extractions
  this_authors <- xml2::xml_find_all(this_xml, ".//Author")

  this_doi <- c()
  if (aidr::this_exists(this_article_aid_doi)) {
    this_doi <- c(this_doi, this_article_aid_doi)
  }
  if (aidr::this_exists(this_article_eid_doi)) {
    this_doi <- c(this_doi, this_article_eid_doi)
  }

  # NOTE: This is fragile.
  # Conflict resolution strategy: When DOI values from ArticleIdList and ELocationID
  # differ, the ArticleIdList DOI takes precedence (second condition below).
  # This means conflicting DOIs do NOT throw an error - the ArticleIdList value is used.
  if (aidr::this_is_singular(this_doi)) {
    this_doi <- unique(this_doi)
  } else if (aidr::this_exists(this_article_aid_doi) && length(this_article_aid_doi) == 1) {
    # If ArticleIdList has exactly one DOI, use it (even if it conflicts with ELocationID)
    this_doi <- this_article_aid_doi
  } else if (is.null(this_article_aid_doi) && is.null(this_article_eid_doi)) {
    this_doi <- NA
  } else {
    stop("Incompatible DOI values")
  }

  this_pmid <- c()
  if (aidr::this_exists(this_entry_pmid)) {
    this_pmid <- c(this_pmid, this_entry_pmid)
  }
  if (aidr::this_exists(this_article_pmid)) {
    this_pmid <- c(this_pmid, this_article_pmid)
  }

  # NOTE: This is fragile.
  # Conflict resolution strategy: When PMID values from MedlineCitation and ArticleIdList
  # differ, the ArticleIdList PMID takes precedence (second condition below).
  # This means conflicting PMIDs do NOT throw an error - the ArticleIdList value is used.
  if (aidr::this_is_singular(this_pmid)) {
    this_pmid <- unique(this_pmid)
  } else if (aidr::this_exists(this_article_pmid) && length(this_article_pmid) == 1) {
    # If ArticleIdList has exactly one PMID, use it (even if it conflicts with MedlineCitation)
    this_pmid <- this_article_pmid
  } else {
    stop("Incompatible PMID values")
  }

  this_year <- ifelse(aidr::this_exists(this_article_year),
                      this_article_year, this_journal_year)
  this_month <- "01"
  if (!(is.null(this_article_month) && is.null(this_journal_month))) {
    this_month <- ifelse(aidr::this_exists(this_article_month),
                         this_article_month, this_journal_month)
  }
  this_day <- "01"
  if (!(is.null(this_article_day) && is.null(this_journal_day))) {
    this_day <- ifelse(aidr::this_exists(this_article_day),
                       this_article_day, this_journal_day)
  }

  if (!aidr::this_exists(this_year) ||
      !aidr::this_exists(this_month) ||
      !aidr::this_exists(this_day)) {
    stop("problem with date in pubmed article")
  }

  if (aidr::this_exists(this_article_startpage) && aidr::this_exists(this_article_endpage)) {
    article_pages <- glue::glue("{this_article_startpage}-{this_article_endpage}")
  } else if (aidr::this_exists(this_article_startpage)) {
    article_pages <- this_article_startpage
  } else {
    article_pages <- NULL
  }

  res <- list(
    item = list(
      type = "article-journal",
      language = .convert_language(this_article_language),
      abstract = this_article_abstract,
      doi = this_doi,
      pmid = this_pmid,
      pmcid = this_article_pmcid,
      volume = this_journal_volume,
      issue = this_journal_issue,
      container_title = this_journal_title,
      container_title_short = this_journal_abbreviation,
      issued = lubridate::ymd(glue::glue("{this_year}-{this_month}-{this_day}")),
      page = article_pages,
      page_first = this_article_startpage,
      title = this_article_title
    ),
    author = purrr::map(this_authors, .pubmed_one_author_data),
    author_affiliation = purrr::map(this_authors, .pubmed_one_affiliation_data)
  )

  if (format == "edb-list") { return(res) }
  stop("citeproc-json return not implemented")
}
