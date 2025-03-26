#' A single journal article from crossref
#'
#' An R list with the crossref data for a single journal article.
#' It is included here for trial and testing purposes to avoid repeatedly hitting
#' crossref servers for no good reason.
#' It could be accessed directly from crossref using
#' \preformatted{
#' rcrossref::cr_cn(dois = "10.1371/journal.pcbi.1009061",
#'                  format = "citeproc-json")
#' }
#'
#' @format
#' A list with 43 elements.
#'
#' @source <https://www.crossref.org>
"cr_journal_article"

#' A single journal article as a bibentry object
#'
#' A base R bibentry object with the data for a single journal article.
#' It is included here for trial and testing purposes.
#' It could be accessed directly using
#'
#' @format
#' A bibentry object.
#'
#' @source <https://www.openalex.org>
"be_journal_article"

#' A single journal article from openalex
#'
#' An R list with the openalex data for a single journal article.
#' It is included here for trial and testing purposes to avoid repeatedly hitting
#' openalex servers for no good reason.
#' It could be accessed directly using
#' \preformatted{
#' openalexR::oa_query(entity = "works",
#'                     doi = "10.1371/journal.pcbi.1009061") |>
#'            openalexR::oa_request()
#' }
#'
#' @format
#' A list with 50 elements.
#'
#' @source <https://www.openalex.org>
"oa_journal_article"


#' A single journal article in a special volume from openalex
#'
#' An R list with the openalex data for a single journal article.
#' It is included here for trial and testing purposes to avoid repeatedly hitting
#' openalex servers for no good reason.
#' It could be accessed directly using
#' \preformatted{
#' openalexR::oa_query(entity = "works",
#'                     doi = "10.1090/conm/355/06452") |>
#'            openalexR::oa_request()
#' }
#'
#' @format
#' A list with 50 elements.
#'
#' @source <https://www.openalex.org>
"oa_special_volume_article"


#' A single journal article from pubmed
#'
#' XML text with the pubmed data for a single journal article.
#' It is included here for trial and testing purposes to avoid repeatedly hitting
#' pubmed servers for no good reason.
#' It could be accessed directly using
#' \preformatted{
#' this_res <- rentrez::entrez_search("pubmed",
#'                                    term="10.1371/journal.pcbi.1009061")
#' this_pmid <- this_res$ids[1]
#' this_record <- rentrez::entrez_fetch("pubmed",
#'                                      id=this_pmid,
#'                                      rettype="xml")
#' }
#' Note that we cannot save processed XML because the internal R representation
#' does not permit it, so this example data needs to be parsed before use:
#' \preformatted{
#' pm_journal_article <- xml2::read_xml(pm_journal_article_xmltext)
#' }
#'
#' @format
#' A string of XML data.
#'
#' @source <https://pubmed.ncbi.nlm.nih.gov/>
"pm_journal_article_xmltext"

