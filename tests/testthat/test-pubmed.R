
pmdata <- list(
  simple_item_xml = xml2::read_xml(test_path("testdata", "pubmed_basic_journal_article.xml")),
  institutional_item_xml = xml2::read_xml(test_path("testdata", "pubmed_institutional_author_journal_article.xml"))
)

test_that("pubmed2cp edb-list return finishes without error and returns list", {
  for (i in seq_along(pmdata)) {
    expect_no_failure(res <- pubmed2cp(pmdata[[i]], format="edb-list"))
    expect_true(is.list(res),
                label = glue::glue("data source {names(pmdata)[i]}"))
  }
})

test_that("pubmed2cp edb-list result has the right top level entries", {
  for (i in seq_along(pmdata)) {
    res <- pubmed2cp(pmdata[[i]], format="edb-list")
    this_source <- names(pmdata)[i]
    for (this_element in c("item", "author", "author_affiliation")) {
      expect_true(this_element %in% names(res),
                  label=glue::glue("'{this_element}' in {this_source}"))
    }
  }
})

test_that("pubmed2cp edb-list item result has only core citeproc item elements", {
  for (i in seq_along(pmdata)) {
    res <- pubmed2cp(pmdata[[i]], format="edb-list")
    this_source <- names(pmdata)[i]
    for (this_element in names(res$item)) {
      expect_true(this_element %in% csl_core_clean,
                  label = glue::glue("unexpected {this_element} in {this_source}"))
    }
  }
})

test_that("pubmed2cp edb-list author result has only creator citeproc item elements", {
  for (i in seq_along(pmdata)) {
    res <- pubmed2cp(pmdata[[i]], format="edb-list")
    this_source <- names(pmdata)[i]
    author_count <- 0
    for (this_author in res$author) {
      author_count <- author_count + 1
      if (author_count <= 10) { # don't need to overdo this
        for (this_element in names(this_author)) {
          expect_true(this_element %in% csl_creator_clean,
                      label = glue::glue("unexpected {this_element} in {this_source}"))
        }
      }
    }
  }
})
