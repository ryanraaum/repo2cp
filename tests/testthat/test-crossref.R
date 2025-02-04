crdata <- list(
  simple_item_rda = readRDS(test_path("testdata", "crossref_basic_journal_article.rda")),
  institutional_item_rda = readRDS(test_path("testdata", "crossref_institutional_author_journal_article.rda")),
  simple_item_json = jsonlite::read_json(test_path("testdata", "crossref_basic_journal_article.json"),
                                         simplifyVector = TRUE),
  institutional_item_json = jsonlite::read_json(test_path("testdata", "crossref_institutional_author_journal_article.json"),
                                                simplifyVector = TRUE)
)

test_that("crossref2cp edb-list return finishes without error and returns list", {
  for (i in seq_along(crdata)) {
    expect_no_failure(cr_res <- crossref2cp(crdata[[i]], format="edb-list"))
    expect_true(is.list(cr_res),
                label = glue::glue("data source {names(crdata)[i]}"))
  }
})


test_that("crossref2cp edb-list result has the right top level entries", {
  for (i in seq_along(crdata)) {
    cr_res <- crossref2cp(crdata[[i]], format="edb-list")
    this_source <- names(crdata)[i]
    for (this_element in c("item", "author", "author_identifier", "author_affiliation")) {
      expect_true(this_element %in% names(cr_res),
                  label=glue::glue("'{this_element}' in {this_source}"))
    }
  }
})


test_that("crossref2cp edb-list item result has only core citeproc item elements", {
  for (i in seq_along(crdata)) {
    cr_res <- crossref2cp(crdata[[i]], format="edb-list")
    this_source <- names(crdata)[i]
    for (this_element in names(cr_res$item)) {
      expect_true(this_element %in% csl_core_clean,
                  label = glue::glue("unexpected {this_element} in {this_source}"))
    }
  }
})

test_that("crossref2cp edb-list author result has only creator citeproc item elements", {
  for (i in seq_along(crdata)) {
    cr_res <- crossref2cp(crdata[[i]], format="edb-list")
    this_source <- names(crdata)[i]
    for (this_author in cr_res$author) {
      for (this_element in names(this_author)) {
        expect_true(this_element %in% csl_creator_clean,
                    label = glue::glue("unexpected {this_element} in {this_source}"))
      }
    }
  }
})
