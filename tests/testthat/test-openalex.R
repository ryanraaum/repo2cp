
oadata <- list(
  simple_item_rda = oa_journal_article,
  institutional_item_rda = readRDS(test_path("testdata", "openalex_institutional_author_journal_article.rda"))
)

test_that("openalex2cp edb-list return finishes without error and returns list", {
  for (i in seq_along(oadata)) {
    expect_no_failure(res <- openalex2cp(oadata[[i]], format="edb-list"))
    expect_true(is.list(res),
                label = glue::glue("data source {names(oadata)[i]}"))
  }
})

test_that("openalex2cp edb-list result has the right top level entries", {
  for (i in seq_along(oadata)) {
    res <- openalex2cp(oadata[[i]], format="edb-list")
    this_source <- names(oadata)[i]
    for (this_element in c("item", "author", "author_identifier")) {
      expect_true(this_element %in% names(res),
                  label=glue::glue("'{this_element}' in {this_source}"))
    }
  }
})

test_that("openalex2cp edb-list item result has only core citeproc item elements", {
  for (i in seq_along(oadata)) {
    res <- openalex2cp(oadata[[i]], format="edb-list")
    this_source <- names(oadata)[i]
    for (this_element in names(res$item)) {
      expect_true(this_element %in% csl_core_clean,
                  label = glue::glue("unexpected {this_element} in {this_source}"))
    }
  }
})

test_that("openalex2cp edb-list author result has only creator citeproc item elements", {
  for (i in seq_along(oadata)) {
    res <- openalex2cp(oadata[[i]], format="edb-list")
    this_source <- names(oadata)[i]
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
