
oadata <- list(
  simple_item_rda = oa_journal_article,
  special_volume_item_rda = oa_special_volume_article,
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

# Edge case and error handling tests

test_that(".oa_type_to_citeproc returns unknown for unrecognized types", {
  # Test with invalid type
  result <- .oa_type_to_citeproc("invalid-type", NULL, NULL)
  expect_equal(result, "unknown")
})

test_that(".oa_type_to_citeproc handles article type with different locations", {
  # Article in journal
  result_journal <- .oa_type_to_citeproc("article", "journal", "publishedVersion")
  expect_equal(result_journal, "article-journal")

  # Article in conference
  result_conf <- .oa_type_to_citeproc("article", "conference", "publishedVersion")
  expect_equal(result_conf, "paper-conference")

  # Article with submitted version
  result_submitted <- .oa_type_to_citeproc("article", "journal", "submittedVersion")
  expect_equal(result_submitted, "article")
})

test_that("openalex2cp handles missing abstract", {
  test_data <- oa_journal_article
  test_data$abstract_inverted_index <- NULL
  result <- openalex2cp(test_data, format="edb-list")
  expect_true(is.na(result$item$abstract))
})

test_that("openalex2cp handles missing DOI", {
  test_data <- oa_journal_article
  test_data$doi <- NULL
  result <- openalex2cp(test_data, format="edb-list")
  expect_true(is.null(result$item$doi))
})

test_that(".oa_uninvert_abstract reconstructs text correctly", {
  # Simple inverted index
  inverted <- list(
    "The" = list(0),
    "quick" = list(1),
    "brown" = list(2),
    "fox" = list(3)
  )
  result <- .oa_uninvert_abstract(inverted)
  expect_equal(result, "The quick brown fox")
})

test_that(".oa_clean_pages handles various page formats", {
  # First and last page
  result1 <- .oa_clean_pages("100", "110")
  expect_equal(result1, "100-110")

  # Same first and last (electronic)
  result2 <- .oa_clean_pages("e12345", "e12345")
  expect_equal(result2, "e12345")

  # Only first page
  result3 <- .oa_clean_pages("100", NULL)
  expect_equal(result3, "100")
})
