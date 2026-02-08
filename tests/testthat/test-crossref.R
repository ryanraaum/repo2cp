crdata <- list(
  simple_item_rda = cr_journal_article,
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
    author_count <- 0
    for (this_author in cr_res$author) {
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

test_that(".crossref_type_to_citeproc throws error for invalid type", {
  expect_error(.crossref_type_to_citeproc("invalid-type"),
               "is not a valid crossref type")
})

test_that("crossref2cp handles missing abstract", {
  test_data <- cr_journal_article
  test_data$abstract <- NULL
  result <- crossref2cp(test_data, format="edb-list")
  expect_true(is.na(result$item$abstract))
})

test_that("crossref2cp handles missing DOI", {
  test_data <- cr_journal_article
  test_data$DOI <- NULL
  result <- crossref2cp(test_data, format="edb-list")
  expect_true(is.null(result$item$doi))
})

test_that("crossref2cp handles institutional authors with literal field", {
  # Create test data with institutional author
  inst_data <- crdata$institutional_item_rda
  result <- crossref2cp(inst_data, format="edb-list")
  # Check that some authors have literal field (institutional names)
  has_literal <- any(sapply(result$author, function(au) "literal" %in% names(au)))
  expect_true(has_literal)
})

test_that("crossref2cp handles missing volume", {
  test_data <- cr_journal_article
  test_data$volume <- NULL
  result <- expect_no_error(crossref2cp(test_data, format="edb-list"))
  expect_true(is.null(result$item$volume))
})

test_that(".crossref_date handles different date field priorities", {
  # Test with only published-print
  test_data <- list(`published-print` = list("date-parts" = list(c(2023, 6, 15))))
  result <- .crossref_date(test_data)
  expect_equal(result, as.Date("2023-06-15"))

  # Test with issued (should take priority)
  test_data_priority <- list(
    issued = list("date-parts" = list(c(2023, 1, 1))),
    published = list("date-parts" = list(c(2023, 6, 15)))
  )
  result_priority <- .crossref_date(test_data_priority)
  expect_equal(result_priority, as.Date("2023-01-01"))
})

test_that(".crossref_date handles partial dates with defaults", {
  # Year only - should default to June 1
  test_data_year <- list(issued = list("date-parts" = list(c(2023))))
  result_year <- .crossref_date(test_data_year)
  expect_equal(result_year, as.Date("2023-06-01"))

  # Year and month - should default to day 1
  test_data_ym <- list(issued = list("date-parts" = list(c(2023, 3))))
  result_ym <- .crossref_date(test_data_ym)
  expect_equal(result_ym, as.Date("2023-03-01"))
})
