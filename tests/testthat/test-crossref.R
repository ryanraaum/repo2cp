crdata <- list(
  simple_item_rda = cr_journal_article,
  institutional_item_rda = readRDS(test_path("testdata", "crossref_institutional_author_journal_article.rda")),
  simple_item_json = jsonlite::read_json(test_path("testdata", "crossref_basic_journal_article.json"),
                                         simplifyVector = TRUE),
  institutional_item_json = jsonlite::read_json(test_path("testdata", "crossref_institutional_author_journal_article.json"),
                                                simplifyVector = TRUE)
)

test_that("crossref2cp edb-list return finishes without error and returns list", {
  .test_converter_returns_list(crossref2cp, crdata, "crossref2cp")
})

test_that("crossref2cp edb-list result has the right top level entries", {
  .test_converter_top_level_structure(
    crossref2cp, crdata,
    c("item", "author", "author_identifier", "author_affiliation"),
    "crossref2cp"
  )
})

test_that("crossref2cp edb-list item result has only core citeproc item elements", {
  .test_converter_item_elements(crossref2cp, crdata, "crossref2cp")
})

test_that("crossref2cp edb-list author result has only creator citeproc item elements", {
  .test_converter_author_elements(crossref2cp, crdata, "crossref2cp")
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

# Task 1: Test .all_empty() helper function

test_that(".all_empty returns TRUE when all elements have empty target element", {
  test_list <- list(
    list(affiliation = list()),
    list(affiliation = list()),
    list(affiliation = list())
  )
  expect_true(.all_empty(test_list, "affiliation"))
})

test_that(".all_empty returns FALSE when any element has non-empty target element", {
  test_list <- list(
    list(affiliation = list()),
    list(affiliation = list(name = "Harvard University")),
    list(affiliation = list())
  )
  expect_false(.all_empty(test_list, "affiliation"))
})

# Task 2: Test .preprocess_author_data() helper function

test_that(".preprocess_author_data converts data.frame to list", {
  author_df <- data.frame(
    given = c("John", "Jane"),
    family = c("Doe", "Smith"),
    stringsAsFactors = FALSE
  )
  result <- .preprocess_author_data(author_df)
  expect_true(is.list(result))
  expect_equal(length(result), 2)
  # purrr::pmap with ~c(...) creates named vectors, not lists
  expect_equal(result[[1]]["given"], c(given = "John"))
  expect_equal(result[[1]]["family"], c(family = "Doe"))
  expect_equal(result[[2]]["given"], c(given = "Jane"))
  expect_equal(result[[2]]["family"], c(family = "Smith"))
})

test_that(".preprocess_author_data passes through list unchanged", {
  author_list <- list(
    list(given = "John", family = "Doe"),
    list(given = "Jane", family = "Smith")
  )
  result <- .preprocess_author_data(author_list)
  expect_identical(result, author_list)
})

# Task 3: Test .crossref_process_author() helper function

test_that(".crossref_process_author filters to valid CSL creator fields", {
  author_data <- list(
    given = "John",
    family = "Doe",
    invalid_field = "should be removed",
    ORCID = "0000-0001-2345-6789"
  )
  result <- .crossref_process_author(author_data)
  expect_true("given" %in% names(result))
  expect_true("family" %in% names(result))
  expect_false("invalid_field" %in% names(result))
  # ORCID is NOT a CSL creator field - it's handled separately in .crossref_identifier_data
  expect_false("orcid" %in% names(result))
  # Only CSL creator fields should remain
  expect_true(all(names(result) %in% csl_creator_clean))
})

test_that(".crossref_process_author converts institutional name to literal", {
  inst_author <- list(
    name = "World Health Organization"
  )
  result <- .crossref_process_author(inst_author)
  expect_true("literal" %in% names(result))
  expect_equal(result$literal, "World Health Organization")
  expect_false("name" %in% names(result))
  expect_false("given" %in% names(result))
  expect_false("family" %in% names(result))
})

# Task 7: Test Crossref type diversity

test_that(".crossref_type_to_citeproc handles diverse Crossref types", {
  # Test various common types beyond journal-article
  expect_equal(.crossref_type_to_citeproc("book"), "book")
  expect_equal(.crossref_type_to_citeproc("book-chapter"), "chapter")
  expect_equal(.crossref_type_to_citeproc("monograph"), "book")
  expect_equal(.crossref_type_to_citeproc("dissertation"), "thesis")
  expect_equal(.crossref_type_to_citeproc("dataset"), "dataset")
  expect_equal(.crossref_type_to_citeproc("proceedings-article"), "paper-conference")
  expect_equal(.crossref_type_to_citeproc("posted-content"), "post")
})

test_that("crossref2cp processes book-chapter type correctly", {
  # Integration test with full book-chapter object
  book_chapter_data <- crdata$simple_item_rda
  book_chapter_data$type <- "book-chapter"
  book_chapter_data$`container-title` <- "Handbook of Testing"

  result <- crossref2cp(book_chapter_data, format="edb-list")
  expect_equal(result$item$type, "chapter")
  expect_equal(result$item$container_title, "Handbook of Testing")
})

# Task 12: Test empty/malformed data in Crossref

test_that("crossref2cp handles empty author list", {
  test_data <- crdata$simple_item_rda
  test_data$author <- list()
  result <- expect_no_error(crossref2cp(test_data, format="edb-list"))
  expect_equal(length(result$author), 0)
})

test_that("crossref2cp handles NULL title", {
  test_data <- crdata$simple_item_rda
  test_data$title <- NULL
  result <- expect_no_error(crossref2cp(test_data, format="edb-list"))
  expect_true(is.null(result$item$title) || is.na(result$item$title))
})

# Task 16: Test format parameter validation

test_that("crossref2cp throws error for invalid format parameter", {
  test_data <- crdata$simple_item_rda
  expect_error(crossref2cp(test_data, format="invalid-format"),
               "unknown format 'invalid-format'")
})
