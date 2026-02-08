
oadata <- list(
  simple_item_rda = oa_journal_article,
  special_volume_item_rda = oa_special_volume_article,
  institutional_item_rda = readRDS(test_path("testdata", "openalex_institutional_author_journal_article.rda"))
)

test_that("openalex2cp edb-list return finishes without error and returns list", {
  .test_converter_returns_list(openalex2cp, oadata, "openalex2cp")
})

test_that("openalex2cp edb-list result has the right top level entries", {
  .test_converter_top_level_structure(
    openalex2cp, oadata,
    c("item", "author", "author_identifier"),
    "openalex2cp"
  )
})

test_that("openalex2cp edb-list item result has only core citeproc item elements", {
  .test_converter_item_elements(openalex2cp, oadata, "openalex2cp")
})

test_that("openalex2cp edb-list author result has only creator citeproc item elements", {
  .test_converter_author_elements(openalex2cp, oadata, "openalex2cp")
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

# Task 4: Test .oa_extract_authors() helper function

test_that(".oa_extract_authors parses simple names correctly", {
  authorships <- list(
    list(author = list(display_name = "John Smith")),
    list(author = list(display_name = "Jane Doe"))
  )
  result <- .oa_extract_authors(authorships)
  expect_equal(length(result), 2)
  expect_equal(result[[1]]$given, "John")
  expect_equal(result[[1]]$family, "Smith")
  expect_equal(result[[2]]$given, "Jane")
  expect_equal(result[[2]]$family, "Doe")
})

test_that(".oa_extract_authors handles names with particles", {
  authorships <- list(
    list(author = list(display_name = "Ludwig van Beethoven")),
    list(author = list(display_name = "Leonardo da Vinci"))
  )
  result <- .oa_extract_authors(authorships)
  expect_equal(length(result), 2)
  # humaniformat should parse particles - check that family name is populated
  expect_true(nchar(result[[1]]$family) > 0)
  expect_true(nchar(result[[2]]$family) > 0)
  # The exact parsing may vary, but should have some structure
  expect_true("family" %in% names(result[[1]]))
  expect_true("family" %in% names(result[[2]]))
})
