
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
    c("item", "author", "author_affiliation", "author_identifier"),
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
  # After bug fix, .clean_doi() returns character(0) instead of NULL
  expect_equal(length(result$item$doi), 0)
  expect_true(is.character(result$item$doi))
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

# Task 9: Test OpenAlex type diversity

test_that(".oa_type_to_citeproc handles OpenAlex-specific types", {
  # preprint
  expect_equal(.oa_type_to_citeproc("preprint", NULL, NULL), "article")

  # editorial in journal
  expect_equal(.oa_type_to_citeproc("editorial", "journal", NULL), "article-journal")

  # review in journal
  expect_equal(.oa_type_to_citeproc("review", "journal", NULL), "article-journal")

  # letter in journal
  expect_equal(.oa_type_to_citeproc("letter", "journal", NULL), "article-journal")

  # erratum in journal
  expect_equal(.oa_type_to_citeproc("erratum", "journal", NULL), "article-journal")

  # paratext
  expect_equal(.oa_type_to_citeproc("paratext", NULL, NULL), "document")

  # libguides
  expect_equal(.oa_type_to_citeproc("libguides", NULL, NULL), "document")

  # supplementary-materials
  expect_equal(.oa_type_to_citeproc("supplementary-materials", NULL, NULL), "document")

  # other with journal location
  expect_equal(.oa_type_to_citeproc("other", "journal", NULL), "article-journal")
})

test_that(".oa_type_to_citeproc handles crossref fallback types", {
  # When OA type is not in their special list, should use crossref mapping
  expect_equal(.oa_type_to_citeproc("book", NULL, NULL), "book")
  expect_equal(.oa_type_to_citeproc("book-chapter", NULL, NULL), "chapter")
  expect_equal(.oa_type_to_citeproc("dissertation", NULL, NULL), "thesis")
  expect_equal(.oa_type_to_citeproc("dataset", NULL, NULL), "dataset")
  expect_equal(.oa_type_to_citeproc("posted-content", NULL, NULL), "post")
})

# Task 10: Test .oa_uninvert_abstract() edge cases

test_that(".oa_uninvert_abstract handles empty inverted index", {
  # Empty inverted index should produce empty result
  empty_inverted <- list()
  result <- .oa_uninvert_abstract(empty_inverted)
  # With empty list, max(unlist(list())) is -Inf, +1 gives 0, creates zero-length vector
  expect_true(is.character(result))
  expect_equal(result, "")
})

test_that(".oa_uninvert_abstract handles gaps in position sequence", {
  # Positions 0, 2, 5 (missing 1, 3, 4)
  inverted <- list(
    "first" = list(0),
    "third" = list(2),
    "sixth" = list(5)
  )
  result <- .oa_uninvert_abstract(inverted)
  # Should create empty strings for missing positions
  expect_true(is.character(result))
  # Empty positions will be empty strings in the vector, collapsed with spaces
  expect_true(grepl("first", result))
  expect_true(grepl("third", result))
  expect_true(grepl("sixth", result))
})

test_that(".oa_uninvert_abstract handles words at multiple positions", {
  # Word appearing multiple times (duplicate positions for same word)
  inverted <- list(
    "the" = list(0, 3),
    "quick" = list(1),
    "brown" = list(2),
    "fox" = list(4)
  )
  result <- .oa_uninvert_abstract(inverted)
  expect_equal(result, "the quick brown the fox")
})

# Task 13: Test empty/malformed data in OpenAlex

test_that("openalex2cp handles empty authorships", {
  test_data <- oadata$simple_item_rda
  test_data$authorships <- list()
  result <- expect_no_error(openalex2cp(test_data, format="edb-list"))
  expect_equal(length(result$author), 0)
})

test_that("openalex2cp handles NULL primary_location", {
  test_data <- oadata$simple_item_rda
  test_data$primary_location <- NULL
  result <- expect_no_error(openalex2cp(test_data, format="edb-list"))
  # Should still complete without error, but may have missing container info
  expect_true("item" %in% names(result))
})

# Test author affiliation extraction

test_that(".oa_extract_affiliations extracts raw_affiliation_strings correctly", {
  # Use real test data
  result <- openalex2cp(oadata$simple_item_rda, format="edb-list")

  # Should have author_affiliation key
  expect_true("author_affiliation" %in% names(result))

  # Should be a list
  expect_true(is.list(result$author_affiliation))

  # Should have same length as authors
  expect_equal(length(result$author_affiliation), length(result$author))

  # First author should have affiliation (based on test data structure)
  expect_true(length(result$author_affiliation[[1]]) > 0)
})

test_that(".oa_extract_affiliations handles empty authorships", {
  test_data <- oadata$simple_item_rda
  test_data$authorships <- list()
  result <- openalex2cp(test_data, format="edb-list")

  # Should return empty list
  expect_equal(length(result$author_affiliation), 0)
})

test_that(".oa_extract_affiliations handles authors with no affiliations", {
  # Create test data with authorships but no raw_affiliation_strings
  test_data <- oadata$simple_item_rda
  test_data$authorships[[1]]$raw_affiliation_strings <- NULL
  result <- openalex2cp(test_data, format="edb-list")

  # First author should have NULL affiliation
  expect_null(result$author_affiliation[[1]])
})

test_that(".oa_extract_affiliations handles multiple affiliations per author", {
  # Create test data with multiple affiliations for one author
  test_data <- oadata$simple_item_rda
  test_data$authorships[[1]]$raw_affiliation_strings <- list(
    "University of Illinois at Urbana-Champaign",
    "National Center for Supercomputing Applications"
  )
  result <- openalex2cp(test_data, format="edb-list")

  # First author should have multiple affiliations as character vector
  expect_equal(length(result$author_affiliation[[1]]), 2)
  expect_true(is.character(result$author_affiliation[[1]]))
  expect_equal(result$author_affiliation[[1]][1], "University of Illinois at Urbana-Champaign")
  expect_equal(result$author_affiliation[[1]][2], "National Center for Supercomputing Applications")
})

# Test PMID and PMCID extraction

test_that(".oa_clean_pmid extracts PMID from URL correctly", {
  # Test with full URL
  pmid_url <- "https://pubmed.ncbi.nlm.nih.gov/36656910"
  result <- .oa_clean_pmid(pmid_url)
  expect_equal(result, "36656910")
})

test_that(".oa_clean_pmid handles NULL pmid", {
  result <- .oa_clean_pmid(NULL)
  expect_null(result)
})

test_that(".oa_clean_pmcid extracts PMCID from URL correctly", {
  # Test with full URL
  pmcid_url <- "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9970057"
  result <- .oa_clean_pmcid(pmcid_url)
  expect_equal(result, "PMC9970057")
})

test_that(".oa_clean_pmcid handles NULL pmcid", {
  result <- .oa_clean_pmcid(NULL)
  expect_null(result)
})

test_that("openalex2cp extracts PMID correctly", {
  # Test data has PMID
  result <- openalex2cp(oadata$simple_item_rda, format="edb-list")

  # Should have pmid field
  expect_true("pmid" %in% names(result$item))

  # PMID should be extracted and cleaned
  expect_equal(result$item$pmid, "36656910")
})

test_that("openalex2cp handles missing PMID", {
  test_data <- oadata$simple_item_rda
  test_data$ids$pmid <- NULL
  result <- openalex2cp(test_data, format="edb-list")

  # Should have pmid field as NULL
  expect_null(result$item$pmid)
})

test_that("openalex2cp extracts PMCID correctly when present", {
  # Add PMCID to test data
  test_data <- oadata$simple_item_rda
  test_data$ids$pmcid <- "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9970057"
  result <- openalex2cp(test_data, format="edb-list")

  # Should have pmcid field
  expect_true("pmcid" %in% names(result$item))

  # PMCID should be extracted with PMC prefix
  expect_equal(result$item$pmcid, "PMC9970057")
})

test_that("openalex2cp handles missing PMCID", {
  # Test data doesn't have PMCID by default
  result <- openalex2cp(oadata$simple_item_rda, format="edb-list")

  # Should have pmcid field as NULL
  expect_null(result$item$pmcid)
})

# Test publisher extraction

test_that("openalex2cp extracts publisher correctly", {
  # Test with simple_item_rda
  result <- openalex2cp(oadata$simple_item_rda, format="edb-list")

  # Should have publisher field
  expect_true("publisher" %in% names(result$item))

  # Publisher should be extracted from host_organization_name
  expect_equal(result$item$publisher, "International Society for Computational Biology")
  expect_true(is.character(result$item$publisher))
})

test_that("openalex2cp extracts publisher from special volume article", {
  # Test with different publisher
  result <- openalex2cp(oadata$special_volume_item_rda, format="edb-list")

  # Should have publisher field
  expect_true("publisher" %in% names(result$item))
  expect_equal(result$item$publisher, "American Mathematical Society")
})

test_that("openalex2cp handles missing publisher", {
  # Create test data without publisher
  test_data <- oadata$simple_item_rda
  test_data$primary_location$source$host_organization_name <- NULL
  result <- openalex2cp(test_data, format="edb-list")

  # Should have publisher field as NULL
  expect_null(result$item$publisher)
})

test_that("openalex2cp handles missing primary_location gracefully for publisher", {
  # Create test data without primary_location.source
  test_data <- oadata$simple_item_rda
  test_data$primary_location$source <- NULL
  result <- expect_no_error(openalex2cp(test_data, format="edb-list"))

  # Should still complete, publisher will be NULL
  expect_null(result$item$publisher)
})

# Task 15: Test name parsing edge cases (OpenAlex)

test_that(".oa_extract_authors handles names with suffixes", {
  authorships <- list(
    list(author = list(display_name = "John Smith Jr.")),
    list(author = list(display_name = "Jane Doe III"))
  )
  result <- .oa_extract_authors(authorships)
  expect_equal(length(result), 2)
  # humaniformat should parse suffixes into the suffix field
  # Check that parsing completed without error
  expect_true("family" %in% names(result[[1]]))
  expect_true("given" %in% names(result[[1]]))
})

test_that(".oa_extract_authors handles mononyms (single names)", {
  authorships <- list(
    list(author = list(display_name = "Madonna")),
    list(author = list(display_name = "Plato"))
  )
  result <- .oa_extract_authors(authorships)
  expect_equal(length(result), 2)
  # With single names, humaniformat should put them in family field
  # At least one name field should be populated for each author
  expect_true(nchar(result[[1]]$family) > 0 || nchar(result[[1]]$given) > 0)
  expect_true(nchar(result[[2]]$family) > 0 || nchar(result[[2]]$given) > 0)
})

# Task 16: Test format parameter validation

test_that("openalex2cp throws error for non-edb-list format", {
  test_data <- oadata$simple_item_rda
  expect_error(openalex2cp(test_data, format="citeproc-json"),
               "citeproc-json return not implemented")
})
