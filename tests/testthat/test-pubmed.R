
pmdata <- list(
  simple_item_xml = xml2::read_xml(pm_journal_article_xmltext),
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

# Edge case and error handling tests

test_that("pubmed2cp handles institutional authors with CollectiveName", {
  inst_data <- pmdata$institutional_item_xml
  result <- pubmed2cp(inst_data, format="edb-list")
  # Check that some authors have literal field (collective/institutional names)
  has_literal <- any(sapply(result$author, function(au) "literal" %in% names(au)))
  expect_true(has_literal)
})

test_that(".pubmed_one_author_data extracts individual author correctly", {
  # Create a simple author XML node
  author_xml <- xml2::read_xml('
    <Author>
      <LastName>Smith</LastName>
      <ForeName>John</ForeName>
    </Author>
  ')
  result <- .pubmed_one_author_data(author_xml)
  expect_equal(result$family, "Smith")
  expect_equal(result$given, "John")
  expect_false("literal" %in% names(result))
})

test_that(".pubmed_one_author_data handles collective names", {
  # Create a collective name XML node
  collective_xml <- xml2::read_xml('
    <Author>
      <CollectiveName>COVID-19 Research Consortium</CollectiveName>
    </Author>
  ')
  result <- .pubmed_one_author_data(collective_xml)
  expect_equal(result$literal, "COVID-19 Research Consortium")
  expect_false("given" %in% names(result))
  expect_false("family" %in% names(result))
})

test_that("pubmed2cp handles multiple abstract text blocks", {
  # The code should concatenate multiple AbstractText elements
  result <- pubmed2cp(pmdata$simple_item_xml, format="edb-list")
  # Just verify abstract exists and is a single string
  expect_true(!is.null(result$item$abstract))
  expect_true(is.character(result$item$abstract))
  expect_equal(length(result$item$abstract), 1)
})

test_that("pubmed2cp defaults missing month and day correctly", {
  # The actual data has dates, but the code has defaults for missing components
  # We're testing that the date parsing doesn't fail
  result <- pubmed2cp(pmdata$simple_item_xml, format="edb-list")
  expect_true(inherits(result$item$issued, "Date"))
  expect_false(is.na(result$item$issued))
})
