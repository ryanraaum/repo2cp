# Helper functions for testing converter functions
# This file is automatically loaded by testthat

#' Test that converter completes without error and returns a list
#'
#' @param converter_func The converter function to test (e.g., crossref2cp)
#' @param test_data Named list of test data sources
#' @param converter_name Name of the converter for error messages
.test_converter_returns_list <- function(converter_func, test_data, converter_name) {
  for (i in seq_along(test_data)) {
    expect_no_failure(res <- converter_func(test_data[[i]], format="edb-list"))
    expect_true(is.list(res),
                label = glue::glue("{converter_name}: data source {names(test_data)[i]}"))
  }
}

#' Test that converter result has expected top level entries
#'
#' @param converter_func The converter function to test
#' @param test_data Named list of test data sources
#' @param expected_elements Character vector of expected top-level element names
#' @param converter_name Name of the converter for error messages
.test_converter_top_level_structure <- function(converter_func, test_data,
                                                 expected_elements, converter_name) {
  for (i in seq_along(test_data)) {
    res <- converter_func(test_data[[i]], format="edb-list")
    this_source <- names(test_data)[i]
    for (this_element in expected_elements) {
      expect_true(this_element %in% names(res),
                  label=glue::glue("{converter_name}: '{this_element}' in {this_source}"))
    }
  }
}

#' Test that converter item result has only core citeproc item elements
#'
#' @param converter_func The converter function to test
#' @param test_data Named list of test data sources
#' @param converter_name Name of the converter for error messages
.test_converter_item_elements <- function(converter_func, test_data, converter_name) {
  for (i in seq_along(test_data)) {
    res <- converter_func(test_data[[i]], format="edb-list")
    this_source <- names(test_data)[i]
    for (this_element in names(res$item)) {
      expect_true(this_element %in% csl_core_clean,
                  label = glue::glue("{converter_name}: unexpected {this_element} in {this_source}"))
    }
  }
}

#' Test that converter author result has only creator citeproc item elements
#'
#' @param converter_func The converter function to test
#' @param test_data Named list of test data sources
#' @param converter_name Name of the converter for error messages
.test_converter_author_elements <- function(converter_func, test_data, converter_name) {
  for (i in seq_along(test_data)) {
    res <- converter_func(test_data[[i]], format="edb-list")
    this_source <- names(test_data)[i]
    author_count <- 0
    for (this_author in res$author) {
      author_count <- author_count + 1
      if (author_count <= 10) { # don't need to overdo this
        for (this_element in names(this_author)) {
          expect_true(this_element %in% csl_creator_clean,
                      label = glue::glue("{converter_name}: unexpected {this_element} in {this_source}"))
        }
      }
    }
  }
}
