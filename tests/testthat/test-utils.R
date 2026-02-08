
test_that(".clean_doi properly extracts doi", {
  the_doi = "10.1371/journal.pcbi.1009061"
  doi_in_url <- "https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1009061"
  doi_in_string <- "The DOI is 10.1371/journal.pcbi.1009061 in this case"
  expect_equal(.clean_doi(doi_in_url), the_doi)
  expect_equal(.clean_doi(doi_in_string), the_doi)
  expect_equal(.clean_doi(c(doi_in_url, doi_in_string)), c(the_doi, the_doi))
  expect_true(is.na(.clean_doi(NA)))
  # expect_true(is.na(.clean_doi(NULL))) # what to do with NULL input?? does it matter?
})

test_that(".convert_language functions given proper input", {
  expect_equal(.convert_language("eng"), "en")
  expect_equal(.convert_language("en"), "en")
  expect_equal(.convert_language("chi"), "zh")
  expect_equal(.convert_language("zho"), "zh")
})

test_that(".convert_language throws errors for improper input", {
  expect_error(.convert_language("english")) # only takes 2 or 3 letter inputs
  expect_error(.convert_language("zz")) # not all 2 letter codes are valid
  expect_error(.convert_language("zzz")) # not all 3 letter codes are valid
  expect_error(.convert_language("ikx")) # ISOcodes data doesn't have all languages (ikx = Ik)
})

test_that("xml_text_or_null extracts text from xml elements", {
  xml_doc <- xml2::read_xml(xml2::xml2_example("cd_catalog.xml"))
  nodeset <- xml2::xml_find_all(xml_doc, ".//TITLE")
  expect_true(is.character(xml_text_or_null(nodeset)))
})

test_that("xml_text_or_null returns null for empty xml nodesets", {
  xml_doc <- xml2::read_xml(xml2::xml2_example("cd_catalog.xml"))
  nodeset <- xml2::xml_find_all(xml_doc, ".//HORSES")
  expect_true(is.null(xml_text_or_null(nodeset)))
})

# Tests for cplist2json()

test_that("cplist2json converts simple list to JSON", {
  simple_list <- list(title = "Test Article", type = "article-journal")
  result <- cplist2json(simple_list)
  expect_s3_class(result, "json")
  expect_true(grepl("\"title\": \"Test Article\"", result))
  expect_true(grepl("\"type\": \"article-journal\"", result))
})

test_that("cplist2json produces valid JSON that can be parsed", {
  test_list <- list(
    title = "Sample Title",
    volume = "42",
    page = "123-456"
  )
  result <- cplist2json(test_list)
  # Parse it back to verify it's valid JSON
  parsed <- jsonlite::fromJSON(result)
  expect_equal(parsed$title, "Sample Title")
  expect_equal(parsed$volume, "42")
  expect_equal(parsed$page, "123-456")
})

test_that("cplist2json handles nested structures with authors", {
  nested_list <- list(
    title = "Article with Authors",
    author = list(
      list(given = "John", family = "Doe"),
      list(given = "Jane", family = "Smith")
    )
  )
  result <- cplist2json(nested_list)
  expect_s3_class(result, "json")
  parsed <- jsonlite::fromJSON(result)
  expect_equal(length(parsed$author), 2)
  expect_equal(parsed$author$given[1], "John")
  expect_equal(parsed$author$family[2], "Smith")
})

test_that("cplist2json handles various data types correctly", {
  mixed_list <- list(
    title = "Test",
    volume = 42,
    page = "100-110",
    issued = as.Date("2023-01-15")
  )
  result <- cplist2json(mixed_list)
  parsed <- jsonlite::fromJSON(result)
  expect_equal(parsed$title, "Test")
  expect_equal(parsed$volume, 42)
  expect_equal(parsed$page, "100-110")
})

test_that("cplist2json uses auto_unbox for scalar values", {
  test_list <- list(title = "Single Value", volume = "1")
  result <- cplist2json(test_list)
  # Should not have array brackets around scalar values
  expect_true(grepl("\"title\": \"Single Value\"", result))
  expect_false(grepl("\"title\": \\[\"Single Value\"\\]", result))
})

test_that("cplist2json produces pretty-printed JSON", {
  test_list <- list(title = "Test", type = "article")
  result <- cplist2json(test_list)
  # Pretty-printed JSON should have newlines and indentation
  expect_true(grepl("\n", result))
  expect_true(grepl("  ", result))
})

# Tests for cjson()

test_that("cjson combines multiple JSON objects with comma syntax", {
  json1 <- cplist2json(list(title = "Article 1"))
  json2 <- cplist2json(list(title = "Article 2"))
  result <- cjson(json1, json2)
  expect_s3_class(result, "json")
  expect_true(grepl("\"Article 1\"", result))
  expect_true(grepl("\"Article 2\"", result))
  expect_true(grepl("^\\[", result))  # Starts with [
  expect_true(grepl("\\]$", result))  # Ends with ]
})

test_that("cjson combines multiple JSON objects with list syntax", {
  json1 <- cplist2json(list(title = "Article 1"))
  json2 <- cplist2json(list(title = "Article 2"))
  json3 <- cplist2json(list(title = "Article 3"))
  result <- cjson(list(json1, json2, json3))
  expect_s3_class(result, "json")
  expect_true(grepl("\"Article 1\"", result))
  expect_true(grepl("\"Article 2\"", result))
  expect_true(grepl("\"Article 3\"", result))
})

test_that("cjson produces valid JSON array structure", {
  json1 <- cplist2json(list(title = "First", volume = "1"))
  json2 <- cplist2json(list(title = "Second", volume = "2"))
  result <- cjson(json1, json2)
  # Parse the result to verify it's a valid JSON array
  parsed <- jsonlite::fromJSON(result)
  expect_equal(length(parsed), 2)
  expect_equal(parsed$title[1], "First")
  expect_equal(parsed$title[2], "Second")
})

test_that("cjson properly formats nested JSON in array", {
  json1 <- cplist2json(list(title = "Test"))
  result <- cjson(json1)
  expect_true(grepl("\\[\\n  \\{", result))  # Array with indented object
  expect_true(grepl("\\n\\]$", result))      # Closing bracket on new line
})

test_that("cjson throws error for non-JSON inputs", {
  json1 <- cplist2json(list(title = "Valid"))
  not_json <- "just a string"
  expect_error(cjson(json1, not_json), "can only combine `json` elements")
})

test_that("cjson handles single JSON object", {
  json1 <- cplist2json(list(title = "Single Article"))
  result <- cjson(json1)
  expect_s3_class(result, "json")
  expect_true(grepl("\"Single Article\"", result))
  parsed <- jsonlite::fromJSON(result)
  expect_equal(length(parsed), 1)
})

# Task 11: Test utility function edge cases

test_that(".clean_doi handles empty vector", {
  result <- .clean_doi(character(0))
  expect_equal(length(result), 0)
  expect_true(is.character(result))
})

test_that(".clean_doi handles vector of all NA values", {
  result <- .clean_doi(c(NA, NA, NA))
  # str_extract_all on NA returns NA, unlist gives logical(0) or NA
  expect_true(all(is.na(result)))
})

test_that(".clean_doi handles mixed valid and invalid strings", {
  mixed <- c("10.1234/valid", "not a doi", "10.5678/another", "invalid", "10.9999/third")
  result <- .clean_doi(mixed)
  # Should extract only valid DOI patterns
  expect_equal(length(result), 3)
  expect_equal(result[1], "10.1234/valid")
  expect_equal(result[2], "10.5678/another")
  expect_equal(result[3], "10.9999/third")
})

test_that(".convert_language throws error for NULL input", {
  # nchar(NULL) should cause an error
  expect_error(.convert_language(NULL))
})

test_that(".convert_language throws error for empty string", {
  # nchar("") returns 0, which is neither 2 nor 3
  expect_error(.convert_language(""), "language code is neither 2 nor three letters")
})


