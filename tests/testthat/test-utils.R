
test_that(".this_exists finds true things", {
  expect_true(.this_exists("a"))
  expect_true(.this_exists(c("a", "b"))) # single response regardless of length of input
  expect_true(.this_exists(0))
  expect_true(.this_exists(list(a=1)))
})

test_that(".this_exists finds false things", {
  expect_false(.this_exists(NA))
  expect_false(.this_exists(NULL))
  expect_false(.this_exists(list()))
  expect_false(.this_exists(c()))
})

test_that(".this_is_singular finds true things", {
  expect_true(.this_is_singular("a"))
  expect_true(.this_is_singular(1))
  expect_true(.this_is_singular(c("a", "a")))
  expect_true(.this_is_singular(c(1,1)))
  expect_true(.this_is_singular(c(TRUE,TRUE)))
})

test_that(".this_is_singular finds false things", {
  expect_false(.this_is_singular(c("a", "b")))
  expect_false(.this_is_singular(c(1,2)))
  expect_false(.this_is_singular(c(TRUE,FALSE)))
})

test_that(".this_is_singular handles missing and null", {
  # and by "handles", we're going with: returns NA
  expect_true(is.na(.this_is_singular(NA)))
  expect_true(is.na(.this_is_singular(NULL)))
})

test_that(".this_or_empty_string returns this", {
  one_string <- "hello"
  two_strings <- c(one_string, one_string)
  expect_equal(.this_or_empty_string(one_string), one_string)
  expect_equal(.this_or_empty_string(two_strings), two_strings)
})

test_that(".this_or_empty_string returns empty string when appropriate", {
  expect_equal(.this_or_empty_string(NA), "")
  expect_equal(.this_or_empty_string(NULL), "")
  expect_equal(.this_or_empty_string(c(NA, NA)), c("", ""))
})

test_that(".this_or_na returns this", {
  one_string <- "hello"
  two_strings <- c(one_string, one_string)
  expect_equal(.this_or_na(one_string), one_string)
  expect_equal(.this_or_na(two_strings), two_strings)
})

test_that(".this_or_na returns NA when appropriate", {
  expect_equal(.this_or_na(NA), NA)
  expect_equal(.this_or_na(NULL), NA)
  expect_equal(.this_or_na(c(NA, NA)), c(NA, NA))
})

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

test_that(".xml_text_or_null extracts text from xml elements", {
  xml_doc <- xml2::read_xml(xml2::xml2_example("cd_catalog.xml"))
  nodeset <- xml2::xml_find_all(xml_doc, ".//TITLE")
  expect_true(is.character(.xml_text_or_null(nodeset)))
})

test_that(".xml_text_or_null returns null for empty xml nodesets", {
  xml_doc <- xml2::read_xml(xml2::xml2_example("cd_catalog.xml"))
  nodeset <- xml2::xml_find_all(xml_doc, ".//HORSES")
  expect_true(is.null(.xml_text_or_null(nodeset)))
})


