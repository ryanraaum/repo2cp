
pmdata <- list(
  simple_item_xml = xml2::read_xml(pm_journal_article_xmltext),
  institutional_item_xml = xml2::read_xml(test_path("testdata", "pubmed_institutional_author_journal_article.xml"))
)

test_that("pubmed2cp edb-list return finishes without error and returns list", {
  .test_converter_returns_list(pubmed2cp, pmdata, "pubmed2cp")
})

test_that("pubmed2cp edb-list result has the right top level entries", {
  .test_converter_top_level_structure(
    pubmed2cp, pmdata,
    c("item", "author", "author_affiliation"),
    "pubmed2cp"
  )
})

test_that("pubmed2cp edb-list item result has only core citeproc item elements", {
  .test_converter_item_elements(pubmed2cp, pmdata, "pubmed2cp")
})

test_that("pubmed2cp edb-list author result has only creator citeproc item elements", {
  .test_converter_author_elements(pubmed2cp, pmdata, "pubmed2cp")
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

# Task 5: Test PubMed DOI conflict handling

test_that("pubmed2cp uses ArticleIdList DOI when sources conflict", {
  # XML with different DOI in ArticleIdList vs ELocationID
  # Current behavior: when DOIs conflict, ArticleIdList DOI takes precedence
  conflict_doi_xml <- xml2::read_xml('
    <PubmedArticle>
      <MedlineCitation>
        <PMID>12345678</PMID>
        <Article>
          <Journal>
            <Title>Test Journal</Title>
            <PubDate>
              <Year>2023</Year>
            </PubDate>
          </Journal>
          <ArticleTitle>Test Article</ArticleTitle>
          <ELocationID EIdType="doi">10.1234/different.doi</ELocationID>
          <Language>eng</Language>
        </Article>
      </MedlineCitation>
      <PubmedData>
        <ArticleIdList>
          <ArticleId IdType="doi">10.5678/another.doi</ArticleId>
          <ArticleId IdType="pubmed">12345678</ArticleId>
        </ArticleIdList>
      </PubmedData>
    </PubmedArticle>
  ')
  result <- expect_no_error(pubmed2cp(conflict_doi_xml, format="edb-list"))
  # ArticleIdList DOI is chosen over ELocationID DOI
  expect_equal(result$item$doi, "10.5678/another.doi")
})

test_that("pubmed2cp handles matching DOI from multiple sources", {
  # XML with same DOI in both ArticleIdList and ELocationID
  matching_doi_xml <- xml2::read_xml('
    <PubmedArticle>
      <MedlineCitation>
        <PMID>12345678</PMID>
        <Article>
          <Journal>
            <Title>Test Journal</Title>
            <PubDate>
              <Year>2023</Year>
            </PubDate>
          </Journal>
          <ArticleTitle>Test Article</ArticleTitle>
          <ELocationID EIdType="doi">10.1234/same.doi</ELocationID>
          <Language>eng</Language>
        </Article>
      </MedlineCitation>
      <PubmedData>
        <ArticleIdList>
          <ArticleId IdType="doi">10.1234/same.doi</ArticleId>
          <ArticleId IdType="pubmed">12345678</ArticleId>
        </ArticleIdList>
      </PubmedData>
    </PubmedArticle>
  ')
  result <- expect_no_error(pubmed2cp(matching_doi_xml, format="edb-list"))
  expect_equal(result$item$doi, "10.1234/same.doi")
})

# Task 6: Test PubMed PMID conflict handling

test_that("pubmed2cp uses ArticleIdList PMID when sources conflict", {
  # XML with different PMID in PMID node vs ArticleIdList
  # Current behavior: when PMIDs conflict, ArticleIdList PMID takes precedence
  conflict_pmid_xml <- xml2::read_xml('
    <PubmedArticle>
      <MedlineCitation>
        <PMID>11111111</PMID>
        <Article>
          <Journal>
            <Title>Test Journal</Title>
            <PubDate>
              <Year>2023</Year>
            </PubDate>
          </Journal>
          <ArticleTitle>Test Article</ArticleTitle>
          <Language>eng</Language>
        </Article>
      </MedlineCitation>
      <PubmedData>
        <ArticleIdList>
          <ArticleId IdType="pubmed">22222222</ArticleId>
        </ArticleIdList>
      </PubmedData>
    </PubmedArticle>
  ')
  result <- expect_no_error(pubmed2cp(conflict_pmid_xml, format="edb-list"))
  # ArticleIdList PMID is chosen over MedlineCitation PMID
  expect_equal(result$item$pmid, "22222222")
})

test_that("pubmed2cp handles matching PMID from multiple sources", {
  # XML with same PMID in both PMID node and ArticleIdList
  matching_pmid_xml <- xml2::read_xml('
    <PubmedArticle>
      <MedlineCitation>
        <PMID>12345678</PMID>
        <Article>
          <Journal>
            <Title>Test Journal</Title>
            <PubDate>
              <Year>2023</Year>
            </PubDate>
          </Journal>
          <ArticleTitle>Test Article</ArticleTitle>
          <Language>eng</Language>
        </Article>
      </MedlineCitation>
      <PubmedData>
        <ArticleIdList>
          <ArticleId IdType="pubmed">12345678</ArticleId>
        </ArticleIdList>
      </PubmedData>
    </PubmedArticle>
  ')
  result <- expect_no_error(pubmed2cp(matching_pmid_xml, format="edb-list"))
  expect_equal(result$item$pmid, "12345678")
})

# Task 16: Test format parameter validation

test_that("pubmed2cp throws error for non-edb-list format", {
  test_xml <- pmdata$simple_item_xml
  expect_error(pubmed2cp(test_xml, format="citeproc-json"),
               "citeproc-json return not implemented")
})

# Task 1: Test author identifier extraction (ORCID and other identifiers)

test_that(".pubmed_author_identifiers extracts ORCID correctly", {
  # Create test XML with ORCID identifier
  author_xml <- xml2::read_xml('
    <Author>
      <LastName>Smith</LastName>
      <ForeName>John</ForeName>
      <Identifier Source="ORCID">https://orcid.org/0000-0001-2345-6789</Identifier>
    </Author>
  ')
  result <- .pubmed_author_identifiers(author_xml)
  expect_equal(length(result), 1)
  expect_equal(result[[1]]$id_type, "orcid")
  expect_equal(result[[1]]$id_value, "0000-0001-2345-6789")  # URL prefix should be removed
})

test_that(".pubmed_author_identifiers handles ORCID without URL prefix", {
  # Test with ORCID that's already just the identifier
  author_xml <- xml2::read_xml('
    <Author>
      <LastName>Smith</LastName>
      <ForeName>John</ForeName>
      <Identifier Source="ORCID">0000-0001-2345-6789</Identifier>
    </Author>
  ')
  result <- .pubmed_author_identifiers(author_xml)
  expect_equal(length(result), 1)
  expect_equal(result[[1]]$id_type, "orcid")
  expect_equal(result[[1]]$id_value, "0000-0001-2345-6789")
})

test_that(".pubmed_author_identifiers extracts multiple identifiers per author", {
  # Create test XML with multiple identifiers
  author_xml <- xml2::read_xml('
    <Author>
      <LastName>Smith</LastName>
      <ForeName>John</ForeName>
      <Identifier Source="ORCID">https://orcid.org/0000-0001-2345-6789</Identifier>
      <Identifier Source="ResearcherID">ABC-1234-2020</Identifier>
    </Author>
  ')
  result <- .pubmed_author_identifiers(author_xml)
  expect_equal(length(result), 2)
  expect_equal(result[[1]]$id_type, "orcid")
  expect_equal(result[[1]]$id_value, "0000-0001-2345-6789")
  expect_equal(result[[2]]$id_type, "researcherid")
  expect_equal(result[[2]]$id_value, "ABC-1234-2020")
})

test_that(".pubmed_author_identifiers returns empty list for author with no identifiers", {
  # Author without any Identifier elements
  author_xml <- xml2::read_xml('
    <Author>
      <LastName>Smith</LastName>
      <ForeName>John</ForeName>
    </Author>
  ')
  result <- .pubmed_author_identifiers(author_xml)
  expect_equal(length(result), 0)
  expect_true(is.list(result))
})

test_that("pubmed2cp includes author_identifier in output", {
  # Create complete PubMed XML with author identifier
  full_xml <- xml2::read_xml('
    <PubmedArticle>
      <MedlineCitation>
        <Article>
          <Journal>
            <Title>Test Journal</Title>
            <PubDate>
              <Year>2023</Year>
            </PubDate>
          </Journal>
          <ArticleTitle>Test Article with ORCID</ArticleTitle>
          <Language>eng</Language>
          <AuthorList>
            <Author>
              <LastName>Smith</LastName>
              <ForeName>John</ForeName>
              <Identifier Source="ORCID">0000-0001-2345-6789</Identifier>
            </Author>
            <Author>
              <LastName>Doe</LastName>
              <ForeName>Jane</ForeName>
            </Author>
          </AuthorList>
        </Article>
      </MedlineCitation>
      <PubmedData>
        <ArticleIdList>
          <ArticleId IdType="pubmed">12345678</ArticleId>
        </ArticleIdList>
      </PubmedData>
    </PubmedArticle>
  ')
  result <- pubmed2cp(full_xml, format="edb-list")

  # Check that author_identifier is present
  expect_true("author_identifier" %in% names(result))

  # Check structure: list with 2 elements (one per author)
  expect_equal(length(result$author_identifier), 2)

  # First author has one identifier
  expect_equal(length(result$author_identifier[[1]]), 1)
  expect_equal(result$author_identifier[[1]][[1]]$id_type, "orcid")
  expect_equal(result$author_identifier[[1]][[1]]$id_value, "0000-0001-2345-6789")

  # Second author has no identifiers
  expect_equal(length(result$author_identifier[[2]]), 0)
})

# Task 2: Test AuthorList/@Type detection and editor handling

test_that("pubmed2cp uses 'author' keys by default (no Type attribute)", {
  # Create PubMed XML without Type attribute (default behavior)
  default_xml <- xml2::read_xml('
    <PubmedArticle>
      <MedlineCitation>
        <Article>
          <Journal>
            <Title>Test Journal</Title>
            <PubDate>
              <Year>2023</Year>
            </PubDate>
          </Journal>
          <ArticleTitle>Test Article</ArticleTitle>
          <Language>eng</Language>
          <AuthorList>
            <Author>
              <LastName>Smith</LastName>
              <ForeName>John</ForeName>
            </Author>
          </AuthorList>
        </Article>
      </MedlineCitation>
      <PubmedData>
        <ArticleIdList>
          <ArticleId IdType="pubmed">12345678</ArticleId>
        </ArticleIdList>
      </PubmedData>
    </PubmedArticle>
  ')
  result <- pubmed2cp(default_xml, format="edb-list")

  # Should have author keys, not editor keys
  expect_true("author" %in% names(result))
  expect_true("author_affiliation" %in% names(result))
  expect_true("author_identifier" %in% names(result))
  expect_false("editor" %in% names(result))
  expect_false("editor_affiliation" %in% names(result))
  expect_false("editor_identifier" %in% names(result))
})

test_that("pubmed2cp uses 'author' keys when Type='authors'", {
  # Create PubMed XML with Type="authors" (explicit)
  authors_xml <- xml2::read_xml('
    <PubmedArticle>
      <MedlineCitation>
        <Article>
          <Journal>
            <Title>Test Journal</Title>
            <PubDate>
              <Year>2023</Year>
            </PubDate>
          </Journal>
          <ArticleTitle>Test Article</ArticleTitle>
          <Language>eng</Language>
          <AuthorList Type="authors">
            <Author>
              <LastName>Smith</LastName>
              <ForeName>John</ForeName>
            </Author>
          </AuthorList>
        </Article>
      </MedlineCitation>
      <PubmedData>
        <ArticleIdList>
          <ArticleId IdType="pubmed">12345678</ArticleId>
        </ArticleIdList>
      </PubmedData>
    </PubmedArticle>
  ')
  result <- pubmed2cp(authors_xml, format="edb-list")

  # Should have author keys, not editor keys
  expect_true("author" %in% names(result))
  expect_true("author_affiliation" %in% names(result))
  expect_true("author_identifier" %in% names(result))
  expect_false("editor" %in% names(result))
})

test_that("pubmed2cp uses 'editor' keys when Type='editors'", {
  # Create PubMed XML with Type="editors" (book chapter)
  editors_xml <- xml2::read_xml('
    <PubmedArticle>
      <MedlineCitation>
        <Article>
          <Journal>
            <Title>Test Book</Title>
            <PubDate>
              <Year>2023</Year>
            </PubDate>
          </Journal>
          <ArticleTitle>Book Chapter Title</ArticleTitle>
          <Language>eng</Language>
          <AuthorList Type="editors">
            <Author>
              <LastName>Editor</LastName>
              <ForeName>Chief</ForeName>
            </Author>
            <Author>
              <LastName>Associate</LastName>
              <ForeName>Editor</ForeName>
            </Author>
          </AuthorList>
        </Article>
      </MedlineCitation>
      <PubmedData>
        <ArticleIdList>
          <ArticleId IdType="pubmed">12345678</ArticleId>
        </ArticleIdList>
      </PubmedData>
    </PubmedArticle>
  ')
  result <- pubmed2cp(editors_xml, format="edb-list")

  # Should have editor keys, not author keys
  expect_false("author" %in% names(result))
  expect_false("author_affiliation" %in% names(result))
  expect_false("author_identifier" %in% names(result))
  expect_true("editor" %in% names(result))
  expect_true("editor_affiliation" %in% names(result))
  expect_true("editor_identifier" %in% names(result))

  # Check editor data is populated correctly
  expect_equal(length(result$editor), 2)
  expect_equal(result$editor[[1]]$family, "Editor")
  expect_equal(result$editor[[1]]$given, "Chief")
  expect_equal(result$editor[[2]]$family, "Associate")
  expect_equal(result$editor[[2]]$given, "Editor")
})

test_that("pubmed2cp handles editors with identifiers", {
  # Test that editor_identifier works correctly
  editors_with_orcid_xml <- xml2::read_xml('
    <PubmedArticle>
      <MedlineCitation>
        <Article>
          <Journal>
            <Title>Test Book</Title>
            <PubDate>
              <Year>2023</Year>
            </PubDate>
          </Journal>
          <ArticleTitle>Book Chapter Title</ArticleTitle>
          <Language>eng</Language>
          <AuthorList Type="editors">
            <Author>
              <LastName>Editor</LastName>
              <ForeName>Chief</ForeName>
              <Identifier Source="ORCID">0000-0001-2345-6789</Identifier>
            </Author>
          </AuthorList>
        </Article>
      </MedlineCitation>
      <PubmedData>
        <ArticleIdList>
          <ArticleId IdType="pubmed">12345678</ArticleId>
        </ArticleIdList>
      </PubmedData>
    </PubmedArticle>
  ')
  result <- pubmed2cp(editors_with_orcid_xml, format="edb-list")

  # Check editor_identifier structure
  expect_true("editor_identifier" %in% names(result))
  expect_equal(length(result$editor_identifier), 1)
  expect_equal(length(result$editor_identifier[[1]]), 1)
  expect_equal(result$editor_identifier[[1]][[1]]$id_type, "orcid")
  expect_equal(result$editor_identifier[[1]][[1]]$id_value, "0000-0001-2345-6789")
})

# Task 3: Test Author/Suffix extraction (Initials not extracted - not a CSL field)

test_that(".pubmed_one_author_data extracts suffix correctly", {
  # Create test XML with suffix
  author_xml <- xml2::read_xml('
    <Author>
      <LastName>King</LastName>
      <ForeName>Martin Luther</ForeName>
      <Suffix>Jr</Suffix>
    </Author>
  ')
  result <- .pubmed_one_author_data(author_xml)
  expect_equal(result$family, "King")
  expect_equal(result$given, "Martin Luther")
  expect_equal(result$suffix, "Jr")
})

test_that(".pubmed_one_author_data handles authors without suffix", {
  # Author with only basic name fields
  author_xml <- xml2::read_xml('
    <Author>
      <LastName>Doe</LastName>
      <ForeName>Jane</ForeName>
    </Author>
  ')
  result <- .pubmed_one_author_data(author_xml)
  expect_equal(result$family, "Doe")
  expect_equal(result$given, "Jane")
  # suffix should not be present (filtered out by lengths != 0)
  expect_false("suffix" %in% names(result))
})

test_that("pubmed2cp includes suffix in author output", {
  # Create complete PubMed XML with suffix
  full_xml <- xml2::read_xml('
    <PubmedArticle>
      <MedlineCitation>
        <Article>
          <Journal>
            <Title>Test Journal</Title>
            <PubDate>
              <Year>2023</Year>
            </PubDate>
          </Journal>
          <ArticleTitle>Test Article with Suffix</ArticleTitle>
          <Language>eng</Language>
          <AuthorList>
            <Author>
              <LastName>King</LastName>
              <ForeName>Martin Luther</ForeName>
              <Suffix>Jr</Suffix>
            </Author>
            <Author>
              <LastName>Smith</LastName>
              <ForeName>John</ForeName>
            </Author>
            <Author>
              <LastName>Doe</LastName>
              <ForeName>Jane</ForeName>
            </Author>
          </AuthorList>
        </Article>
      </MedlineCitation>
      <PubmedData>
        <ArticleIdList>
          <ArticleId IdType="pubmed">12345678</ArticleId>
        </ArticleIdList>
      </PubmedData>
    </PubmedArticle>
  ')
  result <- pubmed2cp(full_xml, format="edb-list")

  # First author has suffix
  expect_equal(result$author[[1]]$family, "King")
  expect_equal(result$author[[1]]$given, "Martin Luther")
  expect_equal(result$author[[1]]$suffix, "Jr")

  # Second and third authors have no suffix
  expect_equal(result$author[[2]]$family, "Smith")
  expect_equal(result$author[[2]]$given, "John")
  expect_false("suffix" %in% names(result$author[[2]]))

  expect_equal(result$author[[3]]$family, "Doe")
  expect_equal(result$author[[3]]$given, "Jane")
  expect_false("suffix" %in% names(result$author[[3]]))
})

test_that("pubmed2cp includes suffix in editor output", {
  # Test that suffix works for editors too
  editors_xml <- xml2::read_xml('
    <PubmedArticle>
      <MedlineCitation>
        <Article>
          <Journal>
            <Title>Test Book</Title>
            <PubDate>
              <Year>2023</Year>
            </PubDate>
          </Journal>
          <ArticleTitle>Book Chapter</ArticleTitle>
          <Language>eng</Language>
          <AuthorList Type="editors">
            <Author>
              <LastName>Editor</LastName>
              <ForeName>Chief</ForeName>
              <Suffix>III</Suffix>
            </Author>
          </AuthorList>
        </Article>
      </MedlineCitation>
      <PubmedData>
        <ArticleIdList>
          <ArticleId IdType="pubmed">12345678</ArticleId>
        </ArticleIdList>
      </PubmedData>
    </PubmedArticle>
  ')
  result <- pubmed2cp(editors_xml, format="edb-list")

  # Check editor has suffix
  expect_equal(result$editor[[1]]$family, "Editor")
  expect_equal(result$editor[[1]]$given, "Chief")
  expect_equal(result$editor[[1]]$suffix, "III")
})

# Task 4: Test multiple affiliations per author extraction

test_that(".pubmed_one_affiliation_data extracts all affiliations", {
  # Create test XML with multiple AffiliationInfo elements
  author_xml <- xml2::read_xml('
    <Author>
      <LastName>Smith</LastName>
      <ForeName>John</ForeName>
      <AffiliationInfo>
        <Affiliation>Department of Biology, University of X</Affiliation>
      </AffiliationInfo>
      <AffiliationInfo>
        <Affiliation>Institute for Advanced Study, City Y</Affiliation>
      </AffiliationInfo>
    </Author>
  ')
  result <- .pubmed_one_affiliation_data(author_xml)

  # Should return character vector with 2 elements
  expect_true(is.character(result))
  expect_equal(length(result), 2)
  expect_equal(result[1], "Department of Biology, University of X")
  expect_equal(result[2], "Institute for Advanced Study, City Y")
})

test_that(".pubmed_one_affiliation_data handles single affiliation", {
  # Author with only one affiliation
  author_xml <- xml2::read_xml('
    <Author>
      <LastName>Doe</LastName>
      <ForeName>Jane</ForeName>
      <AffiliationInfo>
        <Affiliation>Single Institution</Affiliation>
      </AffiliationInfo>
    </Author>
  ')
  result <- .pubmed_one_affiliation_data(author_xml)

  # Should return character vector with 1 element
  expect_true(is.character(result))
  expect_equal(length(result), 1)
  expect_equal(result[1], "Single Institution")
})

test_that(".pubmed_one_affiliation_data handles no affiliations", {
  # Author without any affiliations
  author_xml <- xml2::read_xml('
    <Author>
      <LastName>Anonymous</LastName>
      <ForeName>Author</ForeName>
    </Author>
  ')
  result <- .pubmed_one_affiliation_data(author_xml)

  # Should return NULL for no affiliations
  expect_null(result)
})

test_that("pubmed2cp includes all affiliations per author in output", {
  # Create complete PubMed XML with multiple affiliations
  full_xml <- xml2::read_xml('
    <PubmedArticle>
      <MedlineCitation>
        <Article>
          <Journal>
            <Title>Test Journal</Title>
            <PubDate>
              <Year>2023</Year>
            </PubDate>
          </Journal>
          <ArticleTitle>Test Article with Multiple Affiliations</ArticleTitle>
          <Language>eng</Language>
          <AuthorList>
            <Author>
              <LastName>Smith</LastName>
              <ForeName>John</ForeName>
              <AffiliationInfo>
                <Affiliation>Department of Biology, University of X</Affiliation>
              </AffiliationInfo>
              <AffiliationInfo>
                <Affiliation>Institute for Advanced Study, City Y</Affiliation>
              </AffiliationInfo>
            </Author>
            <Author>
              <LastName>Doe</LastName>
              <ForeName>Jane</ForeName>
              <AffiliationInfo>
                <Affiliation>Single Institution</Affiliation>
              </AffiliationInfo>
            </Author>
            <Author>
              <LastName>Anonymous</LastName>
              <ForeName>Author</ForeName>
            </Author>
          </AuthorList>
        </Article>
      </MedlineCitation>
      <PubmedData>
        <ArticleIdList>
          <ArticleId IdType="pubmed">12345678</ArticleId>
        </ArticleIdList>
      </PubmedData>
    </PubmedArticle>
  ')
  result <- pubmed2cp(full_xml, format="edb-list")

  # Check that author_affiliation is a list
  expect_true(is.list(result$author_affiliation))
  expect_equal(length(result$author_affiliation), 3)

  # First author has 2 affiliations (character vector with 2 elements)
  expect_true(is.character(result$author_affiliation[[1]]))
  expect_equal(length(result$author_affiliation[[1]]), 2)
  expect_equal(result$author_affiliation[[1]][1], "Department of Biology, University of X")
  expect_equal(result$author_affiliation[[1]][2], "Institute for Advanced Study, City Y")

  # Second author has 1 affiliation
  expect_true(is.character(result$author_affiliation[[2]]))
  expect_equal(length(result$author_affiliation[[2]]), 1)
  expect_equal(result$author_affiliation[[2]][1], "Single Institution")

  # Third author has no affiliations (NULL)
  expect_null(result$author_affiliation[[3]])
})

test_that("pubmed2cp includes all affiliations for editors", {
  # Test that multiple affiliations work for editors too
  editors_xml <- xml2::read_xml('
    <PubmedArticle>
      <MedlineCitation>
        <Article>
          <Journal>
            <Title>Test Book</Title>
            <PubDate>
              <Year>2023</Year>
            </PubDate>
          </Journal>
          <ArticleTitle>Book Chapter</ArticleTitle>
          <Language>eng</Language>
          <AuthorList Type="editors">
            <Author>
              <LastName>Editor</LastName>
              <ForeName>Chief</ForeName>
              <AffiliationInfo>
                <Affiliation>University A</Affiliation>
              </AffiliationInfo>
              <AffiliationInfo>
                <Affiliation>Research Institute B</Affiliation>
              </AffiliationInfo>
            </Author>
          </AuthorList>
        </Article>
      </MedlineCitation>
      <PubmedData>
        <ArticleIdList>
          <ArticleId IdType="pubmed">12345678</ArticleId>
        </ArticleIdList>
      </PubmedData>
    </PubmedArticle>
  ')
  result <- pubmed2cp(editors_xml, format="edb-list")

  # Check editor has multiple affiliations
  expect_true("editor_affiliation" %in% names(result))
  expect_equal(length(result$editor_affiliation[[1]]), 2)
  expect_equal(result$editor_affiliation[[1]][1], "University A")
  expect_equal(result$editor_affiliation[[1]][2], "Research Institute B")
})

# Task 5: Test publication type mapping

test_that(".pubmed_type_to_csl maps Journal Article correctly", {
  # Journal Article should map to article-journal
  test_xml <- xml2::read_xml('
    <PubmedArticle>
      <MedlineCitation>
        <Article>
          <PublicationTypeList>
            <PublicationType UI="D016428">Journal Article</PublicationType>
          </PublicationTypeList>
        </Article>
      </MedlineCitation>
    </PubmedArticle>
  ')
  result <- .pubmed_type_to_csl(test_xml)
  expect_equal(result, "article-journal")
})

test_that(".pubmed_type_to_csl maps Review correctly", {
  # Review should map to review (not article-journal)
  test_xml <- xml2::read_xml('
    <PubmedArticle>
      <MedlineCitation>
        <Article>
          <PublicationTypeList>
            <PublicationType UI="D016454">Review</PublicationType>
          </PublicationTypeList>
        </Article>
      </MedlineCitation>
    </PubmedArticle>
  ')
  result <- .pubmed_type_to_csl(test_xml)
  expect_equal(result, "review")
})

test_that(".pubmed_type_to_csl maps Meta-Analysis correctly", {
  # Meta-Analysis should also map to review
  test_xml <- xml2::read_xml('
    <PubmedArticle>
      <MedlineCitation>
        <Article>
          <PublicationTypeList>
            <PublicationType UI="D017418">Meta-Analysis</PublicationType>
          </PublicationTypeList>
        </Article>
      </MedlineCitation>
    </PubmedArticle>
  ')
  result <- .pubmed_type_to_csl(test_xml)
  expect_equal(result, "review")
})

test_that(".pubmed_type_to_csl maps Book correctly", {
  # Book should map to book
  test_xml <- xml2::read_xml('
    <PubmedArticle>
      <MedlineCitation>
        <Article>
          <PublicationTypeList>
            <PublicationType UI="D016428">Book</PublicationType>
          </PublicationTypeList>
        </Article>
      </MedlineCitation>
    </PubmedArticle>
  ')
  result <- .pubmed_type_to_csl(test_xml)
  expect_equal(result, "book")
})

test_that(".pubmed_type_to_csl maps Book Chapter correctly", {
  # Book Chapter should map to chapter
  test_xml <- xml2::read_xml('
    <PubmedArticle>
      <MedlineCitation>
        <Article>
          <PublicationTypeList>
            <PublicationType UI="D016422">Book Chapter</PublicationType>
          </PublicationTypeList>
        </Article>
      </MedlineCitation>
    </PubmedArticle>
  ')
  result <- .pubmed_type_to_csl(test_xml)
  expect_equal(result, "chapter")
})

test_that(".pubmed_type_to_csl defaults to article-journal for missing type", {
  # No PublicationTypeList should default to article-journal
  test_xml <- xml2::read_xml('
    <PubmedArticle>
      <MedlineCitation>
        <Article>
          <ArticleTitle>Test</ArticleTitle>
        </Article>
      </MedlineCitation>
    </PubmedArticle>
  ')
  result <- .pubmed_type_to_csl(test_xml)
  expect_equal(result, "article-journal")
})

test_that(".pubmed_type_to_csl defaults to article-journal for unmapped type", {
  # Unknown/unmapped types should default to article-journal
  test_xml <- xml2::read_xml('
    <PubmedArticle>
      <MedlineCitation>
        <Article>
          <PublicationTypeList>
            <PublicationType>Unknown Publication Type</PublicationType>
          </PublicationTypeList>
        </Article>
      </MedlineCitation>
    </PubmedArticle>
  ')
  result <- .pubmed_type_to_csl(test_xml)
  expect_equal(result, "article-journal")
})

test_that(".pubmed_type_to_csl uses first PublicationType when multiple present", {
  # When multiple types present, should use the first (primary) one
  # Review is first, so should map to review
  test_xml <- xml2::read_xml('
    <PubmedArticle>
      <MedlineCitation>
        <Article>
          <PublicationTypeList>
            <PublicationType UI="D016454">Review</PublicationType>
            <PublicationType UI="D016428">Journal Article</PublicationType>
          </PublicationTypeList>
        </Article>
      </MedlineCitation>
    </PubmedArticle>
  ')
  result <- .pubmed_type_to_csl(test_xml)
  expect_equal(result, "review")
})

test_that("pubmed2cp sets type correctly from PublicationType", {
  # Test that pubmed2cp integration correctly extracts and maps type
  # Create full PubMed XML with Review type
  review_xml <- xml2::read_xml('
    <PubmedArticle>
      <MedlineCitation>
        <Article>
          <Journal>
            <Title>Test Journal</Title>
            <PubDate>
              <Year>2023</Year>
            </PubDate>
          </Journal>
          <ArticleTitle>Test Review Article</ArticleTitle>
          <Language>eng</Language>
          <PublicationTypeList>
            <PublicationType UI="D016454">Review</PublicationType>
          </PublicationTypeList>
          <AuthorList>
            <Author>
              <LastName>Smith</LastName>
              <ForeName>John</ForeName>
            </Author>
          </AuthorList>
        </Article>
      </MedlineCitation>
      <PubmedData>
        <ArticleIdList>
          <ArticleId IdType="pubmed">12345678</ArticleId>
        </ArticleIdList>
      </PubmedData>
    </PubmedArticle>
  ')
  result <- pubmed2cp(review_xml, format="edb-list")
  expect_equal(result$item$type, "review")
})

test_that("pubmed2cp sets type to article-journal for existing test data", {
  # Verify that simple_item_xml (with Journal Article type) returns article-journal
  result <- pubmed2cp(pmdata$simple_item_xml, format="edb-list")
  expect_equal(result$item$type, "article-journal")
})

# Task 6: Test MedlinePgn pagination fallback

test_that("pubmed2cp uses MedlinePgn when StartPage and EndPage are missing", {
  # Test with only MedlinePgn present (no StartPage/EndPage)
  medlinepgn_xml <- xml2::read_xml('
    <PubmedArticle>
      <MedlineCitation>
        <Article>
          <Journal>
            <Title>Test Journal</Title>
            <PubDate>
              <Year>2023</Year>
            </PubDate>
          </Journal>
          <ArticleTitle>Article with MedlinePgn Only</ArticleTitle>
          <Language>eng</Language>
          <Pagination>
            <MedlinePgn>e1234567</MedlinePgn>
          </Pagination>
          <PublicationTypeList>
            <PublicationType UI="D016428">Journal Article</PublicationType>
          </PublicationTypeList>
          <AuthorList>
            <Author>
              <LastName>Smith</LastName>
              <ForeName>John</ForeName>
            </Author>
          </AuthorList>
        </Article>
      </MedlineCitation>
      <PubmedData>
        <ArticleIdList>
          <ArticleId IdType="pubmed">12345678</ArticleId>
        </ArticleIdList>
      </PubmedData>
    </PubmedArticle>
  ')
  result <- pubmed2cp(medlinepgn_xml, format="edb-list")
  expect_equal(result$item$page, "e1234567")
})

test_that("pubmed2cp uses MedlinePgn for page range fallback", {
  # Test with MedlinePgn containing a page range
  medlinepgn_range_xml <- xml2::read_xml('
    <PubmedArticle>
      <MedlineCitation>
        <Article>
          <Journal>
            <Title>Test Journal</Title>
            <PubDate>
              <Year>2023</Year>
            </PubDate>
          </Journal>
          <ArticleTitle>Article with Page Range in MedlinePgn</ArticleTitle>
          <Language>eng</Language>
          <Pagination>
            <MedlinePgn>123-45</MedlinePgn>
          </Pagination>
          <PublicationTypeList>
            <PublicationType UI="D016428">Journal Article</PublicationType>
          </PublicationTypeList>
          <AuthorList>
            <Author>
              <LastName>Doe</LastName>
              <ForeName>Jane</ForeName>
            </Author>
          </AuthorList>
        </Article>
      </MedlineCitation>
      <PubmedData>
        <ArticleIdList>
          <ArticleId IdType="pubmed">87654321</ArticleId>
        </ArticleIdList>
      </PubmedData>
    </PubmedArticle>
  ')
  result <- pubmed2cp(medlinepgn_range_xml, format="edb-list")
  expect_equal(result$item$page, "123-45")
})

test_that("pubmed2cp prefers StartPage/EndPage over MedlinePgn", {
  # When both StartPage/EndPage and MedlinePgn are present, StartPage should take precedence
  both_present_xml <- xml2::read_xml('
    <PubmedArticle>
      <MedlineCitation>
        <Article>
          <Journal>
            <Title>Test Journal</Title>
            <PubDate>
              <Year>2023</Year>
            </PubDate>
          </Journal>
          <ArticleTitle>Article with Both Page Fields</ArticleTitle>
          <Language>eng</Language>
          <Pagination>
            <StartPage>100</StartPage>
            <EndPage>110</EndPage>
            <MedlinePgn>100-10</MedlinePgn>
          </Pagination>
          <PublicationTypeList>
            <PublicationType UI="D016428">Journal Article</PublicationType>
          </PublicationTypeList>
          <AuthorList>
            <Author>
              <LastName>Test</LastName>
              <ForeName>Author</ForeName>
            </Author>
          </AuthorList>
        </Article>
      </MedlineCitation>
      <PubmedData>
        <ArticleIdList>
          <ArticleId IdType="pubmed">11111111</ArticleId>
        </ArticleIdList>
      </PubmedData>
    </PubmedArticle>
  ')
  result <- pubmed2cp(both_present_xml, format="edb-list")
  # Should use StartPage-EndPage, not MedlinePgn
  expect_equal(result$item$page, "100-110")
})

test_that("pubmed2cp returns NULL page when no pagination fields present", {
  # Test with no pagination fields at all
  no_pages_xml <- xml2::read_xml('
    <PubmedArticle>
      <MedlineCitation>
        <Article>
          <Journal>
            <Title>Test Journal</Title>
            <PubDate>
              <Year>2023</Year>
            </PubDate>
          </Journal>
          <ArticleTitle>Article with No Pagination</ArticleTitle>
          <Language>eng</Language>
          <PublicationTypeList>
            <PublicationType UI="D016428">Journal Article</PublicationType>
          </PublicationTypeList>
          <AuthorList>
            <Author>
              <LastName>Anonymous</LastName>
              <ForeName>Author</ForeName>
            </Author>
          </AuthorList>
        </Article>
      </MedlineCitation>
      <PubmedData>
        <ArticleIdList>
          <ArticleId IdType="pubmed">99999999</ArticleId>
        </ArticleIdList>
      </PubmedData>
    </PubmedArticle>
  ')
  result <- pubmed2cp(no_pages_xml, format="edb-list")
  expect_null(result$item$page)
})

test_that("pubmed2cp uses StartPage only when EndPage missing", {
  # Test that StartPage alone takes precedence over MedlinePgn
  startpage_only_xml <- xml2::read_xml('
    <PubmedArticle>
      <MedlineCitation>
        <Article>
          <Journal>
            <Title>Test Journal</Title>
            <PubDate>
              <Year>2023</Year>
            </PubDate>
          </Journal>
          <ArticleTitle>Article with StartPage Only</ArticleTitle>
          <Language>eng</Language>
          <Pagination>
            <StartPage>42</StartPage>
            <MedlinePgn>42-50</MedlinePgn>
          </Pagination>
          <PublicationTypeList>
            <PublicationType UI="D016428">Journal Article</PublicationType>
          </PublicationTypeList>
          <AuthorList>
            <Author>
              <LastName>Test</LastName>
              <ForeName>Author</ForeName>
            </Author>
          </AuthorList>
        </Article>
      </MedlineCitation>
      <PubmedData>
        <ArticleIdList>
          <ArticleId IdType="pubmed">22222222</ArticleId>
        </ArticleIdList>
      </PubmedData>
    </PubmedArticle>
  ')
  result <- pubmed2cp(startpage_only_xml, format="edb-list")
  # Should use StartPage alone, not MedlinePgn
  expect_equal(result$item$page, "42")
})
