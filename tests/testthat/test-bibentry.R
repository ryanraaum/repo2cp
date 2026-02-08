article_bibentry <- bibentry(
  bibtype="Article",
  title="Test Article",
  author=person(given="John", family="Smith"),
  journal="J Testing",
  year="2025",
  pages="1-50"
)

multiple_family_name_bibentry <- bibentry(
  bibtype="Article",
  title="Test Article",
  author=person(given="Georg", family=c("von", "Trapp")),
  journal="J Testing",
  year="2025",
  pages="1-50"
)

book_bibentry <- bibentry(
  bibtype="Book",
  title="Book Title",
  author=person(given="Book", family="Author"),
  publisher="Book Publisher",
  year="2025",
  pages="1-50"
)

test_that(".bibentry_type_to_citeproc has basic functionality", {
  expect_equal(.bibentry_type_to_citeproc(article_bibentry), "article-journal")
  expect_equal(.bibentry_type_to_citeproc(book_bibentry), "book")
  expect_equal(.bibentry_type_to_citeproc(be_journal_article), "article-journal")
})

test_that(".bibentry_date has basic functionality", {
  expect_equal(.bibentry_date(article_bibentry), as.Date("2025-01-01"))
  expect_equal(.bibentry_date(book_bibentry), as.Date("2025-01-01"))
  expect_equal(.bibentry_date(be_journal_article), as.Date("2023-01-01"))
})

test_that(".bibentry_first_page has basic functionality", {
  expect_equal(.bibentry_first_page(article_bibentry), "1")
  expect_equal(.bibentry_first_page(book_bibentry), "1")
  expect_equal(.bibentry_first_page(be_journal_article), "e1009061")
})

test_that(".bibentry_extract has basic functionality", {
  article_authors <- expect_no_error(.bibentry_extract(multiple_family_name_bibentry, "author"))
  expect_equal(length(article_authors), 1)
  expect_equal(article_authors[[1]]$given, "Georg")
  expect_equal(article_authors[[1]]$family, "von Trapp")
})

test_that(".bibentry_extract combines family names into single string", {
  article_authors <- expect_no_error(.bibentry_extract(article_bibentry, "author"))
  expect_equal(length(article_authors), 1)
  expect_equal(article_authors[[1]]$given, "John")

  book_authors <- expect_no_error(.bibentry_extract(book_bibentry, "author"))
  expect_equal(book_authors[[1]]$given, "Book")

  article_authors <- expect_no_error(.bibentry_extract(be_journal_article, "author"))
  expect_equal(length(article_authors), 6)
  expect_equal(article_authors[[1]]$given, "Oshane O.")
})

test_that("bibentry2cp edb-list format has basic functionality", {
  article_data <- expect_no_error(bibentry2cp(article_bibentry, format="edb-list"))
  expect_true("item" %in% names(article_data))
  expect_true("author" %in% names(article_data))

  book_data <- expect_no_error(bibentry2cp(book_bibentry, format="edb-list"))
  expect_true("item" %in% names(book_data))
  expect_true("author" %in% names(book_data))

  be_journal_article_data <- expect_no_error(bibentry2cp(be_journal_article, format="edb-list"))
  expect_true("item" %in% names(be_journal_article_data))
  expect_true("author" %in% names(be_journal_article_data))
})

# Edge case and error handling tests

test_that(".bibentry_type_to_citeproc handles known types correctly", {
  # Test that Misc maps to document (a known mapping)
  misc_bibentry <- bibentry(
    bibtype="Misc",
    title="Miscellaneous Item",
    year="2025"
  )
  result <- .bibentry_type_to_citeproc(misc_bibentry)
  expect_equal(result, "document")

  # Test Manual maps to report
  manual_bibentry <- bibentry(
    bibtype="Manual",
    title="Manual Title",
    year="2025"
  )
  result2 <- .bibentry_type_to_citeproc(manual_bibentry)
  expect_equal(result2, "report")
})

test_that("bibentry2cp throws error for multiple entries", {
  multi_entry <- c(article_bibentry, book_bibentry)
  expect_error(bibentry2cp(multi_entry, format="edb-list"),
               "Can only parse one bibentry at a time")
})

test_that(".bibentry_date handles missing month with default", {
  # bibentry with year only (no month specified)
  year_only_bibentry <- bibentry(
    bibtype="Article",
    title="Test",
    author=person("Test", "Author"),
    journal="Test Journal",
    year="2024"
  )
  result <- .bibentry_date(year_only_bibentry)
  expect_equal(result, as.Date("2024-01-01"))
})

test_that("bibentry2cp handles missing DOI", {
  # Article without DOI
  no_doi_bibentry <- bibentry(
    bibtype="Article",
    title="Article Without DOI",
    author=person("No", "DOI"),
    journal="Test Journal",
    year="2025"
  )
  result <- bibentry2cp(no_doi_bibentry, format="edb-list")
  expect_true(is.na(result$item$doi))
})

test_that("bibentry2cp handles missing abstract", {
  # Article without abstract (most common case)
  result <- bibentry2cp(article_bibentry, format="edb-list")
  expect_true(is.na(result$item$abstract))
})

test_that(".bibentry_extract returns NULL for missing editors", {
  # Article with no editors
  result <- .bibentry_extract(article_bibentry, "editor")
  expect_null(result)
})

test_that(".bibentry_container_title handles multiple sources", {
  # Test with fjournal present
  fjournal_bibentry <- bibentry(
    bibtype="Article",
    title="Test",
    author=person("Test", "Author"),
    fjournal="Full Journal Name",
    journal="J Short",
    year="2025"
  )
  result <- .bibentry_container_title(fjournal_bibentry)
  expect_equal(result, "Full Journal Name")

  # Test with only journal
  journal_only_bibentry <- bibentry(
    bibtype="Article",
    title="Test",
    author=person("Test", "Author"),
    journal="Journal Name",
    year="2025"
  )
  result2 <- .bibentry_container_title(journal_only_bibentry)
  expect_equal(result2, "Journal Name")
})

# Task 8: Test BibTeX type diversity and subtypes

test_that(".bibentry_type_to_citeproc handles article subtypes", {
  # article-magazine subtype
  magazine_entry <- bibentry(
    bibtype = "Article",
    entrysubtype = "article-magazine",
    title = "Magazine Article",
    author = person("Test", "Author"),
    journal = "Popular Science",
    year = "2025"
  )
  expect_equal(.bibentry_type_to_citeproc(magazine_entry), "article-magazine")

  # article-newspaper subtype (note: typo in bibtex2csldata.csv - "newpaper")
  newspaper_entry <- bibentry(
    bibtype = "Article",
    entrysubtype = "article-newpaper",
    title = "News Article",
    author = person("Test", "Author"),
    journal = "The Times",
    year = "2025"
  )
  expect_equal(.bibentry_type_to_citeproc(newspaper_entry), "article-newspaper")
})

test_that(".bibentry_type_to_citeproc handles diverse base types", {
  # Book
  book_entry <- bibentry(
    bibtype = "Book",
    title = "Book Title",
    author = person("Book", "Author"),
    publisher = "Publisher",
    year = "2025"
  )
  expect_equal(.bibentry_type_to_citeproc(book_entry), "book")

  # PhdThesis
  thesis_entry <- bibentry(
    bibtype = "PhdThesis",
    title = "Thesis Title",
    author = person("PhD", "Student"),
    school = "University",
    year = "2025"
  )
  expect_equal(.bibentry_type_to_citeproc(thesis_entry), "thesis")

  # InBook (should map to chapter) - requires chapter and publisher fields
  inbook_entry <- bibentry(
    bibtype = "InBook",
    title = "Chapter Title",
    author = person("Chapter", "Author"),
    chapter = "3",
    booktitle = "Book Title",
    publisher = "Publisher",
    year = "2025",
    pages = "1-10"
  )
  expect_equal(.bibentry_type_to_citeproc(inbook_entry), "chapter")

  # Manual (maps to report)
  manual_entry <- bibentry(
    bibtype = "Manual",
    title = "Software Manual",
    year = "2025"
  )
  expect_equal(.bibentry_type_to_citeproc(manual_entry), "report")

  # Misc (maps to document)
  misc_entry <- bibentry(
    bibtype = "Misc",
    title = "Miscellaneous Document",
    year = "2025"
  )
  expect_equal(.bibentry_type_to_citeproc(misc_entry), "document")
})

# Task 14: Test empty/malformed data in BibTeX

test_that("bibentry2cp handles missing year with Misc type", {
  # Misc type doesn't require year, so we can test missing year handling
  no_year_entry <- bibentry(
    bibtype = "Misc",
    title = "Document Without Year"
  )
  result <- expect_no_error(bibentry2cp(no_year_entry, format="edb-list"))
  # Should produce NA for issued date
  expect_true(is.na(result$item$issued))
})

test_that("bibentry2cp handles missing authors with Misc type", {
  # Misc type doesn't require author, so we can test missing author handling
  no_author_entry <- bibentry(
    bibtype = "Misc",
    title = "Document Without Author",
    year = "2025"
  )
  result <- expect_no_error(bibentry2cp(no_author_entry, format="edb-list"))
  # Should handle gracefully, author list may be empty or NULL
  expect_true(length(result$author) == 0 || is.null(result$author))
})
