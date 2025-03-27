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
