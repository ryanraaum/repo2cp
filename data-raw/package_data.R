## code to prepare `package_data` goes here

cr_journal_article <- readRDS("data-raw/crossref_basic_journal_article.rda")
usethis::use_data(cr_journal_article, overwrite = TRUE)

oa_journal_article <- readRDS("data-raw/openalex_basic_journal_article.rda")
usethis::use_data(oa_journal_article, overwrite = TRUE)

oa_special_volume_article <- readRDS("data-raw/openalex_journal_special_feature_volume.rds")
usethis::use_data(oa_special_volume_article, overwrite = TRUE)

pm_journal_article_file <- "data-raw/pubmed_basic_journal_article.xml"
pm_journal_article_file_size <- file.info(pm_journal_article_file)$size
pm_journal_article_xmltext <- readChar(pm_journal_article_file,
                                       pm_journal_article_file_size)
usethis::use_data(pm_journal_article_xmltext, overwrite = TRUE)
