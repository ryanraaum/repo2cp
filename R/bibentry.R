# Note: this parses a `bibentry` object - created by `bibtex` package and in base R

.bibentry_type_to_citeproc <- function(this_bibentry) {
  bibtype <- tolower(attr(unclass(this_bibentry[1])[[1]], "bibtype"))
  subtype <- this_bibentry$entrysubtype
  if (!is.null(subtype)) { subtype <- tolower(subtype) }
  if (bibtype %in% bibtex2csl_withsubtypes && !is.null(subtype)) {
    if (bibtype == "article") {
      if (subtype == "article-magazine") {
        return("article-magazine")
      } else if (subtype == "article-newpaper") {
        return("article-newspaper")
      }
    } else if (bibtype == "entry") {
      if (subtype == "entry-dictionary") {
        return("entry-dictionary")
      } else if (subtype == "entry-encyclopedia") {
        return("entry-encyclopedia")
      }
    } else if (bibtype == "pamphlet") {
      if (subtype == "paper-conference") {
        return("paper-conference")
      }
    } else if (bibtype == "post") {
      if (subtype == "post-weblog") {
        return("post-weblog")
      }
    } else if (bibtype == "review") {
      if (subtype == "review-book") {
        return("review-book")
      }
    }
  }
  if (bibtype %in% names(bibtex2csl)) {
    return(bibtex2csl[[bibtype]])
  }
  return("unknown")
}

.bibentry_date <- function(this_bibentry) {
  year <- aidr::this_or_na(this_bibentry$year)
  month <- aidr::this_or_na(this_bibentry$month)
  if (!is.na(year) && !is.na(month)) { return(lubridate::ym(glue::glue("{year}-{month}"))) }
  if (!is.na(year)) { return(lubridate::ym(glue::glue("{year}-01"))) }
  return(NA)
}

.bibentry_first_page <- function(this_bibentry) {
  pages <- aidr::this_or_na(this_bibentry$pages)
  if (!is.na(pages)) {
    parts <- stringr::str_split_1(pages, "-")
    if (length(parts) > 0) {
      return(parts[1])
    }
  }
  pages
}

.bibentry_container_title <- function(this_bibentry) {
  possible_titles <- c(aidr::this_or_na(this_bibentry$fjournal),
                       aidr::this_or_na(this_bibentry$journal),
    aidr::this_or_na(this_bibentry$booktitle))
  possible_titles <- possible_titles[!is.na(possible_titles)]
  aidr::this_or_na(possible_titles[1])
}

.bibentry_container_title_short <- function(this_bibentry) {
  if (aidr::this_exists(this_bibentry$fjournal)) {
    return(aidr::this_or_na(this_bibentry$journal))
  }
  return(NA)
}

.bibentry_extract <- function(this_bibentry, creator_type) {
  people = list()
  if (creator_type == "author") {
    people = this_bibentry$author
  } else if (creator_type == "editor") {
    people = this_bibentry$editor
  } else if (creator_type == "translator") {
    people = this_bibentry$translator
  } else {
    return(NULL)
  }

  # Handle case where field is missing or empty
  if (is.null(people) || length(people) == 0) {
    return(NULL)
  }

  # Check if we have a person object (author/editor) or character (translator in base bibentry)
  if (inherits(people, "person")) {
    # Standard person object extraction
    extracted_people = vector("list", length=length(people))
    for (i in seq_along(people)) {
      extracted_people[[i]] = list(
        given = paste(people[i]$given, collapse=" "),
        family = paste(people[i]$family, collapse=" ")
      )
    }
  } else if (is.character(people)) {
    # Character field (translator in base R bibentry) - store as literal
    extracted_people = list(
      list(literal = paste(people, collapse="; "))
    )
  } else {
    return(NULL)
  }

  if (aidr::this_exists(extracted_people)) {
    return(extracted_people)
  }
  return(NULL)
}

#' Convert a bibentry (from bibtex) to citeproc csl-data
#'
#' @param this_bibentry A bibentry object.
#' @param format The format for return of the processed data.
#'
#' @returns A object of the selected return `format`.
#' @export
#'
#' @examples
#' \dontrun{
#' bib <- bibtex::read.bib(package = "base")
#' bibentry2cp(bib[1])
#' }
bibentry2cp <- function(this_bibentry, format="citeproc-json") {
  if (length(this_bibentry) != 1) {stop("Can only parse one bibentry at a time.")}
  res <- list(
    item = list(
      type = .bibentry_type_to_citeproc(this_bibentry),
      language = aidr::this_or_na(this_bibentry$language),
      doi = aidr::this_or_na(this_bibentry$doi),
      volume = aidr::this_or_na(this_bibentry$volume),
      issue = aidr::this_or_na(this_bibentry$number),
      container_title = .bibentry_container_title(this_bibentry),
      container_title_short = .bibentry_container_title_short(this_bibentry),
      issued = .bibentry_date(this_bibentry),
      page_first = .bibentry_first_page(this_bibentry),
      page = aidr::this_or_na(this_bibentry$pages),
      title = aidr::this_or_na(this_bibentry$title),
      abstract= aidr::this_or_na(this_bibentry$abstract),
      publisher = aidr::this_or_na(this_bibentry$publisher),
      publisher_place = aidr::this_or_na(this_bibentry$address),
      url = aidr::this_or_na(this_bibentry$url),
      collection_title = aidr::this_or_na(this_bibentry$series),
      chapter_number = aidr::this_or_na(this_bibentry$chapter),
      edition = aidr::this_or_na(this_bibentry$edition)
    ),
    author = .bibentry_extract(this_bibentry, "author"),
    author_affiliation = NULL,
    author_identifier = NULL,
    editor = .bibentry_extract(this_bibentry, "editor"),
    editor_affiliation = NULL,
    editor_identifier = NULL,
    translator = .bibentry_extract(this_bibentry, "translator"),
    translator_affiliation = NULL,
    translator_identifier = NULL
  )

  if (format == "edb-list") { return(res) }
  stop("citeproc-json return not implemented")
}
