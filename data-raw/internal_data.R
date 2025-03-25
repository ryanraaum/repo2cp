## code to prepare `DATASET` dataset goes here

## --- data from CSL Data Schema v1.0
# schema as available from github:
#   - user/repository: citation-style-language/schema
#   - path: schemas/input/csl-data.json
csl_schema <- jsonlite::fromJSON("data-raw/csl-data-v1.0.json")

csl_properties <- csl_schema$items$properties
csl_item_types <- csl_properties$type$enum

# "creators" = author, editor, chair, director, etc.
#            = all csl entries referring to some one/group who created the thing
csl_creator_types <- purrr::keep(csl_properties,
                            \(x) all(!is.null(x$type)) &&
                              x$type[1]=="array" &&
                              !is.null(x$items) &&
                              !is.null(x$items$`$ref`)) |>
  names()

csl_creator_types_clean <- janitor::make_clean_names(csl_creator_types)

ctypes2clean <- csl_creator_types_clean
names(ctypes2clean) <- csl_creator_types

clean2ctypes <- csl_creator_types
names(clean2ctypes) <- csl_creator_types_clean

# all csl properties that are about the item and not about the creators
csl_core <- setdiff(names(csl_properties), csl_creator_types)
csl_core_clean <- janitor::make_clean_names(csl_core)

# be able to convert back and forth between valid R/SQL names and the CSL names
core2clean <- csl_core_clean
names(core2clean) <- csl_core

clean2core <- csl_core
names(clean2core) <- csl_core_clean

# properties of csl creators
csl_creator <- names(csl_schema$definitions$`name-variable`$anyOf$properties)
csl_creator_clean <- janitor::make_clean_names(csl_creator)

creator2clean <- csl_creator_clean
names(creator2clean) <- csl_creator

clean2creator <- csl_creator
names(clean2creator) <- csl_creator_clean

csl2clean <- c(core2clean, ctypes2clean, creator2clean)
clean2csl <- c(clean2core, clean2ctypes, clean2creator)

## ---
.create_crossref_to_csl_type_mapping <- function() {
  cr2csl_data <- readr::read_csv("data-raw/crossref2csldata.csv", col_types = "cc")
  cr2csl <- cr2csl_data$csl
  names(cr2csl) <- cr2csl_data$crossref
  cr2csl
}

crossref2csl <- .create_crossref_to_csl_type_mapping()

## ---
.create_language_mapping <- function() {
  # start with the "T" codes
  language3to2 <- ISOcodes::ISO_639_2$Alpha_2
  names3 <- ISOcodes::ISO_639_2$Alpha_3_T

  # find the 20 legacy "B" codes not in the "T" list
  bcode_indices <- which(ISOcodes::ISO_639_2$Alpha_3_B != ISOcodes::ISO_639_2$Alpha_3_T)
  language3to2 <- c(language3to2, ISOcodes::ISO_639_2$Alpha_2[bcode_indices])
  names3 <- c(names3, ISOcodes::ISO_639_2$Alpha_3_B[bcode_indices])

  # put the 3 letter codes on the 2 letter codes as names
  names(language3to2) <- names3

  # clean it up
  language3to2 <- language3to2[!is.na(language3to2)]

  #send it out
  language3to2
}

language3to2 <- .create_language_mapping()

## bibtex to csl mapping from:
## https://tug.ctan.org/biblio/citation-style-language/citeproc-bibtex-data.lua
.create_bibtex_to_csl_mapping <- function() {
  bibtex2csl_data <- readr::read_csv("data-raw/bibtex2csldata.csv", col_types="ccc")
  bibtex2csl <- bibtex2csl_data$csl
  names(bibtex2csl) <- bibtex2csl_data$bibtex
  bibtex2csl
}

bibtex2csl <- .create_bibtex_to_csl_mapping()

## --- save everything for internal data use

usethis::use_data(csl_core, csl_core_clean,
                  core2clean, clean2core,
                  csl_creator_types, csl_creator_types_clean,
                  ctypes2clean, clean2ctypes,
                  csl_creator, csl_creator_clean,
                  creator2clean, clean2creator,
                  csl2clean, clean2csl,
                  crossref2csl, language3to2,
                  bibtex2csl,
                  internal=TRUE, overwrite = TRUE)
