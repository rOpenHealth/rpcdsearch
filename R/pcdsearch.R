#' Constructor function for MedicalDefinition class
#' @export
#' @param terms list of character vectors or NULL
#' @param codes list of character vectors or NULL
#' @param tests list of character vectors or NULL
#' @param drugs list of character vectors or NULL
#' @param drugcodes list of character vectors or NULL
#' @examples
#' def <- MedicalDefinition(terms = list(c("angina", "unstable"), c("angina", "Crescendo "),
#'                                       c("angina", "Refractory")),
#'                          codes = list("G33..00", "G330.00"))
#' class(def)
MedicalDefinition <- function(terms = NULL, codes = NULL, tests = NULL,
                              drugs = NULL, drugcodes = NULL){
    def <- structure(
        list(terms = terms,
             codes = terms,
             tests = terms,
             drugs = terms,
             drugcodes = drugcodes),
        class = "MedicalDefinition")
    for(item in def){
        assert_that(is.null(item) || is.list(item))
    }
    def
}

#' Imports definitions to be searched from a csv file into a MedicalDefinition object
#' @export
#' @param input_file character path to the input file
#' @examples
#' def2 <- import_definitions(system.file("extdata", "example_search.csv",
#'                                             package = "rpcdsearch"))
import_definitions <- function(input_file){
    con  <- file(input_file, open = "r")
    def_list <- MedicalDefinition(list(), list(), list(), list(), list())
    n <- 1

    while (length(point <- readLines(con, n = 1, warn = FALSE)) > 0) {
        point_data <- strsplit(point, ",")[[1]]
        if (all(point_data[1:3] == c("definition", "status", "items"))) {
            point <- readLines(con, n = 1, warn = FALSE)
            if(!length(point)) break
            point_data <- strsplit(point, ",")[[1]]
        }
        assert_that(point_data[1] %in% names(def_list))
        if(point_data[2] == "include"){
            def_list[[point_data[1]]][[length(def_list[[point_data[1]]]) + 1]] <- point_data[3:length(point_data)]
        } else if (point_data[2] == "exclude"){
            def_list[[point_data[1]]][[length(def_list[[point_data[1]]]) + 1]] <- paste0("-", point_data[3:length(point_data)])
        } else stop("You must choose to 'include' or 'exclude' each set")
        n <- n + 1
    }
    close(con)
    def_list[vapply(def_list, function(x) length(x) == 0, TRUE)] <- list(NULL)
    def_list
}

#' Exports definition searches to an excel file
#' @export
#' @param definition_search a list of dataframes as produced by build_definition_lists
#' @param out_file file path to the excel file to be exported
#' @examples \dontrun{
#' medical_table <- read.delim("medical.txt", fileEncoding="latin1", stringsAsFactors = FALSE)
#' drug_table <- read.delim("product.txt", fileEncoding="latin1", stringsAsFactors = FALSE)
#' def2 <- import_definition_lists(system.file("extdata", "example_search.csv",
#'                                             package = "rpcdsearch"))
#' draft_lists <- definition_search(def2, medical_table, drug_table = drug_table)
#' out_file <- "def_searches.xlsx"
#' export_definition_search(draft_lists, out_file)
#' }
export_definition_search <- function(definition_search, out_file){
    append_p <- FALSE
    for (def in names(definition_search)){
        if(!is.null(definition_search[[def]])){
            xlsx::write.xlsx(definition_search[[def]], file = out_file,
                       sheetName = def, append = append_p)
            append_p <- TRUE
        }
    }
}

#' This function is used to build new definition lists based on medical definitions
#'
#'@details
#' You may get an invalid multibyte string error, in which case, set fileEncoding="latin1" on
#' read.delim when reading in the lookup tables
#' Lookup tables are
#' @export
#' @param def an object of class MedicalDefinition
#' @param medical_table Dataframe lookup table of clinical codes
#' @param test_table dataframe lookup table of test codes
#' @param drug_table dataframe lookup table of medication product codes
#' @param lookup list containing elements: "codes", "terms", "tests", "drugs", "drugcodes" (see details)
#' @examples \dontrun{
#' medical_table <- read.delim("medical.txt", fileEncoding="latin1", stringsAsFactors = FALSE)
#' drug_table <- read.delim("product.txt", fileEncoding="latin1", stringsAsFactors = FALSE)
#' def2 <- import_definition_lists(system.file("extdata", "example_search.csv",
#'                                             package = "rpcdsearch"))
#' draft_lists <- definition_search(def2, medical_table, drug_table = drug_table)
#' }
#'
definition_search <- function(def, medical_table = NULL, test_table = NULL,
                                 drug_table = NULL, lookup = NULL){
    if(is.null(lookup)){
        if(exists(".ehr")){
            lookup <- .ehr$lookup
        } else {
            stop("Either library(rEHR) and choose an EHR to lookup from or assign your own lookup (See details)")
        }

    }

    ## Helper functions:

    ## Convert to lowercase if not all capitals, plus replace underscores with spaces
    fix_case <- function(input){
        if(is(input, "list")){
            lapply(input, fix_case)
        } else {
            ifelse(toupper(input) == input, str_replace_all(input, "_", " "),
                   tolower(str_replace_all(input, "_", " ")))
        }
    }
    ## convert character vector s to a regex AND with any word order
    regex_and <- function(s){
        paste0(lapply(combinat::permn(s),
                      function(x) paste0("(", paste0(".*?(", x, ")", collapse=""), ")")),
               collapse = "|")
    }
    ## return dummy regex if no cases
    check_terms <- function(p, out){
        if(any(p)){
            out
        } else "ZZZZZZZZZZZZZZZZZ"
    }

    ## Builds the regex expression for the deired terms then extracts the matching terms from
    ## the lookup table
    lookup_terms <- function(term_table, dname){
        terms <- fix_case(def[[def_name]])
        excludes <- sapply(terms, function(x) substring(x, 1, 1)[1] == "-")
        exclude_terms <- check_terms(excludes, sub("^-", "", unlist(terms[excludes])))
        terms <- terms[!excludes]
        simple_terms_p <- vapply(terms, function(x) length(x) == 1, TRUE)
        simple_terms <- check_terms(simple_terms_p,
                                    paste0("(", paste0(terms[simple_terms_p],
                                                       collapse = ")|("), ")"))
        complex_terms_p <- vapply(terms, function(x) length(x) > 1, TRUE)
        complex_terms <- check_terms(complex_terms_p,
                                     paste0(lapply(terms[complex_terms_p], regex_and),
                                            collapse = "|"))
        regex <- paste0(complex_terms, "|", simple_terms, sep = "")
        exclude_regex <- paste0("(", paste0(exclude_terms, collapse = ")|("), ")")
        if (length(lookup[[dname]]) == 1){
            lookup_terms <- fix_case(term_table[[lookup[[dname]]]])
        } else { # concatenate if multiple search variables
            lookup_terms <- fix_case(apply(term_table[, lookup[[dname]]], 1,
                                           paste, sep = " ", collapse = " "))
        }
        matches <- str_detect(lookup_terms, regex) &
            !str_detect(lookup_terms, exclude_regex)
        filter(term_table, matches)
    }
    ## cases for the different tables to be searched
    for(def_name in names(def)){
        if (def_name == "terms"){
            if (is.null(def[[def_name]])) {
                terms_table <- NULL
                next
            }
            assert_that(!is.null(medical_table))
            terms_table <- lookup_terms(term_table = medical_table, dname = "terms")
        } else if (def_name == "codes"){
            if (is.null(def[[def_name]])) {
                codes_table <- NULL
                next
            }
            assert_that(!is.null(medical_table))
            codes_table <- lookup_terms(term_table = medical_table, dname = "codes")
        } else if (def_name == "drugs"){
            if (is.null(def[[def_name]])) {
                drugs_table <- NULL
                next
            }
            assert_that(!is.null(drug_table))
            drugs_table <- lookup_terms(term_table = drug_table, dname = "drugs")
        } else if (def_name == "tests"){
            if (is.null(def[[def_name]])) {
                tests_table <- NULL
                next
            }
            assert_that(!is.null(test_table))
            tests_table <- lookup_terms(term_table = test_table, dname = "tests")
        }
    }
    ## combine terms and codes tables, removing duplicates
    if(!is.null(terms_table) && !is.null(codes_table)){
        combined_terms_codes <- dplyr::bind_rows(terms_table, codes_table) %>%
            dplyr::distinct_(lookup$codes)
    } else combined_terms_codes <- NULL
    list(terms_table = terms_table, codes_table = codes_table,
         combined_terms_codes = combined_terms_codes,
         drugs_table = drugs_table, tests_table = tests_table)
}

#' Basic print method for medical definition classes
#' @export
#' @param x an object of class "medical_definition"
#' @param \dots Potential further arguments (required for method/generic reasons)
print.MedicalDefinition <- function(x, ...){
    cat("Medical definition:\n")
    assert_that(all(names(x) %in% c("terms", "codes", "tests", "drugs", "drugcodes")))
    str(x, max.level = 1)
}
