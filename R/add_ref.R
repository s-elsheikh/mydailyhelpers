#' Add a Reference to a Bibliography File
#'
#' @description
#' 'add_ref()'  Reads clipboard contents and writes it to references file
#'
#' @param bib_path full path to bibliography file to write to
#'
#' @return Nothing
#' @export
#'
#' @examples
add_ref <- function(bib_path){
    # browser()

    if(!file.exists(bib_path)){
        cat("bib file does not exist: creating.\n")
        file.create(bib_path)
    }

    old_refs <-
        readr::read_lines(bib_path) %>%
        stringr::str_subset("^@") %>%
        stringr::str_extract("(?<=\\{).+(?=,)")

    new_ref <-
        utils::readClipboard() %>%
        stringr::str_subset("^@") %>%
        stringr::str_extract("(?<=\\{).+(?=,)")

    if (length(old_refs) == 0)
        { test_in_bib <- TRUE
    }
    else if (new_ref %in% old_refs)
        { test_in_bib <- FALSE}
    else
        {test_in_bib <- TRUE}

    if (test_in_bib) {
        utils::readClipboard() %>%
            readr::write_lines(bib_path,
                        append = TRUE)
        cat(new_ref, "added to file")
    } else cat(new_ref, "already in bibtex")


    new_ref %>%
        stringr::str_remove("\n") %>%
        utils::writeClipboard()

}


