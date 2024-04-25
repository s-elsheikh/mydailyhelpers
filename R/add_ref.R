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

    old_refs <-
        readr::read_lines(bib_path) %>%
        stringr::str_subset("^@") %>%
        stringr::str_extract("(?<=\\{).+(?=,)")

    new_ref <-
        utils::readClipboard() %>%
        stringr::str_subset("^@") %>%
        stringr::str_extract("(?<=\\{).+(?=,)")

    if(!new_ref %in% old_refs) {
        utils::readClipboard() %>%
            readr::write_lines(bib_path,
                        append = TRUE)
        cat(new_ref, "added to file")
    } else cat(new_ref, "already in bibtex")

    new_ref %>%
        stringr::str_remove("\n") %>%
        utils::writeClipboard()

}


