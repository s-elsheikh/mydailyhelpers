#' Load Reference in Browser
#'
#' @description
#' 'ref_in_browser()' searches for URL in the bibtex reference and lods it in
#' selected browser
#'
#' @param bib_path full path to bibliography file to write to
#' @param wanted_ref reference name as saved in bibtex file or as would be
#' referenced in quarto document
#' @param browser_path path to browser to load the wanted reference
#'
#' @return Nothing
#' @export
#'
#' @examples
ref_in_browser <- function(wanted_ref,
                           bib_path,
                           browser_path = "C:\\Users\\elshikh\\AppData\\Roaming\\Microsoft\\Windows\\Start Menu\\Programs\\UKL Firefox")
{

    refs_raw <- readr::read_lines(bib_path)  %>%
        stringr::str_squish()

    splits <- refs_raw %>%
        stringr::str_which("^@")

    split_at <- function(x, pos){
        split(x, cumsum(seq_along(x) %in% pos))

    }

    old_setting <- Sys.getenv("R_BROWSER")

    Sys.setenv("R_BROWSER" = "C:\\Users\\elshikh\\AppData\\Roaming\\Microsoft\\Windows\\Start Menu\\Programs\\UKL Firefox")

    refs_raw %>%
        split_at(splits) %>%
        purrr::map(function(x){
            if(mean(stringr::str_detect(x, wanted_ref)) > 0)
                return(x)
        }) %>%
        unlist() %>%
        stringr::str_subset("url") %>%
        stringr::str_extract("htt.+(?=\\})") %>%
        utils::browseURL()

    Sys.setenv("R_BROWSER" = old_setting)

}
