#' Title
#'
#' @param lab_list list of parameters produced by prep_labor_pdf_pages
#'
#' @return named list of labor parameters, containing date_time and value
#' @export
#'
#' @examples
combine_lab_pages <- function(lab_list){
    out <- lab_list %>%
        purrr::map_depth(.depth = 1, .f = dplyr::bind_rows, .id = "labor_test") %>%
        dplyr::bind_rows() %>%
        split(., .$labor_test) %>%
        purrr::map(dplyr::select, -labor_test) %>%
        purrr::map(dplyr::arrange, dplyr::desc(date_time))
    return(out)
}
