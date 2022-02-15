#' Title
#'
#' @param nested_list complex nested list, tested with sublists or subtibbles
#'
#' @return tidy tibble. column names are numbered 0 based, to correspond to the depth argument in maap_depth()
#' @export
#'
#' @examples
tidy_nested_lists <- function(nested_list){
    nested_list %>%
        unlist() %>%  # unlist retains names and separates nesting with .
        tibble::tibble(names = names(.), values = .) %>% # take names as column in tibble
        dplyr::mutate(
            n_seps = stringr::str_count(names, "\\."),
            #max_n_seps = max(n_seps)
        ) %>%
        tidyr::separate(
            names,
            stringr::str_c(
                "col_",
                #c(1:(.$max_n_seps +1)
                c(1:(max(.$n_seps) +1)
                )), sep = "\\.") %>% #print( n = 100)# separate names of nesting levels into different columns
        dplyr::select(-n_seps)
}
