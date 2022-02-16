#' Title Tidy nested list into tibble
#'
#' This functions returns a tidy tibble from a nested list, preserving hierarchy of nestingg levels and adding NA where
#' appropriate. backbone is the base R function unlist(), which returns a named vector of all values in the lsit.
#' names a re concatenated with a "." as separator. LAter the names are separated using sep = "\\.".
#' If the names of the nested list happen to contain "." they will be internally replaced with "_". At the end,
#' the original name will be restored
#'
#' @param nested_list complex nested list, tested with sublists or subtibbles
#'
#' @return tidy tibble. column names are numbered 0 based, to correspond to the depth argument in maap_depth()
#' @export
#'
#' @examples
tidy_nested_lists <- function(nested_list){

    renamed <- rename_nested_list(nested_list,
                       "\\.",
                       "q#w#e#r#t#z#")



    out_renamed <- renamed %>%
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


    out <- out_renamed %>% dplyr::mutate(
        dplyr::across(
            where(is.character),
            ~ stringr::str_replace_all(.x, "q#w#e#r#t#z#","\\.")
        )
    )


    # out <- rename_nested_list(out_renamed,
    #                           "q#w#e#r#t#z#",
    #                           "\\."
    #                           )

    return(out)
}
