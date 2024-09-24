#' Title Search for specific text in output DF of readtext::readtext()
#'
#' @param df Tibble resulting form readtext::readtext(). must contain doc_id and
#' text columns
#' @param col_name quoted character string: name of new column containg results of search
#' @param search_text quoted character string: regex pattern to search for
#' @param b_win integer: nnumber of characters before search strung to show
#' @param f_win integer: nnumber of characters after search strung to show
#'
#' @return Tibble, second column is the requested column
#' @export
#'
#' @examples
text_vector_df <- function(df,
                           col_to_search,
                           output_col,
                           search_text,
                           b_win = 10,
                           f_win = 80){

    browser()

    ensym_col_to_search <- ensyms(col_to_search)
    out_c <- ensym(output_col)

    out

    out <- df %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
            "{out_c}" := list(locate_text_to_vector(stringr::str_to_lower(!!ensym_col_to_search),
                                                       search_text, b_win, f_win))
        ) %>%
        tidyr::unnest(!!out_c) %>%
        dplyr::relocate(!!out_c, .after = 1) %>%
        dplyr::ungroup()

    return(out)
}
