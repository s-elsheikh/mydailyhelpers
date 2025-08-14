#' Title Search for specific text in tibble text column
#'
#' @param df Tibble containing a chatacter column. unique identifier is expectd in
#' first column
#' @param col_name quoted character string: name of new column containg results of search
#' @param search_text quoted character string: regex pattern to search for. small letters only!!!
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
                           f_win = 80) {

    col_sym <- rlang::ensym(col_to_search)
    out_sym <- rlang::ensym(output_col)

    df %>%
        dplyr::mutate(
            "{rlang::as_string(out_sym)}" :=
                locate_text_to_vector(!!col_sym, search_text, b_win, f_win)
        )  %>%
        tidyr::unnest(!!out_sym) %>%   # flatten so one match per row
        dplyr::relocate(!!out_sym, .after = 1)
}
