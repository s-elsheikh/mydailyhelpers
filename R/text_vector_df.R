#' @title Deprecated: Search for specific text in a tibble text column
#'
#' @description
#' This function is **deprecated**. Use \code{locate_text_to_vector()} instead.
#' It was used to search a character column in a tibble for a regex pattern
#' and return a new column with snippets of text surrounding each match.
#' The second column of the returned tibble contains the search results.
#'
#' @param df Tibble containing a character column. A unique identifier is expected in
#' the first column.
#' @param col_to_search Unquoted column name in \code{df} to search.
#' @param output_col Unquoted name for the new column that would have contained search results.
#' @param search_text Quoted string: regex pattern to search for. Lowercase letters only.
#' @param b_win Integer: number of characters before the search string to include in the snippet.
#' @param f_win Integer: number of characters after the search string to include in the snippet.
#'
#' @return The function does not return a value; it always stops with an error.
#' Originally, it returned a tibble with one row per match, and the second column
#' contained the requested output column.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' text_vector_df(df, col_to_search = text, output_col = snippet, search_text = "keyword")
#' }
text_vector_df <- function(df,
                           col_to_search,
                           output_col,
                           search_text,
                           b_win = 10,
                           f_win = 80) {

    stop("Deprecated, use locate_text_to_vector() instead", call. = FALSE)

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
