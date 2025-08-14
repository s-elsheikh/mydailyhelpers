#' Extract text snippets around a search term in a data frame column
#'
#' Searches a character column in a data frame for a pattern, and returns
#' a new data frame where each match is on its own row, along with a
#' snippet of text surrounding the match. The search is case-insensitive,
#' but the returned snippet preserves the original casing.
#'
#' @param df A data frame or tibble.
#' @param col_to_search The column in \code{df} to search (unquoted).
#' @param pattern A string or regular expression to search for.
#' @param back_window Number of characters to include before the match (default: 10).
#' @param front_window Number of characters to include after the match (default: 80).
#'
#' @return A data frame with one row per match. Contains all original columns
#' plus a new column \code{snippet} with the extracted text.
#' The \code{snippet} column is always placed in the second position
#' of the returned data frame for easier viewing in the console.
#' If no matches are found in a row, the snippet is \code{NA}.
#'
#' @examples
#' library(dplyr)
#'
#' df <- tibble(
#'   id = 1:3,
#'   text = c(
#'     "This is a keyword in the middle of the sentence for testing.",
#'     "Another keyword appears here, and another keyword later.",
#'     "No match here."
#'   )
#' )
#'
#' locate_text_to_vector(df, text, "keyword", 5, 15)
#'
#' @export

locate_text_to_vector <- function(df, col_to_search, pattern,
                                  back_window = 10, front_window = 80) {

    col_sym <- rlang::ensym(col_to_search)

    df %>%
        dplyr::mutate(
            snippet = purrr::map(
                !!col_sym,
                function(txt) {
                    if (is.na(txt)) return(NA_character_)

                    # Use case-insensitive search but keep original casing in snippet
                    posis_list <- stringr::str_locate_all(stringr::str_to_lower(txt),
                                                          stringr::str_to_lower(pattern))

                    if (nrow(posis_list[[1]]) == 0) return(NA_character_)

                    starts <- pmax(posis_list[[1]][, 1] - back_window, 1)
                    ends   <- pmin(posis_list[[1]][, 1] + front_window, nchar(txt))

                    stringr::str_sub(txt, starts, ends)
                }
            )
        ) %>%
        tidyr::unnest(snippet) %>%
        dplyr::relocate(snippet, .after = 1)
}
