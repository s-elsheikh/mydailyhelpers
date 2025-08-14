#' Locate Pattern in Character Columns within Mutate
#'
#' Used within mutate(). Takes column name (unquoted) and text (in quotes),
#' searches for text in this column, and returns its location. Then text is
#' extracted and returned as a charachter vector
#' Mutate call has to be within a list, that is later unnested
#'
#'
#'
#'
#'
#' @param col column name unquoted
#' @param pattern text un quotes
#' @param back_window number of characters before search term to include in output
#' @param front_window number of characters after search term to include in output
#'
#' @return vector of strings
#' @export
#'
#' @examples
#' df <- tibble::tibble(col_id = letters[1:2], col_text = c("I love ananas", "I love lololo"))
#' df |> dplyr::rowwise() |> dplyr::mutate(lo = list(locate_text_to_vector(col_text, "lo", 2, 7)))
locate_text_to_vector <- function(col, pattern, back_window = 10, front_window = 80){
    # browser()
    # if the string is short, always keep sub aurguments
    # within nchar of string
    # added str_to_lower here
    locate_text_to_vector <- function(text_vec, pattern, back_window = 10, front_window = 80){
        # browser()
        # if the string is short, always keep sub aurguments
        # within nchar of string
        # added str_to_lower here
        text_vec <- stringr::str_to_lower(text_vec)

        purrr::map(
            text_vec,
            function(txt) {
                if (is.na(txt)) return(NA_character_)

                posis_list <- stringr::str_locate_all(txt, pattern)
                if (length(posis_list[[1]]) == 0) return(NA_character_)

                starts <- pmax(posis_list[[1]][, 1] - back_window, 1)
                ends   <- pmin(posis_list[[1]][, 1] + front_window, nchar(txt))

                stringr::str_sub(txt, starts, ends)
            }
        )
    }

}
