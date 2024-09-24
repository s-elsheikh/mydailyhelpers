#' Locate Pattern in Character Columns within Mutate
#'
#' Tibble mut grouped first using rowwise()
#' Used within mutate(). Takes column name (unquoted) and text (in quotes),
#' searches for text in this column, and returns its location. Then text is
#' extracted and returned as a charachter vector
#' Mutate call has to be within a list, that is later unnet
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
locate_text_to_vector <- function(col,pattern, back_window = 10, front_window = 80){
    # browser()
    # if the string is short, always keep sub aurguments
    # within nchar of string
    # added str_to_lower here
    posis_list <- stringr::str_locate_all(col, pattern)
    posis <- posis_list[[1]][,1]

    out <- purrr::map_chr(posis, ~ stringr::str_sub(string = stringr::str_to_lower(col),
                                                    start = dplyr::if_else(.x - back_window >0,
                                                                           .x - back_window,
                                                                           0L),
                                                    end = dplyr::if_else(.x + front_window > nchar(col),
                                                                         -1L,
                                                                         .x + front_window)
    )
    )

    return(out)

}
