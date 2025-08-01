#' Takes a named numeric vector vector and collapses it into publication style sentences.
#'
#' @param vect String: named numeric vector.
#' @param plural_text_unit String: Noun that is being counted in plural. If the plural does not in 's'
#' the automatic singular plural changes will not work with a warning
#' @param singular_text_unit default is NULL, needed if numeric vectors contains 1
#' and plural form does not end with s
#'
#' @description
#'This function takes a numeric vector and spells it out in a sentence. numerics < 10 will be spelled out
#'singular and plural works somewhat, anot all cases are covered
#'
#' @returns
#' text sentence, should be gramatically formated for publication style
#' @export
#'
#' @examples
collapse_vect_2_text <- function(vect, plural_text_unit,
                                 singular_text_unit = NULL){
    # browser()

    if (str_sub(plural_text_unit, -1) != "s"){
        if (min(vect) > 1){
            warning("Automatic singular/plural setting will not work!!!!!\nno singular needed")
        } else if (is.null(singular_text_unit)){
            stop("Automatic singular/plural setting will not work!!!!!\nsingular form needed and is not supplied")
        }
    }

    # descending sort hahaha
    detros_vect <- sort(vect, decreasing = TRUE)
    text_vect <- map_chr(detros_vect, ~ if_else(.x < 10,
                                                num_2_words(.x),
                                                as.character(.x)))

    singular_text_unit <- str_remove(plural_text_unit, "s$")

    tib <- tibble(
        names = names(vect),
        num  = text_vect,
        unit = if_else(num == "one",
                       singular_text_unit,
                       plural_text_unit),
        sep = c(rep(", ", length(text_vect) - 2),
                ", and ",
                ""
        )
    )


    out <- tib %>%
        pmap(function(names, num, unit, sep){
            str_c(names, " in ", num, " ", unit, sep)
        }) %>%
        unlist() %>%
        str_c(collapse = "")

    # to_collapse <- text_vect %>%
    #   imap_chr(~ str_c(.x, .y, text_unit, sep = " "))
    #
    # out <- to_collapse[1:(length(to_collapse)-1)] %>%
    #   str_c(collapse = ", ")  %>%
    #   str_c(", and ", to_collapse[length(to_collapse)])

    return(out)
}
