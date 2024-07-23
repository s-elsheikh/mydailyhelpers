#' Title
#'
#' @param pdf_df a single dataframe from the list produced by pdftools::pdf_data()
#' @param relevant_filter stringr::regex() expression containing text to search for to define the where the columns along the x axis
#' @param empty_filter stringr::regex() expression containing string patterns that will be changed to NA
#' @param wanted_labs stringr::regex() expression containing labor parameters needed
#' @param y_upper_limit stringr::regex() expression for upper limit to include (usually name of the first column)
#' @param stop_term stringr::regex() expression to stop at if encountered
#'
#' @return a named list (after labor parameters) containing date_time and value
#' @export
#'
#' @examples
prep_labor_pdf_pages <- function(pdf_df,
                                 relevant_filter,
                                 empty_filter,
                                 wanted_labs,
                                 y_upper_limit,
                                 stop_term){

    # check if a relevant page or not
    if(max(str_detect(pdf_df$text, y_upper_limit)) == 0) return(NULL)


    # check for empty fields = 6x point

    prepped_page <- pdf_df %>%
        dplyr::select(x,y,text) %>%
        dplyr::arrange(y,x) %>%
        dplyr::mutate(
            text = dplyr::if_else(stringr::str_detect(text, empty_filter),
                                  "empty",
                                  text)
        )
    # x coordinates of each column
    relevant_coords <-
        prepped_page %>%
        dplyr::arrange(x) %>%
        dplyr::filter(
            stringr::str_detect(text, relevant_filter)

        )
    # browser()
    # find top y coord
    y_top <- relevant_coords %>% # find top y coord
        dplyr::filter(stringr::str_detect(text, y_upper_limit)) %>%
        dplyr::pull(y)
    # stop before "Kommentar" to prevent garbage or errors
    stop_y_location <-  prepped_page %>% # find y coord to stop
        dplyr::filter(
            stringr::str_detect(text, stop_term)
        ) %>% dplyr::pull(y)
    #browser()

    if(length(y_top) != 0) {
        relevant_coords <- relevant_coords %>% dplyr::filter(
            y >= y_top
        )
    }

    if(length(stop_y_location) != 0) {
        relevant_coords <- relevant_coords %>%
            dplyr::filter(y < stop_y_location)
    }


    prepped_page_with_dummy <- prepped_page %>% # added dummy entry, not to lose dates
        dplyr::bind_rows(
            tibble::tibble_row(x = relevant_coords$x[1] +1 ,
                       y = relevant_coords$y[2],
                       text =  "datum")
        )

    #browser()
    split_page <- prepped_page_with_dummy %>%
        dplyr::mutate(
            grp = cut(x, c(unique(relevant_coords$x), Inf), right = FALSE)
        ) %>%
        dplyr::filter(
            !is.na(grp) &
                y > relevant_coords$y[1]
        ) %>%
        split(.$grp)

    # browser()
    to_reduce_list <-  split_page %>%
        purrr::map( function(df) {

            out <- df %>%
                dplyr::arrange(y, x) %>%
                dplyr::group_by(y) %>%
                tidyr::pivot_wider(names_from = x, values_from = text, names_sort = TRUE) %>%
                tidyr::unite(
                    auftrag, where(is.character), sep = " ") %>%
                dplyr::mutate(
                    auftrag = stringr::str_remove_all(auftrag, "NA") %>% stringr::str_squish()) %>%
                dplyr::select(-grp)
            return(out)
        }
        )

    out <- to_reduce_list %>%
        purrr::reduce(~ dplyr::left_join(.x, .y, by= "y")) %>%
        purrr::when (dim(.)[2]> 2 ~ tidyr::unite(., wert, 2:3),
              dim(.)[2]< 3 ~ dplyr::rename(., wert = auftrag) ) %>%
        dplyr::filter(stringr::str_detect(wert, wanted_labs)|
                   stringr::str_detect(wert, "datum_bereich|Auftrags-Nr"))
    #browser()
    if(dim(out)[2] >= 3) {

        new_names <- out %>% dplyr::ungroup() %>%
            dplyr::slice_head(n = 2) %>%
            dplyr::mutate(
                dplyr::across(tidyselect::everything(), ~ stringr::str_c(.x, dplyr::lead(.x), sep = "_"))
            ) %>% tidyr::unite(new_names, 3:tidyselect::last_col(), sep = "###") %>%
            .[1,3] %>%
            dplyr::pull() %>%
            stringr::str_split(., "###") %>%
            unlist()

        # check for duplicate colnames
        while(sum(duplicated(new_names)) != 0) {

            new_number <- stringr::str_sub(new_names, -1, -1) %>% as.integer()


            new_number[duplicated(new_names)] <- new_number[duplicated(new_names)] + 1

            new_names <- stringr::str_replace(new_names, "\\d$", as.character(new_number))

        }



        new_names <- c("y", "wert", new_names)

        names(out) <- new_names
        out <- out[c(-1, -2),-1]

    }

    sum_col_names <-
        names(out) %>%
        stringr::str_detect("(\\d{2}\\.){2}\\d{2}_\\d{2}:\\d\\d") %>%
        sum()

    if(sum_col_names == 0| is.na(sum_col_names)) return()

    #browser()
    out <- out %>%
        tidyr::pivot_longer(2:tidyselect::last_col(), names_to = "date_time", values_to = "value") %>%
        dplyr::distinct() %>%
        dplyr::mutate(
            dplyr::across(where(is.character), dplyr::na_if, "empty"),
            date_time = lubridate::dmy_hm(date_time)
        ) %>%
        tidyr::drop_na() %>%
        split(., .$wert) %>%
        purrr::map(dplyr::select, - wert)
    return(out)

}
