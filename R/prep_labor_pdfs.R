#' Title Create tidy table from labor PDF
#'
#' @param pdf_df a single dataframe from the list produced by pdftools::pdf_data()
#' @param relevant_filter stringr::regex() expression containing text to search for to define the where the columns along the x axis
#' @param empty_filter stringr::regex() expression containing string patterns that will be changed to NA
#' @param wanted_labs stringr::regex() expression containing labor parameters needed
#' @param y_upper_limit stringr::regex() expression for upper limit to include (usually name of the first column)
#' @param stop_term stringr::regex() expression to stop at if encountered
#' @param verbose prints piz and page number of the dicument being processed to console
#'
#' @return a tibble containing date_time and value
#' @export
#'
#' @examples
prep_labor_pdf_pages <- function(pdf_df,
                                 relevant_filter,
                                 empty_filter,
                                 wanted_labs,
                                 y_upper_limit,
                                 stop_term,
                                 verbose = TRUE){

    if(verbose){
        # get piz

        piz <- pdf_df %>%
            dplyr::filter(stringr::str_detect(text, "PIZ")) %>%
            dplyr::pull(text) %>%
            stringr::str_extract("\\d{8}")

        # get page number
        y_coord_pg_num <- pdf_df %>%
            dplyr::filter(text == "Seite") %>%
            dplyr::pull(y)
        pg_num <- pdf_df %>%
            dplyr::filter(y == y_coord_pg_num) %>%
            dplyr::slice_tail(n = 1) %>%
            dplyr::pull(text)
        cat(glue::glue("Now extracting from Pat num: {piz}, Page number {pg_num}\n\n"))

        }

    # check if a relevant page or not
    if(max(stringr::str_detect(pdf_df$text, y_upper_limit)) == 0) return(NULL)




    # check for empty fields = 6x point
        prepped_page <- pdf_df %>%
        dplyr::select(x,y,text) %>%
        dplyr::arrange(y,x) %>%
        dplyr::mutate(
            text = dplyr::if_else(stringr::str_detect(text, empty_filter),
                                  "empty",
                                  text)
            )


    # stop before "Kommentar" to prevent garbage or errors
    stop_y_location <-  prepped_page %>% # find y coord to stop
        dplyr::filter(
            stringr::str_detect(text, stop_term)
        ) %>% dplyr::pull(y) %>% .[1]

    relevant_loc <- prepped_page %>%
        dplyr::filter(text == "Auftragsdatum")

    relevant_coords <-
        prepped_page %>%
        dplyr::arrange(x) %>%
        dplyr::filter(
            stringr::str_detect(text, relevant_filter)) %>%
        dplyr::filter(y >= relevant_loc$y)


    # remove non relevant entries
    # starting from auftragsdatum and
    # ending before kommentar

    relevant_loc <- relevant_coords %>%
        dplyr::filter(text == "Auftragsdatum")

    relevant_prepped_page <- prepped_page %>%
        # include starting from location of relevant string
        # first x (right left)
        dplyr::filter(x >= relevant_loc$x) %>%
        # then y (up-down),
        dplyr::filter(y >= relevant_loc$y)

    # remove everything starting from "kommentare"
    if(!is.na(stop_y_location)){
        relevant_coords <- relevant_coords %>%
            dplyr::filter(y < stop_y_location)

        relevant_prepped_page <- relevant_prepped_page %>%
            dplyr::filter(y < stop_y_location)
        }

   lab_table <- relevant_prepped_page %>%
        # group right left entries according to positions of
        # date format strings from relevant_coords
        # sometimes is the auftragsnummer longer than the date
        # and projects to the left, so i have to decrease at least 4
        # from x coords of the date to get the correct Auftragsnummer
        # for the date. Either decrease by 6 or coallece in post proc
        dplyr::mutate(
            grp = cut(x, c(unique(relevant_coords$x), Inf), right = FALSE)
        )  %>%
        dplyr::arrange(x)   %>%
        dplyr::nest_by(y,grp, .key = "l_col") %>%
        dplyr::mutate(
            text = list(dplyr::pull(l_col[,2]) %>%
                            stringr::str_c(collapse = " " ))
        ) %>%
        tidyr::unnest(text)  %>%
        dplyr::select(-l_col) %>%
        dplyr::arrange(grp) %>%
        tidyr::pivot_wider(names_from = grp, values_from = text)  %>%
        dplyr::arrange(y) %>%
        dplyr::ungroup() %>%
        dplyr::select(-y) %>%
       # add standard naming for next step
       `names<-`(letters[1:dim(.)[2]])

   # browser()
   # take colnames from upper rows
   names_of_col <- lab_table %>%
       dplyr::filter(
           stringr::str_detect(a, "^Auftrag") |
               b == "bereich" |
               # some pats have 2 columns with the
               # same time stamp, si i included
               # auftragsnummer
               dplyr::lag(a) == "Auftrags-Nr."
       ) %>%
       # map along cols, to remove nas
       # and collapse then clean
       purrr::map(~ stats::na.omit(.x) %>%
                      stringr::str_c(collapse = "_")
       ) %>%
       unlist() %>%
       stringr::str_replace("-", "_") %>%
       stringr::str_remove_all(" ")

    names_of_col[1] <- "auftrag"


    pivoted_tidy_lab_tib <- lab_table %>%
        # filter for rows, not taken for naming
        dplyr::filter(!(stringr::str_detect(a, "^Auftrag") |
                b == "bereich"  |
                    dplyr::lag(a) == "Auftrags-Nr."
                )

        ) %>%
        `names<-`(names_of_col)   %>%
        dplyr::filter(!is.na(auftrag))  %>%
        tidyr::pivot_longer(!c(1:2),
                     names_to = "date_time") %>%
        dplyr::mutate(
            date_time = stringr::str_remove(date_time, "_\\d+$"),
            date_time = lubridate::dmy_hm(date_time)
        )  %>%
        dplyr::filter(value != "empty") %>%
        dplyr::filter(stringr::str_detect(auftrag, wanted_labs))


    # tidy output
    out <- pivoted_tidy_lab_tib %>%
        mutate(
            num_value = str_extract(value, "[0-9,]*") %>%
                str_replace(",", ".") %>%
                as.numeric(),
            opinion = case_when(
                str_detect(value, "-") ~ "lo",
                str_detect(value, "\\+") ~ "hi",
                TRUE ~ "normal"
            )
        )
    # there are inconsistencies in Referenzbereich

    return(out)

}
