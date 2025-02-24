#' Title Format summary stats into CI string for Publication
#'
#' @param tib Tibble from num_denom_accuracy_measures()
#' @param output String, c("value_only", "num_denom", "ci", "both")
#' @param round_digs Int: number of digits to round to
#'
#'
#' @description
#' takes the tibble produced by num_denom_accuracy_measures()
#' Output types
#' value_only: returns the Accuracy, sensitivity and spec only
#' num_denom: returns numerator and denominator only
#' ci: returns CI only
#' both: (Default) Returns both num/denum and CI
#'
#' @returns String fomrated ready for Publication containing acc, sens and spec
#' with/without CI and or numerator and denominator
#' @export
#'
#' @examples
format_num_denom_accuracy_measures  <- function(tib, output = "both",
                                                round_digs = 2){

    # this function takes tibble from num_denom_accuracy_measures(),
    # wanted output format and digit rounding
    # and returns a named vector, good for text
    # and possibly results tables
    # Auto naming the list was somewhat problematic


    valid_reses <- c("value_only", "num_denom", "ci", "both")

    if( !output %in% valid_reses)
        stop(cat("invalid result\nmust be one of\n", valid_reses, "\n"))

    rounded_tib <- tib %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, digits = round_digs)))

    formated_out <- purrr::pmap(.l = rounded_tib,
                         .f = function(.metric,
                                       num,
                                       denom,
                                       .estimate,
                                       ci_low,
                                       ci_hi,
                                       desired_format = output,
                                       ...){



                             if(desired_format == "value_only"){
                                 out <- c(as.character(.estimate))
                             }
                             else if (desired_format == "ci") {
                                 out <-  c(glue::glue('{.estimate} (95% CI: {ci_low}, {ci_hi})'))


                             }
                             else if (desired_format == "num_denom") {
                                 out <-

                                     c(glue::glue('{.estimate} ({num} of {denom})'))
                             }
                             else if (desired_format == "both") {
                                 out <-
                                     c(glue::glue('{.estimate} ({num} of {denom}; 95% CI: {ci_low}, {ci_hi})'))

                             }
                             # returned a named vector
                             names(out) <- .metric
                             return(out)
                         })
    # rename list from vector within
    names(formated_out) <- purrr::map_chr(formated_out, names)
    # change to vector, keeping the names of the list
    formated_out_vect <- purrr::list_simplify(formated_out)

    return(formated_out_vect)
}
