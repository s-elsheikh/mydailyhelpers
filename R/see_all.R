#' Quickly Print a Tibble to console
#' Gives the option to print an object using print(n=Inf) and optionally
#' filtering if any value is.na
#'
#' @param df Dataframe / Tibble to be viewd
#' @param filter_na Boolean: should df be filtered for any NA values. Default is FALSE
#'
#' @return nothing, prints to screen
#' @export
#'
#' @examples
see_all <- function(df, filter_na = FALSE){
    if(filter_na) df <- df %>%
            dplyr::filter(dplyr::if_any(dplyr::everything(), is.na))

    print(df, n = Inf)

}
