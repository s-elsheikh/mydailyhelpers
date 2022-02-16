#' Title replace pattern in nested list
#'
#'  Recursively applies str_replace to the names of all elements of a named nested list
#'  credit to https://stackoverflow.com/a/63075776/14604591 and to https://stackoverflow.com/a/38539734/14604591
#'
#'
#' @param nested_list a nested named list to rename. behaviour with unnamed lists is not tested
#' @param pattern as text string
#' @param replacement as text string
#'
#' @return list
#' @export
#'
#' @examples
rename_nested_list <- function(nested_list, pattern, replacement) {
    found <- stringr::str_detect(names(nested_list), pattern)
    names(nested_list)[found] <- stringr::str_replace_all(names(nested_list)[found], pattern, replacement)
    purrr::pmap(
        .l = list(
            nested_list,
            pattern,
            replacement
        ),
        ~{
            if (inherits(.x, "list")) {
                rename_nested_list(., pattern, replacement)
            } else {
                .x
            }
        })
}
