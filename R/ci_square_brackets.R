#' Title Change CI Formating, when in Round Brackets
#'
#' @description
#' When writing manuscript, default formating of CI is round brackets, if mentioned
#' in round brackets, then changes theround brackets to square ones
#'
#' @param CI fomrated confidence interval from format_num_denom_accuracy_measures()
#'
#' @returns string: fomrated confidence interval
#' @export
#'
#' @examples
ci_square_brackets <- function(CI){
    CI %>%
    # I looked at a random paper in radiology
    # CI intervals were reported as xxx (95%CI: )
    # but if the measurement is already in brackets, then
    # change normal brackets to square brackets
    stringr::str_replace("\\(", "\\[") %>%
    stringr::str_replace("\\)", "\\]")
}
