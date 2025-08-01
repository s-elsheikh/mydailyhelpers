#' Title Render quarto or Rmarkdown text to console
#'
#' @param tex String: to render, or default: read text from clipboard
#'
#' @description
#'When writing a quarto or Rmarkdown document, this function is
#' useful to check the rendered output, e. g. checking the variables
#' are correct and are rendered correctly.
#' BUG!!!: sometimes extra spaces are in the output
#'
#' @returns
#' Nothing
#' @export
#'
#' @examples
render_text <- function(tex = NULL){
    if(is.null(tex)) tex <- readClipboard()
    out <- paste0(tex,  collapse = " ")  %>%
        stringr::str_replace_all("\\n", " ") %>%
        stringr::str_replace_all("\\s?`r ", " \\{") %>%
        stringr::str_replace_all("`(?=\\s?[^r])", " \\}")
    out <- stringr::str_trim(out)
    glue::glue(out)
}
