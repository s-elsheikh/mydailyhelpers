#' Title Calculate Numerator and Denom, CI from Confusion Matrix
#'
#' @param cm confusion matrix from Tidy models
#' @param positive_class String defining the positive class
#'
#' @returns Tibble containing numerator, denom, CI and summary stats
#' @export
#'
#' @examples
num_denom_accuracy_measures <- function(cm, positive_class){
    # this function takes a conf matrix and
    # positive class and returns a tibble with estimate
    # numerator, denominator and CI (using binomial test)
    # will not work with ROC-AUC
    # the unrounded values are returned
    # formatiing is in the next function
    tp_fps <- cm$table  %>%
        tibble::as_tibble()  %>%
        dplyr::mutate(
            name = dplyr::case_when(
                Prediction == positive_class & Prediction == Truth ~ "tp",
                Prediction == positive_class & Prediction != Truth ~ "fp",
                Prediction != positive_class & Prediction == Truth ~ "tn",
                Prediction != positive_class & Prediction != Truth ~ "fn",
            )
        )%>% dplyr::select(name, n) %>%
        tibble::deframe()
    # summary fnction inf conf_matrix always assumes thet event is first,
    # so if event is second, this has to be recognised

    levels <- cm$table %>%
        tibble::as_tibble() %>%
        dplyr::distinct(Truth) %>%
        dplyr::pull()

    if (which(levels == positive_class) == 1) event <- "first" else event <- "second"

    summary_stats <- summary(cm, event_level = event)

    out_numer_denom <- tibble::tribble(
        ~ ".metric", ~ "num", ~"denom",
        "accuracy", sum(tp_fps[c("tp", "tn")]), sum(tp_fps),
        "sens", tp_fps["tp"], sum(tp_fps[c("tp", "fn")]),
        "spec", tp_fps["tn"], sum(tp_fps[c("tn", "fp")])
    )

    out_estimate <- out_numer_denom %>%
        dplyr::left_join(summary_stats)

    get_ci <- function(x, tot, which_ci){
        # if total is 0 binom.test returns an error

        if (tot == 0){
            ci <- rep(as.numeric(NA), 2)
        } else {
            t_result <- stats::binom.test(x, tot)

            ci <- t_result$conf.int
        }

        if (which_ci == "l")
            return(ci[1])
        else if (which_ci == "u")
            return(ci[2])
        else stop("which ci has to be 'l' or 'u'")


    }

    out_ci <- out_estimate %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
            ci_low = get_ci(num, denom, "l"),
            ci_hi = get_ci(num, denom, "u"),
        ) %>%
        dplyr::ungroup()
    return(out_ci)
}
