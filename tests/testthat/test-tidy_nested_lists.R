test_that("Unnesting deeply nested lists returns tidy output, testing with sub tibbles and sub lists", {
    experiments <- list(
        A = list(
            C1 = list(
                group = "Control",
                weight = 10,
                sex = list(q = "f",
                           w = list(
                               d = tibble::tibble(
                                   v = 1:3,
                                   l = list(rt = 1:3)
                               ),
                               g = "my_behind"
                           ))
            ),
            C2 = list(
                group = "Control",
                weight = 8,
                exp_a_c2_tibble = tibble::tibble(
                    exp_a_c2_tibble_numbers = 1:3,
                    exp_b_c2_tibble_letters = letters[1:3]
                )
            ),
            T1 = list(
                group = "Treatment",
                weight = 15
            ),
            T2 = list(
                group = "Treatment",
                weight = 14
            )
        ),
        B = list(
            C1 = list(
                group = "Control",
                weight = 8
            ),
            C2 = list(
                group = "Control",
                weight = 7
            ),
            T1 = list(
                group = "Treatment",
                weight = 15.2
            ),
            T2 = list(
                group = "Treatment",
                weight = 16
            )
        )
    )

    expect_equal(
        dim(experiments %>% tidy_nested_lists()),
        c(36,8)
    )

})
