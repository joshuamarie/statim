ptest_def = test_define(
    model_type = prop,
    impl = agendas(
        base = baseline(
            fn = function(.proc, .p = 0.5, .alt = "two.sided", .ci = 0.95) {
                res = stats::binom.test(
                    x = .proc$x,
                    n = .proc$n,
                    p = .p,
                    alternative = .alt,
                    conf.level = .ci
                )
                ptest_build(res, .proc, .ci)
            }
        ),
        prop = variant(
            fn = function(.proc, .p = 0.5, .alt = "two.sided", .ci = 0.95, correct = TRUE) {
                res = stats::prop.test(
                    x = .proc$x,
                    n = .proc$n,
                    p = .p,
                    alternative = .alt,
                    conf.level = .ci,
                    correct = correct
                )
                ptest_build(res, .proc, .ci)
            }
        )
    ),
    compatible_params = list(PI),
    claim_translator = claim_translate(
        default = map_claim(
            .p = function(claim, processed) claim_scalar_diff(claim)$scalar,
            .alt = function(claim, processed) {
                switch(
                    claim@op,
                    "==" = , "!=" = "two.sided",
                    ">=" = , ">" = "less",
                    "<=" = , "<" = "greater"
                )
            }
        ),
        prop = map_claim(
            .p = function(claim, processed) claim_scalar_diff(claim)$scalar,
            .alt = function(claim, processed) {
                switch(
                    claim@op,
                    "==" = , "!=" = "two.sided",
                    ">=" = , ">" = "less",
                    "<=" = , "<" = "greater"
                )
            }
        )
    )
)
