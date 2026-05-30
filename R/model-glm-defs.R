glm_def_formula = model_infer_define(
    model_type = S7::class_formula,
    impl = agendas(
        base = baseline(
            fn = function(.proc, family = stats::gaussian(), ...) {
                formula = .proc$formula
                data = .proc$data
                glm_to_glm_object(stats::glm(formula, data = data, family = family, ...))
            }
        )
    )
)
