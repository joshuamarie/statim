linear_reg_def_rel = model_infer_define(
    model_type = rel,
    impl_class = "linear_reg_rel",
    impl = agendas(
        base = baseline(
            fn = function(x_data, resp_data, ...) {
                x_nm = names(x_data)
                resp_nm = names(resp_data)
                df = vctrs::vec_cbind(resp_data, x_data)
                f = stats::reformulate(x_nm, response = resp_nm)
                lm_to_lm_object(stats::lm(f, data = df, ...))
            }
        )
    )
)

linear_reg_def_formula = model_infer_define(
    model_type = S7::class_formula,
    impl_class = "linear_reg_formula",
    impl = agendas(
        base = baseline(
            fn = function(formula, data, ...) {
                lm_to_lm_object(stats::lm(formula, data = data, ...))
            }
        )
    )
)
