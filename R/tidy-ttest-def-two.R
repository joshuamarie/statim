making_tidy(TTEST, x_by) %<-% method_tidy(
    default = function(.x, ...) {
        dat = .x@data
        dplyr::bind_rows(lapply(seq_len(nrow(dat)), function(i) {
            broom::tidy(dat$ttest[[i]]) |>
                dplyr::mutate(group = dat$group[[i]], .before = 1)
        }))
    },
    boot = function(.x, ...) {
        ci = .x@data$ci
        tibble::tibble(
            lower = ci[[1]],
            upper = ci[[2]],
            n_reps = .x@data$n
        )
    },
    weighted = function(.x, ...) {
        dat = .x@data
        tibble::tibble(
            group = dat$group,
            est = dat$est,
            tstat = dat$tstat,
            df = dat$df,
            p_value = dat$p.value,
            lower = dat$ci[["lower"]],
            upper = dat$ci[["upper"]]
        )
    }
)
