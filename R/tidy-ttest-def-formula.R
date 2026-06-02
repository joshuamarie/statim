making_tidy(TTEST, S7::class_formula) %<-% method_tidy(
    default = function(.x, ...) {
        dat = .x@data
        dplyr::bind_rows(lapply(seq_len(nrow(dat)), function(i) {
            broom::tidy(dat$ttest[[i]]) |>
                dplyr::mutate(
                    type   = dat$type[[i]],
                    groups = dat$group[[i]],
                    .before = 1
                )
        }))
    }
)
