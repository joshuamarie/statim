`%||%` = function (x, y)
    if (is.null(x)) y else x

`%notin%` = Negate(`%in%`)
update = S7::new_external_generic("stats", "update", "object")
