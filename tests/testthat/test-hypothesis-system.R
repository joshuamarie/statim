test_that("parse_null_claim: rejects non-relational top-level expression", {
    expect_error(
        parse_null_claim(rlang::quo(MU(x) + 1)),
        class = "rlang_error"
    )
})

test_that("parse_null_claim: rejects unsupported operator", {
    expect_error(
        parse_null_claim(rlang::quo(MU(x) %in% 0)),
        class = "rlang_error"
    )
})

test_that("parse_null_claim: parses simple equality claim", {
    claim = parse_null_claim(rlang::quo(MU(extra) == 0))
    expect_true(S7::S7_inherits(claim, null_claim))
    expect_equal(claim@op, "==")
    expect_equal(claim@alt_op, "!=")
    expect_true(S7::S7_inherits(claim@lhs, MU))
    expect_equal(as.numeric(claim@rhs), 0)
})

test_that("parse_null_claim: parses inequality operators", {
    for (op in c("!=", "<", ">", "<=", ">=")) {
        expr = rlang::parse_expr(paste("MU(extra)", op, "0"))
        claim = parse_null_claim(rlang::new_quosure(expr, rlang::base_env()))
        expect_equal(claim@op, op)
        expect_equal(claim@alt_op, unname(FLIP_OP[op]))
    }
})

test_that("parse_null_claim: flips operator when scalar is on LHS", {
    claim = parse_null_claim(rlang::quo(0 == MU(extra)))
    expect_equal(claim@op, "==")
})

test_that("parse_null_claim: parses %=% chain into flat node list", {
    claim = parse_null_claim(rlang::quo(MU(extra) %=% MU(extra)))
    expect_true(S7::S7_inherits(claim, null_claim))
    expect_equal(claim@op, "%=%")
    expect_null(claim@rhs)
    expect_length(claim@lhs, 2L)
})

test_that("parse_null_claim: flattens three-node %=% chain", {
    claim = parse_null_claim(rlang::quo(MU(x) %=% MU(y) %=% MU(z)))
    expect_length(claim@lhs, 3L)
})

test_that("parse_null_claim: parses arithmetic LHS", {
    claim = parse_null_claim(rlang::quo(2 * MU(extra) - MU(extra) == 0))
    expect_true(S7::S7_inherits(claim, null_claim))
    expect_equal(claim@op, "==")
})

test_that("parse_null_claim: rejects non-param, non-numeric symbol", {
    expect_error(
        parse_null_claim(rlang::quo(foo == 0)),
        class = "rlang_error"
    )
})

# ---- `<param_obj> constructors ----

test_that("MU: constructs with x only", {
    p = MU(extra)
    expect_true(S7::S7_inherits(p, MU))
    expect_true(S7::S7_inherits(p, param_obj))
    expect_equal(rlang::as_label(p@x), "extra")
    expect_null(p@given)
})

test_that("MU: constructs with x and given", {
    p = MU(extra, group == "1")
    expect_equal(rlang::as_label(p@x), "extra")
    expect_false(is.null(p@given))
})

test_that("PI: constructs with no arguments", {
    p = PI()
    expect_true(S7::S7_inherits(p, PI))
    expect_null(p@x)
    expect_null(p@given)
})

test_that("PI: constructs with x only", {
    p = PI(success)
    expect_equal(rlang::as_label(p@x), "success")
    expect_null(p@given)
})

test_that("SIGMA: constructs with x and given", {
    p = SIGMA(score, group == "control")
    expect_equal(rlang::as_label(p@x), "score")
    expect_false(is.null(p@given))
})

test_that("RHO: constructs with x and y", {
    p = RHO(speed, dist)
    expect_equal(rlang::as_label(p@x), "speed")
    expect_equal(rlang::as_label(p@y), "dist")
})

test_that("param_obj: is abstract — cannot instantiate directly", {
    expect_error(param_obj(), class = "error")
})

# ---- Parsing `<param_obj>` calls ----

test_that("parse_param_call MU: rejects zero arguments", {
    expect_error(
        parse_param_call(MU(), args = list(), env = rlang::base_env()),
        class = "rlang_error"
    )
})

test_that("parse_param_call MU: rejects more than two arguments", {
    expect_error(
        parse_param_call(
            MU(),
            args = list(
                quote(x),
                quote(g == "1"),
                quote(extra)
            ),
            env = rlang::base_env()
        ),
        class = "rlang_error"
    )
})

test_that("parse_param_call PI: rejects more than two arguments", {
    expect_error(
        parse_param_call(
            PI(),
            args = list(
                quote(x),
                quote(g == "1"),
                quote(extra)
            ),
            env = rlang::base_env()
        ),
        class = "rlang_error"
    )
})

test_that("parse_param_call RHO: rejects non-two arguments", {
    expect_error(
        parse_param_call(RHO(x, y), args = list(quote(x)), env = rlang::base_env()),
        class = "rlang_error"
    )
})

# ---- Testing `state_null()` ----

test_that("state_null: attaches claim to test_lazy", {
    lazy = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        state_null(MU(extra) == 0)

    expect_true(S7::S7_inherits(lazy, stated_null))
    expect_true(S7::S7_inherits(lazy@claims, null_claim))
})

test_that("state_null: returns stated_null subclass", {
    lazy = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        state_null(MU(extra) == 0)
    expect_true(S7::S7_inherits(lazy, stated_null))
    expect_true(S7::S7_inherits(lazy, test_lazy))
})

test_that("state_null: accepts more_h0 block", {
    lazy = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        state_null(more_h0(
            h01 = MU(extra) == 0,
            h02 = MU(extra) == 1
        ))
    expect_true(S7::S7_inherits(lazy@claims, null_claims))
    expect_length(lazy@claims@claims, 2L)
})

test_that("state_null: more_h0 requires named expressions", {
    expect_error(
        sleep |>
            define_model(x_by(extra, group)) |>
            prepare_test(TTEST) |>
            state_null(more_h0(MU(extra) == 0)),
        class = "rlang_error"
    )
})

test_that("state_null: errors when used without `prepare_test()` / `prepare_model()`", {
    expect_error(
        state_null(list(), MU(extra) == 0)
    )
})

# ---- Testing `<param_obj>` compatibility ----

test_that("attach_claim_to_lazy: rejects incompatible param type", {
    expect_error(
        sleep |>
            define_model(x_by(extra, group)) |>
            prepare_test(TTEST) |>
            state_null(PI() == 0.5),
        class = "rlang_error"
    )
})

test_that("attach_claim_to_lazy: rejects incompatible param in more_h0", {
    expect_error(
        sleep |>
            define_model(x_by(extra, group)) |>
            prepare_test(TTEST) |>
            state_null(more_h0(
                h01 = PI() == 0.5
            )),
        class = "rlang_error"
    )
})

# ---- Validating variables from `state_null()` ----
# Now made better

test_that("validate_claim_vars x_by: rejects wrong x variable", {
    expect_error(
        sleep |>
            define_model(x_by(extra, group)) |>
            prepare_test(TTEST) |>
            state_null(MU(wrong) == 0),
        regexp = "Unknown variable",
        class = "rlang_error"
    )
})

test_that("validate_claim_vars x_by: rejects wrong grouping variable", {
    expect_error(
        sleep |>
            define_model(x_by(extra, group)) |>
            prepare_test(TTEST) |>
            state_null(MU(extra, wrong == "1") == 0),
        regexp = "Unknown grouping variable",
        class = "rlang_error"
    )
})

test_that("validate_claim_vars x_by: rejects both wrong x and given together", {
    err = expect_error(
        sleep |>
            define_model(x_by(extra, group)) |>
            prepare_test(TTEST) |>
            state_null(2 * MU(x, g == "1") >= MU(x, g == "2")),
        class = "rlang_error"
    )
    expect_match(conditionMessage(err), "Unknown variable")
    expect_match(conditionMessage(err), "Unknown grouping variable")
})

test_that("validate_claim_vars x_by: accepts correct variable references", {
    expect_no_error(
        sleep |>
            define_model(x_by(extra, group)) |>
            prepare_test(TTEST) |>
            state_null(MU(extra) == 0)
    )
})

test_that("validate_claim_vars x_by: accepts correct given reference", {
    expect_no_error(
        sleep |>
            define_model(x_by(extra, group)) |>
            prepare_test(TTEST) |>
            state_null(
                2 * MU(extra, group == "1") >= MU(extra, group == "2")
            )
    )
})

test_that("validate_claim_vars prop: passes through without validation", {
    expect_no_error(
        define_model(prop(45, 100)) |>
            prepare_test(P_TEST) |>
            state_null(PI() == 0.3)
    )
})

test_that("validate_claim_vars unknown model_id: passes through silently", {
    my_id = S7::new_class("my_id", parent = model_id)()
    processed = list()
    claim = parse_null_claim(rlang::quo(MU(x) == 0))
    expect_no_error(validate_claim_vars(my_id, processed, claim))
})

# ---- Checking parameters by node ----

test_that("check_param_nodes: passes for correct references", {
    claim = parse_null_claim(rlang::quo(MU(extra) == 0))
    expect_no_error(
        check_param_nodes(claim, x_vars = "extra", by_vars = "group")
    )
})

test_that("check_param_nodes: collects multiple errors in one abort", {
    claim = parse_null_claim(rlang::quo(MU(x) == 0))
    err = expect_error(
        check_param_nodes(claim, x_vars = "extra", by_vars = NULL),
        class = "rlang_error"
    )
    expect_match(conditionMessage(err), "Unknown variable")
})

test_that("check_param_nodes: deduplicates identical errors across nodes", {
    claim = parse_null_claim(rlang::quo(2 * MU(x) >= MU(x)))
    err = expect_error(
        check_param_nodes(claim, x_vars = "extra", by_vars = NULL),
        class = "rlang_error"
    )
    # Two nodes, same error — should appear only once
    expect_equal(lengths(regmatches(
        conditionMessage(err),
        gregexpr("Unknown variable", conditionMessage(err))
    )), 1L)
})

test_that("validate_one_param_node MU: returns empty for valid references", {
    node = parse_null_claim(rlang::quo(MU(extra) == 0))@lhs
    errs = validate_one_param_node(node, x_vars = "extra", by_vars = "group")
    expect_equal(errs, character(0))
})

test_that("validate_one_param_node MU: returns error for bad x", {
    node = parse_null_claim(rlang::quo(MU(wrong) == 0))@lhs
    errs = validate_one_param_node(node, x_vars = "extra", by_vars = NULL)
    expect_length(errs, 1L)
    expect_match(errs, "Unknown variable")
})

test_that("validate_one_param_node MU: returns error for bad given", {
    node = parse_null_claim(rlang::quo(MU(extra, wrong == "1") == 0))@lhs
    errs = validate_one_param_node(node, x_vars = "extra", by_vars = "group")
    expect_length(errs, 1L)
    expect_match(errs, "Unknown grouping variable")
})

test_that("validate_one_param_node RHO: validates both x and y", {
    node = parse_null_claim(rlang::quo(RHO(speed, dist) %=% RHO(speed, dist)))@lhs[[1]]
    errs = validate_one_param_node(node, x_vars = c("speed", "dist"), by_vars = NULL)
    expect_equal(errs, character(0))

    errs_bad = validate_one_param_node(node, x_vars = "speed", by_vars = NULL)
    expect_length(errs_bad, 1L)
    expect_match(errs_bad, "Unknown variable")
})

test_that("validate_one_param_node unknown param_obj: returns empty", {
    MY_PARAM = S7::new_class("MY_PARAM", parent = param_obj)
    node = MY_PARAM()
    errs = validate_one_param_node(node, x_vars = "extra", by_vars = "group")
    expect_equal(errs, character(0))
})

# ---- Checking vars from `x` and `by` ----

test_that("check_x_and_given: returns empty when both NULL", {
    errs = check_x_and_given(NULL, NULL, x_vars = "extra", by_vars = "group", cls_name = "MU")
    expect_equal(errs, character(0))
})

test_that("check_x_and_given: returns empty when x_vars and by_vars are NULL", {
    x_quo = rlang::quo(wrong)
    errs = check_x_and_given(x_quo, NULL, x_vars = NULL, by_vars = NULL, cls_name = "MU")
    expect_equal(errs, character(0))
})

test_that("check_x_and_given: catches bad x", {
    x_quo = rlang::quo(wrong)
    errs = check_x_and_given(x_quo, NULL, x_vars = "extra", by_vars = NULL, cls_name = "MU")
    expect_length(errs, 1L)
    expect_match(errs, "Unknown variable")
})

test_that("check_x_and_given: catches bad given", {
    given_quo = rlang::quo(wrong == "1")
    errs = check_x_and_given(NULL, given_quo, x_vars = NULL, by_vars = "group", cls_name = "MU")
    expect_length(errs, 1L)
    expect_match(errs, "Unknown grouping variable")
})

test_that("check_x_and_given: catches both bad x and bad given", {
    x_quo = rlang::quo(wrong)
    given_quo = rlang::quo(bad == "1")
    errs = check_x_and_given(x_quo, given_quo, x_vars = "extra", by_vars = "group", cls_name = "MU")
    expect_length(errs, 2L)
})

test_that("check_x_and_given: ignores given with non-== predicate", {
    given_quo = rlang::quo(group %in% c("1", "2"))
    errs = check_x_and_given(NULL, given_quo, x_vars = NULL, by_vars = "group", cls_name = "MU")
    expect_equal(errs, character(0))
})
