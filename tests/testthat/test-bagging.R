make_bag_regression <- function() {
  Bagging(type = "regression", B = 5L, max_depth = 3L, min_leaf_size = 1L)
}

make_bag_classification <- function() {
  Bagging(type = "classification", B = 5L, max_depth = 3L, min_leaf_size = 1L)
}

make_X <- function() matrix(
  c(0.1, 0.5, 0.9,
    0.2, 0.6, 0.8,
    0.3, 0.7, 0.4,
    0.2, 0.6, 0.8,
    0.3, 0.7, 0.4,
    0.1, 0.5, 0.9), ncol = 2
)
make_y_reg <- function() c(1.0, 1.0, 5.0, 1.0, 5.0, 5.0, 1.0, 5.0, 1.0)
make_y_cls <- function() c(1L, 1L, 2L, 1L, 2L, 2L, 1L, 2L, 1L)

# Constructor
test_that("Bagging initializes correctly", {
    bag <- make_bag_regression()
    expect_s7_class(bag, Bagging)
    expect_equal(bag@ref$B, 5L)
    expect_equal(bag@ref$type, "regression")
    expect_length(bag@ref$trees, 5L)
})

# fit_bag
test_that("fit_bag errors when X is not matrix", {
    bag <- make_bag_regression()
    expect_error(fit_bag(bag, c(1,2,3), c(1.0, 2.0, 3.0)))
})
test_that("fit_bag errors when X and y have differenet lengths", {
    bag <- make_bag_regression()
    expect_error(fit_bag(bag, make_X(), c(1.0, 2.0)))
})
test_that("fit_bag builds B trees", {
    set.seed(42)
    bag <- make_bag_regression()
    fit_bag(bag, make_X(), make_y_reg())
    expect_equal(length(bag@ref$trees), 5L)
    expect_true(all(vapply(bag@ref$trees, function(t) S7::S7_inherits(t, CART), logical(1))))
})

# predict_bag
test_that("predict_bag errors on unfitted ensemble", {
  bag <- make_bag_regression()
  expect_error(predict_bag(bag, c(0.1, 0.2)))
})
test_that("predict_bag returns numeric for regression", {
  set.seed(42)
  bag <- make_bag_regression()
  fit_bag(bag, make_X(), make_y_reg())
  pred <- predict_bag(bag, c(0.1, 0.2))
  expect_true(is.numeric(pred))
})
test_that("predict_bag returns integer for classification", {
  set.seed(42)
  bag <- make_bag_classification()
  fit_bag(bag, make_X(), make_y_cls())
  pred <- predict_bag(bag, c(0.1, 0.2))
  expect_true(is.integer(pred))
})
test_that("predict_bag regression prediction is within range of y", {
  set.seed(42)
  bag <- make_bag_regression()
  y <- make_y_reg()
  fit_bag(bag, make_X(), y)
  pred <- predict_bag(bag, c(0.1, 0.2))
  expect_gte(pred, min(y))
  expect_lte(pred, max(y))
})
test_that("predict_bag classification returns valid class", {
  set.seed(42)
  bag <- make_bag_classification()
  y <- make_y_cls()
  fit_bag(bag, make_X(), y)
  pred <- predict_bag(bag, c(0.1, 0.2))
  expect_true(pred %in% unique(y))
})

# get_tree
test_that("get_tree returns correct tree", {
    set.seed(42)
    bag <- make_bag_regression()
    fit_bag(bag, make_X(), make_y_reg())
    tree <- get_tree(bag, 1L)
    expect_s7_class(tree, CART)
})
test_that("get_tree errors on out-of-range index", {
  set.seed(42)
  bag <- make_bag_regression()
  fit_bag(bag, make_X(), make_y_reg())
  expect_error(get_tree(bag, 0L))
  expect_error(get_tree(bag, 6L))
})
