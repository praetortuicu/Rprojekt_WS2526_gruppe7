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
  # TODO
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
  # TODO
})
# predict_bag
test_that("predict_bag errors on unfitted ensemble", {
  # TODO
})

test_that("predict_bag returns numeric for regression", {
  # TODO
})

test_that("predict_bag returns integer for classification", {
  # TODO
})

test_that("predict_bag regression prediction is within range of y", {
  # TODO
})

test_that("predict_bag classification returns valid class", {
  # TODO
})


# get_tree
test_that("get_tree returns correct tree", {
  # TODO
})

test_that("get_tree errors on out-of-range index", {
  # TODO
})

