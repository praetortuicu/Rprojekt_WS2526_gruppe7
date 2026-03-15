Bagging <- S7::new_class("Bagging",
  properties = list(
    ref = S7::new_property(S7::class_environment,
            default = new.env(parent = emptyenv()))
  ),
  constructor = function(type = "regression", B = 100L,
                         max_depth = 10L, min_leaf_size = 1L) {
    env               <- new.env(parent = emptyenv())
    env$type          <- type
    env$B             <- B
    env$max_depth     <- max_depth
    env$min_leaf_size <- min_leaf_size
    env$trees         <- vector("list", B)
    S7::new_object(S7::S7_object(), ref = env)
  }
)
###     GENERICS    ###

fit_bag <- S7::new_generic("fit_bag", "bagging")
predict_bag <- S7::new_generic("predict_bag", "bagging")

###     METHODS     ###

#' Get a single tree from the bagged ensemble
#'
#' @name Bagging@get_tree
#' @param bagging A Bagging object
#' @param b Integer index of the tree
#' @return The `CART` object stored at index `b`
#' @examples
#' get_tree(bagging, 1L)
S7::method(get_tree, Bagging) <- function(rf, b) {
    if (b < 1 || b > rf@ref$B) stop("Tree index out of range!\n")
    return(rf@ref$trees[[b]])
}

#' Fit the Bagging ensemble to training data
#'
#' @name Bagging@fit_bag
#' @param bagging A Bagging object
#' @param X Matrix of predictors
#' @param y Numeric target vector
#' @return Invisibly returns `NULL`
#' @examples
#' fit_bag(bag, X, y)
S7::method(fit_bag, Bagging) <- function(bagging, X, y) {
    # Safety copied from random forests
    if (!is.matrix(X)) {
        stop("X must be a matrix!\n")
    }
    if (nrow(X) != length(y)) {
        stop("X and y must have same number of rows!\n")
    }

    n <- nrow(X)

    for (b in seq_len(bagging@ref$B)) {
        # bootstrap sample
        idx  <- sample(n, n, replace = TRUE)

        tree <- CART(
        type          = bagging@ref$type,
        max_depth     = bagging@ref$max_depth,
        min_leaf_size = bagging@ref$min_leaf_size
        )
        fit(tree, X[idx, , drop = FALSE], y[idx])

        bagging@ref$trees[[b]] <- tree
    }
}

# Predict using the Bagging ensemble for a single observation
#
# Get predictions from all B trees and combines them to make a prediction
# Like random forset

#' Predict using the Bagging ensemble
#'
#' @name Bagging@predict_bag
#' @param bagging A Bagging object
#' @param x Numeric vector representing one observation
#' @return Numeric prediction
#' @examples
#' predict_bag(bag, c(0.1, 0.2))
S7::method(predict_bag, Bagging) <- function(bagging, x) {
  # Safety like random forests
    if (is.null(bagging@ref$trees[[1]])) { stop("Ensemble has not been fitted!\n") }

    preds <- vapply(bagging@ref$trees, function(tree) {
        predict_tree(tree, x)
    }, numeric(1))

    if (bagging@ref$type == "regression") {
        return(mean(preds))
    } else {
        return(as.integer(which.max(tabulate(as.integer(preds)))))
    }
}
