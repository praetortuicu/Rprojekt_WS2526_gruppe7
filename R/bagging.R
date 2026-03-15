Bagging <- S7::new_class("Bagging",
    properties = list(
        ref = s7::new_property(s7::class_environment, default = new.env(parent = emptyenv()))
    ),
    constructor = function(type = "regression", b = 100L, max_depth = 10L, min_leaf_size = 1L) {
        env <- new.env(parent = emptyenv())
        env$type <- type
        env$b <- b
        env$max_depth <- max_depth
        env$min_leaf_size <- min_leaf_size
        env$trees <- vector("list", b)
        s7::new_object(s7::s7_object(), ref = env)
    }
)
###     GENERICS    ###

fit_bag <- S7::new_generic("fit_bag", "bagging")
predict_bag <- S7::new_generic("predict_bag", "bagging")

###     METHODS     ###

# Get a single tree from the bagged ensemble
# same like in random forests
S7::method(get_tree, Bagging) <- function(bagging, b) {
    # TODO refactor copied code from rnadom forests for bagging
    if	(b	<	1	||	b	>	rf@ref$B)	{	stop("Tree index out of range!\n")	}
    return(rf@ref$trees[[b]])
}

# Fit the Bagging ensemble to training data
S7::method(fit_bag, Bagging) <- function(bagging, X, y) {
    # TODO Implement
    # Similar to random forest code but with replace = TRUE
}

# Predict using the Bagging ensemble for a single observation
#
# Get predictions from all B trees and combines them to make a prediction
S7::method(predict_bag, Bagging) <- function(bagging, x) {
    # TODO
#   - for Regression: mean of all predictions
#   - for Classification:  majority class across all predictions
}
