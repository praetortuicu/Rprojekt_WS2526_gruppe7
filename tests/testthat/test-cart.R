#	helpers used across tests
make_regression_cart	<-	function()	{
	CART(type	=	"regression",	max_depth	=	3L,	min_leaf_size	=	1L)
}
make_classification_cart	<- function() {
	CART(type	= "classification", max_depth = 3L, min_leaf_size = 1L)
}

#	Constructor validation
test_that("CART	initializes correctly for regression", {
	cart	<- make_regression_cart()
	testthat::expect_s7_class(cart,	CART)
	testthat::expect_equal(cart@ref$type,	"regression")
	testthat::expect_equal(cart@ref$max_depth,	3L)
	testthat::expect_equal(cart@ref$min_leaf_size,	1L)
})

test_that("CART	initializes correctly for classification", {
	cart	<- make_classification_cart()
	testthat::expect_s7_class(cart,	CART)
	testthat::expect_equal(cart@ref$type,	"classification")
})

test_that("CART	inherits from BinaryTree", {
	cart	<- make_regression_cart()
	testthat::expect_true(S7::S7_inherits(cart,	BinaryTree))
})

#	compute_leaf_value
test_that("compute_leaf_value	returns mean for regression", {
	cart	<- make_regression_cart()
	testthat::expect_equal(compute_leaf_value(cart,	c(1.0, 2.0, 3.0)), 2.0)
})

test_that("compute_leaf_value	returns majority class for classification", {
	cart	<- make_classification_cart()
	testthat::expect_equal(compute_leaf_value(cart,	c(1L, 1L, 2L)), 1L)
})

#	find_split
test_that("find_split	returns valid j and s for regression", {
	cart	 <- make_regression_cart()
	X		 <- matrix(c(0.1, 0.5, 0.9, 0.2, 0.6, 0.8), ncol = 2)
	y		 <- c(1.0, 1.0, 5.0)
	split	<- find_split(cart, X, y)
	testthat::expect_false(is.null(split))
	testthat::expect_true(split$j	%in% 1:2)
	testthat::expect_true(is.numeric(split$s))
})

test_that("find_split	returns NULL when no valid split exists", {
	cart	 <- make_regression_cart()
	X		 <- matrix(c(1.0, 1.0, 1.0), ncol = 1)  # all identical
	y		 <- c(1.0, 2.0, 3.0)
	split	<- find_split(cart, X, y)
	testthat::expect_null(split)
})

test_that("find_split	returns valid split for classification", {
	cart	 <- make_classification_cart()
	X		 <- matrix(c(0.1, 0.5, 0.9), ncol = 1)
	y		 <- c(1L, 1L, 2L)
	split	<- find_split(cart, X, y)
	testthat::expect_false(is.null(split))
})

#	fit and predict_tree
test_that("fit	builds a non-empty tree", {
	cart	<- make_regression_cart()
	X		<- matrix(c(0.1, 0.5, 0.9, 0.2, 0.6, 0.8), ncol = 2)
	y		<- c(1.0, 1.0, 5.0)
	fit(cart,	X, y)
	testthat::expect_false(is_empty(cart))
})

test_that("fit	+ predict_tree works for regression", {
	cart	<- make_regression_cart()
	X		<- matrix(c(0.1, 0.9, 0.2, 0.8), ncol = 2)
	y		<- c(1.0, 5.0)
	fit(cart,	X, y)
	pred	<- predict_tree(cart, c(0.1, 0.2))
	testthat::expect_true(is.numeric(pred))
})

test_that("fit + predict_tree works for classification",	{
	cart	<-	make_classification_cart()
	X		<-	matrix(c(0.1,	0.9,	0.2,	0.8),	ncol	=	2)
	y		<-	c(1L,	2L)
	fit(cart,	X,	y)
	pred	<-	predict_tree(cart,	c(0.1,	0.2))
	testthat::expect_true(is.numeric(pred)	||	is.integer(pred))
})

test_that("fit	errors when X is not a matrix",	{
	cart	<-	make_regression_cart()
	testthat::expect_error(fit(cart,	c(1,	2,	3),	c(1.0,	2.0,	3.0)))
})

test_that("fit	errors when X and y have different lengths",	{
	cart	<-	make_regression_cart()
	X		<-	matrix(c(0.1,	0.9),	ncol	=	1)
	testthat::expect_error(fit(cart,	X,	c(1.0,	2.0,	3.0)))
})

test_that("stopping	criterion max_depth is respected",	{
	cart	<-	CART(type	=	"regression",	max_depth	=	1L,	min_leaf_size	=	1L)
	X		<-	matrix(seq(0.1,	0.9,	by	=	0.1),	ncol	=	1)
	y		<-	as.double(seq_len(9))
	fit(cart,	X,	y)
	testthat::expect_lte(get_depth(cart),	1L)
})

test_that("stopping	criterion min_leaf_size is respected",	{
	cart	<-	CART(type	=	"regression",	max_depth	=	10L,	min_leaf_size	=	3L)
	X		<-	matrix(seq(0.1,	0.9,	by	=	0.1),	ncol	=	1)
	y		<-	as.double(seq_len(9))
	fit(cart,	X,	y)
	testthat::expect_true(count_leaves(cart)	>	0L)
})
