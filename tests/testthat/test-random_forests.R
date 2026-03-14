#	helpers
make_rf_regression	<-	function()	{
	RandomForest(type	=	"regression",	B	=	5L,	max_depth	=	3L,	min_leaf_size	=	1L)
}
make_rf_classification	<-	function()	{
	RandomForest(type	=	"classification",	B	=	5L,	max_depth	=	3L,	min_leaf_size	=	1L)
}

#	small reproducible datasets
make_X		<-	function()	matrix(c(	0.1,	0.5,	0.9,
										0.2,	0.6,	0.8,
										0.3,	0.7,	0.4,
										0.2,	0.6,	0.8,
										0.3,	0.7,	0.4,
										0.1,	0.5,	0.9),	ncol	=	2)
make_y_reg	<-	function()	c(1.0,	1.0,	5.0,	1.0,	5.0,	5.0,	1.0,	5.0,	1.0)
make_y_cls	<-	function()	c(1L,	1L,	2L,	1L,	2L,	2L,	1L,	2L,	1L)

#	RandomForestCART
test_that("RandomForestCART initializes correctly",	{
	rfc	<-	RandomForestCART(type	=	"regression",	max_depth	=	3L,
							min_leaf_size	=	1L,	m	=	2L)
	testthat::expect_s7_class(rfc,	RandomForestCART)
	testthat::expect_equal(rfc@ref$m,	2L)
})

test_that("RandomForestCART inherits from CART",	{
	rfc	<-	RandomForestCART(type	=	"regression",	max_depth	=	3L,
							min_leaf_size	=	1L,	m	=	2L)

	testthat::expect_true(S7::S7_inherits(rfc,	CART))
})

test_that("RandomForestCART find_split uses feature subset",	{
	set.seed(42)
	rfc		<-	RandomForestCART(type	=	"regression",	max_depth	=	3L,
								min_leaf_size	=	1L,	m	=	1L)
	X		<-	make_X()
	y		<-	make_y_reg()
	split	<-	find_split(rfc,	X,	y)

	testthat::expect_false(is.null(split))
	testthat::expect_true(split$j	%in%	1:3)
})

test_that("RandomForestCART fit and predict work",	{
	set.seed(42)
	rfc	<-	RandomForestCART(type = "regression",	max_depth	=	3L,
							min_leaf_size	=	1L,	m	=	2L)
	fit(rfc,	make_X(),	make_y_reg())
	pred	<-	predict_tree(rfc,	c(0.1,	0.2,	0.3))

	testthat::expect_true(is.numeric(pred))
})

#	RandomForest constructor
test_that("RandomForest initializes correctly",	{
	rf	<-	make_rf_regression()

	testthat::expect_s7_class(rf,		RandomForest)
	testthat::expect_equal(rf@ref$B,	5L)
	testthat::expect_equal(rf@ref$type,	"regression")
})

test_that("RandomForest sets default m and A_n at fit time",	{
	set.seed(42)
	rf	<-	make_rf_regression()
	fit_rf(rf,	make_X(),	make_y_reg())

	testthat::expect_false(is.null(rf@ref$m))
	testthat::expect_false(is.null(rf@ref$A_n))
})

#	fit_rf
test_that("fit_rf errors when X is not a matrix",	{
	rf	<-	make_rf_regression()

	testthat::expect_error(fit_rf(rf,	c(1,	2,	3),	c(1.0,	2.0,	3.0)))
})

test_that("fit_rf errors when X and y lengths differ",	{
	rf	<-	make_rf_regression()

	testthat::expect_error(fit_rf(rf,	make_X(),	c(1.0,	2.0)))
})

test_that("fit_rf builds B trees",	{
	set.seed(42)
	rf	<-	make_rf_regression()
	fit_rf(rf,	make_X(),	make_y_reg())

	testthat::expect_equal(length(rf@ref$trees),	5L)
	testthat::expect_true(all(vapply(rf@ref$trees,
		function(t)	S7::S7_inherits(t,	RandomForestCART),	logical(1))))
})

#	predict_rf
test_that("predict_rf errors on unfitted forest",	{
	rf <- make_rf_regression()

	testthat::expect_error(predict_rf(rf,	c(0.1,	0.2,	0.3)))
})

test_that("predict_rf returns numeric for regression",	{
	set.seed(42)
	rf	<-	make_rf_regression()
	fit_rf(rf,	make_X(),	make_y_reg())
	pred	<-	predict_rf(rf,	c(0.1,	0.2,	0.3))

	testthat::expect_true(is.numeric(pred))
})

test_that("predict_rf returns integer for classification",	{
	set.seed(42)
	rf	<-	make_rf_classification()
	fit_rf(rf,	make_X(),	make_y_cls())
	pred	<-	predict_rf(rf,	c(0.1,	0.2,	0.3))

	testthat::expect_true(is.integer(pred))
})

test_that("predict_rf regression prediction is within range of y",	{
	set.seed(42)
	rf	<-	make_rf_regression()
	y	<-	make_y_reg()
	fit_rf(rf,	make_X(),	y)
	pred	<-	predict_rf(rf,	c(0.1,	0.2,	0.3))

	testthat::expect_gte(pred,	min(y))
	testthat::expect_lte(pred,	max(y))
})

test_that("predict_rf classification returns valid class",	{
	set.seed(42)
	rf	<-	make_rf_classification()
	y	<-	make_y_cls()
	fit_rf(rf,	make_X(),	y)
	pred	<-	predict_rf(rf,	c(0.1,	0.2,	0.3))

	testthat::expect_true(pred	%in%	unique(y))
})

#	get_tree
test_that("get_tree returns correct tree",	{
	set.seed(42)
	rf	<-	make_rf_regression()
	fit_rf(rf,	make_X(),	make_y_reg())
	tree	<-	get_tree(rf,	1L)

	testthat::expect_s7_class(tree,	RandomForestCART)
})

test_that("get_tree errors on out of range index",	{
	set.seed(42)
	rf	<-	make_rf_regression()
	fit_rf(rf,	make_X(),	make_y_reg())

	testthat::expect_error(get_tree(rf,	0L))
	testthat::expect_error(get_tree(rf,	6L))
})

#	feature importance
test_that("feature importance sums to 1",	{
	set.seed(42)
	rf	<-	make_rf_regression()
	fit_rf(rf,	make_X(),	make_y_reg())
	imp	<-	get_feature_importance(rf)

	testthat::expect_equal(sum(imp),	1.0,	tolerance	=	1e-10)
})

test_that("feature importance has correct length",	{
	set.seed(42)
	rf	<-	make_rf_regression()
	fit_rf(rf,	make_X(),	make_y_reg())
	imp	<-	get_feature_importance(rf)

	testthat::expect_equal(length(imp),	ncol(make_X()))
})

test_that("get_feature_importance errors on unfitted forest",	{
	rf	<-	make_rf_regression()

	testthat::expect_error(get_feature_importance(rf))
})
