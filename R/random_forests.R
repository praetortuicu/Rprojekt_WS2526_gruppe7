RandomForest	<-	S7::new_class("RandomForest",
		properties	=	list(
			ref	=	S7::new_property(S7::class_environment,
					default	=	new.env(parent	=	emptyenv()))
		),
		constructor	=	function(type	=	"regression",	B	=	100L,
								m	=	NULL,	A_n	=	NULL,
								max_depth	=	10L,	min_leaf_size	=	1L)	{
			env						<-	new.env(parent	=	emptyenv())
			env$type					<-	type
			env$B					<-	B
			env$m					<-	m		# set at fit time if NULL: floor
			env$A_n					<-	A_n		# set at fit time if NULL: n
			env$max_depth			<-	max_depth
			env$min_leaf_size		<-	min_leaf_size
			env$trees				<-	vector("list",	B)
			env$feature_importance	<-	NULL
			S7::new_object(S7::S7_object(),	ref	=	env)
		}
)

###		GENERICS	###
fit_rf					<-	S7::new_generic("fit_rf",					"rf")
predict_rf				<-	S7::new_generic("predict_rf",				"rf")
get_tree				<-	S7::new_generic("get_tree",					"rf")
get_feature_importance	<-	S7::new_generic("get_feature_importance",	"rf")
compute_importance		<-	S7::new_generic("compute_importance",		"rf")

###		GETTERS		###

#'	Get a single tree from the forest
#'
#'	@name		RandomForest@get_tree
#'	@param		rf		A RandomForest object
#'	@param		b		Integer index of the tree
#'	@return				A RandomForestCART object
#'	@examples
#'	get_tree(rf,	1L)
S7::method(get_tree,	RandomForest)	<-	function(rf,	b)	{
		if	(b	<	1	||	b	>	rf@ref$B)	{	stop("Tree index out of range!\n")	}
		return(rf@ref$trees[[b]])
}

#'	Get feature importance scores
#'
#'	@name		RandomForest@get_feature_importance
#'	@param		rf		A RandomForest object
#'	@return				Named numeric vector of length d
#'	@examples
#'	get_feature_importance(rf)
S7::method(get_feature_importance,	RandomForest)	<-	function(rf)	{
		if	(is.null(rf@ref$feature_importance))	{	stop("Forest has not been fitted!\n")	}
		return(rf@ref$feature_importance)
}

#'	Compute feature importance by split frequency
#'
#'	@name	RandomForest@compute_importance
#'	@param	rf		A RandomForest object
#'	@return	named	Numeric vector of length d, sums to 1
#'	@examples
#'	compute_importance(rf)
S7::method(compute_importance,	RandomForest)	<-	function(rf)	{
		d		<-	rf@ref$trees[[1]]@ref$n_features
		counts	<-	integer(d)

		count_splits	<-	function(node)	{
			if	(is.null(node)	||	is_leaf(node))	{	return(invisible(NULL))	}
			j	<-	node@ref$s_feature
			if	(!is.na(j))	counts[j]	<<-	counts[j]	+	1L
			count_splits(node@ref$left_node)
			count_splits(node@ref$right_node)
		}

		for	(tree	in	rf@ref$trees)	{
			count_splits(tree@ref$root)
		}

		total	<-	sum(counts)
		if	(total	==	0)	{	return(counts)	}
		counts	/	total
}

###		DATA	###

#'	Fit the Random Forest to training data
#'
#'	@name	RandomForest@fit_rf
#'	@param	rf	A RandomForest object
#'	@param	X	A numeric matrix of features (n x d)
#'	@param	y	A numeric vector of target values
#'	@return void
#'	@examples
#'	fit_rf(rf,	X,	y)
S7::method(fit_rf,	RandomForest)	<-	function(rf,	X,	y)	{
		if	(!is.matrix(X))			{	stop("X must be a matrix!\n")	}
		if	(nrow(X)	!=	length(y))	{	stop("X and y must have same number of rows!\n")	}

		n	<-	nrow(X)
		d	<-	ncol(X)

		#	set defaults if not provided
		if	(is.null(rf@ref$m))		rf@ref$m	<-	max(1L,	floor(sqrt(d)))
		if	(is.null(rf@ref$A_n))	rf@ref$A_n	<-	n

		for	(b	in	seq_len(rf@ref$B))	{
			#	subsample
			idx		<-	sample(n,	rf@ref$A_n,	replace	=	FALSE)
			X_sub	<-	X[idx,	,	drop	=	FALSE]
			y_sub	<-	y[idx]

			#	build one RandomForestCART
			tree	<-	RandomForestCART(
				type			=	rf@ref$type,
				max_depth		=	rf@ref$max_depth,
				min_leaf_size	=	rf@ref$min_leaf_size,
				m				=	rf@ref$m
			)
			fit(tree,	X_sub,	y_sub)
			rf@ref$trees[[b]]	<-	tree
		}

		rf@ref$feature_importance	<-	compute_importance(rf)
}

#'	Predict using the Random Forest for a single observation
#'
#'	@name	RandomForest@predict_rf
#'	@param	rf		A RandomForest object
#'	@param	x		A numeric vector of length d
#'	@return	Double	(regression) or integer (classification)
#'	@examples
#'	predict_rf(rf,	x)
S7::method(predict_rf,	RandomForest)	<-	function(rf,	x)	{
		if	(is.null(rf@ref$trees[[1]]))	{	stop("Forest has not been fitted!\n")	}

		preds	<-	vapply(rf@ref$trees,	function(tree)	{
			predict_tree(tree,	x)
		},	numeric(1))

		if	(rf@ref$type	==	"regression")	{
			return(mean(preds))
		}	else	{
			return(as.integer(which.max(tabulate(as.integer(preds)))))
		}
}
