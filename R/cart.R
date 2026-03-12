CART	<-	S7::new_class("CART",
			parent	=	BinaryTree,
			properties	=	list(
				ref		=	S7::new_property(S7::class_environment,	default	=	new.env(parent	=	emptyenv()))
			),
			constructor	=	function(root=NULL,	type,	max_depth,	min_leaf_size)	{
				env					<-	new.env(parent	=	emptyenv())
				env$root			<-	root
				env$type			<-	type
				env$max_depth		<-	max_depth
				env$min_leaf_size	<-	min_leaf_size
				env$n_features		<-	NULL
				S7::new_object(BinaryTree(root=root),	ref=env)
			}
)

###		GENERICS	###
fit					<-	S7::new_generic("fit",					"cart")
build_tree			<-	S7::new_generic("build_tree",			"cart")
find_split			<-	S7::new_generic("find_split",			"cart")
compute_leaf_value	<-	S7::new_generic("compute_leaf_value",	"cart")

###		METHODS		###

#'	Compute	the leaf value for a node
#'
#'	@name	CART@compute_leaf_value
#'	@param	cart	A CART object
#'	@param	y		A numeric vector of target values
#'	@return	Double	(regression: mean) or integer (classification: majority class)
#'	@examples
#'	compute_leaf_value(cart,	y)
S7::method(compute_leaf_value,	CART)	<-	function(cart,	y)	{
			if	(cart@ref$type	==	"regression")	{
					return(mean(y))
			}	else	{
					return(as.integer(which.max(tabulate(y))))
			}
}

#'	Find the best split (j,	s) for a node given data subset
#'
#'	@name	CART@find_split
#'	@param	cart	A CART object
#'	@param	X		A numeric matrix of features (n	x	d)
#'	@param	y		A numeric vector of target values
#'	@return	A list with j (feature	index) and s (split	point), or NULL if no valid split
#'	@examples
#'	find_split(cart,	X,	y)
S7::method(find_split,	CART)	<-	function(cart,	X,	y)	{
			n			<-	nrow(X)
			d			<-	ncol(X)
			best_loss	<-	Inf
			best_j		<-	NULL
			best_s		<-	NULL
			for	(j	in	seq_len(d))	{
					candidates	<-	sort(unique(X[,	j]))
					if	(length(candidates)	<	2)	next
						for	(s	in	candidates[-length(candidates)])	{
								left		<-	y[X[,	j]	<		s]
								right	<-	y[X[,	j]	>=	s]
									if	(length(left)	==	0	||	length(right)	==	0)	next
									if	(cart@ref$type	==	"regression")	{
										loss	<-	sum((left	-	mean(left))^2)	+
																		sum((right	-	mean(right))^2)
										}	else	{
											# Gini index per Bemerkung 6.17
											gini	<-	function(y_sub)	{
															n_sub	<-	length(y_sub)
															if	(n_sub	==	0)	return(0)
															p	<-	tabulate(y_sub)	/	n_sub
															sum(p	*	(1	-	p))
														}
													loss	<-	length(left)		*	gini(left)	+
																					length(right)	*	gini(right)
										}
										if	(loss	<	best_loss)	{
														best_loss	<-	loss
														best_j		<-	j
														best_s		<-	s
										}
						}
			}
			if	(is.null(best_j))	{	return(NULL)	}
			return(list(j	=	best_j,	s	=	best_s))
}

#'	Recursively build the CART tree
#'
#'	@name	CART@build_tree
#'	@param	cart	A CART object
#'	@param	node	A Node object
#'	@param	X		A numeric matrix of features (n	x	d)
#'	@param	y		A numeric vector of target values
#'	@param	depth	Current depth integer
#'	@return	void
#'	@examples
#'	build_tree(cart,	node,	X,	y,	depth)
S7::method(build_tree,	CART)	<-	function(cart,	node,	X,	y,	depth)	{
				# always set leaf value in case we stop here
				node@ref$leaf_value	<-	compute_leaf_value(cart,	y)
				# stopping criteria
				if	(depth	>=	cart@ref$max_depth							||
								nrow(X)	<=	cart@ref$min_leaf_size	||
								length(unique(y))	==	1)	{
								return(invisible(NULL))
				}
				# find best split
				split	<-	find_split(cart,	X,	y)
				if	(is.null(split))	return(invisible(NULL))
				j	<-	split$j
				s	<-	split$s

				# set split parameters on node
				node@ref$s_feature	<-	j
				node@ref$s_value	<-	s

				# partition data
				left_idx	<-	X[,	j]	<	s
				right_idx	<-	X[,	j]	>=	s
				X_left		<-	X[left_idx,	,	drop	=	FALSE]
				y_left		<-	y[left_idx]
				X_right		<-	X[right_idx,	,	drop	=	FALSE]
				y_right		<-	y[right_idx]

				# create and attach children
				left_child	<-	Node()
				right_child	<-	Node()
				assign_left_child(cart,		node,	left_child)
				assign_right_child(cart,	node,	right_child)
				# recurse
				build_tree(cart,	left_child,		X_left,		y_left,		depth	+	1L)
				build_tree(cart,	right_child,	X_right,	y_right,	depth	+	1L)
}

#'	Fit the CART to training data
#'
#'	@name	CART@fit
#'	@param	cart	A CART object
#'	@param	X		A numeric matrix of features (n	x	d)
#'	@param	y		A numeric vector of target values
#'	@return	void
#'	@examples
#'	fit(cart,	X,	y)
S7::method(fit,	CART)	<-	function(cart,	X,	y)	{
				if	(!is.matrix(X))	stop("X must be a matrix!\n")
				if	(nrow(X)	!=	length(y))	stop("X and y must have same number of rows!\n")
				cart@ref$n_features	<-	ncol(X)
				root				<-	Node(ROOT	=	TRUE)
				cart@ref$root		<-	root
				cart@ref$n_nodes	<-	1L
				cart@ref$n_leaves	<-	1L

				build_tree(cart,	root,	X,	y,	depth	=	0L)
}
