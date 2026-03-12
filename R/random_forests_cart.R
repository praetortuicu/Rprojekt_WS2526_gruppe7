RandomForestCART	<- S7::new_class("RandomForestCART",
	parent		=	CART,
	properties	=	list(
			ref	=	S7::new_property(S7::class_environment,
					default	=	new.env(parent	=	emptyenv()))
		),
		constructor	=	function(root	=	NULL,	type	=	"regression",
							max_depth	=	10L,	min_leaf_size	=	1L,	m	=	1L)	{
			env					<-	new.env(parent	=	emptyenv())
			env$root				<-	root
			env$type				<-	type
			env$max_depth		<-	max_depth
			env$min_leaf_size	<-	min_leaf_size
			env$n_features		<-	NULL
			env$m				<-	m
			S7::new_object(CART(root	=	root,	type	=	type,
								max_depth		=	max_depth,
								min_leaf_size	=	min_leaf_size),
								ref				=	env)
		}
)
#'	Find the best split (j, s) for a node using a random feature subset
#'
#'	Overrides the CART find_split method. Instead of searching all d features,
#'	randomly samples m features per Definition 6.52 from Richter (2019) to
#'	decorrelate trees within the Random Forest ensemble.
#'
#'	@name	RandomForestCART@find_split
#'	@param	cart	A RandomForestCART object
#'	@param	X		A numeric matrix of features (n x d)
#'	@param	y		A numeric vector of target values
#'	@return			A list with j (feature index) and s (split point), or NULL if no valid split
#'	@examples
#'	find_split(rfc,	X,	y)
S7::method(find_split,	RandomForestCART)	<-	function(cart,	X,	y)	{
		d			<-	ncol(X)
		S			<-	sample(d,	cart@ref$m)
		best_loss	<-	Inf
		best_j		<-	NULL
		best_s		<-	NULL

		for	(j	in	S)	{
			candidates	<-	sort(unique(X[,	j]))
			if	(length(candidates)	<	2)	next

			for	(s	in	candidates[-length(candidates)])	{
				left	<-	y[X[,	j]	<	s]
				right	<-	y[X[,	j]	>=	s]

				if	(length(left)	==	0	||	length(right)	==	0)	next

				if	(cart@ref$type	==	"regression")	{
					loss		<-	sum((left	-	mean(left))^2)	+
							sum((right	-	mean(right))^2)
				}	else	{
					gini		<-	function(y_sub)		{
						n_sub	<-	length(y_sub)
						if	(n_sub	==	0)	return(0)
						p	<-	tabulate(y_sub)		/	n_sub
						sum(p	*	(1	-	p))
					}
					loss	<-	length(left)	*	gini(left)	+
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
