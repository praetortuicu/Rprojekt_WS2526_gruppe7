BinaryTree	<-	S7::new_class("BinaryTree",
			properties = list(
				ref			=	S7::new_property(S7::class_environment,	default	=	new.env(parent	=	emptyenv()))
			),
			constructor	=	function(root=NULL)	{
				env				<-	new.env(parent	=	emptyenv())
				env$root		<-	root
				env$n_leaves	<-	if (is.null(root)) 0L else 1L
				env$n_nodes		<-	NA_integer_
				S7::new_object(S7::S7_object(),	ref=env)
			}
)

###		GENERICS	###
is_empty			<-	S7::new_generic("is_empty",					"tree")
get_depth			<-	S7::new_generic("get_depth",				"tree")
assign_left_child	<-	S7::new_generic("assign_left_child",		"tree")
assign_right_child	<-	S7::new_generic("assign_right_child",		"tree")
count_leaves		<-	S7::new_generic("count_leaves",				"tree")
predict_tree		<-	S7::new_generic("predict_tree",				"tree")
prune				<-	S7::new_generic("prune",					"tree")

###		GETTERS		###
#'	Getter function for tree depth
#'
#'	@name	BinaryTree@get_depth
#'	@param	tree	A BinaryTree object
#'	@return	depth	Integer value
#'	@examples
#'	get_depth(tree)
S7::method(get_depth, BinaryTree) <- function(tree) {
	if (is_empty(tree)) return(0L)
    node_depth <- function(node) {
        if (is_leaf(node) || is_root(node) && 
            is.null(node@ref$left_node)) return(0L)
        1L + max(node_depth(get_left_child(node)),
                 node_depth(get_right_child(node)))
    }
    node_depth(tree@ref$root)

}
###		SETTERS		###

###		VALIDATORS	###

#'	Verify whether tree is empty
#'
#'	@name	BinaryTree@is_empty
#'	@param	tree	BinaryTree object
#'	@return	True/False
#'	@examples
#'	is_empty(tree)
S7::method(is_empty,	BinaryTree)	<-	function(tree)	{
			if	(identical(tree@ref$root,	NULL))	{
			return(TRUE)
			}
	return(FALSE)
}

###		DATA	###

#'	Assign left child
#'
#'	@name	BinaryTree@assign_left_child
#'	@param	tree	A BinaryTree object
#'	@param	parent	A Node object
#'	@param	child	A Node object
#'	@return	void
#'	@examples
#'	assign_left_child(tree,	parent,	child)
S7::method(assign_left_child,	BinaryTree)	<-	function(tree,	parent,	child)	{
	set_left_child(parent,	child)
	tree@ref$n_nodes	<-	tree@ref$n_nodes	+	1L
}
#'	Assign right child
#'
#'	@name	BinaryTree@assign_right_child
#'	@param	tree	A BinaryTree object
#'	@param	parent	A Node object
#'	@param	child	A Node object
#'	@return	void
#'	@examples
#'	assign_right_child(tree,	parent,	child)
S7::method(assign_right_child,	BinaryTree)	<-	function(tree,	parent,	child)	{
	set_right_child(parent,	child)
	tree@ref$n_nodes	<-	tree@ref$n_nodes	+	1L
}

#'	Counts the number of leaves in a tree
#'
#'	@name	BinaryTree@count_leaves
#'	@param	tree	A BinaryTree object
#'	@return	Integer number of leaves
#'	@examples
#'	count_leaves(tree)
S7::method(count_leaves,	BinaryTree)	<-	function(tree)	{
	if	(is_empty(tree))	return(0L)
	count	<-	function(node)	{
		if	(is.null(node@ref$left_node)	&&	is.null(node@ref$right_node))	return(1L)
		count(node@ref$left_node)	+	count(node@ref$right_node)
	}
	count(tree@ref$root)
}

#'	Predict the output for a single observation
#'
#'	@name	BinaryTree@predict_tree
#'	@param	tree	A BinaryTree object
#'	@param	x		A numeric vector of length d (one observation)
#'	@return	Double corresponding to the leaf value reached by x
#'	@examples
#'	predict_tree(tree, x)
S7::method(predict_tree,	BinaryTree)	<-	function(tree,	x)	{
	if	(is_empty(tree))	stop("Cannot predict on empty tree!\n")
	current	<-	tree@ref$root
	while	(!is.null(current@ref$left_node))	{
		current	<-	route(current,	x)
	}
	return(current@ref$leaf_value)
}

#'	Prune a node, collapsing it and its subtree back into a leaf
#'
#'	@name	BinaryTree@prune
#'	@param	tree		A BinaryTree object
#'	@param	node		A Node object to prune (must be an inner node)
#'	@param	leaf_value	A double to assign as the new leaf value
#'	@return	void
#'	@examples
#'	prune(tree, node, leaf_value)
S7::method(prune,	BinaryTree)	<-	function(tree,	node,	leaf_value)	{
	if	(is_leaf(node))	stop("Node is already a leaf!\n")
	node@ref$left_node	<-	NULL
	node@ref$right_node	<-	NULL
	node@ref$leaf_value	<-	leaf_value
	tree@ref$n_leaves	<-	count_leaves(tree)
	tree@ref$depth		<-	get_depth(tree)
}
