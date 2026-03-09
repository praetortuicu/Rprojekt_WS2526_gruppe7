# binary_tree.R
#
# Temporary mock until Theo finishes 
BinaryTree	<-	S7::new_class("BinaryTree",
			properties = list(
				ref			=	S7::new_property(S7::class_environment,	default	=	new.env(parent	=	emptyenv()))
			),
			constructor	=	function(root=NULL)	{
				env				<-	new.env(parent	=	emptyenv())
				env$root		<-	root
				env$depth		<-	NA_integer_
				env$n_leaves	<-	NA_integer_
				env$n_nodes		<-	NA_integer_
				S7::new_object(S7::S7_object(),	ref=env)
			}
)

###		GENERICS	###
is_empty			<-	S7::new_generic("is_empty",					"tree")
get_depth			<-	S7::new_generic("get_depth",				"tree")
update_depth		<-	S7::new_generic("update_depth",				"tree")
update_n_nodes		<-	S7::new_generic("update_n_nodes",			"tree")
update_n_leaves		<-	S7::new_generic("update_n_leaves",			"tree")
assign_left_child	<-	S7::new_generic("assign_left_child",		"tree")
assign_right_child	<-	S7::new_generic("assign_right_child",		"tree")

###		GETTERS		###
#'	Getter function for tree depth
#'
#'	@name	BinaryTree@get_depth
#'	@param	tree	A BinaryTree object
#'	@return	depth	Integer value
#'	@examples
#'	get_depth(tree)
S7::method(get_depth,	BinaryTree)	<-	function(tree)	{
			if	(is.na(tree@ref$depth)	||	is.null(tree@ref$root@get_left_child()))	{	return(0)	}
			return(tree@ref$depth)
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

#'	Increment tree depth
#'
#'	@name	BinaryTree@update_depth
#'	@param	tree	A BinaryTree object
#'	@return	void
#'	@examples
#'	update_depth(tree)
S7::method(update_depth,	BinaryTree)	<-	function(tree)	{
			tree@ref$depth	<-	tree@ref$depth	+	1
}

#'	Increment tree number of nodes
#'
#'	@name	BinaryTree@update_n_nodes
#'	@param	tree	A BinaryTree object
#'	@return	void
#'	@examples
#'	update_n_nodes(tree)
S7::method(update_n_nodes,	BinaryTree)	<-	function(tree)	{
	#	TODO
}

#'	Increment tree number of leaves
#'
#'	@name	BinaryTree@update_n_leaves
#'	@param	tree	A BinaryTree object
#'	@return	void
#'	@examples
#'	update_n_leaves(tree)
S7::method(update_n_leaves,	BinaryTree)	<-	function(tree)	{
	#	TODO
}
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
			#	TODO
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
			#	TODO
}
