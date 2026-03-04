Node	<-	S7::new_class("Node",
			properties = list(
				ROOT		=	S7::new_property(class	=	S7::class_logical,	default = FALSE),
				ref			=	S7::new_property(S7::class_environment,	default	=	new.env(parent	=	emptyenv()))
			),
			constructor	=	function(ROOT=FALSE,	s_feature=NA_real_,	s_value=NA_real_,	leaf_value=NA_real_)	{
				env	<-	new.env(parent	=	emptyenv())
				env$s_feature	<-	s_feature
				env$s_value		<-	s_value
				env$leaf_value	<-	leaf_value
				env$left_node	<-	NULL
				env$right_node	<-	NULL
				S7::new_object(S7::S7_object(),	ROOT=ROOT,	ref=env)
			}
)

###		GENERICS	###
is_root				<-	S7::new_generic("is_root",	"node")
is_leaf				<-	S7::new_generic("is_leaf",	"node")

get_value			<-	S7::new_generic("get_value",	"node")
set_value			<-	S7::new_generic("set_value",	"node")

get_left_child		<-	S7::new_generic("get_left_child",	"node")
get_right_child		<-	S7::new_generic("get_right_child",	"node")
set_left_child		<-	S7::new_generic("get_left_child",	"node")
set_right_child		<-	S7::new_generic("set_right_child",	"node")

route				<-	S7::new_generic("route",			"node")

###		GETTERS	###

#'	Get value of target node. If the node is a leaf, then leaf_value is returned, otherwise the split value is returned.
#'
#'	@name	Node@get_value
#'	@param	node				A node object
#'	@return	node@value			A double corresponding to the node's value
#'	@examples
#'	get_value(node)
S7::method(get_value,	Node)	<-	function(node)	{
			if	(is_leaf(node))	{	return(node@ref$leaf_value)	}
			return(node@ref$s_value)
}

#'	Get the left child of the node
#'
#'	@name	Node@get_left_child
#'	@param	node				A node object
#'	@return	node@left_node		A node object corresponding to the left child 
#'	@examples
#'	get_left_child(node)
S7::method(get_left_child,	Node)	<-	function(node)	{
			if	(!is_leaf(node))	{
			return(node@ref$left_node)
			}
			stop("Leaves do not have child nodes!\n")
}

#'	Get the right child of the node
#'
#'	@name	Node@get_right_child
#'	@param	node				A node object
#'	@return	node@right_node		A node object corresponding to the right child 
#'	@examples
#'	get_right_child(node)
S7::method(get_right_child,	Node)	<-	function(node)	{
			if	(!is_leaf(node))	{
			return(node@ref$right_node)
			}
			stop("Leaves do not have child nodes!\n")
}

###		SETTERS	###

#'	Set value of target node
#'
#'	@name	Node@set_value
#'	@param	node				A node object
#'	@param	value				A double value to set
#'	@return	void				Void
#'	@examples
#'	set_value(node)
S7::method(set_value,	Node)	<-	function(node,	value)	{
			if	(!is_leaf(node))	{
			node@ref$s_value	<-	value
			}	else	{
			node@ref$leaf_value	<-	value
			}
}

#'	Set the left child of the node
#'
#'	@name	Node@get_left_child
#'	@param	node					A node object
#'	@param	new_node				A node object
#'	@return	node@left_node			A node object corresponding to the left child 
#'	@examples
#'	set_left_child(node)
S7::method(set_left_child,	Node)	<-	function(node,	new_node)	{
			node@ref$left_node	<-	new_node
}

#'	Set the right child of the node
#'
#'	@name	Node@set_right_child
#'	@param	node					A node object
#'	@param	new_node				A node object
#'	@return	node@right_node			A node object corresponding to the right child 
#'	@examples
#'	set_right_child(node)
S7::method(set_right_child,	Node)	<-	function(node,	new_node)	{
			node@ref$right_node	<-	new_node
}


###		VALIDATORS	###

#'	Verify if node is root
#'
#'	@name	Node@is_root
#'	@param	node	A node object
#'	@return	True/False
#'	@examples
#'	is_root(node)
S7::method(is_root,	Node)	<-	function(node)	{
			if	(node@ROOT)	{
				return(TRUE)
			}	else	{	return(FALSE)	}
}

#'	Verify if node is a leaf
#'
#'	@name	Node@is_leaf
#'	@param	node	A node object
#'	@return	True/False
#'	@examples
#'	is_leaf(node)
S7::method(is_leaf,	Node)	<-	function(node)	{
			if	(node@ROOT)	{	return(FALSE)	}
			else	{
				return(is.null(node@ref$left_node)	&&	is.null(node@ref$right_node))
			}
}


###		DATA	###

#'	Decision making method for routing children depending on the node
#'
#'	@name	Node@route
#'	@param	node	Node Object
#'	@param	obs		Numeric value vector
#'	@return	void
#'	@examples
#'	route(node)
S7::method(route,	Node)	<-	function(node,	obs)	{
			if	(obs[node@ref$s_feature]	<=	node@ref$s_value)	{	return(get_left_child(node))	}
			return(get_right_child(node))
}
