Node	<-	S7::new_class("Node",
			properties = list(
				value		=	S7::new_property(class	=	S7::class_double,	default	=	0.0),
				ROOT		=	S7::new_property(class	=	S7::class_logical,	default = FALSE),
				parent_node	=	S7::new_property(								default	=	NULL),
				left_node	=	S7::new_property(								default	=	NULL),
				right_node	=	S7::new_property(								default	=	NULL),
				ref			=	S7::new_property(S7::class_environment,	default	=	new.env(parent	=	emptyenv())),
				s_feature	=	S7::new_property(S7::class_double,	default	=	NA_real_),
				s_value		=	S7::new_property(S7::class_double,	default	=	NA_real_),
				leaf_value	=	S7::new_property(S7::class_double,	default	=	NA_real_)
			)
)

###		GENERICS	###
is_root				<-	S7::new_generic("is_root",	"node")
is_leaf				<-	S7::new_generic("is_leaf",	"node")

get_parent			<-	S7::new_generic("get_parent",	"node")
set_parent			<-	S7::new_generic("set_parent",	"node")
get_value			<-	S7::new_generic("get_value",	"node")
set_value			<-	S7::new_generic("set_value",	"node")

get_left_child		<-	S7:::new_generic("get_left_child",	"node")
get_right_child		<-	S7:::new_generic("get_right_child",	"node")
set_left_child		<-	S7:::new_generic("get_left_child",	"node")
set_right_child		<-	S7:::new_generic("set_right_child",	"node")
assign_children		<-	S7::new_generic("assign_children",	"node")

assign_left_child	<-	S7::new_generic("assign_left_child",	"node")
assign_right_child	<-	S7::new_generic("assign_right_child",	"node")
remove_child		<-	S7::new_generic("remove_child",			"node")

###		GETTERS	###

#'	Get parent node of target node
#'
#'	@name	Node@get_parent
#'	@param	node				A node object
#'	@return	node@parent_node	A node object
#'	@examples
#'	get_parent(node)
S7::method(get_parent,	Node)	<-	function(node)	{
			if	(is_root(node))	{
				stop("Root node does not have a parent!\n")
			}	else	{
				return(node@ref$parent_node)
			}
}

#'	Get value of target node
#'
#'	@name	Node@get_value
#'	@param	node				A node object
#'	@return	node@value			A double corresponding to the node's value
#'	@examples
#'	get_value(node)
S7::method(get_value,	Node)	<-	function(node)	{
			return(node@value)
}

#'	Get the left child of the node
#'
#'	@name	Node@get_left_child
#'	@param	node				A node object
#'	@return	node@left_node		A node object corresponding to the left child 
#'	@examples
#'	get_left_child(node)
S7::method(get_left_child,	Node)	<-	function(node)	{
			return(node@ref$left_node)
}

#'	Get the right child of the node
#'
#'	@name	Node@get_right_child
#'	@param	node				A node object
#'	@return	node@right_node		A node object corresponding to the right child 
#'	@examples
#'	get_right_child(node)
S7::method(get_right_child,	Node)	<-	function(node)	{
			return(node@ref$right_node)
}

###		SETTERS	###

#'	Set the parent node of the target to a new node object

#'	@name	Node@set_parent
#'	@param	node			Target node object
#'	@param	new_parent_node	New node object
#'	@examples
#'	set_parent(target,	new_parent)
#'	set_parent(root,	new_parent)
S7::method(set_parent,	Node)	<-	function(node,	new_parent_node	)	{
			if	(is_root(node))		{	stop("Root node does not have a parent!\n")	}
			#paste0("Setting parent node of ", node, " ...\n")

			node@ref$parent_node	<-	new_parent_node
}

#'	Set value of target node
#'
#'	@name	Node@set_value
#'	@param	node				A node object
#'	@param	value				A double value to set
#'	@return	void				Void
#'	@examples
#'	set_value(node)
S7::method(set_value,	Node)	<-	function(node,	value)	{
			node@ref$value	<-	value
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

###		DATA	###

#'	Assign left child to a node
#'	@name	node@assign_left_child
#'	@param	node	A node object
#'	@param	child2be	A node object
#'	@return	void
#'	@examples
#'	assign_left_child(node,	child)

S7::method(assign_left_child,	Node)	<-	function(node,	child2be)	{
			if	(is.null(child2be))	{
				stop("Cannot add invalid object as child!\n")
			}	else	if	(identical(get_left_child(node),	NULL))	{
				set_left_child(node,	child2be)
			}	else	if	(get_value(get_left_child(node))	>	get_value(child2be))	{
				#	TODO
			}	else	if	(get_value(get_left_child(node))	<	get_value(child2be))	{
				#	TODO
			}
}

#'	Assign right child to a node
#'	@name	node@assign_right_child
#'	@param	node	A node object
#'	@param	child2be	A node object
#'	@return	void
#'	@examples
#'	assign_right_child(node,	child)

S7::method(assign_right_child,	Node)	<-	function(node,	child2be)	{
			if	(is.null(child2be))	{
				stop("Cannot add invalid object as child!\n")
			}	else	if	(identical(get_right_child(node),	NULL))	{
				set_right_child(node,	child2be)
			}	else	if	(get_value(get_right_child(node))	>	get_value(child2be))	{
				#	TODO
			}	else	if	(get_value(get_left_child(node))	<	get_value(child2be))	{
				#	TODO
			}
}
#	TODO refactor this to be less text see function below
#'	Assign children to a node
#'	@name	node@assign_children
#'	@param	node	A node object, to which children are assigned
#'	@param	child1	A node object, to assign to node
#'	@param	child2	(Optional) A further node object, to assign to node
#'	@return	void
#'	@examples
#'	assign_children(node,	child1,	child2)
#'	assign_children(node,	child)

S7::method(assign_children,	Node)	<-	function(node,	child1,	child2=NULL)	{
			if	(is.null(child1))	{
				stop("You must provide at least one node!\n")
			}	else	if	(get_value(get_right_child(node))	>	get_value(child1))	{
				assign_left_child(node,	child1)
			}
}

#'	Remove child from node
#'	@name	node@remove_child
#'	@param	node	A (parent) node object
#'	@param	node	A (child) node object
#'	@param	return	void
#'	@examples
#'	remove_child(node,	child)

S7::method(remove_child,	Node)	<-	function(node,	child)	{
			#	TODO
}
