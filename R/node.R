Node	<-	S7::new_class("Node",
			properties = list(
				value		=	S7::new_property(class	=	S7::class_double,	default	=	0.0),
				ROOT		=	S7::new_property(class	=	S7::class_logical,	default = FALSE),
				parent_node	=	S7::new_property(								default	=	NULL),
				left_node	=	S7::new_property(								default	=	NULL),
				right_node	=	S7::new_property(								default	=	NULL),
				ref			=	S7::new_property(S7::class_environment,	default	=	new.env(parent	=	emptyenv()))
			)
)

###		GENERICS	###
is_root			<-	S7::new_generic("is_root",	"node")
get_parent		<-	S7::new_generic("get_parent",	"node")
set_parent		<-	S7::new_generic("set_parent",	"node")
get_value		<-	S7::new_generic("get_value",	"node")
set_value		<-	S7::new_generic("set_value",	"node")
get_left_child	<-	S7:::new_generic("get_left_child",	"node")
get_right_child	<-	S7:::new_generic("get_right_child",	"node")
set_left_child	<-	S7:::new_generic("get_left_child",	"node")
set_right_child	<-	S7:::new_generic("set_right_child",	"node")

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
