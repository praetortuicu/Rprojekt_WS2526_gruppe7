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
is_root		<-	S7::new_generic("is_root",	"node")
get_parent	<-	S7::new_generic("get_parent",	"node")
set_parent	<-	S7::new_generic("set_parent",	"node")
get_value	<-	S7::new_generic("get_value",	"node")
set_value	<-	S7::new_generic("set_value",	"node")

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
