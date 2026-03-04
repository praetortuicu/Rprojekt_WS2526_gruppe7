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
is_empty	<-	S7::new_generic("is_empty",		"tree")
get_depth	<-	S7::new_generic("get_depth",	"tree")

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
