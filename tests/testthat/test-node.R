test_that("Node initialization",	{
			root	<-	Node(value	=	0,	ROOT	=	TRUE,	left	=	NULL,	right	=	NULL)
			testthat::expect_s7_class(root, Node)

			node	<-	Node(value	=	1,	ROOT	=	FALSE,	left	=	NULL,	right	=	NULL,	parent	=	root)
			testthat::expect_s7_class(node,	Node)
})

test_that("Node	validation",	{
			root	<-	Node(value	=	0,ROOT	=	TRUE,	left	=	NULL,	right	=	NULL)
			node	<-	Node(value	=	1,ROOT	=	FALSE,	left	=	NULL,	right	=	NULL,	parent	=	root)
			testthat::expect_true(is_root(root),	TRUE)
			testthat::expect_false(is_root(node),	FALSE)
})

test_that("Node getters and setters",	{
			root	<-	Node(value	=	0,			ROOT	=	TRUE,	left	=	NULL,	right	=	NULL)
			node	<-	Node(value	=	NA_real_,	ROOT	=	FALSE,	left	=	NULL,	right	=	NULL,	parent	=	root)
			node2	<-	Node(value	=	NA_real_,	ROOT	=	FALSE,	left	=	NULL,	right	=	NULL,	parent	=	root)

			testthat::expect_no_error(set_parent(node,	root))
			testthat::expect_no_error(set_parent(node2,	root))

			testthat::expect_error(get_parent(root))
			testthat::expect_error(set_parent(root,	node))

			testthat::expect_identical(get_parent(node),	root)

			testthat::expect_identical(get_value(node),		NA_real_)
			testthat::expect_identical(get_value(node2),	NA_real_)

			testthat::expect_identical(set_value(node,	1),		1)
			testthat::expect_identical(set_value(node,	2),		2)

			testthat::expect_no_error(set_left_child(root,	node))
			testthat::expect_no_error(set_right_child(root,	node2))
			
			testthat::expect_identical(get_left_child(root),	node)
			testthat::expect_identical(get_right_child(root),	node2)

})

test_that("Node correctly assigns children left/right by value",	{
			root	<-	Node(value	=	0,			ROOT	=	TRUE,	left	=	NULL,	right	=	NULL)
			node1	<-	Node(value	=	NA_real_,	ROOT	=	FALSE,	left	=	NULL,	right	=	NULL,	parent	=	root)
			node2	<-	Node(value	=	NA_real_,	ROOT	=	FALSE,	left	=	NULL,	right	=	NULL,	parent	=	root)
			node3	<-	Node(value	=	NA_real_,	ROOT	=	FALSE,	left	=	NULL,	right	=	NULL)
			node4	<-	Node(value	=	NA_real_,	ROOT	=	FALSE,	left	=	NULL,	right	=	NULL)

			set_value(node1,	1)
			set_value(node2,	2)
			set_value(node3,	3)
			set_value(node4,	4)

			testthat::expect_error(assign_left_child(root,	NULL))
			testthat::expect_error(assign_right_child(root,	NULL))
			testthat::expect_no_error(assign_left_child(root,	node1))
			testthat::expect_error(assign_left_child(root,	node3))
			testthat::expect_no_error(assign_right_child(root,	node2))
			testthat::expect_no_error(remove_child(root,	node1))
			testthat::expect_no_error(remove_child(root,	node2))

			testthat::expect_no_error(assign_children(root,	node1,	node2))
			testthat::expect_lt(get_value(get_left_child(root)),	get_value(get_right_child(root)))
			testthat::expect_error(assign_children(root,	node3,	node4))

})
