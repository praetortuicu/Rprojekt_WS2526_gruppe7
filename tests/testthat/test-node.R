test_that("Node initialization",	{
			root	<-	Node(s_value	=	0,	ROOT	=	TRUE)
			testthat::expect_s7_class(root, Node)

			node	<-	Node(s_value	=	1,	ROOT	=	FALSE)
			testthat::expect_s7_class(node,	Node)

			testthat::expect_true(is_root(root))
			testthat::expect_false(is_root(node))

			testthat::expect_false(is_leaf(root))
			testthat::expect_true(is_leaf(node))
})

test_that("Node getters and setters",	{
			root	<-	Node(s_value	=	0,							ROOT	=	TRUE)
			node	<-	Node(s_value	=	1,	leaf_value	=	0.5,	ROOT	=	FALSE)
			node1	<-	Node(s_value	=	2,	leaf_value	=	1.5,	ROOT	=	FALSE)

			set_left_child(root,	node)
			set_right_child(root,	node1)

			testthat::expect_true(is_leaf(node))
			testthat::expect_true(is_leaf(node1))

			testthat::expect_true(identical(get_left_child(root),	node))
			testthat::expect_true(identical(get_right_child(root),	node1))

			testthat::expect_equal(get_value(root),		0)
			testthat::expect_false(identical(get_value(node),	1))
			testthat::expect_false(identical(get_value(node1),	2))

			#	These nodes are leaves as of now
			testthat::expect_true(identical(get_value(node),	0.5))
			testthat::expect_true(identical(get_value(node1),	1.5))
			set_value(node,	5)
			testthat::expect_true(identical(get_value(node),	5))

			node2	<-	Node(s_value	=	3,	leaf_value	=	7,		ROOT	=	FALSE)
			node3	<-	Node(s_value	=	4,	leaf_value	=	3.5,	ROOT	=	FALSE)


			set_left_child(node,	node2)
			set_right_child(node,	node3)

			testthat::expect_identical(get_left_child(node),	node2)
			testthat::expect_identical(get_right_child(node),	node3)


			testthat::expect_false(is_leaf(node))
			testthat::expect_true(is_leaf(node1))

			# node is not a leaf anymore, node1 is
			testthat::expect_true(identical(get_value(node),	1))
			testthat::expect_false(identical(get_value(node1),	2))

			set_value(node,	10)
			testthat::expect_identical(get_value(node),	10)

})
