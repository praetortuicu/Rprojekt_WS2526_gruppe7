#	Construction
test_that("empty tree initializes correctly", {
			tree	<-	BinaryTree()
			testthat::expect_s7_class(tree,	BinaryTree)
			testthat::expect_true(is_empty(tree))
})

#	Depth and size
test_that("getters",	{
			tree	<-	BinaryTree()
			testthat::expect_equal(get_depth(tree),	0)

			root	<-	Node(ROOT=TRUE)
			tree1	<-	BinaryTree(root=root)
			testthat::expect_false(is_empty(tree1))
			testthat::expect_equal(get_depth(tree1),	1)

			node1	<-	Node(s_value=1,	l_value=0.0)
			node2	<-	Node(s_value=2,	l_value=0.5)
			node3	<-	Node(s_value=3,	l_value=7.0)
			node4	<-	Node(s_value=4,	l_value=1.5)
			node5	<-	Node(s_value=5,	l_value=9.0)
			node6	<-	Node(s_value=6,	l_value=2.5)

			set_left_child(root,	node1)
			set_right_child(root,	node2)

			set_left_child(node1,	node3)
			set_right_child(node1,	node4)

			set_left_child(node2,	node5)
			set_right_child(node2,	node6)


			testthat::expect_false(is_empty(tree1))
			testthat::expect_equal(get_depth(tree1),	3)
})
#	Prediction

#	Prune
