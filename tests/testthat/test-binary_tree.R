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
			testthat::expect_equal(get_depth(tree1),	0)

			node1	<-	Node(s_value=1,	leaf_value=0.0)
			node2	<-	Node(s_value=2,	leaf_value=0.5)
			node3	<-	Node(s_value=3,	leaf_value=7.0)
			node4	<-	Node(s_value=4,	leaf_value=1.5)
			node5	<-	Node(s_value=5,	leaf_value=9.0)
			node6	<-	Node(s_value=6,	leaf_value=2.5)

			set_left_child(root,	node1)
			set_right_child(root,	node2)

			set_left_child(node1,	node3)
			set_right_child(node1,	node4)

			set_left_child(node2,	node5)
			set_right_child(node2,	node6)

			testthat::expect_false(is_empty(tree1))
			testthat::expect_equal(get_depth(tree1),	2)

})

test_that("children assignment",	{
			root	<-	Node(ROOT=TRUE)
			tree	<-	BinaryTree(root=root)

			node1	<-	Node(s_value=1,	leaf_value=0.0)
			node2	<-	Node(s_value=2,	leaf_value=0.5)
			node3	<-	Node(s_value=3,	leaf_value=7.0)
			node4	<-	Node(s_value=4,	leaf_value=1.5)
			node5	<-	Node(s_value=5,	leaf_value=9.0)
			node6	<-	Node(s_value=6,	leaf_value=2.5)

			assign_left_child(tree,	root,	node1)
			assign_right_child(tree,	root,	node2)

			assign_left_child(tree,	node1,	node3)
			assign_right_child(tree,	node1,	node4)

			assign_left_child(tree,	node3,	node5)
			assign_right_child(tree,	node3,	node6)
})

## Count leaves
test_that("count_leaves on empty tree returns 0",	{
			tree	<-	BinaryTree()
			testthat::expect_equal(count_leaves(tree),	0L)
})

test_that("count_leaves on root-only tree returns 1",	{
			root	<-	Node(ROOT	=	TRUE)
			tree	<-	BinaryTree(root	=	root)
			testthat::expect_equal(count_leaves(tree),	1L)
})

test_that("count_leaves on two-level tree returns 2",	{
			root	<-	Node(ROOT	=	TRUE,	s_feature	=	1L,	s_value	=	0.5)
			left	<-	Node(leaf_value	=	1.0)
			right	<-	Node(leaf_value	=	2.0)
			set_left_child(root,	left)
			set_right_child(root,	right)
			tree	<-	BinaryTree(root	=	root)
			testthat::expect_equal(count_leaves(tree),	2L)
})

test_that("count_leaves on three-level tree returns correct count",	{
	root	<-	Node(ROOT	=	TRUE,	s_feature	=	1L,	s_value	=	0.5)
	inner	<-	Node(s_feature	=	1L,	s_value	=	0.3)
	leaf1	<-	Node(leaf_value	=	1.0)
	leaf2	<-	Node(leaf_value	=	2.0)
	leaf3	<-	Node(leaf_value	=	3.0)
	set_left_child(root,	inner)
	set_right_child(root,	leaf1)
	set_left_child(inner,	leaf2)
	set_right_child(inner,	leaf3)
	tree	<-	BinaryTree(root	=	root)
	testthat::expect_equal(count_leaves(tree),	3L)
})

# Predict
test_that("predict errors on empty tree",	{
	tree	<-	BinaryTree()
	testthat::expect_error(predict_tree(tree,	c(0.5,	0.5)))
})

test_that("predict on root-only tree returns root leaf_value",	{
	root	<-	Node(ROOT	=	TRUE,	leaf_value	=	42.0)
	tree	<-	BinaryTree(root	=	root)
	testthat::expect_equal(predict_tree(tree,	c(0.5,	0.5)),	42.0)
})

test_that("predict routes left correctly",	{
	root	<-	Node(ROOT	=	TRUE,	s_feature	=	1L,	s_value	=	0.5)
	left	<-	Node(leaf_value	=	1.0)
	right	<-	Node(leaf_value	=	2.0)
	set_left_child(root,	left)
	set_right_child(root,	right)
	tree	<-	BinaryTree(root	=	root)
	testthat::expect_equal(predict_tree(tree,	c(0.3,	0.9)),	1.0)
})

test_that("predict routes right correctly",	{
	root	<-	Node(ROOT	=	TRUE,	s_feature	=	1L,	s_value	=	0.5)
	left	<-	Node(leaf_value	=	1.0)
	right	<-	Node(leaf_value	=	2.0)
	set_left_child(root,	left)
	set_right_child(root,	right)
	tree	<-	BinaryTree(root	=	root)
	testthat::expect_equal(predict_tree(tree,	c(0.7,	0.9)),	2.0)
})

test_that("predict traverses multiple levels correctly",	{
	root	<-	Node(ROOT	=	TRUE,	s_feature	=	1L,	s_value	=	0.5)
	inner	<-	Node(s_feature	=	2L,	s_value	=	0.3)
	leaf1	<-	Node(leaf_value	=	99.0)
	leaf2	<-	Node(leaf_value	=	1.0)
	leaf3	<-	Node(leaf_value	=	2.0)
	set_left_child(root,	inner)
	set_right_child(root,	leaf1)
	set_left_child(inner,	leaf2)
	set_right_child(inner,	leaf3)
	tree	<-	BinaryTree(root	=	root)
    # x[1]=0.3 <= 0.5 -> inner, x[2]=0.1 <= 0.3 -> leaf2
	testthat::expect_equal(predict_tree(tree,	c(0.3,	0.1)),	1.0)
    # x[1]=0.3 <= 0.5 -> inner, x[2]=0.5 > 0.3 -> leaf3
	testthat::expect_equal(predict_tree(tree,	c(0.3,	0.5)),	2.0)
    # x[1]=0.7 > 0.5 -> leaf1
    testthat::expect_equal(predict_tree(tree,	c(0.7,	0.1)),	99.0)
})

# Prune
test_that("prune errors on a leaf node",	{
	root	<-	Node(ROOT	=	TRUE)
	leaf	<-	Node(leaf_value	=	1.0)
	set_left_child(root,	leaf)
	tree	<-	BinaryTree(root	=	root)
	testthat::expect_error(prune(tree,	leaf,	0.0))
})

test_that("prune converts inner node to leaf",	{
	root	<-	Node(ROOT	=	TRUE,	s_feature	=	1L,	s_value	=	0.5)
	left	<-	Node(leaf_value	=	1.0)
	right	<-	Node(leaf_value	=	2.0)
	set_left_child(root,	left)
	set_right_child(root,	right)
	tree	<-	BinaryTree(root	=	root)
	prune(tree,	root,	5.0)
	testthat::expect_true(is_leaf(root))
})

test_that("prune sets correct leaf value",	{
	root	<-	Node(ROOT	=	TRUE,	s_feature	=	1L,	s_value	=	0.5)
	left	<-	Node(leaf_value	=	1.0)
	right	<-	Node(leaf_value	=	2.0)
	set_left_child(root,	left)
	set_right_child(root,	right)
	tree	<-	BinaryTree(root	=	root)
	prune(tree,	root,	5.0)
	testthat::expect_equal(root@ref$leaf_value,	5.0)
})

test_that("prune updates leaf count correctly",	{
	root	<-	Node(ROOT	=	TRUE,	s_feature	=	1L,	s_value	=	0.5)
	left	<-	Node(leaf_value	=	1.0)
	right	<-	Node(leaf_value	=	2.0)
	set_left_child(root,	left)
	set_right_child(root,	right)
	tree	<-	BinaryTree(root	=	root)
	testthat::expect_equal(count_leaves(tree),	2L)
	prune(tree,	root,	5.0)
	testthat::expect_equal(tree@ref$n_leaves,	1L)
})

test_that("prune updates depth correctly",	{
	root	<-	Node(ROOT	=	TRUE,	s_feature	=	1L,	s_value	=	0.5)
	inner	<-	Node(s_feature	=	2L,	s_value	=	0.3)
	leaf1	<-	Node(leaf_value	=	1.0)
	leaf2	<-	Node(leaf_value	=	2.0)
	leaf3	<-	Node(leaf_value	=	3.0)
	set_left_child(root,	inner)
	set_right_child(root,	leaf1)
	set_left_child(inner,	leaf2)
	set_right_child(inner,	leaf3)
	tree	<-	BinaryTree(root	=	root)
	testthat::expect_equal(get_depth(tree),	2L)
	prune(tree,	inner,	5.0)
	testthat::expect_equal(get_depth(tree),	1L)
})
