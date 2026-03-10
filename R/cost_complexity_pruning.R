# cost_complexity_pruning.R
#
# DONE Generics
# TODO Method skeletons
# TODO Documentation like Theo

get_subtree_error <- S7::new_generic("get_subtree_error", "tree")
get_weakest_link <- S7::new_generic("get_weakest_link", "tree")
prune_node <- S7::new_generic("prune_node", "tree")
cost_complexity_prune <- S7::new_generic("cost_complexity_prune", "tree")get_subtree_error <- S7::new_generic("get_subtree_error", "tree")

