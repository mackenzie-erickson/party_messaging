library(data.table)
library(igraph)
library(NetworkInference)
library(tidyverse)

data('policies')

policy_cascades <- as_cascade_long(policies, cascade_node_name = 'statenam',
  event_time = 'adopt_year', 
  cascade_id = 'policy')

results <- netinf(policy_cascades, trans_mod = "exponential", 
  p_value_cutoff = 0.1, params = 0.5, quiet = TRUE)

nodes <- sort(unique(c(results$origin_node, results$destination_node)))

edge_list_destination_points_to_origin <- as.matrix(cbind(
  match(results$destination_node, nodes),
  match(results$origin_node, nodes)))

edge_list_origin_points_to_destination <- as.matrix(cbind(
  match(results$origin_node, nodes),
  match(results$destination_node, nodes)))

g_destination_points_to_origin <- graph_from_edgelist(
  edge_list_destination_points_to_origin)

g_origin_points_to_destination <- graph_from_edgelist(
  edge_list_origin_points_to_destination)

pr_destination_points_to_origin <- page_rank(g_destination_points_to_origin)

pr_origin_points_to_destination <- page_rank(g_origin_points_to_destination)


plot(
  pr_destination_points_to_origin$vector,
  pr_origin_points_to_destination$vector)
# interesting! they're correlated!


# let's compare to in- and out-degree
degree_data <- merge(
  as.data.table(edge_list_destination_points_to_origin)[, .N, V1][,
    .(V = V1, destination_degree = N)],
  as.data.table(edge_list_destination_points_to_origin)[, .N, V2][, 
    .(V = V2, origin_degree = N)],
  by = "V", all = TRUE
)


degree_data[, 
  pr_destination_points_to_origin := pr_destination_points_to_origin$vector]

degree_data[, 
  pr_origin_points_to_destination := pr_origin_points_to_destination$vector]

degree_data[, diff := pr_destination_points_to_origin - pr_origin_points_to_destination]

degree_data[which.max(diff)] # this identifies a very influential node
degree_data[which.min(diff)] # this identifies a not very influential node

# so it looks like page rank for the graph in which desination -> origin is
# much larger for the really influential node (5) than it is for the graph
# that has origin -> destination 

# bottom line: to use page rank, but destination nodes as "from" or 1st in the
# edge list

