#' @title Create the event graph by given nodes and edges.
#' @usage create_pmap(nodes, edges, render = T)
#' @param nodes Event list
#' @param edges Event transform list
#' @param render Whether should call `render_graph()` on the final result. Set to `FALSE` if you want further manipulate the result by `DiagrammeR` functions.
#' @description
#' `nodes` should be a `data.frame` containing following columns:
#'   * `name`: Nodes name, will be used as label. (`character`)
#'   * `is_target`: Whether it's final stage. (`logical`)
#'   * `percentage`: The percentage of customer affected by the given event. (`numeric`)
#'
#' `edges` should be a `data.frame` containing following columns:
#'   * `from`: the begining event of the edge. (`character`)
#'   * `to`: the ending event of the edge (`character`)
#'   * `value`: How many of customer affected by the given event. (`numeric`)
#' @importFrom dplyr        %>%
#' @importFrom dplyr        mutate
#' @importFrom dplyr        mutate_if
#' @importFrom DiagrammeR   create_graph
#' @importFrom DiagrammeR   add_nodes_from_table
#' @importFrom DiagrammeR   add_edges_from_table
#' @importFrom DiagrammeR   add_global_graph_attrs
#' @importFrom DiagrammeR   set_node_attrs
#' @importFrom DiagrammeR   set_edge_attrs
#' @importFrom DiagrammeR   select_nodes_by_degree
#' @importFrom DiagrammeR   get_selection
#' @importFrom DiagrammeR   delete_nodes_ws
#' @importFrom DiagrammeR   render_graph
#' @export
create_pmap <- function(nodes, edges, render = T) {
  # print("Converting factor to character [nodes]...")
  nodes <- nodes %>%
    mutate_if(is.factor, as.character) %>%
    mutate(
      index = 1:nrow(nodes),
      tooltip = get_attrs_desc(nodes)
    )
  # print(str(nodes))

  # print("Converting factor to character [edges]...")
  edges <- edges %>%
    mutate_if(is.factor, as.character) %>%
    mutate(
      tooltips = get_attrs_desc(edges)
    )
  # print(str(edges))
  # print("create_graph()")
  p <- create_graph()

  # print("add_nodes_from_table()")
  p <- p %>%
    add_nodes_from_table(
      table = nodes,
      label_col = "name"
    )

  # print("add_edges_from_table()")
  p <- p %>% add_edges_from_table(
      table = edges,
      from_col = "from",
      to_col = "to",
      ndf_mapping = "name")

  # print("add_global_graph_attrs()")
  p <- p %>% 
    # graph [ layout = "dot" ]
    add_global_graph_attrs(attr_type = "graph", attr = "layout", value = "dot") %>%
    # node [...]
    add_global_graph_attrs(attr_type = "node", attr = "fixedsize", value = "false") %>%
    add_global_graph_attrs(attr_type = "node", attr = "shape", value = "box") %>%
    add_global_graph_attrs(attr_type = "node", attr = "style", value = "filled,rounded") %>%
    add_global_graph_attrs(attr_type = "node", attr = "gradientangle", value = "90") %>%
    # edge [...]
    ## grey900(#212121)
    add_global_graph_attrs(attr_type = "edge", attr = "color", value = "#21212180") %>%
    ## grey900(#212121)
    add_global_graph_attrs(attr_type = "edge", attr = "fontcolor", value = "#212121")

  # print("set_node_attrs()")
  p <- p %>%
    # node attributes
    ## grey900(#212121)
    set_node_attrs(node_attr = "fontcolor", values = "#212121") %>%
    ## lightBlue900(#01579B)
    set_node_attrs(node_attr = "color", values = "#01579B") %>%
    ## lightBlue100(#B3E5FC) => lightBlue50(#E1F5FE)
    set_node_attrs(node_attr = "fillcolor", values = "#B3E5FC:#E1F5FE") %>%
    set_node_attrs(node_attr = "fontsize", values = (nodes$percentage * 15 + 5)) %>%
    set_node_attrs(node_attr = "label", values = nodes$name) %>%
    set_node_attrs(node_attr = "tooltip", values = nodes$tooltip)
    
  # print("set_edge_attrs() for target nodes")
  target_ids <- nodes[nodes$is_target, "index"]
  p <- p %>%
    ## deepOrange900(#BF360C)
    set_node_attrs(node_attr = "color", values = "#BF360C", nodes = target_ids) %>%
    ## deepOrange100(#FFCCBC):deepOrange50(#FBE9E7)
    set_node_attrs(node_attr = "fillcolor", values = "#FFCCBC:#FBE9E7", nodes = target_ids) %>%
    set_node_attrs(node_attr = "fontsize", values = (nodes$percentage * 10 + 15), nodes = target_ids)

  # print("set_edge_attrs()")
  p <- p %>%
    # Edge attributes
    set_edge_attrs(edge_attr = "penwidth", values = log10(edges$value) + 1) %>%
    set_edge_attrs(edge_attr = "label", values = edges$value) %>%
    set_edge_attrs(edge_attr = "tooltip", values = edges$tooltips) %>%
    set_edge_attrs(edge_attr = "labeltooltip", values = edges$tooltips)

  # remove node without edges
  zero_degree_nodes <- p %>% select_nodes_by_degree("deg == 0") %>% get_selection()
  if (!is.na(zero_degree_nodes) && length(zero_degree_nodes) > 0) {
    p <- p %>% select_nodes_by_degree("deg == 0") %>% delete_nodes_ws()
  }

  if (render) {
    p <- render_graph(p)
  }

  return(p)
}
