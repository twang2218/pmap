#' @title Create the event graph by given nodes and edges.
#' @usage create_pmap(nodes, edges, target_types = NULL)
#' @param nodes Event list
#' @param edges Event transform list
#' @param target_types A vector contains the target event types
#' @description
#' `nodes` should be a `data.frame` containing following columns:
#'   * `name`: Event name, will be used as label. (`character`)
#'   * `type`: The event type (`character`)
#'
#' `edges` should be a `data.frame` containing following columns:
#'   * `from`: the begining event of the edge. (`character`)
#'   * `to`: the ending event of the edge (`character`)
#'   * `amount`: How many of customer affected by the given event. (`numeric`)
#' @seealso [create_pmap_from_eventlog]
#' @importFrom dplyr        %>%
#' @importFrom dplyr        mutate
#' @importFrom dplyr        mutate_if
#' @importFrom dplyr        left_join
#' @importFrom dplyr        summarize
#' @importFrom DiagrammeR   create_graph
#' @importFrom DiagrammeR   add_nodes_from_table
#' @importFrom DiagrammeR   add_edges_from_table
#' @importFrom DiagrammeR   add_global_graph_attrs
#' @importFrom DiagrammeR   set_node_attrs
#' @importFrom DiagrammeR   set_edge_attrs
#' @importFrom DiagrammeR   select_nodes
#' @importFrom DiagrammeR   set_node_attrs_ws
#' @importFrom DiagrammeR   get_selection
#' @importFrom DiagrammeR   clear_selection
#' @export
create_pmap <- function(nodes, edges, target_types = NULL) {
  # make 'R CMD Check' happy
  amount <- from <- to <- type <- NULL

  # Collect inbound and outbound count
  nodes_outbound <- edges %>%
    group_by(name = from) %>%
    summarize(outbound = sum(amount))

  nodes_inbound <- edges %>%
    group_by(name = to) %>%
    summarize(inbound = sum(amount))

  nodes <- nodes %>%
    left_join(nodes_inbound, by = "name") %>%
    left_join(nodes_outbound, by = "name")

  # Set all 'NA' to zero
  nodes$inbound[is.na(nodes$inbound)] <- 0
  nodes$outbound[is.na(nodes$outbound)] <- 0

  # print("Converting factor to character [nodes]...")
  nodes <- nodes %>%
    mutate_if(is.factor, as.character) %>%
    mutate(
      index = 1:nrow(nodes),
      tooltip = get_attrs_desc(nodes),
      name_without_space = gsub(" ", "_", nodes$name)
    ) %>%
    rename(catelog = type)
  # print(str(nodes))

  # print("Converting factor to character [edges]...")
  edges <- edges %>%
    mutate_if(is.factor, as.character) %>%
    mutate(
      tooltips = get_attrs_desc(edges),
      from = gsub(" ", "_", edges$from),
      to = gsub(" ", "_", edges$to)
    )
  # print(str(edges))

  # print("create_graph()")
  p <- create_graph()

  # print("add_nodes_from_table()")
  p <- add_nodes_from_table(
    p,
    table = nodes,
    label_col = "name",
    type_col = "catelog"
  )

  # print("add_edges_from_table()")
  if (nrow(edges) > 0) {
    p <- add_edges_from_table(
      p,
      table = edges %>% select(-amount),
      from_col = "from",
      to_col = "to",
      ndf_mapping = "name_without_space"
    )
  }

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
    set_node_attrs(node_attr = "fontsize", values = (log10(nodes$inbound + nodes$outbound) + 10)) %>%
    set_node_attrs(node_attr = "label", values = nodes$name) %>%
    set_node_attrs(node_attr = "tooltip", values = nodes$tooltip)

  # print("set_edge_attrs() for target nodes")
  p <- select_nodes(p, conditions = type %in% target_types)
  if (!any(is.na(get_selection(p)))) {
    p <- p %>%
      ## deepOrange900(#BF360C)
      set_node_attrs_ws(node_attr = "color", value = "#BF360C") %>%
      ## deepOrange100(#FFCCBC):deepOrange50(#FBE9E7)
      set_node_attrs_ws(node_attr = "fillcolor", value = "#FFCCBC:#FBE9E7")
  }
  p <- clear_selection(p)

  # print("set_edge_attrs()")
  p <- p %>%
    # Add amount attribute
    set_edge_attrs(edge_attr = "amount", values = edges$amount) %>%
    # Edge attributes
    set_edge_attrs(edge_attr = "penwidth", values = (log10(edges$amount) + 1)) %>%
    set_edge_attrs(edge_attr = "label", values = edges$amount) %>%
    set_edge_attrs(edge_attr = "tooltip", values = edges$tooltips) %>%
    set_edge_attrs(edge_attr = "labeltooltip", values = edges$tooltips)

  return(p)
}
