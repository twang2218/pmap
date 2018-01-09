#' @title Create the event graph by given nodes and edges.
#' @description Create the process map graph by specify the nodes and edges
#' @usage create_pmap_graph(nodes, edges, target_types = NULL)
#' @param nodes Event list, it should be a `data.frame` containing following columns:
#'   * `name`: Event name, will be used as label. (`character`)
#'   * `type`: The event type (`character`)
#' @param edges Event transform list, it should be a `data.frame` containing following columns:
#'   * `from`: the beginning event of the edge. (`character`)
#'   * `to`: the ending event of the edge (`character`)
#'   * `amount`: How many of customer affected by the given event. (`numeric`)
#' @param target_types A vector contains the target event types
#' @examples
#' eventlog <- generate_eventlog()
#' nodes <- generate_nodes(eventlog)
#' head(nodes)
#' #  # A tibble: 6 x 3
#' #    name              type   amount
#' #    <chr>             <chr>   <int>
#' #  1 Event 1 (normal)  normal    105
#' #  2 Event 10 (target) target     97
#' #  3 Event 2 (normal)  normal     94
#' #  4 Event 3 (normal)  normal     94
#' #  5 Event 4 (normal)  normal    101
#' #  6 Event 5 (normal)  normal     95
#' edges <- generate_edges(eventlog)
#' head(edges)
#' #  # A tibble: 6 x 3
#' #    from             to                amount
#' #    <chr>            <chr>              <int>
#' #  1 Event 1 (normal) Event 1 (normal)       8
#' #  2 Event 1 (normal) Event 10 (target)     10
#' #  3 Event 1 (normal) Event 2 (normal)      12
#' #  4 Event 1 (normal) Event 3 (normal)       9
#' #  5 Event 1 (normal) Event 4 (normal)       7
#' #  6 Event 1 (normal) Event 5 (normal)      10
#' p <- create_pmap_graph(nodes, edges, target_types = c("target"))
#' render_pmap(p)
#' @seealso [create_pmap]
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
create_pmap_graph <- function(nodes, edges, target_types = NULL) {
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
    rename(catalog = type)
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
    type_col = "catalog"
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
