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
#' @importFrom dplyr        select
#' @importFrom dplyr        mutate
#' @importFrom dplyr        mutate_if
#' @importFrom dplyr        left_join
#' @importFrom dplyr        rename
#' @importFrom dplyr        summarize
#' @importFrom DiagrammeR   create_graph
#' @importFrom DiagrammeR   add_nodes_from_table
#' @importFrom DiagrammeR   add_edges_from_table
#' @importFrom DiagrammeR   add_global_graph_attrs
#' @importFrom DiagrammeR   set_node_attrs
#' @importFrom DiagrammeR   set_edge_attrs
#' @export
create_pmap_graph <- function(nodes, edges, target_types = NULL) {
  # make 'R CMD Check' happy
  amount <- from <- to <- type <- NULL

  # Collect inbound and outbound count
  nodes_outbound <- edges %>%
    dplyr::group_by(name = from) %>%
    dplyr::summarize(outbound = sum(amount))

  nodes_inbound <- edges %>%
    dplyr::group_by(name = to) %>%
    dplyr::summarize(inbound = sum(amount))

  nodes <- nodes %>%
    dplyr::left_join(nodes_inbound, by = "name") %>%
    dplyr::left_join(nodes_outbound, by = "name")

  # Set all 'NA' to zero
  nodes$inbound[is.na(nodes$inbound)] <- 0
  nodes$outbound[is.na(nodes$outbound)] <- 0
  if (!is.null(nodes$amount)) {
    nodes$amount[is.na(nodes$amount)] <- 0
  }

  # print("Converting factor to character [nodes]...")
  nodes <- nodes %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate(
      index = 1:nrow(nodes),
      tooltip = get_attrs_desc(nodes),
      name_without_space = gsub(" ", "_", nodes$name)
    ) %>%
    dplyr::rename(catalog = type)
  # print(str(nodes))

  # print("Converting factor to character [edges]...")
  edges <- edges %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate(
      tooltips = get_attrs_desc(edges),
      from = gsub(" ", "_", edges$from),
      to = gsub(" ", "_", edges$to)
    )
  # print(str(edges))

  # print("create_graph()")
  p <- DiagrammeR::create_graph()

  # print("add_nodes_from_table()")
  p <- DiagrammeR::add_nodes_from_table(
    p,
    table = nodes,
    label_col = "name",
    type_col = "catalog"
  )

  # print("add_edges_from_table()")
  if (nrow(edges) > 0) {
    p <- DiagrammeR::add_edges_from_table(
      p,
      table = dplyr::select(edges, -amount),
      from_col = "from",
      to_col = "to",
      ndf_mapping = "name_without_space"
    )
  }

  # print("add_global_graph_attrs()")
  p <- p %>%
    # graph [ layout = "dot" ]
    DiagrammeR::add_global_graph_attrs(attr_type = "graph", attr = "layout", value = "dot") %>%
    # node [...]
    DiagrammeR::add_global_graph_attrs(attr_type = "node", attr = "fixedsize", value = "false") %>%
    DiagrammeR::add_global_graph_attrs(attr_type = "node", attr = "shape", value = "box") %>%
    DiagrammeR::add_global_graph_attrs(attr_type = "node", attr = "style", value = "filled,rounded") %>%
    DiagrammeR::add_global_graph_attrs(attr_type = "node", attr = "gradientangle", value = "90") %>%
    # edge [...]
    ## grey900(#212121)
    DiagrammeR::add_global_graph_attrs(attr_type = "edge", attr = "color", value = "#000000A0") %>%
    ## grey900(#212121)
    DiagrammeR::add_global_graph_attrs(attr_type = "edge", attr = "fontcolor", value = "#212121")

  # node default attributes
  p <- p %>%
    ## grey900(#212121)
    DiagrammeR::set_node_attrs(node_attr = "fontcolor", values = "#212121") %>%
    ## lightBlue900(#01579B)
    DiagrammeR::set_node_attrs(node_attr = "color", values = "#01579B") %>%
    ## lightBlue100(#B3E5FC) => lightBlue50(#E1F5FE)
    DiagrammeR::set_node_attrs(node_attr = "fillcolor", values = "#B3E5FC:#E1F5FE") %>%
    DiagrammeR::set_node_attrs(node_attr = "tooltip", values = nodes$tooltip) %>%
    DiagrammeR::set_node_attrs(node_attr = "fontsize", values = 16) %>%
    DiagrammeR::set_node_attrs(node_attr = "label", values = nodes$name)

  p <- adjust_node_style(p)

  # print("set_edge_attrs()")
  p <- p %>%
    # Add amount attribute
    DiagrammeR::set_edge_attrs(edge_attr = "amount", values = edges$amount) %>%
    # Edge attributes
    DiagrammeR::set_edge_attrs(edge_attr = "penwidth", values = 1) %>%
    DiagrammeR::set_edge_attrs(edge_attr = "label", values = edges$amount) %>%
    DiagrammeR::set_edge_attrs(edge_attr = "tooltip", values = edges$tooltips) %>%
    DiagrammeR::set_edge_attrs(edge_attr = "labeltooltip", values = edges$tooltips)

  if (!any(is.null(edges$amount))) {
    projections <- projection(edges$amount, 1, 15)
    p <- p %>%
      DiagrammeR::set_edge_attrs(edge_attr = "penwidth", values = projections) %>%
      DiagrammeR::set_edge_attrs(edge_attr = "weight", values = projections)
  }

  p <- clean_graph(p)

  return(p)
}
