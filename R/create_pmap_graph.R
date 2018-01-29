#' @title Create the event graph by given nodes and edges.
#' @description Create the process map graph by specify the nodes and edges
#' @usage create_pmap_graph(
#'    nodes,
#'    edges,
#'    target_categories = NULL,
#'    edge_label = c(
#'      "amount",
#'      "mean_duration",
#'      "median_duration",
#'      "max_duration",
#'      "min_duration"
#'    )
#'  )
#' @param nodes Event list, it should be a `data.frame` containing following columns:
#'   * `name`: Event name, will be used as label. (`character`)
#'   * `category`: The event category (`character`)
#' @param edges Event transform list, it should be a `data.frame` containing following columns:
#'   * `from`: the beginning event of the edge. (`character`)
#'   * `to`: the ending event of the edge (`character`)
#'   * `amount`: How many of customer affected by the given event. (`numeric`)
#' @param target_categories A vector contains the target event categories
#' @param edge_label Specify which attribute is used for the edge label.
#' @examples
#' eventlog <- generate_eventlog()
#' nodes <- generate_nodes(eventlog)
#' head(nodes)
#' #  # A tibble: 6 x 3
#' #    name              category   amount
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
#' p <- create_pmap_graph(nodes, edges, target_categories = c("target"))
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
#' @importFrom DiagrammeR   create_node_df
#' @importFrom DiagrammeR   create_edge_df
#' @importFrom DiagrammeR   add_global_graph_attrs
#' @export
create_pmap_graph <- function(
  nodes,
  edges,
  target_categories = NULL,
  edge_label = c(
    "amount",
    "mean_duration",
    "median_duration",
    "max_duration",
    "min_duration"
  )
) {
  # make 'R CMD Check' happy
  amount <- from <- to <- id <- name <- NULL

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

  # Set all 'NA' from joins to zero
  nodes$inbound[is.na(nodes$inbound)] <- 0
  nodes$outbound[is.na(nodes$outbound)] <- 0
  if (is.null(nodes$amount)) {
    # deal with amount column missing case
    nodes$amount <- 0
  } else {
    nodes$amount[is.na(nodes$amount)] <- 0
  }

  nodes <- nodes %>% dplyr::mutate(id = 1:nrow(nodes))
  # print(str(nodes))

  nodes_id <- dplyr::select(nodes, id, name)
  edges <- edges %>%
    dplyr::mutate(
      tooltip = get_attrs_desc(edges),
      size = projection(edges$amount, 1, 15)
    ) %>%
    dplyr::left_join(nodes_id, by = c("from" = "name")) %>%
    dplyr::rename(from_id = id) %>%
    dplyr::left_join(nodes_id, by = c("to" = "name")) %>%
    dplyr::rename(to_id = id)

  edges_cols <- names(edges)

  # Fill missing duration columns with empty string
  if (!"max_duration" %in% edges_cols) edges <- dplyr::mutate(edges, max_duration = "")
  if (!"mean_duration" %in% edges_cols) edges <- dplyr::mutate(edges, mean_duration = "")
  if (!"median_duration" %in% edges_cols) edges <- dplyr::mutate(edges, median_duration = "")
  if (!"min_duration" %in% edges_cols) edges <- dplyr::mutate(edges, min_duration = "")

  # print(str(edges))

  nodes_df <- DiagrammeR::create_node_df(
    nrow(nodes),
    type = nodes$category,
    label = nodes$name,
    name = nodes$name,
    tooltip = get_attrs_desc(nodes),
    fontcolor = "#212121",
    color = "#01579B",
    fillcolor = "#B3E5FC:#E1F5FE",
    fontsize = 16,
    margin = 0.2,
    inbound = nodes$inbound,
    outbound = nodes$outbound,
    amount = nodes$amount
  )

  edge_label <- match.arg(edge_label)
  edge_label_value <- switch(
    edge_label,
    amount = edges$amount,
    max_duration = edges$max_duration,
    mean_duration = edges$mean_duration,
    median_duration = edges$median_duration,
    min_duration = edges$min_duration
  )

  edges_df <- DiagrammeR::create_edge_df(
    nrow(edges),
    from = edges$from_id,
    to = edges$to_id,
    amount = edges$amount,
    label = paste("  ", edge_label_value, "  "),
    penwidth = edges$size,
    weight = edges$size,
    tooltip = edges$tooltip,
    labeltooltip = edges$tooltip,
    max_duration = edges$max_duration,
    mean_duration = edges$mean_duration,
    median_duration = edges$median_duration,
    min_duration = edges$min_duration
  )

  # print("create_graph()")
  p <- DiagrammeR::create_graph(nodes_df, edges_df)

  # print("add_global_graph_attrs()")
  p <- p %>%
    # graph [ layout = "dot" ]
    DiagrammeR::add_global_graph_attrs(attr_type = "graph", attr = "layout", value = "dot") %>%
    # node [...]
    DiagrammeR::add_global_graph_attrs(attr_type = "node", attr = "fixedsize", value = "false") %>%
    DiagrammeR::add_global_graph_attrs(attr_type = "node", attr = "shape", value = "box") %>%
    DiagrammeR::add_global_graph_attrs(attr_type = "node", attr = "style", value = "filled,rounded") %>%
    DiagrammeR::add_global_graph_attrs(attr_type = "node", attr = "gradientangle", value = "90") %>%
    DiagrammeR::add_global_graph_attrs(attr_type = "node", attr = "margin", value = "0.2") %>%
    # edge [...]
    ## grey900(#212121)
    DiagrammeR::add_global_graph_attrs(attr_type = "edge", attr = "color", value = "#000000A0") %>%
    ## grey900(#212121)
    DiagrammeR::add_global_graph_attrs(attr_type = "edge", attr = "fontcolor", value = "#212121")

  p <- adjust_node_style(p)

  p <- clean_graph(p)

  return(p)
}
