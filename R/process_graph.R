library(dplyr)
library(data.table)
library(DiagrammeR)

#' @title Produce nodes from event logs
#'
#' @param event_logs Event logs
#' @description `event_logs` should be a `data.frame` or `data.table`, which contains, at least, following columns:
#'
#'  * `event_name`: event name. (`character`)
#'  * `is_target`: whether it's the final stage. (`logical`)
#'
#' `get_nodes_from_event_logs()` will generate the node list from the given `event_logs` for the graph purpose.
#' However, it is very basic, so, in most cases, you might want to provide your own node list to the
#' `create_event_graph()` function.
#' @export
get_nodes_from_event_logs <- function(event_logs) {
  nodes <- event_logs %>%
      select(event_name, is_target) %>%
      distinct() %>%
      rename(name = event_name) %>%
      mutate(name = as.character(name), percentage = 0.5) %>%
      arrange(name)
  return(nodes)
}

#' @title Produce edges from event logs
#' @param event_logs Event logs
#' @description `event_logs` should be a `data.frame` or `data.table`, which contains, at least, following columns:
#'
#'  * `timestamp`: timestamp column which indicates when event happened. (`POSIXct`)
#'  * `customer_id`: cutomer identifier. (`character`)
#'  * `event_name`: event name. (`character`)
#'  * `is_target`: whether it's the final stage. (`logical`)
#' @export
get_edges_from_event_logs <- function(event_logs) {
  # sort by customer_id and timestamp
  event_logs <- data.table(event_logs) %>% arrange(customer_id, timestamp)

  edges <- list()
  temp_edges <- list()
  previous <- NA

  # `apply()` is about up to 10x faster than `for-loop`
  apply(event_logs, 1, function(current) {
    if (!any(is.na(previous))) {
      # pre-process
      if (current["customer_id"] != previous["customer_id"]) {
        # it's a new customer, so set it to 'previous' and continue to the next record
        previous <<- current
        # clear temp_edges
        temp_edges <<- list()
        return
      }

      edge <- list(
        from = previous["event_name"],
        to = current["event_name"],
        customer_id = current["customer_id"]#,
        # duration = current["timestamp"] - previous["timestamp"]
      )
      temp_edges[[length(temp_edges) + 1]] <<- edge
    }

    # post-process
    if (current["is_target"] != FALSE) {
      # put temp_edges to final edges set
      if (length(temp_edges) > 0) {
        for (i in 1:length(temp_edges)) {
          edges[[length(edges) + 1]] <<- temp_edges[[i]]
        }
      }
      # target events should not be the starting point
      previous <<- NA
      # clear temp_edges
      temp_edges <<- list()
    } else {
      previous <<- current
    }
  })

  edges <- rbindlist(edges) %>% 
    group_by(from, to) %>%
    # Add attributes: `value` => count
    summarize(value = n()) %>%
    ungroup() %>%
    mutate_if(is.factor, as.character) %>%
    arrange(from, to)

  return(edges)
}

#' @title Create the event graph by given nodes and edges.
#' @param nodes Event list, which should be a `data.frame` containing following columns:
#'   * `name`: Nodes name, will be used as label. (`character`)
#'   * `is_target`: Whether it's final stage. (`logical`)
#'   * `percentage`: The percentage of customer affected by the given event. (`numeric`)
#
#' @param edges Event transform list, which should be a `data.frame` containing following columns:
#'   * `from`: the begining event of the edge. (`character`)
#'   * `to`: the ending event of the edge (`character`)
#'   * `value`: How many of customer affected by the given event. (`numeric`)
#' @export
create_event_graph <- function(nodes, edges) {
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
      label_col = name
    )

  # print("add_edges_from_table()")
  p <- p %>% add_edges_from_table(
      table = edges,
      from_col = from,
      to_col = to,
      ndf_mapping = name)

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
    set_node_attrs(node_attr = fontcolor, values = "#212121") %>%
    ## lightBlue900(#01579B)
    set_node_attrs(node_attr = color, values = "#01579B") %>%
    ## lightBlue100(#B3E5FC) => lightBlue50(#E1F5FE)
    set_node_attrs(node_attr = fillcolor, values = "#B3E5FC:#E1F5FE") %>%
    set_node_attrs(node_attr = fontsize, values = (nodes$percentage * 15 + 5)) %>%
    set_node_attrs(node_attr = label, values = nodes$name) %>%
    set_node_attrs(node_attr = tooltip, values = nodes$tooltip)
    
  # print("set_edge_attrs() for target nodes")
  target_ids <- nodes[nodes$is_target, "index"]
  p <- p %>%
    ## deepOrange900(#BF360C)
    set_node_attrs(node_attr = color, values = "#BF360C", nodes = target_ids) %>%
    ## deepOrange100(#FFCCBC):deepOrange50(#FBE9E7)
    set_node_attrs(node_attr = fillcolor, values = "#FFCCBC:#FBE9E7", nodes = target_ids) %>%
    set_node_attrs(node_attr = fontsize, values = (nodes$percentage * 10 + 15), nodes = target_ids)

  # print("set_edge_attrs()")
  p <- p %>%
    # Edge attributes
    set_edge_attrs(edge_attr = penwidth, values = log10(edges$value) + 1) %>%
    set_edge_attrs(edge_attr = label, values = edges$value) %>%
    set_edge_attrs(edge_attr = tooltip, values = edges$tooltips) %>%
    set_edge_attrs(edge_attr = labeltooltip, values = edges$tooltips)

  # remove node without edges
  zero_degree_nodes <- p %>% select_nodes_by_degree("deg == 0") %>% get_selection()
  if (!is.na(zero_degree_nodes) && length(zero_degree_nodes) > 0) {
    p <- p %>% select_nodes_by_degree("deg == 0") %>% delete_nodes_ws()
  }

  return(p)
}
