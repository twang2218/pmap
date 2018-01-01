library(dplyr)
library(data.table)
# install.packages("DiagrammeR")
library(DiagrammeR)

# event_logs <- (
#   timestamp,
#   customer_id,
#   event_name,
#   is_target
# )
# nodes <- (
#   event_name,
#   is_target,
#   ...
# )
generate_graph_data <- function(event_logs, nodes = NULL) {
  # sort by customer_id and timestamp
  event_logs <- data.table(event_logs) %>% arrange(customer_id, timestamp)

  graph_data <- list(
    nodes = data.table(),
    links = data.table()
  )

  # Generates nodes if it's not given
  if (any(is.null(nodes))) {
    graph_data$nodes <- event_logs %>%
      select(event_name, is_target) %>%
      distinct() %>%
      mutate(event_name = as.character(event_name)) %>%
      arrange(event_name)
  }


  links <- list()
  sales_links <- list()
  previous <- NA

  # Use `apply()` instead of `for-loop` can speed up to 10x
  apply(event_logs, 1, function(current) {
    if (!any(is.na(previous))) {
      # pre-process
      if (current["customer_id"] != previous["customer_id"]) {
        # it's a new customer, so set it to 'previous' and continue to the next record
        previous <<- current
        # clear sales_links
        sales_links <<- list()
        return
      }

      event_row <- list(
        from = previous["event_name"],
        to = current["event_name"],
        customer_id = current["customer_id"],
        duration = as.integer(current["timestamp"]) - as.integer(previous["timestamp"])
      )
      sales_links[[length(sales_links)+1]] <<- event_row
      # sales_links <- rbind(sales_links, event_row)
    }

    # post-process
    if (current["is_target"] != FALSE) {
      # put sales_links to final links set
      links <<- c(links, sales_links)
      # target events should not be the starting point
      previous <<- NA
      # clear sales_links
      sales_links <<- list()
    } else {
      previous <<- current
    }
  })


  graph_data$links <- rbindlist(links) %>% 
    group_by(from, to) %>%
    summarize(count = n(), mean_duration = mean(duration)) %>%
    ungroup() %>%
    mutate_if(is.factor, as.character) %>%
    arrange(from, to)

  return(graph_data)
}

# nodes (name, is_target)
# links (from, to, value)
create_process_graph <- function(nodes, links, node_label_col = NULL, edge_label_col = NULL) {
  print("Converting factor to character [nodes]...")
  nodes <- nodes %>%
    mutate_if(is.factor, as.character) %>%
    mutate(
      index = 1:nrow(nodes),
      label = apply(
        sapply(
          colnames(nodes),
          function(colname) {
            paste0(colname, ": ", nodes[[colname]])
          }
        ),
        1,
        function(row) {
          paste(row, collapse = "\n")
        }
      )
    )

  print("Converting factor to character [links]...")
  links <- links %>% mutate_if(is.factor, as.character)

  print("create_graph()")
  p <- create_graph()

  print("add_nodes_from_table()")
  p <- p %>%
    add_nodes_from_table(
      table = nodes,
      label_col = event_name
    )

  print("add_edges_from_table()")
  p <- p %>% add_edges_from_table(
      table = links,
      from_col = from,
      to_col = to,
      ndf_mapping = event_name)

  print("add_*_attr()")
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
    add_global_graph_attrs(attr_type = "edge", attr = "fontcolor", value = "#212121") %>%
    # node attributes
    ## grey900(#212121)
    set_node_attrs(node_attr = fontcolor, values = "#212121") %>%
    ## lightBlue900(#01579B)
    set_node_attrs(node_attr = color, values = "#01579B") %>%
    ## lightBlue100(#B3E5FC) => lightBlue50(#E1F5FE)
    set_node_attrs(node_attr = fillcolor, values = "#B3E5FC:#E1F5FE") %>%
    set_node_attrs(node_attr = fontsize, values = "15") %>%
    set_node_attrs(node_attr = label, values = nodes$event_name) %>%
    set_node_attrs(node_attr = tooltip, values = nodes$label)
    
  # Set target node special style
  target_ids <- nodes[nodes$is_target, "index"]
  p <- p %>%
    ## deepOrange900(#BF360C)
    set_node_attrs(node_attr = color, value = "#BF360C", nodes = target_ids) %>%
    ## deepOrange100(#FFCCBC):deepOrange50(#FBE9E7)
    set_node_attrs(node_attr = fillcolor, value = "#FFCCBC:#FBE9E7", nodes = target_ids) %>%
    set_node_attrs(node_attr = fontsize, value = "17", nodes = target_ids)

  weights <- links$count
  p <- p %>%
    # Edge attributes
    set_edge_attrs(edge_attr = penwidth, values = log10(weights) + 1) %>%
    set_edge_attrs(edge_attr = label, values = weights) %>%
    set_edge_attrs(edge_attr = tooltip, values = weights) %>%
    set_edge_attrs(edge_attr = labeltooltip, values = weights)

  print("render_graph()")
  p <- p %>% render_graph(output = "graph")

  return(p)
}
