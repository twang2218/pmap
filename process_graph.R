library(dplyr)
library(data.table)
# install.packages("DiagrammeR")
library(DiagrammeR)

generate_graph_data <- function(event_logs, is_target = "is_target", timestamp = "timestamp", customer_id = "customer_id", event_id = "event_id", event_name = "event_name") {

  event_logs <- data.table(event_logs) %>% arrange(customer_id, timestamp)

  graph_data <- list(
    nodes = event_logs %>%
      select(event_id, event_name, is_target) %>%
      distinct() %>%
      mutate(event_name = as.character(event_name)) %>%
      arrange(event_id),
    links = data.table()
  )

  links = list()
  sales_links <- list()
  previous <- NA

  apply(event_logs, 1, function(current) {
    # current[[event_id]] <- current[[event_id]]
    # current[[customer_id]] <- current[[customer_id]]
    # Add event to nodes list if it's not exist

    if (!any(is.na(previous))) {
      # pre-process
      if (current[[customer_id]] != previous[[customer_id]]) {
        # it's a new customer, so set it to 'previous' and continue to the next record
        previous <<- current
        # clear sales_links
        sales_links <<- list()
        return
      }

      event_row <- list(
        # source_id = graph_data$nodes[graph_data$nodes$event_original_id == previous[[event_id]],]$event_id,
        source_id = previous[[event_id]],
        source_name = previous[[event_name]],
        # target_id = graph_data$nodes[graph_data$nodes$event_original_id == current[[event_id]],]$event_id,
        target_id = current[[event_id]],
        target_name = current[[event_name]],
        customer_id = current[[customer_id]],
        duration = as.integer(current[[timestamp]]) - as.integer(previous[[timestamp]])
      )
      sales_links[[length(sales_links)+1]] <<- event_row
      # sales_links <- rbind(sales_links, event_row)
    }

    # post-process
    # print("current: ")
    # print(str(current))
    # print(current)
    # print(is_target)
    # print(current[[is_target]])
    if (current[[is_target]] != FALSE) {
      # put sales_links to final links set
      # print("Appending sales_links:")
      # print(sales_links)
      links <<- c(links, sales_links)
      # graph_data$links <- rbind(graph_data$links, sales_links)
      # target events should not be the starting point
      previous <<- NA
      # clear sales_links
      sales_links <<- list()
    } else {
      previous <<- current
    }
  })


  graph_data$links <- rbindlist(links) %>% 
    group_by(source_id, source_name, target_id, target_name) %>%
    summarize(count = n(), mean_duration = mean(duration)) %>%
    ungroup() %>%
    mutate_if(is.factor, as.character) %>%
    arrange(source_id, target_id)

  return(graph_data)
}

# nodes (name, is_target)
# links (from, to, value)
create_process_graph <- function(nodes, links, node_label_col = NULL, edge_label_col = NULL) {
  ###### DiagrammeR ########

  print("Converting factor to character [nodes]...")
  nodes <- nodes %>% mutate_if(is.factor, as.character)
  # print(nodes)

  print("Converting factor to character [links]...")
  links <- links %>% mutate_if(is.factor, as.character)
  # print(links)

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
      from_col = source_name,
      to_col = target_name,
      ndf_mapping = event_name)

  print("add_*_attr()")
  weights <- links$count
  p <- p %>% 
    add_global_graph_attrs(attr_type = "graph", attr = "layout", value = "dot") %>%
    add_global_graph_attrs(attr_type = "node", attr = "fixedsize", value = "false") %>%
    add_global_graph_attrs(attr_type = "node", attr = "shape", value = "box") %>%
    add_global_graph_attrs(attr_type = "node", attr = "style", value = "filled") %>%
    # add_global_graph_attrs(attr_type = "node", attr = "color", value = "#b2aea3") %>%
    # add_global_graph_attrs(attr_type = "node", attr = "fillcolor", value = "#edeceb") %>%
    # set_node_attrs(node_attr = shape, value = "box") %>%
    # set_node_attrs(node_attr = style, value = "filled") %>%
    # set_node_attrs(node_attr = fontname, value = "Helvetica") %>%
    # set_node_attrs(node_attr = fontsize, value = "10") %>%
    # set_node_attrs(node_attr = fixedsize, value = "false") %>%
    set_node_attrs(node_attr = color, value = "#b2aea3") %>%
    set_node_attrs(node_attr = fillcolor, value = "#edeceb") %>%
    set_node_attrs(node_attr = fontsize, value = "13")

# if (!is.null(target_nodes)) {
#   nodes$index <- rownames(nodes)
#   target_ids <- nodes[nodes$event_name %in% target_nodes, "index"]
#   p <- p %>%
#     set_node_attrs(node_attr = color, value = "#b21d00", nodes = target_ids) %>%
#     set_node_attrs(node_attr = fillcolor, value = "#edd9d5", nodes = target_ids) %>%
#     set_node_attrs(node_attr = fontsize, value = "15", nodes = target_ids)
# }

  # color="#b2aea3" fillcolor="#edeceb"
  # N23 -> N49 [label=" 0.38s" weight=21 penwidth=2 color="#b24200" tooltip="net.(*conn).Write /usr/local/go/src/net/net.go ... internal/poll.(*FD).Write /usr/local/go/src/internal/poll/fd_unix.go (0.38s)" labeltooltip="net.(*conn).Write /usr/local/go/src/net/net.go ... internal/poll.(*FD).Write /usr/local/go/src/internal/poll/fd_unix.go (0.38s)" style="dotted"]
  p <- p %>%
    set_edge_attrs(edge_attr = color, values = "#b29d81") %>%
    # set_edge_attrs(edge_attr = weight, values = weights * 10, from = links$source_name, to = links$target_name) %>%
    set_edge_attrs(edge_attr = penwidth, values = log2(weights), from = links$source_name, to = links$target_name) %>%
    set_edge_attrs(edge_attr = label, values = weights, from = links$source_name, to = links$target_name) %>%
    set_edge_attrs(edge_attr = tooltip, values = weights, from = links$source_name, to = links$target_name) %>%
    set_edge_attrs(edge_attr = labeltooltip, values = weights, from = links$source_name, to = links$target_name)

  print("render_graph()")
  # gp <- p %>% render_graph(layout = "tree")
  # gp <- p %>% render_graph(output = "visNetwork")
  gp <- p %>% render_graph(output = "graph")

  return(gp)
}
