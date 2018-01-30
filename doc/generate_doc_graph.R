library(eventdataR)
library(dplyr)
library(pmap)

eventlog <- eventdataR::sepsis %>%
  rename(
    timestamp = Complete_Timestamp,
    customer_id = Case_ID,
    event_name = Activity
  ) %>%
  mutate(
    event_type = event_name
  ) %>%
  select(timestamp, customer_id, event_name, event_type) %>%
  na.omit()

p <- create_pmap(eventlog)
render_pmap(p)
render_pmap_file(p, "man/figures/example.prune_edges.none.svg", use_external_dot = TRUE)


p %>% prune_edges(0.5) %>%
  render_pmap_file("man/figures/example.prune_edges.edges.svg", use_external_dot = TRUE)

p %>% prune_nodes(0.5) %>% prune_edges(0.5) %>%
  render_pmap_file("man/figures/example.prune_edges.both.svg", use_external_dot = TRUE)

p <- create_pmap(eventlog, distinct_repeated_events = TRUE)
p %>% prune_nodes(0.5) %>% prune_edges(0.93) %>% render_pmap()
p %>% prune_nodes(0.5) %>% prune_edges(0.93) %>%
  render_pmap_file("man/figures/example.distinct_repeated_events.svg", use_external_dot = TRUE)

# create_pmap()

eventlog <- data.frame(
  timestamp = c(
    as.POSIXct("2017-10-01"),
    as.POSIXct("2017-10-02"),
    as.POSIXct("2017-10-03"),
    as.POSIXct("2017-10-04"),
    as.POSIXct("2017-10-05"),
    as.POSIXct("2017-10-06"),
    as.POSIXct("2017-10-07"),
    as.POSIXct("2017-10-08"),
    as.POSIXct("2017-10-09"),
    as.POSIXct("2017-10-10")
  ),
  customer_id = c("c1", "c1", "c1", "c1", "c1", "c1", "c1", "c1", "c1", "c1"),
  event_name =  c("a",  "b",  "d",  "a",  "c",  "a",  "b",  "c",  "a",  "d"),
  event_category =  c("campaign", "campaign", "sale", "campaign", "sale", "campaign", "campaign", "sale", "campaign", "sale"),
  stringsAsFactors = FALSE
)

create_pmap(eventlog, target_categories = c("sale")) %>%
  render_pmap_file("man/figures/example.create_pmap.simple.svg", use_external_dot = TRUE)


eventlog <- generate_eventlog(
  size_of_eventlog = 10000,
  number_of_customers = 2000,
  event_categories = c("campaign", "sale"),
  event_categories_size = c(8, 2))

create_pmap(eventlog, target_categories = c("sale")) %>%
  render_pmap_file("man/figures/example.create_pmap.complex.svg", use_external_dot = TRUE)
