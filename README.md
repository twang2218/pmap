# Process Map Visualizer

The goal of `pmv` is to provide functionality of generating process map from given event logs.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code

edges <- get_edges_from_event_logs(event_logs)
p <- create_event_graph(events, edges)
print(render_graph(p))
```
