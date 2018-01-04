# Process Map Visualizer [![Build Status]][Travis] [![CRAN Version]][CRAN Link]  [![Coverage status]][Coverage status link] [![Download Stats]][CRAN Link]

[Build Status]: https://travis-ci.org/twang2218/pmv.svg?branch=master
[Travis]: https://travis-ci.org/twang2218/pmv

[CRAN Version]: http://www.r-pkg.org/badges/version/pmv
[CRAN Link]: https://cran.r-project.org/web/packages/pmv/index.html

[Coverage status]: https://codecov.io/gh/twang2218/pmv/branch/master/graph/badge.svg
[Coverage status link]: https://codecov.io/github/twang2218/pmv?branch=master

[Download Stats]: https://cranlogs.r-pkg.org/badges/grand-total/DiagrammeR?color=brightgreen

The goal of `pmv` is to provide functionality of generating process map from given event logs.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code

edges <- get_edges_from_event_logs(event_logs)
p <- create_event_graph(events, edges)
print(render_graph(p))
```
