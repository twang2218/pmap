# Process Map Visualizer [![Build Status]][Travis] [![CRAN Version]][CRAN Link]  [![Coverage status]][Coverage status link] [![Download Stats]][CRAN Link]

[Build Status]: https://travis-ci.org/twang2218/pmap.svg?branch=master
[Travis]: https://travis-ci.org/twang2218/pmap

[CRAN Version]: http://www.r-pkg.org/badges/version/pmap
[CRAN Link]: https://cran.r-project.org/web/packages/pmap/index.html

[Coverage status]: https://coveralls.io/repos/github/twang2218/pmap/badge.svg?branch=master
[Coverage status link]: https://coveralls.io/github/twang2218/pmap?branch=master

[Download Stats]: https://cranlogs.r-pkg.org/badges/grand-total/pmap?color=brightgreen


The goal of `pmap` is to provide functionality of generating process map from given event logs.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
library(pmap)

edges <- get_edges_from_event_logs(event_logs)
p <- create_event_graph(events, edges)
print(render_graph(p))
```
