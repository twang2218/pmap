# Process Map [![Build Status]][Travis] [![AppVeyor Build Status]][AppVeyor Link] [![Coverage status]][Coverage status link] [![CRAN Version]][CRAN Link] [![Download Stats]][CRAN Link]

[Build Status]: https://travis-ci.org/twang2218/pmap.svg?branch=master
[Travis]: https://travis-ci.org/twang2218/pmap

[CRAN Version]: http://www.r-pkg.org/badges/version/pmap
[CRAN Link]: https://cran.r-project.org/web/packages/pmap/index.html

[Coverage status]: https://coveralls.io/repos/github/twang2218/pmap/badge.svg?branch=master
[Coverage status link]: https://coveralls.io/github/twang2218/pmap?branch=master

[Download Stats]: https://cranlogs.r-pkg.org/badges/grand-total/pmap?color=brightgreen

[AppVeyor Build Status]: https://ci.appveyor.com/api/projects/status/github/twang2218/pmap?branch=master&svg=true
[AppVeyor Link]: https://ci.appveyor.com/project/twang2218/pmap

The goal of `pmap` is to provide functionality of generating process map from given event logs.

## Usage

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
library(pmap)
library(dplyr)

# Generate simulated eventlog
> eventlog <- generate_eventlog(
     size_of_eventlog = 10000,
     number_of_customers = 2000,
     event_catalogs = c("campaign", "sale"),
     event_catalogs_size = c(10, 4))

# Check eventlog data frame structure
> head(eventlog)
            timestamp   customer_id         event_name event_type
1 2017-01-01 00:08:35 Customer 1072 Event 6 (campaign)   campaign
2 2017-01-01 00:14:42 Customer 1979 Event 1 (campaign)   campaign
3 2017-01-01 00:55:58   Customer 32 Event 3 (campaign)   campaign
4 2017-01-01 01:04:01 Customer 1877 Event 8 (campaign)   campaign
5 2017-01-01 01:47:37  Customer 833 Event 7 (campaign)   campaign
6 2017-01-01 03:34:39 Customer 1119 Event 4 (campaign)   campaign
> str(eventlog)
'data.frame':   10000 obs. of  4 variables:
 $ timestamp  : POSIXct, format: "2017-01-01 00:08:35" "2017-01-01 00:14:42" ...
 $ customer_id: chr  "Customer 1072" "Customer 1979" "Customer 32" "Customer 1877" ...
 $ event_name : chr  "Event 6 (campaign)" "Event 1 (campaign)" "Event 3 (campaign)" "Event 8 (campaign)" ...
 $ event_type : chr  "campaign" "campaign" "campaign" "campaign" ...

# Create process map
> p <- create_pmap(eventlog, target_types = c("sale"))
# Render the process map
> print(render_pmap(p))
```

The result will be a bit messy.

![process map without prune](man/figures/example.prune_edges.none.svg)

Let's prune the process map.

```R
# Prune the process map
> p <- p %>% prune_edges(0.5) %>% prune_nodes(0.5)
# Render the pruned process map
> print(render_pmap(p))
```

![cleaner process map](man/figures/example.prune_edges.both.svg)
