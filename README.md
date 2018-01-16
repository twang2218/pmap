# Process Map

[![Build Status]][Travis] [![AppVeyor Build Status]][AppVeyor Link] [![Coverage status]][Coverage status link] [![CRAN Version]][CRAN Link] [![Download Stats]][CRAN Link]

[Build Status]: https://travis-ci.org/twang2218/pmap.svg?branch=master
[Travis]: https://travis-ci.org/twang2218/pmap

[CRAN Version]: http://www.r-pkg.org/badges/version/pmap
[CRAN Link]: https://cran.r-project.org/web/packages/pmap/index.html

[Coverage status]: https://coveralls.io/repos/github/twang2218/pmap/badge.svg?branch=master
[Coverage status link]: https://coveralls.io/github/twang2218/pmap?branch=master

[Download Stats]: https://cranlogs.r-pkg.org/badges/grand-total/pmap?color=brightgreen

[AppVeyor Build Status]: https://ci.appveyor.com/api/projects/status/github/twang2218/pmap?branch=master&svg=true
[AppVeyor Link]: https://ci.appveyor.com/project/twang2218/pmap

The goal of `pmap` is to provide functionality of generating a process map from an event log.

## Installation

An older version of `pmap` is available on [CRAN](https://cran.r-project.org/web/packages/pmap/index.html), if you want to install this version, you can do it by:

```R
install.packages("pmap")
```

However, as [CRAN policy](https://cran.r-project.org/web/packages/policies.html#Submission) states, I shouldn't submit package to CRAN more than once a month, so I will do normal [release on GitHub](https://github.com/twang2218/pmap/releases), and submit to CRAN only when it's possible. That is, the version on CRAN might be a little bit old.

To install the latest version, you can install `pmap` from GitHub directly:

```R
devtools::install_github("twang2218/pmap")
```

And, as I [tagged](https://github.com/twang2218/pmap/tags) each released version, you can even specify which version you want to install:

```R
devtools::install_github("twang2218/pmap", ref = "v0.4.0")
```

## Usage

This is a basic example which shows you how to use `pmap` create a process map from an event log. We use `sepsis` dataset in `eventdataR` package as the example here.

``` r
library(eventdataR)
library(dplyr)
library(pmap)

# Prepare the event log data frame
> eventlog <- eventdataR::sepsis %>%
    rename(
      timestamp = Complete_Timestamp,
      customer_id = Case_ID,
      event_name = Activity
    ) %>%
    mutate(
      event_type = event_name
    ) %>%
    select(timestamp, customer_id, event_name, event_type) %>%
    filter(!is.na(customer_id))
```

Check `eventlog` data frame structure.

```R
> head(eventlog)
# A tibble: 6 x 4
  timestamp           customer_id event_name       event_type      
  <dttm>              <chr>       <chr>            <chr>           
1 2014-10-22 11:15:41 A           ER Registration  ER Registration 
2 2014-10-22 11:27:00 A           Leucocytes       Leucocytes      
3 2014-10-22 11:27:00 A           CRP              CRP             
4 2014-10-22 11:27:00 A           LacticAcid       LacticAcid      
5 2014-10-22 11:33:37 A           ER Triage        ER Triage       
6 2014-10-22 11:34:00 A           ER Sepsis Triage ER Sepsis Triage
> str(eventlog)
Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	15190 obs. of  4 variables:
 $ timestamp  : POSIXct, format: "2014-10-22 11:15:41" "2014-10-22 11:27:00" "2014-10-22 11:27:00" ...
 $ customer_id: chr  "A" "A" "A" "A" ...
 $ event_name : chr  "ER Registration" "Leucocytes" "CRP" "LacticAcid" ...
 $ event_type : chr  "ER Registration" "Leucocytes" "CRP" "LacticAcid" ...
```

Create process map from the `eventlog`.

```R
# Create process map
> p <- create_pmap(eventlog)
# Render the process map
> print(render_pmap(p))
```

The result will be a bit messy.

<p align="center"><img src="man/figures/example.prune_edges.none.svg" alt="process map without prune" height="500px" /></p>

Let's prune the process map.

```R
# Prune the process map
> p <- p %>% prune_nodes(0.5) %>% prune_edges(0.5)
# Render the pruned process map
> print(render_pmap(p))
```

<p align="center"><img src="man/figures/example.prune_edges.both.svg" alt="cleaner process map" height="500px" /></p>