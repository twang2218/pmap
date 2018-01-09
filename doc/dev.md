# Prepare dev environment

```R
install.packages("devtools")
install.packages("roxygen2")
install.packages("usethis")

library(devtools)
library(roxygen2)
library(usethis)

use_testthat()
use_mit_license("Tao Wang <twang2218@gmail.com>")

# use_roxygen_md()
use_readme_md()

use_travis()
use_appveyor()
use_coverage()

usethis::use_cran_comments()
usethis::use_news_md()

document()
```

## Export SVG

Prerequest

```bash
brew install v8@3.15 librsvg

```

```R
install.packages("DiagrammeRsvg")
install.packages("rsvg")

```

Export

```R
library(dplyr)
library(DiagrammeR)

p %>% render_pmap() %>% export_graph(file_name = "mygraph.svg", file_type = "svg")
```