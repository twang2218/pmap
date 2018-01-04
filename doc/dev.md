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

document()
```
