# traitfinder

R functions and data for inferring traits of benthic species

Installation
------------

You can install the development version of traitfinder from github using:

``` r
# install.packages("devtools")
devtools::install_github("pmlmodelling/traitfinder", dependencies = TRUE)
```
Example
------------
Currently the package is only able to identify whether benthic species are suspension or deposit feeders. This can be found as follows:

``` r
traitfinder::find_feeding("Acanthocardia tuberculata")
```

