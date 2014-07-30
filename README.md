# Enhancing the National Climate Database with clifro

This package is designed to minimise the hassle in downloading data
from New Zealand's National Climate Database via 
[CliFlo](http://cliflo.niwa.co.nz/), and reading it into **R**. It allows the 
user to login, select the data and download it efficiently into an **R** 
session, available for plotting, analysis and/or export. The `clifro` package 
requires the user to have a current [CliFlo](http://cliflo.niwa.co.nz/) account, 
available from http://cliflo.niwa.co.nz/pls/niwp/wsubform.intro.

To install `clifro` into R: 

```R
install.packages("devtools")
library(devtools)
install_github(username = "blasee", repo = "clifro", build_vignettes = FALSE)
```

To get started with `clifro`:

```R
library(clifro)
vignette("clifro")
demo(clifro)
```