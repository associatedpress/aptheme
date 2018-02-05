This package provides visual themes for R in AP style.

Right now only a `ggplot2` theme is available.

Installation:

``` r
devtools::install_github("associatedpress/aptheme")
```

Usage:

``` r
library(aptheme)

# ggplot2
plot + theme_ap()
```

To bring legends back:

``` r
plot + theme_ap() + theme(legend.position = 'bottom')
```
