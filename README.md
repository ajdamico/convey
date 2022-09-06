<!-- badges: start -->
[![R-CMD-check](https://github.com/ajdamico/convey/workflows/R-CMD-check/badge.svg)](https://github.com/ajdamico/convey/actions)
<!-- badges: end -->

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/convey)](https://cran.r-project.org/package=convey) 

[![CRAN monthly downloads](http://cranlogs.r-pkg.org/badges/convey "CRAN monthly downloads")](https://cran.r-project.org/package=convey)


# Poverty and Inequality with Complex Survey Data

The R `convey` library estimates measures of poverty, inequality, and wellbeing.  There are two other R libraries covering this subject, [vardpoor](https://CRAN.R-project.org/package=vardpoor)  and [laeken](https://CRAN.R-project.org/package=laeken), however, only `convey` integrates seamlessly with the [R survey package](https://CRAN.R-project.org/package=survey).

`convey` is free and open-source software that runs inside the [R environment for statistical computing](https://www.r-project.org/).  Anyone can review and propose changes to [the source code](https://github.com/DjalmaPessoa/convey) for this software.  

## Installation {#install}

In order to work with the `convey` library, you will need to have R running on your machine.  If you have never used R before, you will need to [install that software](https://www.r-project.org/) before `convey` can be accessed.  Check out [FlowingData](http://flowingdata.com/2012/06/04/resources-for-getting-started-with-r/) for a concise list of resources for new R users.  Once you have R loaded on your machine, you can install..

* the latest released version from [CRAN](https://CRAN.R-project.org/package=convey) with

```R
install.packages("convey")
````

* the latest development version from github with

```R
remotes::install_github("djalmapessoa/convey")
```

In order to know how to cite this package, run `citation("convey")`.

## __Vignettes__: Complex surveys and statistical inference 

Please consult the package vignette, where we demonstrate how to measure poverty and income concentration in a population based on microdata collected from a complex survey sample.  

Most surveys administered by government agencies or larger research organizations utilize a sampling design that violates the assumption of simple random sampling (SRS), including:

1. Different units selection probabilities;
2. Clustering of units;
3. Stratification of clusters;
4. Reweighting to compensate for missing values and other adjustments.


