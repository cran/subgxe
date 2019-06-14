## Introduction
Classical methods for combining summary data from genome-wide association studies (GWAS) only use marginal genetic effects and power can be compromised in the presence of heterogeneity.

`subgxe` is an R package that implements p-value Assisted Subset Testing for Association, a method that generalizes the previously proposed association analysis based on subsets (ASSET) method by incorporating gene-environment (G-E) interactions into the testing procedure.

## Installation
```{R}
library(devtools)
install_github("umich-cphds/subgxe", build_opt = c("--no-resave-data", "--no-manual"))
```

## Example
load up R and type
```{R}
library(subgxe)
vignette("subgxe")
```
for a tutorial.
