
<!-- README.md is generated from README.Rmd. Please edit that file -->

# eiger

<!-- badges: start -->

[![R-CMD-check](https://github.com/applied-phylo-lab/eiger/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/applied-phylo-lab/eiger/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of eiger is to incorporating eigenvectors of
variance-covariance matrices into phylogenetic analyses. Given a
phylogenetic tree, ‘eiger’ computes and visualizes the contributions of
the branches to the eigenvectors of some version of variance-covariance
matrix of the tree. If trait values are provided, ‘eiger’ can also run
phylogenetic generalized least squares with eigenvectors included as
fixed effects to investigate whether the traits are associated.

## Installation

You can install the development version of eiger from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("applied-phylo-lab/eiger")
```

## Example

This is a basic example which shows you how to compute the contributions
of the branches to the eigenvectors of a variance-covariance matrix of
the phylogenetic tree:

``` r
library(eiger)

set.seed(42)
yule_tree <- TreeSim::sim.bd.taxa(50, 1, 1, 0, 1, complete = FALSE)[[1]]
branch_contributions <- compute_branch_contributions(yule_tree, "vcv", 1:10)
utils::head(branch_contributions)
#>           dim_1         dim_2         dim_3         dim_4         dim_5
#> 1  1.141685e-02 -1.127013e-02 -5.150734e-05 -3.597569e-40 -4.515492e-05
#> 2 -8.862850e-03  1.152640e-01 -9.970039e-02 -2.984063e-37 -5.560201e-03
#> 3  3.261372e-04  3.635843e-03  1.038511e-01 -7.936061e-37 -8.963616e-02
#> 4 -4.771130e-02 -2.099319e-01 -2.418292e-01 -5.916281e-35  3.608013e+00
#> 5 -2.222644e-04 -7.211538e-04 -3.324073e-04 -4.516270e-34 -1.417111e-02
#> 6 -3.657032e-17 -5.885154e-17  1.065220e-16  1.009457e-32 -8.902533e-17
#>           dim_6         dim_7         dim_8         dim_9        dim_10
#> 1 -2.504167e-08 -1.083502e-05 -8.261328e-07 -5.778918e-06 -9.905262e-09
#> 2  2.139920e-07 -7.075109e-04  7.309899e-06 -3.442461e-04 -1.656317e-06
#> 3 -1.035484e-07  1.047791e-03 -1.224681e-05 -1.170989e-02  1.399652e-06
#> 4  3.144584e-06 -1.748662e+00 -9.341354e-04 -1.654548e+00  5.267388e-05
#> 5 -2.333177e-08  7.166371e-02  7.377272e-05  1.635627e-01  1.447558e-06
#> 6  6.150042e-16  2.680683e-16  1.312735e-16 -2.838803e-16  1.576034e-18
```

You can also plot the branch contributions:

``` r
plot_branch_contributions(yule_tree, branch_contributions, "vcv", 3)
```

<img src="man/figures/README-plot_branch_contributions_1-1.png" width="100%" />

Alternatively, you can compute and plot the contributions of the
branches to an eigenvector of a variance-covariance matrix of the
phylogenetic tree in one step:

``` r
plot_branch_contributions(yule_tree, cov_matrix = "dcvcv", dim = 8)
```

<img src="man/figures/README-plot_branch_contributions_2-1.png" width="100%" />

Another common issue to mitigate the confounding in phylogenetic
regressions. Given two quantitative traits and a phylogenetic tree, you
can first get the eigenvectors of the variance-covariance matrix for
convenience of downstream analyses:

``` r
BM <- phytools::fastBM(yule_tree, 1, nsim = 2)
x <- BM[, 1]
y <- BM[, 2]

df <- prepare_eiger(tree = yule_tree, n_eigenvectors = 20, x = x, y = y)
utils::head(df[, 1:10])
#>              X        Y    eigen_1    eigen_2     eigen_3       eigen_4
#> t48  0.5059172 7.960040 -0.1576493 -0.1515420  0.00842073  0.000000e+00
#> t49  2.3404129 6.848499 -0.1447424  0.1734111  0.18673123  8.348522e-17
#> t11  2.4644374 7.064110 -0.1609621 -0.1642439  0.01000946 -9.060398e-19
#> t28  1.0119205 6.579663 -0.1344856  0.1275235  0.08691925 -1.780920e-17
#> t18  1.2161309 3.357931 -0.1350099  0.1254461  0.06078392 -3.004505e-16
#> t34 -2.1840856 3.065954 -0.1362000  0.1246287 -0.29848002  1.012814e-16
#>          eigen_5       eigen_6       eigen_7      eigen_8
#> t48 -0.006638794 -1.364881e-01  0.0002481392 -0.188593425
#> t49 -0.205940030 -6.340932e-05 -0.2114313538 -0.006001149
#> t11 -0.009714741 -2.240949e-01 -0.0114187533  0.347526123
#> t28  0.000502537  1.154914e-05  0.1190882974  0.001914110
#> t18  0.353855732  6.495563e-04 -0.1132047733  0.002073595
#> t34 -0.052243903  1.433370e-04 -0.0171612601  0.001118717
```

Then you can run eiger regression (phylolm with eigenvectors of the
variance-covariance matrix as fixed effects):

``` r
run_eiger(data = df, tree = yule_tree, n_eigenvectors = 20, x = x, y = y, prepared = TRUE)
#> Call:
#> phylolm::phylolm(formula = reg_formula, data = df, phy = tree)
#> 
#>    AIC logLik 
#> 122.57 -38.28 
#> 
#> Parameter estimate(s) using ML:
#> sigma2: 0.3873585 
#> 
#> Coefficients:
#>  (Intercept)            X      eigen_1      eigen_2      eigen_3      eigen_4 
#>  -70.7508706    0.1097984 -516.1221221   28.6089632   -6.9693645  133.9414754 
#>      eigen_5      eigen_6      eigen_7      eigen_8      eigen_9     eigen_10 
#>    5.9160672   -2.2098423    4.0636454   -2.0317750    3.2329012    1.3908234 
#>     eigen_11     eigen_12     eigen_13     eigen_14     eigen_15     eigen_16 
#>   -6.2229469  -10.0293783   -3.7525465    0.4010527    3.0584358    0.1024626 
#>     eigen_17     eigen_18     eigen_19     eigen_20 
#>    0.2997828    1.9634039    1.5420587    0.7518774
```

Alternatively, you can compute the eigenvectors and run eiger regression
in one step:

``` r
run_eiger(tree = yule_tree, n_eigenvectors = 30, x = x, y = y)
#> Call:
#> phylolm::phylolm(formula = reg_formula, data = df, phy = tree)
#> 
#>    AIC logLik 
#> 131.81 -32.91 
#> 
#> Parameter estimate(s) using ML:
#> sigma2: 0.3123935 
#> 
#> Coefficients:
#>  (Intercept)            X      eigen_1      eigen_2      eigen_3      eigen_4 
#>  41.60120742   0.13076109 243.86041859 -23.82713774   4.51857322 -90.71404236 
#>      eigen_5      eigen_6      eigen_7      eigen_8      eigen_9     eigen_10 
#>  -7.99290734  -1.94580464  -3.60781165  -0.85319660  -2.35654129   1.61123302 
#>     eigen_11     eigen_12     eigen_13     eigen_14     eigen_15     eigen_16 
#>  -0.36518067  -0.55599456   2.84144694   0.44044312   1.71120187   0.13306831 
#>     eigen_17     eigen_18     eigen_19     eigen_20     eigen_21     eigen_22 
#>  -0.70493596   1.80444593   1.78527914   0.06556509  -0.95606472  -0.43365771 
#>     eigen_23     eigen_24     eigen_25     eigen_26     eigen_27     eigen_28 
#>   0.05399031   0.20354970  -0.10939309   1.43038338   0.93074007  -0.39278312 
#>     eigen_29     eigen_30 
#>  -0.37959140  -0.11687517
```
