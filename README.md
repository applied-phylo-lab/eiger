
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
#> 1  5.997563e-01 -5.920484e-01 -2.705811e-03 -1.390093e-37 -2.372103e-03
#> 2 -4.655880e-01  6.055110e+00 -5.237515e+00 -2.421425e-35 -2.920915e-01
#> 3  1.713282e-02  1.910001e-01  5.455564e+00 -4.368296e-35 -4.708815e+00
#> 4 -2.506396e+00 -1.102826e+01 -1.270390e+01  1.747282e-33  1.895381e+02
#> 5 -1.167611e-02 -3.788404e-02 -1.746220e-02 -5.472827e-32 -7.444445e-01
#> 6 -1.986950e-15 -2.491013e-13  1.492051e-15  1.151638e-30  8.328758e-15
#>           dim_6         dim_7         dim_8         dim_9        dim_10
#> 1 -1.315503e-06 -5.691909e-04 -4.339885e-05 -3.035812e-04 -5.203486e-07
#> 2  1.124154e-05 -3.716735e-02  3.840075e-04 -1.808412e-02 -8.701055e-05
#> 3 -5.439658e-06  5.504310e-02 -6.433562e-04 -6.151502e-01  7.352728e-05
#> 4  1.651930e-04 -9.186165e+01 -4.907250e-02 -8.691762e+01  2.767092e-03
#> 5 -1.225677e-06  3.764676e+00  3.875468e-03  8.592365e+00  7.604388e-05
#> 6  2.104160e-13 -3.408082e-15 -1.678960e-14  4.248932e-14 -1.722425e-16
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

df <- prepare_eiger(x, y, yule_tree, 20)
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
run_eiger(x, y, yule_tree, 20, df = df)
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
run_eiger(x, y, yule_tree, 30)
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
