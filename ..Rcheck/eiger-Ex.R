pkgname <- "eiger"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('eiger')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("check_dimensions")
### * check_dimensions

flush(stderr()); flush(stdout())

### Name: check_dimensions
### Title: Check if the input dimensions are integers between some lower
###   and upper bound
### Aliases: check_dimensions
### Keywords: internal

### ** Examples

## Not run: 
##D check_dimensions(1, 1, 5)
##D check_dimensions(1:9, 1, 10)
##D check_dimensions(c(5, 4, 9, 8, 1, 3), 1, 10)
##D try(check_dimensions(-7, 1, 10))
##D try(check_dimensions(0, 1, 8))
##D try(check_dimensions(c(3, 2, 11, 4, 6), 1, 8))
##D try(check_dimensions(c(0.5, 2, 1, 8), 1, 8))
## End(Not run)




cleanEx()
nameEx("check_phylo")
### * check_phylo

flush(stderr()); flush(stdout())

### Name: check_phylo
### Title: Check if an object is a valid '"Phylo"' object
### Aliases: check_phylo
### Keywords: internal

### ** Examples

## Not run: 
##D set.seed(42)
##D yule_tree <- TreeSim::sim.bd.taxa(10, 1, 1, 0, 1, complete = FALSE)[[1]]
##D check_phylo(yule_tree)
##D broken_tree <- list(edge = matrix(1:4, 2, 2), tip.label = letters[1:3])
##D try(check_phylo(broken_tree))
## End(Not run)




cleanEx()
nameEx("check_phylo_branches")
### * check_phylo_branches

flush(stderr()); flush(stdout())

### Name: check_phylo_branches
### Title: Check if a '"Phylo"' object has valid branch lengths
### Aliases: check_phylo_branches
### Keywords: internal

### ** Examples

## Not run: 
##D set.seed(42)
##D yule_tree <- TreeSim::sim.bd.taxa(10, 1, 1, 0, 1, complete = FALSE)[[1]]
##D check_phylo_branches(yule_tree)
##D 
##D # Input tree must have all the branch lengths defined and non-negative
##D broken_tree <- list(edge = matrix(c(4, 1, 4, 5, 5, 2, 5, 3), 4, 2, byrow = TRUE),
##D                     tip.label = letters[1:3],
##D                     Nnode = 2)
##D class(broken_tree) <- "phylo"
##D try(check_phylo_branches(broken_tree))
##D broken_tree$edge.length <- c(1, 2)
##D try(check_phylo_branches(broken_tree))
##D broken_tree$edge.length <- c(1, 2, NA, 1)
##D try(check_phylo_branches(broken_tree))
##D broken_tree$edge.length <- c(1, 2, -1, 1)
##D try(check_phylo_branches(broken_tree, "vcv", 2))
## End(Not run)




cleanEx()
nameEx("compute_branch_contributions")
### * compute_branch_contributions

flush(stderr()); flush(stdout())

### Name: compute_branch_contributions
### Title: Compute branch contributions to eigenvectors of
###   variance-covariance matrices
### Aliases: compute_branch_contributions

### ** Examples

set.seed(42)
yule_tree <- TreeSim::sim.bd.taxa(10, 1, 1, 0, 1, complete = FALSE)[[1]]
compute_branch_contributions(yule_tree, "vcv", 1)
compute_branch_contributions(yule_tree, "egrm", 3)
compute_branch_contributions(yule_tree, "dcvcv", 1:5)
compute_branch_contributions(yule_tree, dims = c(2, 4, 6, 7))

# Input tree must be a valid object of class "phylo"
broken_tree <- list(edge = matrix(1:4, 2, 2), tip.label = letters[1:3])
try(compute_branch_contributions(broken_tree, "vcv", 2))

# Input tree must have all the branch lengths defined and non-negative
broken_tree <- list(edge = matrix(c(4, 1, 4, 5, 5, 2, 5, 3), 4, 2, byrow = TRUE),
                    tip.label = letters[1:3],
                    Nnode = 2)
class(broken_tree) <- "phylo"
try(compute_branch_contributions(broken_tree, "vcv", 2))
broken_tree$edge.length <- c(1, 2)
try(compute_branch_contributions(broken_tree, "vcv", 2))
broken_tree$edge.length <- c(1, 2, NA, 1)
try(compute_branch_contributions(broken_tree, "vcv", 2))
broken_tree$edge.length <- c(1, 2, -1, 1)
try(compute_branch_contributions(broken_tree, "vcv", 2))

# Input dimensions must all be integers from 1 to the number of tips in the tree
try(compute_branch_contributions(yule_tree, "vcv", 11))
try(compute_branch_contributions(yule_tree, "dcvcv", c(8, 9, 2, 14, 1)))




cleanEx()
nameEx("compute_branch_egrm")
### * compute_branch_egrm

flush(stderr()); flush(stdout())

### Name: compute_branch_egrm
### Title: Construct the expected genetic relatedness matrix for a branch
### Aliases: compute_branch_egrm
### Keywords: internal

### ** Examples

## Not run: 
##D compute_branch_egrm(c(1, 0, 0, 1, 1))
## End(Not run)




cleanEx()
nameEx("compute_branch_vcv")
### * compute_branch_vcv

flush(stderr()); flush(stdout())

### Name: compute_branch_vcv
### Title: Construct the variance-covariance matrix for a branch
### Aliases: compute_branch_vcv
### Keywords: internal

### ** Examples

## Not run: 
##D compute_branch_vcv(c(1, 0, 0, 1, 1))
## End(Not run)




cleanEx()
nameEx("compute_cov_matrices")
### * compute_cov_matrices

flush(stderr()); flush(stdout())

### Name: compute_cov_matrices
### Title: Construct variance-covariance matrices for a tree and each of
###   its branches
### Aliases: compute_cov_matrices
### Keywords: internal

### ** Examples

## Not run: 
##D set.seed(42)
##D yule_tree <- TreeSim::sim.bd.taxa(10, 1, 1, 0, 1, complete = FALSE)[[1]]
##D compute_cov_matrices(yule_tree)
##D compute_cov_matrices(yule_tree, cov_matrix = "egrm")
##D 
##D # Input tree must be a valid object of class "phylo"
##D broken_tree <- list(edge = matrix(1:4, 2, 2), tip.label = letters[1:3])
##D try(compute_cov_matrices(broken_tree))
##D 
##D # Input tree must have all the branch lengths defined and non-negative
##D broken_tree <- list(edge = matrix(c(4, 1, 4, 5, 5, 2, 5, 3), 4, 2, byrow = TRUE),
##D                     tip.label = letters[1:3],
##D                     Nnode = 2)
##D class(broken_tree) <- "phylo"
##D try(compute_cov_matrices(broken_tree))
##D broken_tree$edge.length <- c(1, 2)
##D try(compute_cov_matrices(broken_tree))
##D broken_tree$edge.length <- c(1, 2, NA, 1)
##D try(compute_cov_matrices(broken_tree))
##D broken_tree$edge.length <- c(1, 2, -1, 1)
##D try(compute_branch_contributions(broken_tree, "vcv", 2))
## End(Not run)




cleanEx()
nameEx("compute_descendancy_matrix")
### * compute_descendancy_matrix

flush(stderr()); flush(stdout())

### Name: compute_descendancy_matrix
### Title: Calculate the descendancy matrix for branches of a tree
### Aliases: compute_descendancy_matrix
### Keywords: internal

### ** Examples

## Not run: 
##D set.seed(42)
##D yule_tree <- TreeSim::sim.bd.taxa(10, 1, 1, 0, 1, complete = FALSE)[[1]]
##D compute_descendancy_matrix(yule_tree)
##D 
##D # Input must be a valid object of class "phylo"
##D broken_tree <- list(edge = matrix(1:4, 2, 2), tip.label = letters[1:3])
##D try(compute_descendancy_matrix(broken_tree))
## End(Not run)




cleanEx()
nameEx("create_color_palette")
### * create_color_palette

flush(stderr()); flush(stdout())

### Name: create_color_palette
### Title: Create a color palette from a color-generating function or a set
###   of colors
### Aliases: create_color_palette
### Keywords: internal

### ** Examples

## Not run: 
##D create_color_palette(c("red", "blue"), 30)
##D create_color_palette(c("#BBCCEE", "#CCEEFF", "#CCDDAA", "#EEEEBB", "#FFCCCC"), 50)
##D create_color_palette("Viridis", 80)
##D try(create_color_palette("red", 30))
##D try(create_color_palette(c("apple", "blueberry"), 30))
##D try(create_color_palette(c("red", "blue"), 3.14))
##D try(create_color_palette("CoolColors", 80))
## End(Not run)




cleanEx()
nameEx("map_colors")
### * map_colors

flush(stderr()); flush(stdout())

### Name: map_colors
### Title: Map a vector of values to colors in the color palette
### Aliases: map_colors
### Keywords: internal

### ** Examples

## Not run: 
##D map_colors(-8:8, c("blue", "red"))
##D x <- c(-3.5, 9.8, 0.3, -1.1, -4.6, 2.1)
##D color_palette <- c("#BBCCEE", "#CCEEFF", "#CCDDAA", "#EEEEBB", "#FFCCCC")
##D map_colors(x, color_palette)
## End(Not run)




cleanEx()
nameEx("plot_branch_contributions")
### * plot_branch_contributions

flush(stderr()); flush(stdout())

### Name: plot_branch_contributions
### Title: Plot branch contributions to an eigenvector of
###   variance-covariance matrices
### Aliases: plot_branch_contributions

### ** Examples

set.seed(42)
yule_tree <- TreeSim::sim.bd.taxa(20, 1, 1, 0, 1, complete = FALSE)[[1]]
df <- compute_branch_contributions(yule_tree, "egrm", 1:10)
plot_branch_contributions(yule_tree, df, "egrm", 3)
plot_branch_contributions(yule_tree, df, "egrm", 5, n_colors = 50)
plot_branch_contributions(yule_tree, cov_matrix = "dcvcv", dim = 5, n_colors = 80)




cleanEx()
nameEx("prepare_eiger")
### * prepare_eiger

flush(stderr()); flush(stdout())

### Name: prepare_eiger
### Title: Prepare the data frame for running eiger regression
### Aliases: prepare_eiger

### ** Examples

set.seed(42)
yule_tree <- TreeSim::sim.bd.taxa(100, 1, 1, 0, 1, complete = FALSE)[[1]]
BM <- phytools::fastBM(yule_tree, 1, nsim = 2)
x <- BM[, 1]
y <- BM[, 2]
prepare_eiger(x, y, yule_tree, 20)
prepare_eiger(x, y, yule_tree, 30, TRUE)




cleanEx()
nameEx("run_eiger")
### * run_eiger

flush(stderr()); flush(stdout())

### Name: run_eiger
### Title: Run eiger regression
### Aliases: run_eiger

### ** Examples

set.seed(42)
yule_tree <- TreeSim::sim.bd.taxa(100, 1, 1, 0, 1, complete = FALSE)[[1]]
BM <- phytools::fastBM(yule_tree, 1, nsim = 2)
x <- BM[, 1]
y <- BM[, 2]
run_eiger(x, y, yule_tree, n_eigenvectors = 20)
run_eiger(x, y, yule_tree, 30, dc = TRUE, intercept = FALSE)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
