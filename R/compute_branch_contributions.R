#' Check if an object is a valid `"Phylo"` object
#'
#' `check_phylo()` checks if an object is a valid object of class `"Phylo"`.
#'
#' @param tree A phylogenetic tree as an object of class `"phylo"`.
#'
#' @returns Invisible returns `NULL` if the phylogenetic tree is a valid object
#'   of class `"phylo"`, otherwise raises an error.
#'
#' @examples
#' \dontrun{
#' set.seed(42)
#' yule_tree <- TreeSim::sim.bd.taxa(10, 1, 1, 0, 1, complete = FALSE)[[1]]
#' check_phylo(yule_tree)
#' broken_tree <- list(edge = matrix(1:4, 2, 2), tip.label = letters[1:3])
#' try(check_phylo(broken_tree))
#' }
#'
#' @keywords internal
check_phylo <- function(tree) {
  ape_check <- utils::capture.output(ape::checkValidPhylo(tree))
  if (any(grepl("FATAL", ape_check, ignore.case = TRUE))) {
    stop("The input tree must be a valid object of class 'phylo'.",
         call. = FALSE)
  }
  invisible(NULL)
}

#' Calculate the descendancy matrix for branches of a tree
#'
#' `compute_descendancy_matrix()` computes a binary matrix for descendancy
#' relationships between branches and tips of a tree, with an entry being `1` if
#' the corresponding tip (column) is a descendant of the corresponding branch
#' (row) and `0` otherwise.
#'
#' @inheritParams check_phylo
#'
#' @returns A binary descendancy matrix with each row representing a branch of
#'   the tree and each column a tip of the tree. An entry of the matrix is `1`
#'   if the corresponding tip (column) is a descendant of the corresponding
#'   branch (row) and `0` otherwise. Raises error if the input is not a valid
#'   tree of class `"phylo"`.
#'
#' @examples
#' \dontrun{
#' set.seed(42)
#' yule_tree <- TreeSim::sim.bd.taxa(10, 1, 1, 0, 1, complete = FALSE)[[1]]
#' compute_descendancy_matrix(yule_tree)
#'
#' # Input must be a valid object of class "phylo"
#' broken_tree <- list(edge = matrix(1:4, 2, 2), tip.label = letters[1:3])
#' try(compute_descendancy_matrix(broken_tree))
#' }
#'
#' @keywords internal
compute_descendancy_matrix <- function(tree) {
  # check valid input
  check_phylo(tree)

  # extract the matrix dimension from the tree
  n_tips <- length(tree$tip.label)
  n_branches <- length(tree$edge[, 1])
  G <- matrix(0, nrow = n_branches, ncol = n_tips)

  # find the descendancy relationships for all branches
  for (i in 1:n_branches) {
    node <- tree$edge[i, 2]
    descendants <- phytools::getDescendants(tree, node)
    descendants_tips <- descendants[descendants <= n_tips]
    G[i, descendants_tips] = 1
  }
  G
}

#' Construct the variance-covariance matrix for a branch
#'
#' `compute_branch_vcv()` computes the variance-covariance matrix for a branch
#' of a phylogenetic tree. An entry in the matrix is `1` if the two tips
#' corresponding to the row and the column are both descendants of the branch,
#' and `0` otherwise.
#'
#' @param x A binary vector for the descendancy relationships between the tips
#'   and the branch. An entry is `1` if the corresponding tip is a descendant of
#'   the branch cocnerned and `0` otherwise.
#'
#' @returns A binary variance-covariance matrix for the branch. An entry in the
#'   matrix is `1` if the two tips corresponding to the row and the column are
#'   both descendants of the branch, and `0` otherwise.
#'
#' @examples
#' \dontrun{
#' compute_branch_vcv(c(1, 0, 0, 1, 1))
#' }
#'
#' @keywords internal
compute_branch_vcv <- function(x) {
  outer(x, x)
}

#' Construct the expected genetic relatedness matrix for a branch
#'
#' `compute_branch_egrm()` computes the expected genetic relatedness matrix for
#' a branch of a phylogenetic tree.
#'
#' @param x A binary vector for the descendancy relationships between the tips
#'   and the branch. An entry is `1` if the corresponding tip is a descendant of
#'   the branch cocnerned and `0` otherwise.
#'
#' @returns The expected genetic variance-covariance matrix for the branch. An
#'   entry in the matrix is positive if the two tips corresponding to the row
#'   and the column are both descendants or non-descendants of the branch, and
#'   negative if one tip is a descendant and the other is not. The exact values
#'   depend on the partition of all the tips into descendants and
#'   non-descendants.
#'
#' @examples
#' \dontrun{
#' compute_branch_egrm(c(1, 0, 0, 1, 1))
#' }
#'
#' @keywords internal
compute_branch_egrm <- function(x) {
  n <- length(x)
  x_mean <- mean(x)
  x_sd <- sqrt(sum((x - x_mean) ^ 2) / n)
  x_z <- (x - x_mean) / x_sd
  outer(x_z, x_z)
}

#' Check if a `"Phylo"` object has valid branch lengths
#'
#' `check_phylo_branches()` checks if a `"Phylo"` object has defined lengths for
#' all its branches and if the branch lengths are all non-negative.
#'
#' @param tree A phylogenetic tree as a valid object of class `"phylo"`.
#'
#' @returns Invisible returns `NULL` if the phylogenetic tree has defined
#'   non-negative lengths for all its branches, otherwise raises an error.
#'
#' @examples
#' \dontrun{
#' set.seed(42)
#' yule_tree <- TreeSim::sim.bd.taxa(10, 1, 1, 0, 1, complete = FALSE)[[1]]
#' check_phylo_branches(yule_tree)
#'
#' # Input tree must have all the branch lengths defined and non-negative
#' broken_tree <- list(edge = matrix(c(4, 1, 4, 5, 5, 2, 5, 3), 4, 2, byrow = TRUE),
#'                     tip.label = letters[1:3],
#'                     Nnode = 2)
#' class(broken_tree) <- "phylo"
#' try(check_phylo_branches(broken_tree))
#' broken_tree$edge.length <- c(1, 2)
#' try(check_phylo_branches(broken_tree))
#' broken_tree$edge.length <- c(1, 2, NA, 1)
#' try(check_phylo_branches(broken_tree))
#' broken_tree$edge.length <- c(1, 2, -1, 1)
#' try(check_phylo_branches(broken_tree, "vcv", 2))
#' }
#'
#' @keywords internal
check_phylo_branches <- function(tree) {
  if (is.null(tree$edge.length)) {
    stop("The input tree must have branch lengths.", call. = FALSE)
  }
  if (length(tree$edge.length) != length(tree$edge[, 1]) || any(is.na(tree$edge.length))) {
    stop("The input tree must have defined lengths for all of its branches.",
         call. = FALSE)
  }
  if (any(tree$edge.length < 0)) {
    stop("The input tree must have branches of non-negative lengths.",
         call. = FALSE)
  }
  invisible(NULL)
}

#' Construct variance-covariance matrices for a tree and each of its branches
#'
#' `compute_cov_matrices()` computes some version of variance-covariance matrix
#' for a phylogenetic tree as well as for each of its branches.
#'
#' @param tree A phylogenetic tree as an object of class `"phylo"` with all the
#'   branch lengths defined and non-negative.
#' @param cov_matrix A string representing the way to calculate the
#'   variance-covariance matrices. The options are `"dcvcv"` (double-centered
#'   phylogenetic variance-covariance matrix), `"vcv"` (non-centered
#'   phylogenetic variance-covariance matrix), and `"egrm"` (expected genetic
#'   relatedness matrix). Default to `"dcvcv"`.
#'
#' @returns A list of two values.
#'
#'   The first value `cov_matrices` is an `n^2` by `(m + 1)` dataframe, where
#'   `n` is the number of tips in the phylogenetic tree and `m` is the number of
#'   branches. The first `m` columns refer to the variance-covariance matrices
#'   for the corresponding branches in the tree. The `k`-th of the first `m`
#'   columns has name `"dcvcv_k"`, `"vcv_k"`, or `"egrm_k"` depending on the way
#'   the variance-covariance matrices are calculated. The last column refers to
#'   the variance-covariance matrix for the whole tree and has name
#'   `"dcvcv_tot"`, `"vcv_tot"`, or `"egrm_tot"`. The matrices are collapsed
#'   into vectors for easier visualization and downstream analyses.
#'
#'   The second value `cov_matrix_tree` is the variance-covariance matrix for
#'   the whole phylogenetic tree.
#'
#'   Raises error if the input is not a valid tree of class `"phylo"` or if not
#'   all the branch lengths are defined or if some branch lengths are negative.
#'
#' @examples
#' \dontrun{
#' set.seed(42)
#' yule_tree <- TreeSim::sim.bd.taxa(10, 1, 1, 0, 1, complete = FALSE)[[1]]
#' compute_cov_matrices(yule_tree)
#' compute_cov_matrices(yule_tree, cov_matrix = "egrm")
#'
#' # Input tree must be a valid object of class "phylo"
#' broken_tree <- list(edge = matrix(1:4, 2, 2), tip.label = letters[1:3])
#' try(compute_cov_matrices(broken_tree))
#'
#' # Input tree must have all the branch lengths defined and non-negative
#' broken_tree <- list(edge = matrix(c(4, 1, 4, 5, 5, 2, 5, 3), 4, 2, byrow = TRUE),
#'                     tip.label = letters[1:3],
#'                     Nnode = 2)
#' class(broken_tree) <- "phylo"
#' try(compute_cov_matrices(broken_tree))
#' broken_tree$edge.length <- c(1, 2)
#' try(compute_cov_matrices(broken_tree))
#' broken_tree$edge.length <- c(1, 2, NA, 1)
#' try(compute_cov_matrices(broken_tree))
#' broken_tree$edge.length <- c(1, 2, -1, 1)
#' try(compute_branch_contributions(broken_tree, "vcv", 2))
#' }
#'
#' @keywords internal
compute_cov_matrices <- function(tree, cov_matrix = c("dcvcv", "vcv", "egrm")) {
  cov_matrix <- match.arg(cov_matrix)
  G <- compute_descendancy_matrix(tree)

  # check if all the branch lengths of the tree are defined
  check_phylo_branches(tree)

  sum_branch_lengths <- sum(tree$edge.length)
  n_tips <- length(tree$tip.label)
  n_branches <- length(tree$edge[, 1])

  # determine the function to calculate the variance-covariance matrix for a branch
  if (cov_matrix == "egrm") {
    compute_matrix_branch <- compute_branch_egrm
  } else if (cov_matrix == "vcv") {
    compute_matrix_branch <- compute_branch_vcv
  } else {
    C_N <- diag(n_tips) - 1 / n_tips * matrix(1, n_tips, n_tips)
    compute_matrix_branch <- function(...) C_N %*% compute_branch_vcv(...) %*% C_N
  }

  cov_matrices <- matrix(0, nrow = n_tips^2, ncol = n_branches + 1)
  # calculate the weighted variance-covariance matrix for each branch
  for (i in 1:n_branches) {
    branch <- G[i, ]
    weight <- tree$edge.length[i] / sum_branch_lengths
    cov_matrix_branch <- compute_matrix_branch(branch) * weight
    cov_matrices[, i] <- as.vector(cov_matrix_branch)
  }

  # calculate the variance-covariance matrix for the whole tree
  cov_matrices[, n_branches + 1] <- rowSums(cov_matrices)
  cov_matrix_total <- matrix(cov_matrices[, n_branches + 1], nrow = n_tips,
                             byrow = TRUE)

  # wrap the computations in a data frame
  df <- as.data.frame(cov_matrices)
  colnames(df) <- c(paste0(cov_matrix, "_", 1:n_branches),
                    paste0(cov_matrix, "_tot"))

  list(cov_matrices = df, cov_matrix_tree = cov_matrix_total)
}

#' Check if the input dimensions are integers between some lower and upper bound
#'
#' `check_dimensions()` checks if each of the input dimensions is an integer
#' between some lower bound and some upper bound.
#'
#' @param x An integer or a vector of integers for the input dimensions.
#' @param low An integer for the lower bound of the dimensions.
#' @param high An integer for the upper bound of the dimensions.
#'
#' @returns Invisible returns `NULL` if each of the input dimensions is an
#'   integer between some lower bound and some upper bound, otherwise raises an
#'   error.
#'
#' @examples
#' \dontrun{
#' check_dimensions(1, 1, 5)
#' check_dimensions(1:9, 1, 10)
#' check_dimensions(c(5, 4, 9, 8, 1, 3), 1, 10)
#' try(check_dimensions(-7, 1, 10))
#' try(check_dimensions(0, 1, 8))
#' try(check_dimensions(c(3, 2, 11, 4, 6), 1, 8))
#' try(check_dimensions(c(0.5, 2, 1, 8), 1, 8))
#' }
#'
#' @keywords internal
check_dimensions <- function(x, low, high) {
  if (!rlang::is_integerish(x)) {
    stop("The input dimensions must be integers.", call. = FALSE)
  }
  if (any(x < low | x > high)) {
    error_message <- paste0("The input dimensions must be between ", low, " and ", high, ".")
    stop(error_message, call. = FALSE)
  }
  invisible(NULL)
}

#' Compute branch contributions to eigenvectors of variance-covariance matrices
#'
#' `compute_branch_contributions()` computes the contributions of each branch of
#' a phylogenetic tree to the eigenvectors of some version of
#' variance-covariance matrix for the whole tree.
#'
#' @inheritParams compute_cov_matrices
#' @param dims An integer or a vector of integers for the dimensions of
#'   eigenvectors to compute the branch contributions.
#'
#' @returns A data frame of `m` rows and `n` columns, where `m` is the number of
#'   branches, and `n` is the number of dimensions of eigenvectors to compute
#'   the branch contributions. The `i`-th row corresponds to the contributions
#'   of the `i`-th branch to the eigenvectors, and the `j`-th column has name
#'   `"dim_k"`, where `k` is the . The last column refers to where `k` is the
#'   `j`-th dimension in `dims` to compute the branch contributions. Raises
#'   error if the input tree is not a valid tree of class `"phylo"` or if not
#'   all the branch lengths are defined or if some branch lengths are negative.
#'   Also raises error if the input dimensions contain either non-integers or
#'   integers less than `1` or greater than the number of tips in the tree.
#'
#' @examples
#' set.seed(42)
#' yule_tree <- TreeSim::sim.bd.taxa(10, 1, 1, 0, 1, complete = FALSE)[[1]]
#' compute_branch_contributions(yule_tree, "vcv", 1)
#' compute_branch_contributions(yule_tree, "egrm", 3)
#' compute_branch_contributions(yule_tree, "dcvcv", 1:5)
#' compute_branch_contributions(yule_tree, dims = c(2, 4, 6, 7))
#'
#' # Input tree must be a valid object of class "phylo"
#' broken_tree <- list(edge = matrix(1:4, 2, 2), tip.label = letters[1:3])
#' try(compute_branch_contributions(broken_tree, "vcv", 2))
#'
#' # Input tree must have all the branch lengths defined and non-negative
#' broken_tree <- list(edge = matrix(c(4, 1, 4, 5, 5, 2, 5, 3), 4, 2, byrow = TRUE),
#'                     tip.label = letters[1:3],
#'                     Nnode = 2)
#' class(broken_tree) <- "phylo"
#' try(compute_branch_contributions(broken_tree, "vcv", 2))
#' broken_tree$edge.length <- c(1, 2)
#' try(compute_branch_contributions(broken_tree, "vcv", 2))
#' broken_tree$edge.length <- c(1, 2, NA, 1)
#' try(compute_branch_contributions(broken_tree, "vcv", 2))
#' broken_tree$edge.length <- c(1, 2, -1, 1)
#' try(compute_branch_contributions(broken_tree, "vcv", 2))
#'
#' # Input dimensions must all be integers from 1 to the number of tips in the tree
#' try(compute_branch_contributions(yule_tree, "vcv", 11))
#' try(compute_branch_contributions(yule_tree, "dcvcv", c(8, 9, 2, 14, 1)))
#'
#' @export
compute_branch_contributions <- function(tree, cov_matrix = c("dcvcv", "vcv", "egrm"), dims) {
  # check the dimensions
  n_tips <- length(tree$tip.label)
  n_branches <- length(tree$edge[, 1])
  check_dimensions(dims, 1, n_tips)

  # compute the covariance matrices for each branch and for the whole tree
  cov_matrix <- match.arg(cov_matrix)
  cov_results <- compute_cov_matrices(tree, cov_matrix)
  cov_matrices <- cov_results$cov_matrices
  cov_matrix_tree <- cov_results$cov_matrix_tree

  # get the names for the covariance matrices for each branch
  cov_matrices_names <- colnames(cov_matrices)
  cov_matrices_names <- cov_matrices_names[-(n_branches + 1)]

  # calculate eigenvectors for the covariance matrix of the tree
  vs <- eigen(cov_matrix_tree)$vectors
  for (i in 1:n_tips) {
    matrix_name <- paste0("eigen_", i)
    cov_matrices[[matrix_name]] <- as.vector(outer(vs[, i], vs[, i]))
  }

  # initialize the branch contributions matrix
  branch_contributions <- matrix(0, nrow = n_branches, ncol = length(dims))

  # create the common part of the formula for regression in string format
  str_formula_common <- paste(cov_matrices_names, collapse = " + ")
  str_formula_common <- paste("~", str_formula_common, "- 1")

  # calculate the branch contributions to each of the dimensions
  for (i in 1:length(dims)) {
    j <- dims[i]

    # complete the formula for regression in string format
    eigen_name <- paste0("eigen_", j)
    str_formula <- paste(eigen_name, str_formula_common)
    reg_formula <- stats::as.formula(str_formula)

    # run the regression
    model <- stats::lm(reg_formula, data = cov_matrices)
    coeffs <- as.vector(stats::coef(model))

    # find the two edges from the root of the tree for "dcvcv" and "egrm"
    if (cov_matrix != "vcv") {
      dim_na = which(is.na(coeffs))
      coeffs[1] <- coeffs[1] / 2
      coeffs[dim_na] <- coeffs[1]
    }

    branch_contributions[, i] <- coeffs
  }

  # wrap the branch contributions in a data frame
  df <- as.data.frame(branch_contributions)
  colnames(df) <- paste0("dim_", dims)
  df
}
