#' Check if the data are compatible for running eiger regression.
#'
#' `check_eiger()` checks if the trait values, the phylogenetic tree, and the
#' number of eigenvectors to include in the regression are compatible with each
#' other.
#'
#' @param x A vector of numeric values representing the predictor trait values
#'   of the `n` tips in the tree.
#' @param y A vector of numeric values representing the outcome trait values of
#'   the `n` tips in the tree.
#' @inheritParams check_phylo_branches
#' @param n_eigenvectors The number of eigenvectors to include as fixed effects
#'   in eiger regression.
#'
#' @returns Invisible returns `NULL` if all the following requirements are met:
#'   * The input `tree` is a valid object of class `"phylo"`,
#'   * The input `tree` has all branch lengths defined and non-negative,
#'   * The input `tree` has all unique tip labels,
#'   * `n_eigenvectors` is less than or equal to the number of tips in `tree` and greater than to equal to `0`,
#'   * `x` and `y` are vectors of numeric values,
#'   * The lengths of `x` and `y` are equal to the number of tips in `tree`.
#'   Otherwise raises an error.
#'
#' @keywords internal
check_eiger <- function(x, y, tree, n_eigenvectors) {
  # check the tree
  check_phylo(tree)
  check_phylo_branches(tree)
  if (length(unique(tree$tip.label)) != length(tree$tip.label)) {
    stop("The tip labels of the input tree must be all unique.",
         call. = FALSE)
  }

  # check the dimensions
  n_tips <- length(tree$tip.label)
  check_dimensions(n_eigenvectors, 0, n_tips)

  # check x and y
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("The input trait values must be numeric values.",
         call. = FALSE)
  }
  if (length(x) != n_tips || length(y) != n_tips) {
    stop("The input trait values must be of the same length as the number of tips in the tree.",
         call. = FALSE)
  }
  invisible(NULL)
}

#' Prepare the data frame for running eiger regression
#'
#' `prepare_eiger()` constructs a data frame that is essential for running eiger
#' regression. The data frame includes the original data for the two traits as
#' well as the eigenvectors of the variance-covariance matrix.
#'
#' @inheritParams check_eiger
#' @param dc A boolean value. `TRUE` if the eigenvectors need to be calculated
#'   from double-centered variance-covariance matrix and `FALSE` otherwise.
#'   Default to `FALSE`.
#'
#' @returns A data frame of `n` rows and `(n + 2)` columns, where `n` is the
#'   number of tips in the tree. The first two columns have names `X` and `Y`,
#'   representing the trait values. The next `n` columns represent the `n`
#'   eigenvectors of the variance-covariance matrix. Raises error if:
#'   * The input `tree` is not a valid object of class `"phylo"`,
#'   * The input `tree` does not have all branch lengths defined or have some of them being negative,
#'   * The input `tree` does not have all unique tip labels,
#'   * `n_eigenvectors` is greater than the number of tips in `tree`,
#'   * `x` and `y` are not vectors of numeric values,
#'   * The lengths of `x` and `y` are not equal to the number of tips in `tree`.
#'
#' @examples
#' set.seed(42)
#' yule_tree <- TreeSim::sim.bd.taxa(100, 1, 1, 0, 1, complete = FALSE)[[1]]
#' BM <- phytools::fastBM(yule_tree, 1, nsim = 2)
#' x <- BM[, 1]
#' y <- BM[, 2]
#' prepare_eiger(x, y, yule_tree, 20)
#' prepare_eiger(x, y, yule_tree, 30, TRUE)
#'
#' @export
prepare_eiger <- function(x, y, tree, n_eigenvectors, dc = FALSE) {
  # check the input
  check_eiger(x, y, tree, n_eigenvectors)

  tree$root.edge <- 0

  # calculate eigenvectors of variance-covariance matrix
  cov_matrix <- ape::vcv(tree)
  n_tips <- length(tree$tip.label)
  if (dc) {
    C_N <- diag(n_tips) - 1 / n_tips * matrix(1, n_tips, n_tips)
    cov_matrix <- C_N %*% cov_matrix %*% C_N
  }
  vs <- eigen(cov_matrix)$vectors

  df <- matrix(0, nrow = n_tips, ncol = n_tips + 2)
  df[, 1] <- x
  df[, 2] <- y
  df[, 3:(n_tips + 2)] <- vs
  df <- as.data.frame(df)
  colnames(df) <- c("X", "Y", paste0("eigen_", 1:n_tips))
  rownames(df) <- tree$tip.label
  df
}

#' Run eiger regression
#'
#' `run_eiger()` runs the eiger regression, which includes eigenvectors of the
#' variance-covariance matrix of a phylogenetic tree as fixed effects in
#' `phylolm`.
#'
#' @inheritParams prepare_eiger
#' @param df A data frame of `n` rows and `(n + 2)` columns, where `n` is the
#'   number of tips in the tree. The first two columns have names `X` and `Y`,
#'   representing the trait values. The next `n` columns represent the `n`
#'   eigenvectors of the variance-covariance matrix. Default to `NULL`, in which
#'   case `df` will be computed from the other parameters.
#' @param intercept A boolean. `TRUE` if the intercept is included in the
#'   regression, otherwise `FALSE`. Default to `TRUE`.
#' @param ... Other parameters passed to [phylolm::phylolm].
#'
#' @returns The results of eiger regession, which is the same as the return
#'   values of [phylolm::phylolm]. Raises error if:
#'   * The input `tree` is not a valid object of class `"phylo"`,
#'   * The input `tree` does not have all branch lengths defined or have some of them being negative,
#'   * The input `tree` does not have all unique tip labels,
#'   * `n_eigenvectors` is greater than the number of tips in `tree`,
#'   * `x` and `y` are not vectors of numeric values,
#'   * The lengths of `x` and `y` are not equal to the number of tips in `tree`,
#'   * `n_eigenvectors` is too large (even though it's valid),
#'   * Other problems when running [phylolm::phylolm].
#'
#' @examples
#' set.seed(42)
#' yule_tree <- TreeSim::sim.bd.taxa(100, 1, 1, 0, 1, complete = FALSE)[[1]]
#' BM <- phytools::fastBM(yule_tree, 1, nsim = 2)
#' x <- BM[, 1]
#' y <- BM[, 2]
#' run_eiger(x, y, yule_tree, n_eigenvectors = 20)
#' run_eiger(x, y, yule_tree, 30, dc = TRUE, intercept = FALSE)
#'
#' @export
run_eiger <- function(x, y, tree, n_eigenvectors,
                      dc = FALSE, df = NULL, intercept = TRUE, ...) {
  # compute or check the data frame
  if (is.null(df)) {
    df <- prepare_eiger(x, y, tree, n_eigenvectors, dc)
  } else {
    check_eiger(x, y, tree, n_eigenvectors)
    if (any(df$X != x) || any(df$Y != y)) {
      stop("The input data frame is inconsistent with the trait values.",
           call. = FALSE)
    }
    n_tips <- length(tree$tip.label)
    if (nrow(df) != n_tips || ncol(df) != n_tips + 2) {
      stop("The input data frame is inconsistent with the dimension of the tree.",
           call. = FALSE)
    }
  }

  tree$root.edge <- 0

  # complete the regression formula
  col_names <- colnames(df)
  if (n_eigenvectors >= 1) {
    eigen_names <- col_names[3:(n_eigenvectors + 2)]
    str_formula <- paste(c("Y ~ X", eigen_names), collapse = " + ")
  } else {
    str_formula <- "Y ~ X"
  }
  if (!intercept) {
    str_formula <- paste0(str_formula, " - 1")
  }
  reg_formula <- stats::as.formula(str_formula)

  # run the regression
  tryCatch({
    model <- phylolm::phylolm(reg_formula, data = df, phy = tree, ...)
    model
  }, error = function(e) {
    stop("Too many eigenvectors are included.",
         call. = FALSE)
  })
}
