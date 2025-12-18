#' Check if a list contains redundant values
#'
#' `check_redundant()` checks if a list contains any redundant values.
#'
#' @param list_val The list to check.
#' @param error_message The error message to display if the list contains
#'   redundant values.
#'
#' @returns Invisible returns `NULL` if the list does not contain any redundant
#'   values. Otherwise raises an error.
#'
#' @keywords internal
check_redundant <- function(list_val, error_message) {
  if (length(unique(list_val)) != length(list_val)) {
    cli::cli_abort(error_message)
  }
  invisible(NULL)
}

#' Check if a names list is `NULL` or contains `NA` or redundant values
#'
#' `check_names_list()` checks if a names list is `NULL` or contains `NA` or redundant values.
#'
#' @param list_val The names list to check.
#' @param null_message The error message to display if the list is `NULL`.
#' @param na_top_message A string representing the overall big issue if the list contains `NA` values.
#' @param na_bullet_prefix Part of the bullet message that goes before the listed
#'   variant if the list contains `NA` values.
#' @param na_bullet_suffix Part of the bullet message that goes after the listed
#'   variant if the list contains `NA` values.
#' @param na_extra_message A string stating the number of unlisted problems if the list contains `NA` values.
#' @param redundant_message The error message to display if the list contains redundant values.
#'
#' @returns Invisible returns `NULL` if the list is not `NULL` and does not contain any `NA` or redundant
#'   values. Otherwise raises an error.
#'
#' @keywords internal
check_names_list <- function(list_val, null_message, na_top_message,
                             na_bullet_prefix, na_bullet_suffix,
                             na_extra_message, redundant_message) {
  if (is.null(list_val)) {
    cli::cli_abort(null_message)
  }
  if (any(list_val == "" | is.na(list_val))) {
    na_idx <- which(list_val == "" | is.na(list_val))
    create_bullet_error(na_idx, na_top_message, na_bullet_prefix,
                        na_bullet_suffix, na_extra_message)
  }
  check_redundant(list_val, redundant_message)
  invisible(NULL)
}

#' Check if the data frame is compatible for eiger regression
#'
#' `check_dataframe()` checks if the data frame is compatible for eiger
#' regression and modifies the row orders if the data frame is valid.
#'
#' @param df_val The data frame to check.
#' @param tip_labels The tip labels of the phylogenetic tree for eiger
#'   regression.
#' @param n_tips The number of tips in the phylogenetic tree. Default to
#'   `length(tip_labels)`.
#'
#' @returns The data frame with rows re-ordered based on `tip_labels` if the
#'   data frame is valid. If the data frame is provided in terms of a list, the
#'   list has values of each field re-ordered and names imputed based on the
#'   first field in the list that has names before being coerced to data frame.
#'   Raises error if:
#' * The data frame cannot be coerced to a valid data frame,
#' * The data frame does not have the same number of rows as the number of tips,
#' * There are columns with no labels in the data frame,
#' * There are redundant labels for columns in the data frame,
#' * When the data frame is provided in terms of a list:
#'     * The list is empty,
#'     * The fields in the list do not all have the same lengths,
#'     * The list contains fields that do not have tags,
#'     * The tags of the fields in the list are not all unique,
#'     * The list contains fields that have NA values in names,
#'     * The list contains fields that have redundant names.
#'   Raises warning if:
#' * The row names of the data frame do not match the tip labels,
#' * When the data frame is provided in terms of a list:
#'     * There are fields in the list that have no names,
#'         * If none of the fields has names, the fields will remain having no names,
#'         * If only some of the fields have no names, these fields will take the names of the first field in the list that has names,
#'     * The fields in the list do have have the same set of names, in which case the fields will all will take the names of the first field in the list that has names.
#'
#' @keywords internal
check_dataframe <- function(df_val, tip_labels, n_tips = length(tip_labels)) {
  # only allow data frames or matrices
  if (!("matrix" %in% class(df_val) || "data.frame" %in% class(df_val))) {
    cli::cli_abort(c(
      "Can't run eiger on data other than matrices or data frames.",
      "i" = "The data you provided is of class {.cls {class(df_val)}}.",
      "i" = "The data eiger accepts is of class {.cls matrix} or {.cls data.frame}."))
  }

  # convert data to a data frame
  df_val <- as.data.frame(df_val)
  df_colnames <- colnames(df_val)
  df_rownames <- rownames(df_val)
  df_n_rows <- nrow(df_val)
  df_n_cols <- ncol(df_val)

  # check if the data frame is empty
  if (df_n_rows == 0 || df_n_cols == 0) {
    cli::cli_abort(paste0("The data must not be empty."))
  }

  # check if the data frame has the same number of rows as the number of tips
  if (df_n_rows != n_tips) {
    cli::cli_abort(c(
      "The data frame must have the same number of rows as the number of tips in the tree.",
      "i" = paste0("The data has ", df_n_rows, " rows."),
      "i" = paste0("The tree has ", n_tips, " tips.")))
  }
  # check if the row names are the same as tip labels
  if (setequal(df_rownames, tip_labels)) {
    df_val <- df_val[tip_labels, ]
  } else {
    cli::cli_abort("The row names of the data do not match the tip labels of the tree.")
  }

  # check if there are columns with no names or redundant names
  null_message <- paste0("Can't find column names for the data.")
  na_top_message <- paste0("Can't find the names for all the columns in the data.")
  na_bullet_prefix <- "The name for column "
  na_bullet_suffix <- " can't be found."
  na_extra_message <- paste0(" more columns in the data do not have names.")
  redundant_message <- paste0("The column names in the data must all be unique.")
  check_names_list(df_colnames, null_message, na_top_message, na_bullet_prefix,
                   na_bullet_suffix, na_extra_message, redundant_message)

  df_val
}

#' Check if a column in the data frame contains `NA` values
#'
#' `check_column_na()` checks if a column in the data frame contains any `NA`
#' values.
#'
#' @param df_val The data frame to check.
#' @param col A string for the column name to check in the data frame.
#'
#' @returns Invisible returns `NULL` if the column of the data frame does not
#'   contain any `NA` values. Otherwise raises an error.
#'
#' @keywords internal
check_column_na <- function(df_val, col) {
  if (any(is.na(df_val[[col]]))) {
    cli::cli_abort(paste0("Can't run regressions with NA values in the column ", col, " of the data."))
  }
  invisible(NULL)
}

#' Check if a response variable is numeric
#'
#' `check_numeric_response()` checks if a response variable is numeric.
#'
#' @param response_val The response variable to check.
#' @param response_name The name of the response variable.
#'
#' @returns Invisible returns `NULL` if the response variable is numeric.
#'   Otherwise raises an error.
#'
#' @keywords internal
check_numeric_response <- function(response_val, response_name) {
  if (!is.numeric(response_val)) {
    cli::cli_abort(c(paste0("Response variable ", response_name, " must be a numeric vector."),
                     "x" = "You've supplied a {.cls {class(response_val)}} vector."))
  }
  invisible(NULL)
}

#' Check if the data are compatible for eiger regression
#'
#' @description `check_eiger()` checks if the trait values, the phylogenetic
#'   tree, and the number of eigenvectors to include in the regression are
#'   compatible with each other for running eiger regression.
#'
#' @param formula A formula describing the model to run eiger regression. The
#'   eigenvectors should not appear in the formula.
#' @param data A data frame containing the variables in the model. The data
#'   frame should have `n` rows, where `n` is the number of tips in `tree`.
#' @inheritParams check_phylo_branches
#' @param n_eigenvectors The number of eigenvectors to include as additional
#'   fixed effects in eiger regression.
#'
#' @returns A data frame of `n` rows and `m` columns, where `n` is the number of
#'   tips in `tree`, and `m` is the number of columns of `data`. The rows are
#'   ordered according to the order of tip labels in `tree`. No errors raised if
#'   all the following requirements are met:
#'   * The input `tree` is a valid object of class `"phylo"`,
#'   * The input `tree` has all branch lengths defined and non-negative,
#'   * The input `tree` has all unique tip labels,
#'   * `n_eigenvectors` is less than or equal to the number of tips in `tree` and greater than to equal to `0`,
#'   * All the variables in `formula` can be found in `data`,
#'   * None of the variables in `formula` contains any `NA` values,
#'   * The response variable in `formula` is a vector of numeric values,
#'   * `data` is provided either as a matrix or a data frame,
#'   * The number of rows of `data` is equal to the number of tips in `tree`,
#'   * The row names of `data` are equal to the tip labels of `tree`,
#'   * All columns have names in `data`,
#'   * There are no redundant column names in `data`.
#'   Otherwise raises an error.
#'
#' @examples
#' \dontrun{
#' set.seed(42)
#' yule_tree <- TreeSim::sim.bd.taxa(100, 1, 1, 0, 1, complete = FALSE)[[1]]
#' BM <- phytools::fastBM(yule_tree, 1, nsim = 3)
#'
#' BM_df <- as.data.frame(BM)
#' colnames(BM_df) <- c("x", "y", "z")
#' check_eiger(y ~ x, BM_df, yule_tree, 20)
#' check_eiger(y ~ x + log(z), BM_df, yule_tree, 20)
#' }
#'
#' @keywords internal
check_eiger <- function(formula, data, tree, n_eigenvectors) {
  # check the tree
  check_phylo(tree)
  check_phylo_branches(tree)
  error_message <- "The tip labels of the input tree must be all unique."
  check_redundant(tree$tip.label, error_message)

  # check the dimensions
  tip_labels <- tree$tip.label
  n_tips <- length(tip_labels)
  check_dimensions(n_eigenvectors, 0, n_tips)

  # get the variables
  vars <- all.vars(formula)

  # check the data
  if (is.null(data)) {
    cli::cli_abort(paste0("Can't find the data."))
  } else {
    output_df <- check_dataframe(data, tip_labels, n_tips)
    data_colnames <- colnames(output_df)
  }

  # check the variables
  for (var in vars) {
    if (var %in% data_colnames) {
      check_column_na(output_df, var)
    } else {
      cli::cli_abort(paste0("Can't find variable ", var, " in the data."))
    }
  }

  # find the response variable
  response <- vars[1]
  check_numeric_response(output_df[[response]], response)

  output_df
}

#' Prepare the data frame for running eiger regression
#'
#' @description `prepare_eiger()` constructs a data frame that is essential for
#'   running eiger regression. The data frame includes all the original data
#'   provided for the regression as well as the eigenvectors of the
#'   variance-covariance matrix.
#'
#' @inheritParams check_eiger
#' @param dc A boolean value. `TRUE` if the eigenvectors need to be calculated
#'   from double-centered variance-covariance matrix and `FALSE` otherwise.
#'   Default to `FALSE`.
#'
#' @returns A data frame of `n` rows, where `n` is the number of tips in `tree`.
#'   The data frame contains `(m + n)` columns, where the first `m` columns are
#'   the columns in `data`, and the last `n` columns
#'   represent the `n` eigenvectors of the variance-covariance matrix. The rows
#'   are ordered according to the order of tip labels in `tree`. No errors
#'   raised if all the following requirements are met:
#'   * The input `tree` is a valid object of class `"phylo"`,
#'   * The input `tree` has all branch lengths defined and non-negative,
#'   * The input `tree` has all unique tip labels,
#'   * `n_eigenvectors` is less than or equal to the number of tips in `tree` and greater than to equal to `0`,
#'   * All the variables in `formula` can be found in `data`,
#'   * None of the variables in `formula` contains any `NA` values,
#'   * The response variable in `formula` is a vector of numeric values,
#'   * `data` is provided either as a matrix or a data frame,
#'   * The number of rows of `data` is equal to the number of tips in `tree`,
#'   * The row names of `data` are equal to the tip labels of `tree`,
#'   * All columns have names in `data`,
#'   * There are no redundant column names in `data`.
#'   Otherwise raises an error.
#'
#' @examples
#' set.seed(42)
#' yule_tree <- TreeSim::sim.bd.taxa(100, 1, 1, 0, 1, complete = FALSE)[[1]]
#' BM <- phytools::fastBM(yule_tree, 1, nsim = 3)
#'
#' BM_df <- as.data.frame(BM)
#' colnames(BM_df) <- c("x", "y", "z")
#' prepare_eiger(y ~ x, BM_df, yule_tree, 20)
#' prepare_eiger(y ~ x + log(z), BM_df, yule_tree, 20, dc = TRUE)
#'
#' group <- c(rep("A", 40), rep("B", 60))
#' try(prepare_eiger(y ~ x + group, BM_df, yule_tree, 20))
#'
#' @export
prepare_eiger <- function(formula, data, tree, n_eigenvectors, dc = FALSE) {
  # check the input and get a clean data frame
  df <- check_eiger(formula, data, tree, n_eigenvectors)

  tree$root.edge <- 0

  # calculate eigenvectors of variance-covariance matrix
  cov_matrix <- ape::vcv(tree)
  n_tips <- length(tree$tip.label)
  if (dc) {
    C_N <- diag(n_tips) - 1 / n_tips * matrix(1, n_tips, n_tips)
    cov_matrix <- C_N %*% cov_matrix %*% C_N
  }
  vs <- eigen(cov_matrix)$vectors

  n_cols <- ncol(df)
  df[, (n_cols + 1):(n_tips + n_cols)] <- vs
  colnames(df)[(n_cols + 1):(n_tips + n_cols)] <- paste0("eigen_", 1:n_tips)
  df
}

#' Run eiger regression
#'
#' @description `run_eiger()` runs the eiger regression, which includes
#'   eigenvectors of the variance-covariance matrix of a phylogenetic tree as
#'   fixed effects in `phylolm`.
#'
#'   * If users have not called [prepare_eiger] for the regression, to build the regression model, users should provide `formula` and `data`.
#'   * If users have called [prepare_eiger] for the regression, users should provide both the output data frame from [prepare_eiger] as `data` argument and the original formula as `formula`.
#'
#' @inheritParams prepare_eiger
#' @param data A data frame of `n` rows, where `n` is the number of tips in
#'   `tree`. If `prepared` is `FALSE`, the data frame should contain the
#'   variables in the model. If `prepared`
#'   is `TRUE`, the data frame should be the output data frame from
#'   [prepare_eiger].
#' @param prepared A boolean. `TRUE` if the output data frame from
#'   [prepare_eiger] is used, otherwise `FALSE`. Default to `FALSE`.
#' @param intercept A boolean. `TRUE` if the intercept is included in the
#'   regression, otherwise `FALSE`. Default to `TRUE`.
#' @param ... Other parameters passed to [phylolm::phylolm].
#'
#' @returns The results of eiger regession, which is the same as the return
#'   values of [phylolm::phylolm]. Raises error if:
#'   * The input `tree` is not a valid object of class `"phylo"`,
#'   * The input `tree` does not have all branch lengths defined or have some of them being negative,
#'   * The input `tree` does not have all unique tip labels,
#'   * `n_eigenvectors` is greater than the number of tips in `tree` or less than 0,
#'   * When `prepared` is `FALSE`:
#'        * Some variable in `formula` can't be found in `data`,
#'        * Some variable in `formula` contains `NA` values,
#'        * The response variable in `formula` is a not vector of numeric values,
#'        * `data` is not provided as a matrix or a data frame,
#'        * The number of rows of `data` is not equal to the number of tips in `tree`,
#'        * The row names of `data` are equal to the tip labels of `tree`,
#'        * There are columns with no names in `data`,
#'        * There are redundant column names in `data`,
#'   * When `prepared` is `TRUE`:
#'        * `data` does not exist,
#'        * `data` does not contain all the variables in `formula`,
#'        * `data` does not have all the eigenvectors of the variance-covariance matrix,
#'   * `n_eigenvectors` is too large (even though it's valid),
#'   * Other problems when running [phylolm::phylolm].
#'
#' @examples
#' set.seed(42)
#' yule_tree <- TreeSim::sim.bd.taxa(100, 1, 1, 0, 1, complete = FALSE)[[1]]
#' BM <- phytools::fastBM(yule_tree, 1, nsim = 3)
#' x <- BM[, 1]
#' y <- BM[, 2]
#' z <- BM[, 3]
#'
#' BM_df <- as.data.frame(BM)
#' colnames(BM_df) <- c("x", "y", "z")
#' run_eiger(y ~ x, BM_df, yule_tree, 20)
#' run_eiger(y ~ x + log(z), BM_df, yule_tree, 20, dc = TRUE, intercept = FALSE)
#'
#' group <- c(rep("A", 40), rep("B", 60))
#' try(run_eiger(y ~ x + group, BM_df, yule_tree, 20))
#'
#' @export
run_eiger <- function(formula, data, tree, n_eigenvectors,
                      dc = FALSE, prepared = FALSE, intercept = TRUE, ...) {
  # compute or check the data frame
  if (!prepared) {
    df <- prepare_eiger(formula, data, tree, n_eigenvectors, dc)
  } else {
    # check if the data frame exists
    if (is.null(data)) {
      cli::cli_abort(c("Can't find the prepared data.",
                       "i" = "{.var prepared} is set to TRUE.",
                       "i" = "Have you forgotten to provide {.var data}?"))
    }
    # check if the data frame is clean
    df <- check_eiger(formula, data, tree, n_eigenvectors)
    if (!identical(df, data)) {
      cli::cli_abort(c("Can't run the regression with unprepared data.",
                       "i" = "{.var prepared} is set to TRUE.",
                       "i" = "Have you forgotten to run {.fun prepare_eiger} to prepare the data?"))
    }
    # check if the data frame contains eigenvectors
    n_tips <- length(tree$tip.label)
    eigen_names <- paste0("eigen_", 1:n_tips)
    df_colnames <- colnames(df)
    if (any(!(eigen_names %in% df_colnames))) {
      eigen_idx <- which(!(eigen_names %in% df_colnames))
      top_message <- "Can't find all the eigenvectors in the input data."
      bullet_prefix <- "Eigenvector "
      bullet_suffix <- " is not included in data."
      extra_message <- " more eigenvectors are not included in data."
      create_bullet_error(eigen_idx, top_message, bullet_prefix, bullet_suffix, extra_message)
    }
  }

  # modify root edge to 0 for phylolm::phylolm
  tree$root.edge <- 0

  # modify the formula to match with data frame preparation
  formula_str <- format(formula)

  # complete the regression formula
  if (n_eigenvectors >= 1) {
    eigen_names <- paste0("eigen_", 1:n_eigenvectors)
    formula_str_complete <- paste(c(formula_str, eigen_names), collapse = " + ")
  } else {
    formula_str_complete <- formula_str
  }
  if (!intercept) {
    formula_str_complete <- paste0(formula_str_complete, " - 1")
  }
  reg_formula <- stats::as.formula(formula_str_complete)

  # run the regression
  tryCatch({
    model <- phylolm::phylolm(reg_formula, data = df, phy = tree, ...)
    model
  }, error = function(e) {
    original_lines <- strsplit(conditionMessage(e), "\n")[[1]]
    real_lines <- original_lines[original_lines != ""]
    n_lines <- length(real_lines)
    real_lines <- stats::setNames(real_lines, rep("x", n_lines))
    new_message <- c(
      "Can't successfully run the regression.",
      real_lines
    )
    cli::cli_abort(new_message)
  })
}
