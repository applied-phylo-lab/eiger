#' Parse the formula into names for data frames, fields, and plain variables
#'
#' `parse_formula()` gets the names for data frames, fields, and plain variables
#' from a formula.
#'
#' @param formula A formula to parse.
#'
#' @returns A list containing three vectors: `dfs` for the names of data frames
#'   (before dollar signs), `fields` for the names of fields (after dollar
#'   signs), and `plains` for the names of plain variables (no dollar signs).
#'
#' @examples
#' \dontrun{
#' parse_formula(y ~ x)
#' parse_formula(y ~ x + 1)
#' parse_formula(y ~ x + log(z))
#' parse_formula(y ~ df1$x + df2$a + b)
#' parse_formula(y ~ df1$x + df2$x + x)
#' }
#'
#' @keywords internal
parse_formula <- function(formula) {
  # get the variables
  vars <- all.vars(formula, unique = FALSE)
  comps <- all.names(formula)

  # find fields of data frames
  dollar_idx <- which(comps == "$")
  df_idx <- dollar_idx + 1
  dfs <- comps[df_idx]
  field_idx <- dollar_idx + 2
  fields <- comps[field_idx]

  # find plain variables
  if (length(dollar_idx) == 0) {
    remaining <- comps
  } else {
    remaining <- comps[-c(dollar_idx, df_idx, field_idx)]
  }
  plains <- intersect(vars, remaining)

  list(dfs = dfs, fields = fields, plains = plains)
}

#' Check if the data are compatible for eiger regression and clean the data
#'
#' @description `clean_eiger()` checks if the trait values, the phylogenetic
#' tree, and the number of eigenvectors to include in the regression are
#' compatible with each other and prepares a clean data frame for running eiger
#' regression.
#'
#' To build the regression model, users can either provide:
#' * The predictor and outcome variables `x` and `y`.
#' * `formula` (and `data`).
#'
#' @param formula A formula describing the model to run eiger regression. If
#'   supplied, `x` and `y` are ignored. The eigenvectors should not appear in
#'   the formula. Default to `NULL`.
#' @param data A data frame containing the variables in the model. The data
#'   frame should have `n` rows, where `n` is the number of tips in `tree`. If
#'   not supplied or if variables are not found in `data`, the variables are
#'   taken from the current environment. Default to `NULL`.
#' @inheritParams check_phylo_branches
#' @param n_eigenvectors The number of eigenvectors to include as additional
#'   fixed effects in eiger regression.
#' @param x A vector representing the predictor variables of the `n` tips in the
#'   tree. Used only when `formula` is missing. Default to `NULL`.
#' @param y A vector representing the outcome variables of the `n` tips in the
#'   tree. The values must be numeric. Used only when `formula` is missing.
#'   Default to `NULL`.
#'
#' @returns A data frame of `n` rows, where `n` is the number of tips in `tree`.
#'   If `x` and `y` are used, the output data frame contains two columns: "X"
#'   and "Y"; if `formula` is used, the output data frame contains `(m + k)`
#'   columns, where `m` is the number of columns of `data` and `k` is the number
#'   of additional variables defined in `formula` but not found in `data`. The
#'   rows are ordered according to the order of tip labels in `tree`. No errors
#'   raised if all the following requirements are met:
#'   * The input `tree` is a valid object of class `"phylo"`,
#'   * The input `tree` has all branch lengths defined and non-negative,
#'   * The input `tree` has all unique tip labels,
#'   * `n_eigenvectors` is less than or equal to the number of tips in `tree` and greater than to equal to `0`,
#'   * If `x` and `y` are used:
#'        * `y` is a vector of numeric values,
#'        * The lengths of `x` and `y` are equal to the number of tips in `tree`,
#'        * `x` and `y` do not contain any `NA` values,
#'        * There are no redundant names in `x` and `y` if names exist,
#'   * If `formula` is used:
#'        * All the variables in `formula` can be found either in `data` or in the current environment,
#'        * The lengths of all the variables in `formula` are equal to the number of tips in `tree`,
#'        * None of the variables in `formula` contains any `NA` values,
#'        * The response variable in `formula` is a vector of numeric values,
#'        * If a variable in `formula` is in the current environment:
#'             * There are no redundant names in the variable if names exist,
#'        * If a variable in `formula` is a field of some other data frame:
#'             * The data frame exists in the current environment and the variable is a column of that data frame,
#'             * The number of rows of the data frame is equal to the number of tips in `tree`,
#'             * There are no redundant row names in the data frame if row names exist,
#'   * If `data` is supplied:
#'        * `data` can be coerced to a valid data frame,
#'        * The number of rows of `data` is equal to the number of tips in `tree`,
#'        * There are no redundant row names in `data` if row names exist.
#'   Otherwise raises an error.
#'
#'   Raises a warning if:
#'   * `x` and `y` are used and either don't have names or have names not equal to the tip labels of `tree`,
#'   * The row names of `data` are not equal to the tip labels of `tree`,
#'   * A variable in `formula` is a vector found in the current environment and either don't have names or have names not equal to the tip labels of `tree`,
#'   * A variable in `formula` is column of a data frame found in the current environment and the row names of the data frame are not equal to the tip labels of `tree`.
#'
#' @examples
#' \dontrun{
#' set.seed(42)
#' yule_tree <- TreeSim::sim.bd.taxa(100, 1, 1, 0, 1, complete = FALSE)[[1]]
#' BM <- phytools::fastBM(yule_tree, 1, nsim = 3)
#' x <- BM[, 1]
#' y <- BM[, 2]
#' z <- BM[, 3]
#'
#' BM_df <- as.data.frame(BM)
#' colnames(BM_df) <- c("x", "y", "z")
#' clean_eiger(y ~ x, BM_df, yule_tree, 20)
#' clean_eiger(y ~ x + log(z), BM_df, yule_tree, 20)
#'
#' group <- c(rep("A", 40), rep("B", 60))
#' suppressWarnings(clean_eiger(y ~ x + group, BM_df, yule_tree, 20))
#' suppressWarnings(clean_eiger(BM_df$y ~ BM_df$x + group, tree = yule_tree, n_eigenvectors = 20))
#'
#' suppressWarnings(clean_eiger(tree = yule_tree, n_eigenvectors = 20, x = x, y = y))
#' }
#'
#' @keywords internal
clean_eiger <- function(formula = NULL, data = NULL, tree, n_eigenvectors, x = NULL, y = NULL) {
  # check the tree
  check_phylo(tree)
  check_phylo_branches(tree)
  if (length(unique(tree$tip.label)) != length(tree$tip.label)) {
    cli::cli_abort("The tip labels of the input tree must be all unique.")
  }

  # check the dimensions
  tip_labels <- tree$tip.label
  n_tips <- length(tip_labels)
  check_dimensions(n_eigenvectors, 0, n_tips)

  # case 1: formula provided
  if (!is.null(formula)) {
    # get the environment
    env <- environment(formula)

    # get the variables
    vars <- all.vars(formula, unique = FALSE)
    parsed <- parse_formula(formula)
    dfs <- parsed$dfs
    fields <- parsed$fields
    plains <- parsed$plains

    output_df <- data.frame(matrix(nrow = n_tips, ncol = 0))

    # check the data frames
    if (length(dfs) > 0) {
      for (i in 1:length(dfs)) {
        df <- dfs[i]
        field <- fields[i]
        # check if the data frame exists
        if (!exists(df, envir = env)) {
          cli::cli_abort(paste0("Can't find data frame ", df, "."))
        }
        df_val <- get(df, envir = env)
        # check if the data frame is a valid data frame
        tryCatch({
          df_val <- as.data.frame(df_val)
          df_colnames <- colnames(df_val)
          df_rownames <- rownames(df_val)
          df_n_rows <- nrow(df_val)
        }, error = function(e) {
          cli::cli_abort(paste0("Can't coerce ", df, " to a data frame."))
        })
        # check if the data frame has the variable as a column
        if (!field %in% df_colnames) {
          cli::cli_abort(paste0("Can't find column name ", field, " in data frame ", df, "."))
        }
        # check if the data frame has the same number of rows as the number of tips
        if (df_n_rows != n_tips) {
          cli::cli_abort(c(
            "The data frame must have the same number of rows as the number of tips in the tree.",
            "i" = paste0("The data frame ", df, " has ", df_n_rows, " rows."),
            "i" = paste0("The tree has ", n_tips, " tips.")))
        }
        # check if there are any NA values
        if (any(is.na(df_val[[field]]))) {
          cli::cli_abort(paste0("Can't run regressions with NA values in ", df, "$", field, "."))
        }
        # check if there are redundant labels for rows in the data frame
        if (length(unique(df_rownames)) != length(df_rownames)) {
          cli::cli_abort(paste0("The row names of data frame ", df, " must be all unique."))
        }
        # check if the row names are the same as tip labels
        if (setequal(df_rownames, tip_labels)) {
          df_val <- df_val[tip_labels, ]
        } else {
           cli::cli_warn(c(paste0("The row names of data frame ", df, " do not match the tip labels of the tree."),
                          "i" = "The order of the rows must be the same as the order of tip labels of the tree."))
        }
        var_name <- paste0(df, "_", field)
        output_df[[var_name]] <- df_val[[field]]
      }
    }

    # check the data
    if (!is.null(data)) {
      # check if the data frame is a valid data frame
      tryCatch({
        data <- as.data.frame(data)
        data_colnames <- colnames(data)
        data_rownames <- rownames(data)
        data_n_rows <- nrow(data)
      }, error = function(e) {
        cli::cli_abort("Can't coerce the data to a data frame.")
      })
      # check if the data has the same number of rows as the number of tips
      if (data_n_rows != n_tips) {
        cli::cli_abort(c(
          "The data must have the same number of rows as the number of tips in the tree.",
          "i" = paste0("The data has ", data_n_rows, " rows."),
          "i" = paste0("The tree has ", n_tips, " tips.")))
      }
      # check if there are redundant labels for rows in the data=
      if (length(unique(data_rownames)) != length(data_rownames)) {
        cli::cli_abort(paste0("The row names of data must be all unique."))
      }
      # check if the row names are the same as tip labels
      if (setequal(data_rownames, tip_labels)) {
        data <- data[tip_labels, ]
      } else {
        cli::cli_warn(c(paste0("The row names of data do not match the tip labels of the tree."),
                        "i" = "The order of the rows must be the same as the order of tip labels of the tree."))
      }
    }

    output_df <- dplyr::bind_cols(data, output_df)

    # check the plain variables
    for (plain in plains) {
      # plain variable exists in data
      if (!is.null(data) && plain %in% data_colnames) {
        # check if there are any NA values
        if (any(is.na(data[[plain]]))) {
          cli::cli_abort(paste0("Can't run regressions with NA values in the field ", plain, " of the data."))
        }
      } else {
        # plain variable exists in global env
        if (exists(plain, envir = env)) {
          plain_val <- get(plain, envir = env)
          plain_len <- length(plain_val)
          # check if the plain has the same number of elements as the number of tips
          if (plain_len != n_tips) {
            cli::cli_abort(c(
              "The variable must have the same number of elements as the number of tips in the tree.",
              "i" = paste0("The variable ", plain, " has ", plain_len, " elements."),
              "i" = paste0("The tree has ", n_tips, " tips.")))
          }
          # check if there are any NA values
          if (any(is.na(plain_val))) {
            cli::cli_abort(paste0("Can't run regressions with NA values in variable ", plain_val, "."))
          }
          # check if there are names for the variable
          plain_names <- names(plain_val)
          if (is.null(plain_names)) {
            cli::cli_warn(c(paste0("The names of variable ", plain, " do not exist."),
                            "i" = "The order of the elements must be the same as the order of tip labels of the tree."))
          } else {
            # check if there are redundant names for the variable
            if (length(unique(plain_names)) != length(plain_names)) {
              cli::cli_abort(paste0("The names of variable ", plain, " must be all unique."))
            }
            # check if the names are the same as tip labels
            if (setequal(plain_names, tip_labels)) {
              plain_val <- plain_val[tip_labels]
            } else {
              cli::cli_warn(c(paste0("The variable ", plain, " do not have the same names as the tip labels of the tree."),
                              "i" = "The order of the variable values must be the same as the order of tip labels of the tree."))
            }
          }
          output_df[[plain]] <- plain_val
        } else {
          # plain variable does not exist
          cli::cli_abort(paste0("Can't find variable ", plain, "."))
        }}
    }

    output_df <- as.data.frame(output_df)
    rownames(output_df) <- tree$tip.label

    # find the response variable
    var_1 <- vars[1]
    var_2 <- vars[2]
    if (length(dfs) > 0 && var_1 == dfs[1] && var_2 == fields[1]) {
      response <- paste0(var_1, "_", var_2)
      response_name <- paste0(var_1, "$", var_2)
    } else {
      response <- var_1
      response_name <- var_1
    }

    # check if the response variable is numeric
    if (!is.numeric(output_df[[response]])) {
      cli::cli_abort(c(paste0("Response variable ", response_name, " must be a numeric vector."),
                       "x" = "You've supplied a {.cls {class(output_df[[response]])}} vector."))
    }
  } else {
    # case 2: formula not provided, only use x and y
    # check if both values are provided
    if (is.null(x)) {
      if (is.null(y)) {
        cli::cli_abort(c("Can't build the model for eiger regression.",
                         "i" = "No formula is supplied.",
                         "i" = "No values for {.var x} and {.var y} are supplied."))
      } else {
        cli::cli_abort(c("Can't build the model for eiger regression.",
                         "x" = "Can't find values for {.var x}."))
      }
    } else {
      if (is.null(y)) {
        cli::cli_abort(c("Can't build the model for eiger regression.",
                         "x" = "Can't find values for {.var y}."))
      }
    }

    # check if the response variable is numeric
    if (!is.numeric(y)) {
      cli::cli_abort(c("{.var y} must be a numeric vector.",
                       "x" = "You've supplied a {.cls {class(y)}} vector."))
    }
    # check if the variables have the same number of elements as the number of tips
    x_len <- length(x)
    y_len <- length(y)
    if (x_len != n_tips) {
      cli::cli_abort(c(
        "{.var x} must have the same number of elements as the number of tips in the tree.",
        "i" = paste0("Variable {.var x} has ", x_len, " elements."),
        "i" = paste0("The tree has ", n_tips, " tips.")))
    }
    if (y_len != n_tips) {
      cli::cli_abort(c(
        "{.var y} must have the same number of elements as the number of tips in the tree.",
        "i" = paste0("Variable {.var y} has ", y_len, " elements."),
        "i" = paste0("The tree has ", n_tips, " tips.")))
    }
    # check if there are any NA values
    if (any(is.na(x))) {
      cli::cli_abort("Can't run regressions with NA values in the variable {.var x}.")
    }
    if (any(is.na(y))) {
      cli::cli_abort("Can't run regressions with NA values in the variable {.var y}.")
    }
    # check if there are names for x and y
    x_names <- names(x)
    if (is.null(x_names)) {
      cli::cli_warn(c(paste0("The names of variable {.var x} do not exist."),
                      "i" = "The order of the elements must be the same as the order of tip labels of the tree."))
    } else {
      # check if there are redundant names for the variable
      if (length(unique(x_names)) != length(x_names)) {
        cli::cli_abort(paste0("The names of variable {.var x} must be all unique."))
      }
      # check if the names are the same as tip labels
      if (setequal(x_names, tip_labels)) {
        x <- x[tip_labels]
      } else {
        cli::cli_warn(c(paste0("The variable {.var x} do not have the same names as the tip labels of the tree."),
                        "i" = "The order of the variable values must be the same as the order of tip labels of the tree."))
      }
      }
    y_names <- names(y)
    if (is.null(y_names)) {
      cli::cli_warn(c(paste0("The names of variable {.var y} do not exist."),
                             "i" = "The order of the elements must be the same as the order of tip labels of the tree."))
    } else {
      # check if there are redundant names for the variable
      if (length(unique(y_names)) != length(y_names)) {
          cli::cli_abort(paste0("The names of variable {.var y} must be all unique."))
        }
      # check if the names are the same as tip labels
      if (setequal(y_names, tip_labels)) {
          y <- y[tip_labels]
        } else {
          cli::cli_warn(c(paste0("The variable {.var y} do not have the same names as the tip labels of the tree."),
                          "i" = "The order of the variable values must be the same as the order of tip labels of the tree."))
        }
    }
    output_df <- matrix(0, nrow = n_tips, ncol = 2)
    output_df[, 1] <- x
    output_df[, 2] <- y
    output_df <- as.data.frame(output_df)
    colnames(output_df) <- c("X", "Y")
    rownames(output_df) <- tree$tip.label
  }
  output_df
}

#' Prepare the data frame for running eiger regression
#'
#' @description `prepare_eiger()` constructs a data frame that is essential for
#' running eiger regression. The data frame includes all the original data
#' provided for the regression as well as the eigenvectors of the
#' variance-covariance matrix.
#'
#' Users can either provide:
#' * The predictor and outcome variables `x` and `y`.
#' * `formula` and `data`.
#'
#' @inheritParams clean_eiger
#' @param dc A boolean value. `TRUE` if the eigenvectors need to be calculated
#'   from double-centered variance-covariance matrix and `FALSE` otherwise.
#'   Default to `FALSE`.
#'
#' @returns A data frame of `n` rows, where `n` is the number of tips in `tree`.
#'   If `x` and `y` are used, the output data frame contains `(n + 2)` columns:
#'   the first two columns have names "X" and "Y", and the next `n` columns
#'   represent the `n` eigenvectors of the variance-covariance matrix; if
#'   `formula` is used, the output data frame contains `(m + k + n)` columns,
#'   where the first `m` columns are the columns in `data`, the next `k` columns
#'   are the additional variables defined in `formula` but not found in `data`,
#'   and the last `n` columns represent the `n` eigenvectors of the
#'   variance-covariance matrix. The rows are ordered according to the order of
#'   tip labels in `tree`. No errors raised if all the following requirements
#'   are met:
#'   * The input `tree` is a valid object of class `"phylo"`,
#'   * The input `tree` has all branch lengths defined and non-negative,
#'   * The input `tree` has all unique tip labels,
#'   * `n_eigenvectors` is less than or equal to the number of tips in `tree` and greater than to equal to `0`,
#'   * If `x` and `y` are used:
#'        * `y` is a vector of numeric values,
#'        * The lengths of `x` and `y` are equal to the number of tips in `tree`,
#'        * `x` and `y` do not contain any `NA` values,
#'        * There are no redundant names in `x` and `y` if names exist,
#'   * If `formula` is used:
#'        * All the variables in `formula` can be found either in `data` or in the current environment,
#'        * The lengths of all the variables in `formula` are equal to the number of tips in `tree`,
#'        * None of the variables in `formula` contains any `NA` values,
#'        * The response variable in `formula` is a vector of numeric values,
#'        * If a variable in `formula` is in the current environment:
#'             * There are no redundant names in the variable if names exist,
#'        * If a variable in `formula` is a field of some other data frame:
#'             * The data frame exists in the current environment and the variable is a column of that data frame,
#'             * The number of rows of the data frame is equal to the number of tips in `tree`,
#'             * There are no redundant row names in the data frame if row names exist,
#'   * If `data` is supplied:
#'        * `data` can be coerced to a valid data frame,
#'        * The number of rows of `data` is equal to the number of tips in `tree`,
#'        * There are no redundant row names in `data` if row names exist.
#'   Otherwise raises an error.
#'
#'   Raises a warning if:
#'   * `x` and `y` are used and either don't have names or have names not equal to the tip labels of `tree`,
#'   * The row names of `data` are not equal to the tip labels of `tree`,
#'   * A variable in `formula` is a vector found in the current environment and either don't have names or have names not equal to the tip labels of `tree`,
#'   * A variable in `formula` is column of a data frame found in the current environment and the row names of the data frame are not equal to the tip labels of `tree`.
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
#' prepare_eiger(y ~ x, BM_df, yule_tree, 20)
#' prepare_eiger(y ~ x + log(z), BM_df, yule_tree, 20, dc = TRUE)
#'
#' group <- c(rep("A", 40), rep("B", 60))
#' suppressWarnings(prepare_eiger(y ~ x + group, BM_df, yule_tree, 20))
#' suppressWarnings(prepare_eiger(BM_df$y ~ BM_df$x + group, tree = yule_tree, n_eigenvectors = 20))
#'
#' suppressWarnings(prepare_eiger(tree = yule_tree, n_eigenvectors = 20, x = x, y = y))
#'
#' @export
prepare_eiger <- function(formula = NULL, data = NULL, tree, n_eigenvectors, x = NULL, y = NULL, dc = FALSE) {
  # check the input and get a clean data frame
  df <- clean_eiger(formula, data, tree, n_eigenvectors, x, y)

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
#'   * If users have not called [prepare_eiger] for the regression, to build the regression model, users should provide one of the followings:
#'       * The predictor and outcome variables `x` and `y`.
#'       * `formula` (and `data`).
#'   * If users have called [prepare_eiger] for the regression:
#'       * If users used `x` and `y` for [prepare_eiger], users just need to provide the output data frame from [prepare_eiger] as `data` argument.
#'       * If users used `formula` (and `data`) for [prepare_eiger], users should provide both the output data frame from [prepare_eiger] as `data` argument and the original formula as `formula`.
#'
#' @inheritParams prepare_eiger
#' @param data A data frame of `n` rows, where `n` is the number of tips in
#'   `tree`. If `prepared` is `FALSE`, the data frame should contain the
#'   variables in the model. If not supplied or if variables are not found in
#'   `data`, the variables are taken from the current environment. If `prepared`
#'   is `TRUE`, the data frame should be the output data frame from
#'   [prepare_eiger]. Default to `NULL`.
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
#'   * When `prepared` is `FALSE` and `x` and `y` are used:
#'        * `y` is a not vector of numeric values,
#'        * The lengths of `x` and `y` are not equal to the number of tips in `tree`,
#'        * `x` and `y` contain `NA` values,
#'        * There are redundant names in `x` and `y` if names exist,
#'   * When `prepared` is `FALSE` and `formula` is used:
#'        * Some variable in `formula` can't be found either in `data` or in the current environment,
#'        * The length of some variable in `formula` is not equal to the number of tips in `tree`,
#'        * Some variable in `formula` contains `NA` values,
#'        * The response variable in `formula` is a not vector of numeric values,
#'        * When a variable in `formula` is in the current environment:
#'             * There are redundant names in the variable if names exist,
#'        * When a variable in `formula` is a field of some other data frame:
#'             * The data frame does not exist in the current environment or the variable is not a column of that data frame,
#'             * The number of rows of the data frame is not equal to the number of tips in `tree`,
#'             * There are redundant row names in the data frame if row names exist,
#'   * When `data` is supplied:
#'        * `data` can't be coerced to a valid data frame,
#'        * The number of rows of `data` is not equal to the number of tips in `tree`,
#'        * There are redundant row names in `data` if row names exist,
#'   * When `prepared` is `TRUE`:
#'        * `data` does not exist,
#'        * `data` does not contain all the variables in `formula`,
#'        * `data` does not have all the eigenvectors of the variance-covariance matrix,
#'   * `n_eigenvectors` is too large (even though it's valid),
#'   * Other problems when running [phylolm::phylolm].
#'
#'   Raises a warning if:
#'   * `x` and `y` are used and either don't have names or have names not equal to the tip labels of `tree`,
#'   * The row names of `data` are not equal to the tip labels of `tree`,
#'   * A variable in `formula` is a vector found in the current environment and either don't have names or have names not equal to the tip labels of `tree`,
#'   * A variable in `formula` is column of a data frame found in the current environment and the row names of the data frame are not equal to the tip labels of `tree`.
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
#' suppressWarnings(run_eiger(y ~ x + group, BM_df, yule_tree, 20))
#' suppressWarnings(run_eiger(BM_df$y ~ BM_df$x + group, tree = yule_tree, n_eigenvectors = 20))
#'
#' suppressWarnings(run_eiger(tree = yule_tree, n_eigenvectors = 20, x = x, y = y))
#'
#' suppressWarnings(eiger_df <- prepare_eiger(y ~ x + group, BM_df, yule_tree, 20))
#' run_eiger(y ~ x + group, eiger_df, yule_tree, 20, prepared = TRUE)
#'
#' @export
run_eiger <- function(formula = NULL, data = NULL, tree, n_eigenvectors, x = NULL, y = NULL,
                      dc = FALSE, prepared = FALSE, intercept = TRUE, ...) {
  # compute or check the data frame
  if (!prepared) {
    df <- prepare_eiger(formula, data, tree, n_eigenvectors, x, y, dc)
    # create the formula if needed
    if (is.null(formula)) {
      formula <- Y ~ X
    }
  } else {
    # check if the data frame exists
    if (is.null(data)) {
      cli::cli_abort(c("Can't find the prepared data.",
                       "i" = "{.var prepared} is set to TRUE.",
                       "i" = "Have you forgotten to provide {.var data}?"))
    }
    # create the formula if needed
    if (is.null(formula)) {
      formula <- Y ~ X
    }
    # check if the data frame is clean
    df <- clean_eiger(formula, data, tree, n_eigenvectors, x, y)
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
      n <- length(eigen_idx)
      listed <- utils::head(eigen_idx, 5)
      n_listed <- length(listed)

      bullets <- paste0("Eigenvector ", listed, " is not included in data.")
      bullets <- stats::setNames(bullets, rep("x", n_listed))

      if (n > 5) {
        n_extra <- n - 5
        extra_bullet <- paste0("... and ", n_extra, " more eigenvectors are not included in data.")
        bullets <- c(bullets, extra_bullet)
      }

      cli::cli_abort(c(
        "Can't find all the eigenvectors in the input data.",
        bullets))
    }
  }

  # modify root edge to 0 for phylolm::phylolm
  tree$root.edge <- 0

  # modify the formula to match with data frame preparation
  formula_str <- format(formula)
  formula_str_new <- stringr::str_replace_all(formula_str, "\\$", "_")
  formula_new <- stats::as.formula(formula_str_new)

  # complete the regression formula
  if (n_eigenvectors >= 1) {
    eigen_names <- paste0("eigen_", 1:n_eigenvectors)
    formula_str_complete <- paste(c(formula_str_new, eigen_names), collapse = " + ")
  } else {
    formula_str_complete <- formula_str_new
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
