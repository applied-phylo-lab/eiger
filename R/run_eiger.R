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

#' Check if the list can be coerced to a valid data frame
#'
#' `check_list()` checks if the list can be coerced to a valid data frame with
#' and modifies the names for the fields if the data frame is valid.
#'
#' @param list_val The list to check.
#' @param list_name The name of the list. Default to `NULL`.
#' @param is_data A boolean. `TRUE` if the list is the data argument used for
#'   the regression, otherwise `FALSE`. Default to `FALSE`.
#'
#' @returns The list with values of each field re-ordered and names imputed
#'   based on the first field in the list that has names if the list is valid.
#'   Raises error if:
#' * The list is empty,
#' * The fields in the list do not all have the same lengths,
#' * The list contains fields that do not have tags,
#' * The tags of the fields in the list are not all unique,
#' * The list contains fields that have NA values in names,
#' * The list contains fields that have redundant names.
#'   Raises warning if:
#' * There are fields in the list that have no names,
#'     * If none of the fields has names, the fields will remain having no names,
#'     * If only some of the fields have no names, these fields will take the names of the first field in the list that has names,
#' * The fields in the list do have have the same set of names, in which case the fields will all will take the names of the first field in the list that has names.
#'
#' @keywords internal
check_list <- function(list_val, list_name = NULL, is_data = FALSE) {
  if (is_data) {
    str1 <- "the data"
    str2 <- "The data"
  } else {
    str1 <- paste0("the data list ", list_name)
    str2 <- paste0("The data list ", list_name)
  }

  # check if the list is empty
  if (length(list_val) == 0) {
    cli::cli_abort(paste0(str2, " must not be empty."))
  }
  # check if the fields have the same lengths
  lengths_list <- sapply(list_val, length)
  if (length(unique(lengths_list)) != 1) {
    cli::cli_abort(paste0("Can't find a unique length for all the fields in ", str1, "."))
  }

  # check if there are fields with no tags or redundant tags
  field_tags <- names(list_val)
  null_message <- paste0("Can't find any tags for ", str1, ".")
  na_top_message <- paste0("Can't find the tags for all the fields in ", str2, ".")
  na_bullet_prefix <- "The tag for field "
  na_bullet_suffix <- " can't be found."
  na_extra_message <- paste0(" more fields in ", str2, " do not have tags.")
  redundant_message <- paste0("The tags of the fields in ", str1, " must all be unique.")
  check_names_list(field_tags, null_message, na_top_message, na_bullet_prefix,
                   na_bullet_suffix, na_extra_message, redundant_message)

  # check if there are fields with no names
  field_rownames_list <- lapply(list_val, names)
  field_rownames_null <- sapply(field_rownames_list, is.null)
  if (all(field_rownames_null)) {
    cli::cli_warn(c(paste0(str2, " contains no fields that have names."),
                    "i" = "The order of the values for each field must be the same as the order of tip labels of the tree."))
    return(list_val)
  } else {
    if (any(field_rownames_null)) {
      cli::cli_warn(c(paste0(str2, " contains fields that do not have names."),
                      "i" = paste0("The names for the first field of ", str1, " that has names will be used.")))
      first_not_null_idx <- which(!field_rownames_null)[1]
      first_not_null_rownames <- field_rownames_list[[first_not_null_idx]]
      for (i in which(field_rownames_null)) {
        names(list_val[[i]]) <- first_not_null_rownames
      }
    }
  }
  # check if there are fields with redundant names or NA in names
  n_fields <- length(field_tags)
  for (i in 1:n_fields) {
    field_tag <- field_tags[i]
    field_rownames <- field_rownames_list[[i]]
    if (any(is.na(field_rownames))) {
      cli::cli_abort(c(paste0("The names for each field in ", str1, " must not contain NA values."),
                       "x" = paste0("The names for field ", field_tag, " contains NA values.")))
    }
    error_message <- c(paste0("The names for each field in ", str1, " must all be unique."),
                       "x" = paste0("The names for field ", field_tag, " are not all unique."))
    check_redundant(field_rownames, error_message)
  }
  # check if all the fields have the same set of names
  if (n_fields >= 2) {
    field_rownames_list <- lapply(list_val, names)
    field_1_rownames <- field_rownames_list[[1]]
    field_rownames_all_same <- TRUE
    for (i in 2:n_fields) {
      field_tag <- field_tags[i]
      field_rownames <- field_rownames_list[[i]]
      field_rownames_same <- setequal(field_1_rownames, field_rownames)
      field_rownames_all_same <- field_rownames_all_same && field_rownames_same
      if (!field_rownames_same) {
        names(list_val[[i]]) <- field_1_rownames
      } else {
        list_val[[i]] <- list_val[[i]][field_1_rownames]
      }
    }
    if (!field_rownames_all_same) {
      cli::cli_warn(c(paste0(str2, " do not have the same names across different fields."),
                      "i" = paste0("The names for the first field of ", str1, " that has names will be used.")))
    }
  }
  list_val
}

#' Check if the data frame is compatible for eiger regression
#'
#' `check_dataframe()` checks if the data frame is compatible for eiger
#' regression and modifies the row orders if the data frame is valid.
#'
#' @param df_val The data frame to check.
#' @param tip_labels The tip labels of the phylogenetic tree for eiger
#'   regression.
#' @param df_name The name of the data frame. Default to `NULL`.
#' @param is_data A boolean. `TRUE` if the data frame is the data argument used
#'   for the regression, otherwise `FALSE`. Default to `FALSE`.
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
check_dataframe <- function(df_val, tip_labels, df_name = NULL, is_data = FALSE, n_tips = length(tip_labels)) {
  if (is_data) {
    str1 <- "the data"
    str2 <- "The data"
    str3 <- str1
  } else {
    str1 <- df_name
    str2 <- paste0("The data frame ", df_name)
    str3 <- paste0("the data frame ", df_name)
  }
  # deal with lists, as the names for each field can be tricky
  if (is.list(df_val) && identical(class(df_val), "list")) {
    df_val <- check_list(df_val, df_name, is_data)
  }
  # check if the data frame is a valid data frame
  tryCatch({
    df_val <- as.data.frame(df_val)
    df_colnames <- colnames(df_val)
    df_rownames <- rownames(df_val)
    df_n_rows <- nrow(df_val)
    df_n_cols <- ncol(df_val)
  }, error = function(e) {
    cli::cli_abort(paste0("Can't coerce ", str1, " to a data frame."))
  })
  # check if the data frame is empty
  if (df_n_rows == 0 || df_n_cols == 0) {
    cli::cli_abort(paste0(str2, " must not be empty."))
  }
  # check if the data frame has the same number of rows as the number of tips
  if (df_n_rows != n_tips) {
    cli::cli_abort(c(
      "The data frame must have the same number of rows as the number of tips in the tree.",
      "i" = paste0(str2, " has ", df_n_rows, " rows."),
      "i" = paste0("The tree has ", n_tips, " tips.")))
  }
  # check if the row names are the same as tip labels
  if (setequal(df_rownames, tip_labels)) {
    df_val <- df_val[tip_labels, ]
  } else {
    cli::cli_warn(c(paste0("The row names of ", str3, " do not match the tip labels of the tree."),
                    "i" = "The order of the rows must be the same as the order of tip labels of the tree."))
    rownames(df_val) <- tip_labels
  }

  # check if there are columns with no names or redundant names
  null_message <- paste0("Can't find column names for ", str3, ".")
  na_top_message <- paste0("Can't find the names for all the columns in ", str3, ".")
  na_bullet_prefix <- "The name for column "
  na_bullet_suffix <- " can't be found."
  na_extra_message <- paste0(" more columns in ", str3, " do not have names.")
  redundant_message <- paste0("The column names in ", str3, " must all be unique.")
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
#' @param df_name The name of the data frame. Default to `NULL`.
#' @param is_data A boolean. `TRUE` if the data frame is the data argument used
#'   for the regression, otherwise `FALSE`. Default to `FALSE`.
#'
#' @returns Invisible returns `NULL` if the column of the data frame does not
#'   contain any `NA` values. Otherwise raises an error.
#'
#' @keywords internal
check_column_na <- function(df_val, col, df_name = NULL, is_data = FALSE) {
  if (is_data) {
    str1 <- paste0(df_name, "$", col)
  } else {
    str1 <- paste0("the field ", col, " of the data")
  }
  # check if there are any NA values
  if (any(is.na(df_val[[col]]))) {
    cli::cli_abort(paste0("Can't run regressions with NA values in ", str1, "."))
  }
  invisible(NULL)
}

#' Check if the plain variable is compatible for eiger regression
#'
#' `check_plain()` checks if the plain variable is compatible for eiger
#' regression and modifies the order of the elements if the variable is valid.
#'
#' @param plain_val The variable to check.
#' @param plain The name of the variable.
#' @param tip_labels The tip labels of the phylogenetic tree for eiger
#'   regression.
#' @param n_tips The number of tips in the phylogenetic tree. Default to
#'   `length(tip_labels)`.
#'
#' @returns The plain variable with elements re-ordered based on `tip_labels` if
#'   the variable is valid. Raises error if:
#' * The variable does not have the same number of elements as the number of tips,
#' * The variable contains any `NA` values,
#' * There are redundant names for elements in the variable.
#'   Raises warning if:
#' * The names of the variable do not exist,
#' * The names of the variable do not match the tip labels of the tree.
#'
#' @keywords internal
check_plain <- function(plain_val, plain, tip_labels, n_tips = length(tip_labels)) {
  plain_len <- length(plain_val)
  # check if the plain has the same number of elements as the number of tips
  if (plain_len != n_tips) {
    cli::cli_abort(c(
      paste0("The variable ", plain, " must have the same number of elements as the number of tips in the tree."),
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
    error_message <- paste0("The names of variable ", plain, " must be all unique.")
    check_redundant(plain_names, error_message)
    # check if the names are the same as tip labels
    if (setequal(plain_names, tip_labels)) {
      plain_val <- plain_val[tip_labels]
    } else {
      cli::cli_warn(c(paste0("The variable ", plain, " do not match the tip labels of the tree."),
                      "i" = "The order of the variable values must be the same as the order of tip labels of the tree."))
    }
  }
  plain_val
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

#' Check if the data are compatible for eiger regression and clean the data
#'
#' @description `clean_eiger()` checks if the trait values, the phylogenetic
#'   tree, and the number of eigenvectors to include in the regression are
#'   compatible with each other and prepares a clean data frame for running
#'   eiger regression.
#'
#'   To build the regression model, users can either provide:
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
#'   rows are ordered according to the order of tip labels in `tree`. If `data`
#'   or any other data frame used is provided in terms of a list, the list has
#'   values of each field re-ordered and names imputed based on the first field
#'   in the list that has names before being coerced to data frame. No errors
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
#'             * All columns have names in the data frame,
#'             * There are no redundant column names in the data frame,
#'   * If `data` is supplied:
#'        * `data` can be coerced to a valid data frame,
#'        * The number of rows of `data` is equal to the number of tips in `tree`,
#'        * All columns have names in `data`,
#'        * There are no redundant column names in `data`,
#'   * When `data` or other data frames used are provided in terms of a list:
#'        * The list is not empty,
#'        * The fields in the list all have the same lengths,
#'        * All the fields in the list has tags,
#'        * The tags of the fields in the list are all unique,
#'        * The list does not contain fields that have NA values in names,
#'        * The list does not contain fields that have redundant names.
#'   Otherwise raises an error.
#'
#'   Raises a warning if:
#'   * `x` and `y` are used and either don't have names or have names not equal to the tip labels of `tree`,
#'   * The row names of `data` are not equal to the tip labels of `tree`,
#'   * A variable in `formula` is a vector found in the current environment and either don't have names or have names not equal to the tip labels of `tree`,
#'   * A variable in `formula` is column of a data frame found in the current environment and the row names of the data frame are not equal to the tip labels of `tree`.
#'   * When `data` or other data frames used are provided in terms of a list:
#'       * There are fields in the list that have no names,
#'           * If none of the fields has names, the fields will remain having no names,
#'           * If only some of the fields have no names, these fields will take the names of the first field in the list that has names,
#'       * The fields in the list do have have the same set of names, in which case the fields will all will take the names of the first field in the list that has names.
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
  error_message <- "The tip labels of the input tree must be all unique."
  check_redundant(tree$tip.label, error_message)

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
        df_val <- check_dataframe(df_val, tip_labels, df, FALSE, n_tips)
        df_colnames <- colnames(df_val)

        # check if the data frame has the variable as a column
        if (!field %in% df_colnames) {
          cli::cli_abort(paste0("Can't find column name ", field, " in data frame ", df, "."))
        }
        check_column_na(df_val, field, df, FALSE)
        var_name <- paste0(df, "_", field)
        output_df[[var_name]] <- df_val[[field]]
      }
    }

    # check the data
    if (!is.null(data)) {
      data <- check_dataframe(data, tip_labels, NULL, TRUE, n_tips)
      data_colnames <- colnames(data)
      output_df <- dplyr::bind_cols(data, output_df)
    }

    # check the plain variables
    for (plain in plains) {
      # plain variable exists in data
      if (!is.null(data) && plain %in% data_colnames) {
        check_column_na(data, plain, NULL, TRUE)
      } else {
        # plain variable exists in global env
        if (exists(plain, envir = env)) {
          plain_val <- get(plain, envir = env)
          plain_val <- check_plain(plain_val, plain, tip_labels, n_tips)
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
    check_numeric_response(output_df[[response]], response)
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

    x <- check_plain(x, "{.var x}", tip_labels, n_tips)
    y <- check_plain(y, "{.var y}", tip_labels, n_tips)
    check_numeric_response(y, "{.var y}")

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
#'   running eiger regression. The data frame includes all the original data
#'   provided for the regression as well as the eigenvectors of the
#'   variance-covariance matrix.
#'
#'   Users can either provide:
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
#'   tip labels in `tree`. If `data` or any other data frame used is provided in
#'   terms of a list, the list has values of each field re-ordered and names
#'   imputed based on the first field in the list that has names before being
#'   coerced to data frame. No errors raised if all the following requirements
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
#'             * All columns have names in the data frame,
#'             * There are no redundant column names in the data frame,
#'   * If `data` is supplied:
#'        * `data` can be coerced to a valid data frame,
#'        * The number of rows of `data` is equal to the number of tips in `tree`,
#'        * All columns have names in `data`,
#'        * There are no redundant column names in `data`,
#'   * When `data` or other data frames used are provided in terms of a list:
#'        * The list is not empty,
#'        * The fields in the list all have the same lengths,
#'        * All the fields in the list has tags,
#'        * The tags of the fields in the list are all unique,
#'        * The list does not contain fields that have NA values in names,
#'        * The list does not contain fields that have redundant names.
#'   Otherwise raises an error.
#'
#'   Raises a warning if:
#'   * `x` and `y` are used and either don't have names or have names not equal to the tip labels of `tree`,
#'   * The row names of `data` are not equal to the tip labels of `tree`,
#'   * A variable in `formula` is a vector found in the current environment and either don't have names or have names not equal to the tip labels of `tree`,
#'   * A variable in `formula` is column of a data frame found in the current environment and the row names of the data frame are not equal to the tip labels of `tree`.
#'   * When `data` or other data frames used are provided in terms of a list:
#'       * There are fields in the list that have no names,
#'           * If none of the fields has names, the fields will remain having no names,
#'           * If only some of the fields have no names, these fields will take the names of the first field in the list that has names,
#'       * The fields in the list do have have the same set of names, in which case the fields will all will take the names of the first field in the list that has names.
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
#'             * There are columns with no names in the data frame,
#'             * There are redundant column names in the data frame,
#'   * When `data` is supplied:
#'        * `data` can't be coerced to a valid data frame,
#'        * The number of rows of `data` is not equal to the number of tips in `tree`,
#'        * There are columns with no names in `data`,
#'        * There are redundant column names in `data`,
#'   * When `data` or other data frames used are provided in terms of a list:
#'        * The list is empty,
#'        * The fields in the list do not all have the same lengths,
#'        * Some fields in the list have no tags,
#'        * The tags of the fields in the list are not all unique,
#'        * The list contains fields that have NA values in names,
#'        * The list contains fields that have redundant names,
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
#'   * When `data` or other data frames used are provided in terms of a list:
#'       * There are fields in the list that have no names,
#'           * If none of the fields has names, the fields will remain having no names,
#'           * If only some of the fields have no names, these fields will take the names of the first field in the list that has names,
#'       * The fields in the list do have have the same set of names, in which case the fields will all will take the names of the first field in the list that has names.
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
