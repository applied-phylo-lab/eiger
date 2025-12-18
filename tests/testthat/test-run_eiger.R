test_that("check_redundant() passes for valid lists", {
  expect_silent(check_redundant(1:10, "The list contains redundant elements."))
  expect_silent(check_redundant(c("tip_3", "tip_6", "tip_10"),
                                "The list contains redundant elements."))
})

test_that("check_redundant() fails for invalid lists", {
  expect_error(check_redundant(paste0("tip_", c(1:19, 17)),
                               "The list contains redundant elements."),
               "redundant elements")
})

test_that("check_names_list() passes for valid names lists", {
  list_val <- paste0("Var_", 1:20)
  null_message <- paste0("Can't find names for the variables.")
  na_top_message <- paste0("Can't find the names for all the variables.")
  na_bullet_prefix <- "The name for variable "
  na_bullet_suffix <- " can't be found."
  na_extra_message <- paste0(" more variables do not have names.")
  redundant_message <- paste0("The variable names must all be unique.")
  expect_silent(check_names_list(list_val, null_message, na_top_message,
                                 na_bullet_prefix, na_bullet_suffix,
                                 na_extra_message, redundant_message))
})

test_that("check_names_list() passes for invalid names lists", {
  list_val <- NULL
  null_message <- paste0("Can't find names for the variables.")
  na_top_message <- paste0("Can't find the names for all the variables.")
  na_bullet_prefix <- "The name for variable "
  na_bullet_suffix <- " can't be found."
  na_extra_message <- paste0(" more variables do not have names.")
  redundant_message <- paste0("The variable names must all be unique.")
  expect_error(check_names_list(list_val, null_message, na_top_message,
                                   na_bullet_prefix, na_bullet_suffix,
                                   na_extra_message, redundant_message),
               null_message)

  list_val <- c(paste0("Var_", c(1:19)), NA)
  expect_error(check_names_list(list_val, null_message, na_top_message,
                                na_bullet_prefix, na_bullet_suffix,
                                na_extra_message, redundant_message),
               na_top_message)

  list_val <- c(paste0("Var_", c(1:19)), "")
  expect_error(check_names_list(list_val, null_message, na_top_message,
                                na_bullet_prefix, na_bullet_suffix,
                                na_extra_message, redundant_message),
               na_top_message)

  list_val <- paste0("Var_", c(1:12, 6, 14:20))
  expect_error(check_names_list(list_val, null_message, na_top_message,
                                na_bullet_prefix, na_bullet_suffix,
                                na_extra_message, redundant_message),
               redundant_message)
})

test_that("check_dataframe() passes for valid inputs", {
  tip_labels <- paste0("tip_", 1:20)
  df1 <- data.frame(x = 1:20, y = 21:40, z = 31:50, row.names = tip_labels)

  df1_result <- check_dataframe(df1, tip_labels, 20)
  expect_equal(df1_result, df1)
  df1_result <- check_dataframe(df1, tip_labels)
  expect_equal(df1_result, df1)
  df1_result <- check_dataframe(df1, tip_labels, 20)
  expect_equal(df1_result, df1)
  df1_result <- check_dataframe(df1, tip_labels)
  expect_equal(df1_result, df1)

  df2_rownames <- paste0("tip_", 20:1)
  df2 <- data.frame(x = 20:1, y = 40:21, z = 50:31, row.names = df2_rownames)
  df2_result <- check_dataframe(df2, tip_labels)
  expect_equal(df2_result, df1)

  x <- 1:20
  y <- 21:40
  z <- 31:50
  M1 <- matrix(c(x, y, z), nrow = 20, ncol = 3, byrow = FALSE,
               dimnames = list(tip_labels, c("x", "y", "z")))
  M1_result <- check_dataframe(M1, tip_labels)
  expect_equal(M1_result, df1)

  x <- 20:1
  y <- 40:21
  z <- 50:31
  M2_rownames <- df2_rownames
  M2 <- matrix(c(x, y, z), nrow = 20, ncol = 3, byrow = FALSE,
               dimnames = list(M2_rownames, c("x", "y", "z")))
  M2_result <- check_dataframe(M2, tip_labels)
  expect_equal(M2_result, df1)
})

test_that("check_dataframe() fails for invalid inputs", {
  tip_labels <- paste0("tip_", 1:20)
  x <- 1:20
  y <- 21:40
  z <- 31:50

  ls1 <- list(x = x, y = y, z = z)
  expect_error(check_dataframe(ls1, tip_labels), "class")

  M0 <- matrix(nrow = 0, ncol = 10)
  expect_error(check_dataframe(M0, tip_labels), "empty")
  df0 <- as.data.frame(M0)
  expect_error(check_dataframe(df0, tip_labels), "empty")

  M0 <- matrix(nrow = 20, ncol = 0)
  expect_error(check_dataframe(M0, tip_labels), "empty")
  df0 <- as.data.frame(M0)
  expect_error(check_dataframe(df0, tip_labels), "empty")

  M1 <- matrix(nrow = 30, ncol = 10)
  expect_error(check_dataframe(M1, tip_labels), "number of tips")
  expect_error(check_dataframe(M1, tip_labels, 20), "number of tips")
  df1 <- as.data.frame(M1)
  expect_error(check_dataframe(df1, tip_labels), "number of tips")
  expect_error(check_dataframe(df1, tip_labels, 20), "number of tips")

  wrong_labels <- paste0("tip_", c(16:25, 31:35, 5:1))
  M2 <- matrix(nrow = 20, ncol = 5)
  rownames(M2) <- wrong_labels
  expect_error(check_dataframe(M2, tip_labels), "match the tip labels")
  df2 <- as.data.frame(M2)
  expect_error(check_dataframe(df2, tip_labels, 20), "match the tip labels")

  M3 <- matrix(nrow = 20, ncol = 5)
  rownames(M3) <- tip_labels
  df3 <- as.data.frame(M3)
  colnames(df3) <- NULL
  expect_error(check_dataframe(df3, tip_labels), "find column names")

  colnames(M3) <- c(NA, "x", "w", "y", "z")
  expect_error(check_dataframe(M3, tip_labels), "all the columns")
  colnames(df3) <- colnames(M3)
  expect_error(check_dataframe(df3, tip_labels), "all the columns")

  colnames(df3) <- c("w", "x", "", "y", "z")
  expect_error(check_dataframe(df3, tip_labels), "all the columns")

  colnames(M3) <- c("a", "x", "z", "y", "z")
  expect_error(check_dataframe(M3, tip_labels, 20), "column.*unique")
  colnames(df3) <- colnames(M3)
  expect_error(check_dataframe(df3, tip_labels), "column.*unique")
})

test_that("check_column_na() passes for valid inputs", {
  df1 <- data.frame(x = 1:20, y = 21:40, z = 31:50, w = c(1:19, NA))
  expect_silent(check_column_na(df1, "x"))
  expect_silent(check_column_na(df1, "y"))
  expect_silent(check_column_na(df1, "z"))
})

test_that("check_column_na() fails for invalid inputs", {
  df1 <- data.frame(x = c(1:19, NA), y = c(rep(NA, 10), 31:40), z = 31:50)
  expect_error(check_column_na(df1, "x"), "NA values")
  expect_error(check_column_na(df1, "y"), "NA values")
})

test_that("check_numeric_response() passes for valid inputs", {
  x0 <- 1:20
  expect_silent(check_numeric_response(x0, "x"))
  x1 <- c(0.2, -0.6, 0.9, 3.12, 386.097, -88.8, 0, 0.1, 0, 0, 0)
  expect_silent(check_numeric_response(x1, "x"))
})

test_that("check_numeric_response() fails for invalid inputs", {
  x0 <- paste0("tip_", 1:20)
  expect_error(check_numeric_response(x0, "x"), "character")
  x1 <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)
  expect_error(check_numeric_response(x1, "x"), "logical")
  x2 <- c(list(x = 1), list(x = 2), list(x = 3), list(x = 5))
  expect_error(check_numeric_response(x2, "x"), "list")
})

test_that("check_eiger() passes for valid inputs", {
  yule_tree <- withr::with_seed(17, TreeSim::sim.bd.taxa(20, 1, 1, 0, 1, complete = FALSE)[[1]])
  BM <-  withr::with_seed(42, phytools::fastBM(yule_tree, 1, nsim = 3))
  BM_df1 <- as.data.frame(BM)
  colnames(BM_df1) <- c("x", "y", "z")
  df1 <- check_eiger(y ~ x + log(z), BM_df1, yule_tree, 10)
  expect_true(setequal(colnames(df1), c("x", "y", "z")))
  expect_equal(rownames(df1), yule_tree$tip.label)
  expect_equal(df1$x, as.vector(BM[, 1]))
  expect_equal(df1$y, as.vector(BM[, 2]))
  expect_equal(df1$z, as.vector(BM[, 3]))

  new_order <- paste0("t", 20:1)
  BM_df2 <- BM_df1[new_order,]
  df2 <- check_eiger(y ~ x + log(z) + I(x^2), BM_df2, yule_tree, 10)
  expect_true(setequal(colnames(df2), c("x", "y", "z")))
  expect_equal(rownames(df2), yule_tree$tip.label)
  expect_equal(df2$x, as.vector(BM[, 1]))
  expect_equal(df2$y, as.vector(BM[, 2]))
  expect_equal(df2$z, as.vector(BM[, 3]))
})

test_that("check_eiger() fails for invalid tree inputs", {
  df <- data.frame(x = 1:20, y = 21:40, z = 31:50, w = c(1:19, NA))

  broken_tree <- list(edge = matrix(1:4, 2, 2), tip.label = letters[1:3])
  expect_error(check_eiger(y ~ x, df, broken_tree, 2), "valid")

  class(broken_tree) <- "phylo"
  expect_error(check_eiger(y ~ x, df, broken_tree, 2), "valid")

  broken_tree <- list(edge = matrix(c(4L, 1L, 4L, 5L, 5L, 2L, 5L, 3L),
                                    4, 2, byrow = TRUE),
                      tip.label = letters[1:3],
                      Nnode = 2L)
  class(broken_tree) <- "phylo"
  expect_error(check_eiger(y ~ x, df, broken_tree, 2), "branch lengths")

  broken_tree$edge.length <- c(1, 2)
  expect_error(check_eiger(y ~ x, df, broken_tree, 2), "all")
  broken_tree$edge.length <- c(1, 2, NA, 1)
  expect_error(check_eiger(y ~ x, df, broken_tree, 2), "all")
  broken_tree$edge.length <- c(1, 2, -1, 1)
  expect_error(check_eiger(y ~ x, df, broken_tree, 2), "non-negative")

  broken_tree$edge.length <- c(1, 2, 3, 1)
  broken_tree$tip.label <- c("tip_1", "tip_2", "tip_1")
  expect_error(check_eiger(y ~ x, df, broken_tree, 2), "tip labels.*unique")
})

test_that("check_eiger() fails for invalid dimension inputs", {
  df <- data.frame(x = 1:20, y = 21:40, z = 31:50, w = c(1:19, NA))
  yule_tree <- withr::with_seed(17, TreeSim::sim.bd.taxa(20, 1, 1, 0, 1, complete = FALSE)[[1]])

  expect_error(check_eiger(y ~ x, df, yule_tree, 3.4), "dimensions must be integers")
  expect_error(check_eiger(y ~ x, df, yule_tree, 21), "dimensions must be between")
  expect_error(check_eiger(y ~ x, df, yule_tree, -5), "dimensions must be between")
})

test_that("check_eiger() fails for invalid data inputs", {
  yule_tree <- withr::with_seed(17, TreeSim::sim.bd.taxa(20, 1, 1, 0, 1, complete = FALSE)[[1]])

  expect_error(check_eiger(y ~ x, NULL, yule_tree, 5), "Can't find the data.")

  x <- 1:20
  y <- 21:40
  z <- 31:50
  tip_labels <- yule_tree$tip.label

  ls1 <- list(x = x, y = y, z = z)
  expect_error(check_eiger(y ~ x, ls1, yule_tree, 5), "class")

  M0 <- matrix(nrow = 0, ncol = 10)
  expect_error(check_eiger(y ~ x, M0, yule_tree, 5), "empty")
  df0 <- as.data.frame(M0)
  expect_error(check_eiger(y ~ x, df0, yule_tree, 5), "empty")
  M0 <- matrix(nrow = 20, ncol = 0)
  expect_error(check_eiger(y ~ x, M0, yule_tree, 5), "empty")
  df0 <- as.data.frame(M0)
  expect_error(check_eiger(y ~ x, df0, yule_tree, 5), "empty")

  M1 <- matrix(nrow = 30, ncol = 10)
  expect_error(check_eiger(y ~ x, M1, yule_tree, 5), "number of tips")
  df1 <- as.data.frame(M1)
  expect_error(check_eiger(y ~ x, df1, yule_tree, 5), "number of tips")

  wrong_labels <- paste0("tip_", c(16:25, 31:35, 5:1))
  M2 <- matrix(nrow = 20, ncol = 5)
  rownames(M2) <- wrong_labels
  expect_error(check_eiger(y ~ x, M2, yule_tree, 5), "match the tip labels")
  df2 <- as.data.frame(M2)
  expect_error(check_eiger(y ~ x, df2, yule_tree, 5), "match the tip labels")

  M3 <- matrix(nrow = 20, ncol = 5)
  rownames(M3) <- tip_labels
  df3 <- as.data.frame(M3)
  colnames(df3) <- NULL
  expect_error(check_eiger(y ~ x, df3, yule_tree, 5), "find column names")
  colnames(M3) <- c(NA, "x", "w", "y", "z")
  expect_error(check_eiger(y ~ x, M3, yule_tree, 5), "all the columns")
  colnames(df3) <- colnames(M3)
  expect_error(check_eiger(y ~ x, df3, yule_tree, 5), "all the columns")
  colnames(df3) <- c("w", "x", "", "y", "z")
  expect_error(check_eiger(y ~ x, df3, yule_tree, 5), "all the columns")
  colnames(M3) <- c("a", "x", "z", "y", "z")
  expect_error(check_eiger(y ~ x, M3, yule_tree, 5), "column.*unique")
  colnames(df3) <- colnames(M3)
  expect_error(check_eiger(y ~ x, df3, yule_tree, 5), "column.*unique")
})

test_that("check_eiger() fails for invalid formula and data inputs", {
  yule_tree <- withr::with_seed(17, TreeSim::sim.bd.taxa(20, 1, 1, 0, 1, complete = FALSE)[[1]])
  x <- 1:20
  y <- 21:40
  z <- 31:50
  w <- c(1:19, NA)
  group <- rep(c("A", "B"), each = 10)
  tip_labels <- yule_tree$tip.label

  M1 <- cbind(x, y, z, w, group)
  colnames(M1) <- c("x", "y", "z", "w", "group")
  rownames(M1) <- tip_labels
  expect_error(check_eiger(y ~ x + w, M1, yule_tree, 5), "Can't run regressions with NA values")
  expect_error(check_eiger(y ~ x + k, M1, yule_tree, 5), "Can't find variable")
  expect_error(check_eiger(group ~ y + z, M1, yule_tree, 5), "Response variable.*must be a numeric vector.")

  df1 <- as.data.frame(M1)
  expect_error(check_eiger(y ~ x + w, df1, yule_tree, 5), "Can't run regressions with NA values")
  expect_error(check_eiger(y ~ x + k, df1, yule_tree, 5), "Can't find variable")
  expect_error(check_eiger(group ~ y + z, df1, yule_tree, 5), "Response variable.*must be a numeric vector.")
})

test_that("prepare_eiger() passes for valid inputs for Felsenstein's worst case", {
  s1 <- ape::stree(10, type = "star")
  s2 <- ape::stree(10, type = "star")
  s1$tip.label <- paste0(s1$tip.label, "_1")
  s2$tip.label <- paste0(s2$tip.label, "_2")
  s1$edge.length <- rep(1, 10)
  s2$edge.length <- rep(1, 10)
  s1$root.edge <- 5
  s2$root.edge <- 5
  felsenstein_tree <- s1 + s2
  BM <- withr::with_seed(42, phytools::fastBM(felsenstein_tree, 1, nsim = 2))
  BM_df1 <- as.data.frame(BM)
  colnames(BM_df1) <- c("x", "y")
  df1_expected <- prepare_eiger(y ~ x, BM_df1, felsenstein_tree, 20)
  tol <- 1e-8
  for (i in 3:22) {
    v <- as.vector(df1_expected[, i])
    first_half <- v[1:10]
    second_half <- v[11:20]
    pattern_1 <- all(first_half == 0)
    pattern_2 <- all(second_half == 0)
    if (i == 3 || i == 4) {
      pattern_1 <- pattern_1 && diff(range(second_half)) < tol
      pattern_2 <- pattern_2 && diff(range(first_half)) < tol
    }
    expect_true(pattern_1 || pattern_2)
  }
})

test_that("prepare_eiger() passes for valid inputs for Yule tree", {
  yule_tree <- withr::with_seed(17, TreeSim::sim.bd.taxa(20, 1, 1, 0, 1, complete = FALSE)[[1]])
  BM <-  withr::with_seed(42, phytools::fastBM(yule_tree, 1, nsim = 3))
  BM_df2 <- as.data.frame(BM)
  colnames(BM_df2) <- c("x", "y", "z")
  df2 <- prepare_eiger(y ~ x + log(z), BM_df2, yule_tree, 10)
  expect_true(setequal(colnames(df2), c("x", "y", "z", paste0("eigen_", 1:20))))
  expect_equal(rownames(df2), yule_tree$tip.label)
  expect_equal(df2$x, as.vector(BM[, 1]))
  expect_equal(df2$y, as.vector(BM[, 2]))
  expect_equal(df2$z, as.vector(BM[, 3]))

  new_order <- paste0("t", 20:1)
  BM_df3 <- BM_df2[new_order,]
  df3 <- prepare_eiger(y ~ x + log(z) + I(x^2), BM_df3, yule_tree, 10)
  expect_true(setequal(colnames(df3), c("x", "y", "z", paste0("eigen_", 1:20))))
  expect_equal(rownames(df3), yule_tree$tip.label)
  expect_equal(df3$x, as.vector(BM[, 1]))
  expect_equal(df3$y, as.vector(BM[, 2]))
  expect_equal(df3$z, as.vector(BM[, 3]))
})

test_that("prepare_eiger() fails for invalid tree inputs", {
  df <- data.frame(x = 1:20, y = 21:40, z = 31:50, w = c(1:19, NA))

  broken_tree <- list(edge = matrix(1:4, 2, 2), tip.label = letters[1:3])
  expect_error(prepare_eiger(y ~ x, df, broken_tree, 2), "valid")
  class(broken_tree) <- "phylo"
  expect_error(prepare_eiger(y ~ x, df, broken_tree, 2), "valid")

  broken_tree <- list(edge = matrix(c(4L, 1L, 4L, 5L, 5L, 2L, 5L, 3L),
                                    4, 2, byrow = TRUE),
                      tip.label = letters[1:3],
                      Nnode = 2L)
  class(broken_tree) <- "phylo"
  expect_error(prepare_eiger(y ~ x, df, broken_tree, 2), "branch lengths")

  broken_tree$edge.length <- c(1, 2)
  expect_error(prepare_eiger(y ~ x, df, broken_tree, 2), "all")
  broken_tree$edge.length <- c(1, 2, NA, 1)
  expect_error(prepare_eiger(y ~ x, df, broken_tree, 2), "all")
  broken_tree$edge.length <- c(1, 2, -1, 1)
  expect_error(prepare_eiger(y ~ x, df, broken_tree, 2), "non-negative")

  broken_tree$edge.length <- c(1, 2, 3, 1)
  broken_tree$tip.label <- c("tip_1", "tip_2", "tip_1")
  expect_error(prepare_eiger(y ~ x, df, broken_tree, 2), "tip labels.*unique")
})

test_that("prepare_eiger() fails for invalid dimension inputs", {
  df <- data.frame(x = 1:20, y = 21:40, z = 31:50, w = c(1:19, NA))
  yule_tree <- withr::with_seed(17, TreeSim::sim.bd.taxa(20, 1, 1, 0, 1, complete = FALSE)[[1]])

  expect_error(prepare_eiger(y ~ x, df, yule_tree, 3.4), "dimensions must be integers")
  expect_error(prepare_eiger(y ~ x, df, yule_tree, 21), "dimensions must be between")
  expect_error(prepare_eiger(y ~ x, df, yule_tree, -5), "dimensions must be between")
})

test_that("prepare_eiger() fails for invalid data inputs", {
  yule_tree <- withr::with_seed(17, TreeSim::sim.bd.taxa(20, 1, 1, 0, 1, complete = FALSE)[[1]])

  expect_error(prepare_eiger(y ~ x, NULL, yule_tree, 5), "Can't find the data.")

  x <- 1:20
  y <- 21:40
  z <- 31:50
  tip_labels <- yule_tree$tip.label

  ls1 <- list(x = x, y = y, z = z)
  expect_error(prepare_eiger(y ~ x, ls1, yule_tree, 5), "class")

  M0 <- matrix(nrow = 0, ncol = 10)
  expect_error(prepare_eiger(y ~ x, M0, yule_tree, 5), "empty")
  df0 <- as.data.frame(M0)
  expect_error(prepare_eiger(y ~ x, df0, yule_tree, 5), "empty")
  M0 <- matrix(nrow = 20, ncol = 0)
  expect_error(prepare_eiger(y ~ x, M0, yule_tree, 5), "empty")
  df0 <- as.data.frame(M0)
  expect_error(prepare_eiger(y ~ x, df0, yule_tree, 5), "empty")

  M1 <- matrix(nrow = 30, ncol = 10)
  expect_error(prepare_eiger(y ~ x, M1, yule_tree, 5), "number of tips")
  df1 <- as.data.frame(M1)
  expect_error(prepare_eiger(y ~ x, df1, yule_tree, 5), "number of tips")

  wrong_labels <- paste0("tip_", c(16:25, 31:35, 5:1))
  M2 <- matrix(nrow = 20, ncol = 5)
  rownames(M2) <- wrong_labels
  expect_error(prepare_eiger(y ~ x, M2, yule_tree, 5), "match the tip labels")
  df2 <- as.data.frame(M2)
  expect_error(prepare_eiger(y ~ x, df2, yule_tree, 5), "match the tip labels")

  M3 <- matrix(nrow = 20, ncol = 5)
  rownames(M3) <- tip_labels
  df3 <- as.data.frame(M3)
  colnames(df3) <- NULL
  expect_error(prepare_eiger(y ~ x, df3, yule_tree, 5), "find column names")
  colnames(M3) <- c(NA, "x", "w", "y", "z")
  expect_error(prepare_eiger(y ~ x, M3, yule_tree, 5), "all the columns")
  colnames(df3) <- colnames(M3)
  expect_error(prepare_eiger(y ~ x, df3, yule_tree, 5), "all the columns")
  colnames(df3) <- c("w", "x", "", "y", "z")
  expect_error(prepare_eiger(y ~ x, df3, yule_tree, 5), "all the columns")
  colnames(M3) <- c("a", "x", "z", "y", "z")
  expect_error(prepare_eiger(y ~ x, M3, yule_tree, 5), "column.*unique")
  colnames(df3) <- colnames(M3)
  expect_error(prepare_eiger(y ~ x, df3, yule_tree, 5), "column.*unique")
})

test_that("prepare_eiger() fails for invalid formula and data inputs", {
  yule_tree <- withr::with_seed(17, TreeSim::sim.bd.taxa(20, 1, 1, 0, 1, complete = FALSE)[[1]])
  x <- 1:20
  y <- 21:40
  z <- 31:50
  w <- c(1:19, NA)
  group <- rep(c("A", "B"), each = 10)
  tip_labels <- yule_tree$tip.label

  M1 <- cbind(x, y, z, w, group)
  colnames(M1) <- c("x", "y", "z", "w", "group")
  rownames(M1) <- tip_labels
  expect_error(prepare_eiger(y ~ x + w, M1, yule_tree, 5), "Can't run regressions with NA values")
  expect_error(prepare_eiger(y ~ x + k, M1, yule_tree, 5), "Can't find variable")
  expect_error(prepare_eiger(group ~ y + z, M1, yule_tree, 5), "Response variable.*must be a numeric vector.")

  df1 <- as.data.frame(M1)
  expect_error(prepare_eiger(y ~ x + w, df1, yule_tree, 5), "Can't run regressions with NA values")
  expect_error(prepare_eiger(y ~ x + k, df1, yule_tree, 5), "Can't find variable")
  expect_error(prepare_eiger(group ~ y + z, df1, yule_tree, 5), "Response variable.*must be a numeric vector.")
})

test_that("run_eiger() passes for valid inputs for Felsenstein's worst case", {
  s1 <- ape::stree(10, type = "star")
  s2 <- ape::stree(10, type = "star")
  s1$tip.label <- paste0(s1$tip.label, "_1")
  s2$tip.label <- paste0(s2$tip.label, "_2")
  s1$edge.length <- rep(1, 10)
  s2$edge.length <- rep(1, 10)
  s1$root.edge <- 5
  s2$root.edge <- 5
  felsenstein_tree <- s1 + s2
  BM <- withr::with_seed(42, phytools::fastBM(felsenstein_tree, 1, nsim = 2))
  BM_df <- as.data.frame(BM)
  colnames(BM_df) <- c("x", "y")
  df <- prepare_eiger(y ~ x, BM_df, felsenstein_tree, 10)

  expect_no_error(results_1 <- run_eiger(y ~ x, df, felsenstein_tree, 1, prepared = TRUE))
  expect_no_error(results_2 <- run_eiger(y ~ x, BM_df, felsenstein_tree, 1))
  expect_equal(results_1[c(1:10, 22:23)], results_2[c(1:10, 22:23)])
  expect_no_error(results_3 <- run_eiger(y ~ x, df, felsenstein_tree, 0, prepared = TRUE))
  expect_no_error(results_4 <- run_eiger(y ~ x, BM_df, felsenstein_tree, 0))
  expect_equal(results_3[c(1:10, 22:23)], results_4[c(1:10, 22:23)])
  expect_no_error(results_5 <- run_eiger(y ~ x, df, felsenstein_tree, 2, prepared = TRUE, intercept = FALSE))
  expect_no_error(results_6 <- run_eiger(y ~ x, BM_df, felsenstein_tree, 2, intercept = FALSE))
  expect_equal(results_5[c(1:10, 22:23)], results_6[c(1:10, 22:23)])
})

test_that("run_eiger() passes for valid inputs for Yule tree", {
  yule_tree <- withr::with_seed(17, TreeSim::sim.bd.taxa(100, 1, 1, 0, 1, complete = FALSE)[[1]])
  yule_tree$root.edge <- 0
  BM <- withr::with_seed(17, phytools::fastBM(yule_tree, 1, nsim = 3))
  colnames(BM) <- c("x", "y", "z")
  df1 <- prepare_eiger(y ~ x, BM, yule_tree, 30)

  expect_no_error(results_1 <- run_eiger(y ~ x + z, df1, yule_tree, 30, prepared = TRUE))
  expect_no_error(results_2 <- run_eiger(y ~ x + z, BM, yule_tree, 30))
  expect_equal(results_1[c(1:10, 22:23)], results_2[c(1:10, 22:23)])
  expect_no_error(results_3 <- run_eiger(y ~ x + z + I(x^2) + log(abs(z)), df1, yule_tree, 30, prepared = TRUE, intercept = FALSE))
  expect_no_error(results_4 <- run_eiger(y ~ x + z + I(x^2) + log(abs(z)), BM, yule_tree, 30, intercept = FALSE))
  expect_equal(results_3[c(1:10, 22:23)], results_4[c(1:10, 22:23)])

  group <- rep(c("A", "B"), each = 50)
  BM_df <- as.data.frame(BM)
  BM_df$group <- group
  df2 <- prepare_eiger(y ~ x, BM_df, yule_tree, 30)
  expect_no_error(results_5 <- run_eiger(y ~ x + z + group, df2, yule_tree, 30, prepared = TRUE, intercept = FALSE))
  expect_no_error(results_6 <- run_eiger(y ~ x + z + group, BM_df, yule_tree, 30, intercept = FALSE))
  expect_equal(results_5[c(1:10, 22:23)], results_6[c(1:10, 22:23)])
})

test_that("run_eiger() fails for unprepared ot incorrectly prepared data when users claim to have prepared the data", {
  yule_tree <- withr::with_seed(17, TreeSim::sim.bd.taxa(100, 1, 1, 0, 1, complete = FALSE)[[1]])
  yule_tree$root.edge <- 0
  BM <- withr::with_seed(17, phytools::fastBM(yule_tree, 1, nsim = 3))
  colnames(BM) <- c("x", "y", "z")

  expect_error(run_eiger(y ~ x + z, NULL, yule_tree, 30, prepared = TRUE), "Can't find the prepared data.")

  new_order <- paste0("t", 100:1)
  BM_1 <- BM[new_order,]
  expect_error(run_eiger(y ~ x + z, BM_1, yule_tree, 30, prepared = TRUE), "Can't run the regression with unprepared data.")

  BM_df <- check_eiger(y ~ x + z, BM, yule_tree, 30)
  expect_error(run_eiger(y ~ x + z, BM_df, yule_tree, 30, prepared = TRUE), "Can't find all the eigenvectors in the input data.")
  BM_df <- prepare_eiger(y ~ x + z, BM, yule_tree, 30)
  BM_df <- BM_df[, -c(10:20, 50:60)]
  expect_error(run_eiger(y ~ x + z, BM_df, yule_tree, 30, prepared = TRUE), "Can't find all the eigenvectors in the input data.")
})

test_that("run_eiger() fails for too many eigenvectors", {
  yule_tree <- withr::with_seed(17, TreeSim::sim.bd.taxa(100, 1, 1, 0, 1, complete = FALSE)[[1]])
  yule_tree$root.edge <- 0
  BM <- withr::with_seed(17, phytools::fastBM(yule_tree, 1, nsim = 2))
  colnames(BM) <- c("x", "y")
  df <- prepare_eiger(y ~ x, BM, yule_tree, 95)
  expect_error(run_eiger(y ~ x, df, yule_tree, 95, prepared = TRUE), "successfully")
  expect_error(run_eiger(y ~ x, BM, yule_tree, 95), "successfully")
  expect_error(run_eiger(y ~ x, df, yule_tree, 100, prepared = TRUE, intercept = FALSE), "successfully")
  expect_error(run_eiger(y ~ x, BM, yule_tree, 100, intercept = FALSE), "successfully")
})
