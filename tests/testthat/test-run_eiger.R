test_that("parse_formula() passes for valid formulas", {
  f1 <- parse_formula(y ~ x)
  expect_length(f1$dfs, 0)
  expect_length(f1$fields, 0)
  expect_true(setequal(f1$plains, c("x", "y")))

  f2 <- parse_formula(y ~ x + log(z) + 1)
  expect_length(f2$dfs, 0)
  expect_length(f2$fields, 0)
  expect_true(setequal(f2$plains, c("x", "y", "z")))

  f3 <- parse_formula(y ~ x + I(x^2) + exp(k))
  expect_length(f3$dfs, 0)
  expect_length(f3$fields, 0)
  expect_true(setequal(f3$plains, c("x", "y", "k")))

  f4 <- parse_formula(y ~ df1$x + df2$a + b)
  expect_equal(f4$dfs, c("df1", "df2"))
  expect_equal(f4$fields, c("x", "a"))
  expect_true(setequal(f4$plains, c("y", "b")))

  f5 <- parse_formula(df1$y ~ df1$x + df2$x + df2$y + x + y)
  expect_equal(f5$dfs, c("df1", "df1", "df2", "df2"))
  expect_equal(f5$fields, c("y", "x", "x", "y"))
  expect_true(setequal(f5$plains, c("x", "y")))

  f6 <- parse_formula(df1$y ~ log(df1$x) + I(df2$x^2) + I + log + I:log + y + 1)
  expect_equal(f6$dfs, c("df1", "df1", "df2"))
  expect_equal(f6$fields, c("y", "x", "x"))
  expect_true(setequal(f6$plains, c("I", "y", "log")))
})

test_that("check_list() passes for good inputs", {
  x <- 1:20
  names(x) <- paste0("tip_", 1:20)
  y <- 21:40
  names(y) <- paste0("tip_", 1:20)
  z <- 31:50
  names(z) <- paste0("tip_", 1:20)
  ls1 <- list(x = x, y = y, z = z)
  ls1_result <- check_list(ls1, "ls1", FALSE)
  expect_equal(ls1_result, ls1)

  ls2 <- list(x = x)
  ls2_result <- check_list(ls2, NULL, TRUE)
  expect_equal(ls2_result, ls2)

  y <- 40:21
  names(y) <- paste0("tip_", 20:1)
  z <- withr::with_seed(42, sample(z))
  names(z) <- paste0("tip_", (z - 30))
  ls3 <- list(x = x, y = y, z = z)
  ls3_result <- check_list(ls3, "ls3", FALSE)
  expect_equal(ls3_result, ls1)
})

test_that("check_list() passes for bad but valid inputs", {
  x <- 1:20
  y <- 21:40
  z <- 31:50
  ls1 <- list(x = x, y = y, z = z)
  expect_warning(ls1_result <- check_list(ls1, "ls1", FALSE), "no fields")
  expect_equal(ls1_result, ls1)

  x_named <- x
  names(x_named) <- paste0("tip_", 1:20)
  y_named <- y
  names(y_named) <- paste0("tip_", 1:20)
  z_named <- z
  names(z_named) <- paste0("tip_", 1:20)
  ls2 <- list(x = x_named, y = y_named, z = z_named)

  names(y) <- paste0("tip_", 1:20)
  ls3 <- list(x = x, y = y, z = z)
  expect_warning(ls3_result <- check_list(ls3, "ls3", FALSE), "not have names")
  expect_equal(ls3_result, ls2)

  z <- withr::with_seed(17, sample(z))
  names(z) <- paste0("tip_", (z - 30))
  ls4 <- list(x = x, y = y, z = z)
  expect_warning(ls4_result <- check_list(ls4, "ls4"), "not have names")
  expect_equal(ls4_result, ls2)

  names(x) <- paste0("tip_", 1:20)
  names(y) <- paste0("tip_", z)
  ls5 <- list(x = x, y = y, z = z)
  expect_warning(ls5_result <- check_list(ls5, NULL, TRUE), "have the same names")
  expect_equal(ls5_result, ls2)

  names(x) <- NULL
  names(y) <- paste0("tip_", 1:20)
  z <- 31:50
  names(z) <- paste0("tip_", z)
  ls6 <- list(x = x, y = y, z = z)
  expect_snapshot(ls6_result <- check_list(ls6, "ls6"))
  expect_equal(ls6_result, ls2)
})

test_that("check_list() fails for invalid inputs", {
  ls0 <- list()
  expect_error(check_list(ls0, "ls0", FALSE), "empty")

  x <- 1:10
  y <- 1:20
  ls1 <- list(x = x, y = y)
  expect_error(check_list(ls1, "ls1"), "unique length")

  x <- 11:30
  ls2 <- list(x, y)
  expect_error(check_list(ls2, NULL, TRUE), "find any tags")
  ls3 <- list(x = x, y)
  expect_error(check_list(ls3, NULL, TRUE), "find the tags")

  ls4 <- list(x = x, x = y)
  expect_error(check_list(ls4, NULL, TRUE), "tags.*unique")

  names(x) <- paste0("tip_", 1:20)
  names(y) <- paste0("tip_", c(1:18, 4, 9))
  ls5 <- list(x = x, y = y)
  expect_error(check_list(ls5, "ls5", FALSE), "names.*unique")
  names(x) <- paste0("tip_", c(1:19, 17))
  ls6 <- list(x = x, y = y)
  expect_error(check_list(ls6, "ls6", FALSE), "names.*unique")

  names(x) <- paste0("tip_", 1:20)
  names(y) <- c(paste0("tip_", c(1:19)), NA)
  ls7 <- list(x = x, y = y)
  expect_error(check_list(ls7, "ls7", FALSE), "NA")

  names(x) <- c(NA, paste0("tip_", 2:20))
  ls8 <- list(x = x, y = y)
  expect_error(check_list(ls8, "ls8"), "NA")
})

test_that("check_dataframe() passes for good inputs", {
  tip_labels <- paste0("tip_", 1:20)
  df1 <- data.frame(x = 1:20, y = 21:40, z = 31:50, row.names = tip_labels)

  df1_result <- check_dataframe(df1, tip_labels, "df1", FALSE, 20)
  expect_equal(df1_result, df1)
  df1_result <- check_dataframe(df1, tip_labels, "df1", FALSE)
  expect_equal(df1_result, df1)
  df1_result <- check_dataframe(df1, tip_labels, NULL, TRUE, 20)
  expect_equal(df1_result, df1)
  df1_result <- check_dataframe(df1, tip_labels, NULL, TRUE)
  expect_equal(df1_result, df1)

  df2_rownames <- paste0("tip_", 20:1)
  df2 <- data.frame(x = 20:1, y = 40:21, z = 50:31, row.names = df2_rownames)
  df2_result <- check_dataframe(df2, tip_labels, "df2", FALSE)
  expect_equal(df2_result, df1)

  x <- 1:20
  names(x) <- tip_labels
  y <- 21:40
  names(y) <- tip_labels
  z <- 31:50
  names(z) <- tip_labels
  ls1 <- list(x = x, y = y, z = z)
  ls1_result <- check_dataframe(ls1, tip_labels, "ls1", FALSE)
  expect_equal(ls1_result, df1)

  y <- 40:21
  names(y) <- paste0("tip_", 20:1)
  z <- withr::with_seed(42, sample(z))
  names(z) <- paste0("tip_", (z - 30))
  ls2 <- list(x = x, y = y, z = z)
  ls2_result <- check_dataframe(ls2, tip_labels, "ls2")
  expect_equal(ls2_result, df1)

  y <- 21:40
  z <- 31:50
  M1 <- matrix(c(x, y, z), nrow = 20, ncol = 3, byrow = FALSE,
               dimnames = list(tip_labels, c("x", "y", "z")))
  M1_result <- check_dataframe(M1, tip_labels, "M1")
  expect_equal(M1_result, df1)
})

test_that("check_dataframe() passes for bad but valid inputs", {
  tip_labels <- paste0("tip_", 1:20)
  df1 <- data.frame(x = 1:20, y = 21:40, z = 31:50, row.names = tip_labels)

  df2_rownames <- paste0("tip_", 21:40)
  df2 <- data.frame(x = 1:20, y = 21:40, z = 31:50, row.names = df2_rownames)
  expect_warning(df2_result <- check_dataframe(df2, tip_labels, "df2", FALSE), "match the tip labels")
  expect_equal(df2_result, df1)

  df3 <- data.frame(x = 1:20, y = 21:40, z = 31:50)
  expect_warning(df3_result <- check_dataframe(df3, tip_labels, "df3"), "match the tip labels")
  expect_equal(df3_result, df1)

  x <- 1:20
  y <- 21:40
  z <- 31:50
  names(y) <- tip_labels
  z <- withr::with_seed(17, sample(z))
  names(z) <- paste0("tip_", (z - 30))
  ls1 <- list(x = x, y = y, z = z)
  expect_warning(ls1_result <- check_dataframe(ls1, tip_labels, "ls1"), "not have names")
  expect_equal(ls1_result, df1)

  names(x) <- paste0("tip_", 1:20)
  names(y) <- paste0("tip_", z)
  ls2 <- list(x = x, y = y, z = z)
  expect_warning(ls2_result <- check_dataframe(ls2, tip_labels, "ls2"), "have the same names")
  expect_equal(ls2_result, df1)

  M1 <- matrix(c(1:20, 21:40, 31:50), nrow = 20, ncol = 3, byrow = FALSE)
  colnames(M1) <- c("x", "y", "z")
  expect_warning(M1_result <- check_dataframe(M1, tip_labels, NULL, TRUE), "match the tip labels")
  expect_equal(M1_result, df1)
})

test_that("check_dataframe() fails for invalid inputs", {
  tip_labels <- paste0("tip_", 1:20)
  x <- 1:20
  y <- 21:40
  z <- 31:50

  ls1 <- list(x, y = y)
  expect_error(check_dataframe(ls1, tip_labels, NULL, TRUE), "find the tags")

  M0 <- matrix(nrow = 0, ncol = 10)
  expect_error(check_dataframe(M0, tip_labels, "M0", FALSE), "empty")
  M0 <- matrix(nrow = 20, ncol = 0)
  expect_error(check_dataframe(M0, tip_labels, "M0", FALSE), "empty")

  M1 <- matrix(nrow = 30, ncol = 10)
  expect_error(check_dataframe(M1, tip_labels, "M1", FALSE), "number of tips")
  expect_error(check_dataframe(M1, tip_labels, "M1", FALSE, 20), "number of tips")

  M2 <- matrix(nrow = 20, ncol = 5)
  rownames(M2) <- tip_labels
  df1 <- as.data.frame(M2)
  colnames(df1) <- NULL
  expect_error(check_dataframe(df1, tip_labels, "df1", FALSE), "find column names")

  colnames(df1) <- c(NA, "x", "", "y", "z")
  expect_error(check_dataframe(df1, tip_labels, "df1", FALSE), "all the columns")

  colnames(df1) <- c("a", "x", "z", "y", "z")
  expect_error(check_dataframe(df1, tip_labels, "df1", FALSE), "column.*unique")

})


test_that("clean_eiger() passes for good inputs", {
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
  x <- BM[, 1]
  y <- BM[, 2]

  df <- clean_eiger(tree = felsenstein_tree, n_eigenvectors = 5, x = x, y = y)
  expect_true(setequal(colnames(df), c("X", "Y")))
  expect_equal(rownames(df), felsenstein_tree$tip.label)
  expect_equal(df$X, as.vector(x))
  expect_equal(df$Y, as.vector(y))

  yule_tree <- withr::with_seed(17, TreeSim::sim.bd.taxa(20, 1, 1, 0, 1, complete = FALSE)[[1]])
  BM <-  withr::with_seed(42, phytools::fastBM(yule_tree, 1, nsim = 3))
  BM_df <- as.data.frame(BM)
  colnames(BM_df) <- c("x", "y", "z")
  df <- clean_eiger(y ~ x + log(z), BM_df, yule_tree, 10)
  expect_true(setequal(colnames(df), c("x", "y", "z")))
  expect_equal(rownames(df), yule_tree$tip.label)
  expect_equal(df$x, as.vector(BM[, 1]))
  expect_equal(df$y, as.vector(BM[, 2]))
  expect_equal(df$z, as.vector(BM[, 3]))

  group <- c(rep("A", 10), rep("B", 10))
  names(group) <- yule_tree$tip.label
  df <- clean_eiger(y ~ x + group, BM_df, yule_tree, 10)
  expect_true(setequal(colnames(df), c("x", "y", "z", "group")))
  expect_equal(rownames(df), yule_tree$tip.label)
  expect_equal(df$group, as.vector(group))

  df <- clean_eiger(BM_df$y ~ BM_df$x + group, tree = yule_tree, n_eigenvectors = 10)
  expect_true(setequal(colnames(df), c("BM_df_y", "BM_df_x", "group")))
  expect_equal(rownames(df), yule_tree$tip.label)
  expect_equal(df$BM_df_x, as.vector(BM[, 1]))
  expect_equal(df$BM_df_y, as.vector(BM[, 2]))
  expect_equal(df$group, as.vector(group))

  df_extra <- as.data.frame(matrix(1:40, 20, 2, byrow = TRUE))
  colnames(df_extra) <- c("x", "y")
  rownames(df_extra) <- yule_tree$tip.label
  df <- clean_eiger(y ~ x + I(df_extra$y^2) + df_extra$x - 1, BM_df, tree = yule_tree, n_eigenvectors = 3)
  expect_true(setequal(colnames(df), c("y", "x", "z", "df_extra_y", "df_extra_x")))
  expect_equal(rownames(df), yule_tree$tip.label)
  expect_equal(df$x, as.vector(BM[, 1]))
  expect_equal(df$y, as.vector(BM[, 2]))
  expect_equal(df$df_extra_x, as.vector(df_extra$x))
  expect_equal(df$df_extra_y, as.vector(df_extra$y))

  df <- clean_eiger(y ~ x + df_extra$x + group, BM_df, tree = yule_tree, n_eigenvectors = 3)
  expect_true(setequal(colnames(df), c("y", "x", "z", "df_extra_x", "group")))
  expect_equal(rownames(df), yule_tree$tip.label)
  expect_equal(df$x, as.vector(BM[, 1]))
  expect_equal(df$y, as.vector(BM[, 2]))
  expect_equal(df$df_extra_x, as.vector(df_extra$x))
  expect_equal(df$group, as.vector(group))
})

test_that("clean_eiger() passes and raises warnings for valid but bad inputs", {
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
  x <- BM[, 1]
  y <- BM[, 2]
  names(x) <- NULL
  expect_warning(df <- clean_eiger(tree = felsenstein_tree, n_eigenvectors = 5, x = x, y = y), "exist")
  expect_true(setequal(colnames(df), c("X", "Y")))
  expect_equal(rownames(df), felsenstein_tree$tip.label)
  expect_equal(df$X, as.vector(x))
  expect_equal(df$Y, as.vector(y))

  names(y) <- NULL
  expect_snapshot(df <- clean_eiger(tree = felsenstein_tree, n_eigenvectors = 5, x = x, y = y))
  expect_true(setequal(colnames(df), c("X", "Y")))
  expect_equal(rownames(df), felsenstein_tree$tip.label)
  expect_equal(df$X, as.vector(x))
  expect_equal(df$Y, as.vector(y))

  names(x) <- 1:20
  expect_snapshot(df <- clean_eiger(tree = felsenstein_tree, n_eigenvectors = 5, x = x, y = y))
  expect_true(setequal(colnames(df), c("X", "Y")))
  expect_equal(rownames(df), felsenstein_tree$tip.label)
  expect_equal(df$X, as.vector(x))
  expect_equal(df$Y, as.vector(y))

  names(y) <- 1:20
  expect_snapshot(df <- clean_eiger(tree = felsenstein_tree, n_eigenvectors = 5, x = x, y = y))
  expect_true(setequal(colnames(df), c("X", "Y")))
  expect_equal(rownames(df), felsenstein_tree$tip.label)
  expect_equal(df$X, as.vector(x))
  expect_equal(df$Y, as.vector(y))

  yule_tree <- withr::with_seed(17, TreeSim::sim.bd.taxa(20, 1, 1, 0, 1, complete = FALSE)[[1]])
  BM <-  withr::with_seed(42, phytools::fastBM(yule_tree, 1, nsim = 3))
  BM_df <- as.data.frame(BM)
  colnames(BM_df) <- c("x", "y", "z")

  group <- c(rep("A", 10), rep("B", 10))
  expect_warning(df <- clean_eiger(y ~ x + group, BM_df, yule_tree, 10), "exist")
  expect_true(setequal(colnames(df), c("x", "y", "z", "group")))
  expect_equal(rownames(df), yule_tree$tip.label)
  expect_equal(df$group, group)

  expect_warning(df <- clean_eiger(BM_df$y ~ BM_df$x + group, tree = yule_tree, n_eigenvectors = 10), "exist")
  expect_true(setequal(colnames(df), c("BM_df_y", "BM_df_x", "group")))
  expect_equal(rownames(df), yule_tree$tip.label)
  expect_equal(df$BM_df_x, as.vector(BM[, 1]))
  expect_equal(df$BM_df_y, as.vector(BM[, 2]))
  expect_equal(df$group, group)

  df_extra <- as.data.frame(matrix(1:40, 20, 2, byrow = TRUE))
  colnames(df_extra) <- c("x", "y")
  expect_snapshot(df <- clean_eiger(y ~ x + I(df_extra$y^2) + df_extra$x - 1, BM_df, tree = yule_tree, n_eigenvectors = 3))
  expect_true(setequal(colnames(df), c("y", "x", "z", "df_extra_y", "df_extra_x")))
  expect_equal(rownames(df), yule_tree$tip.label)
  expect_equal(df$x, as.vector(BM[, 1]))
  expect_equal(df$y, as.vector(BM[, 2]))
  expect_equal(df$df_extra_x, as.vector(df_extra$x))
  expect_equal(df$df_extra_y, as.vector(df_extra$y))

  expect_snapshot(df <- clean_eiger(y ~ x + df_extra$x + group, BM_df, tree = yule_tree, n_eigenvectors = 3))
  expect_true(setequal(colnames(df), c("y", "x", "z", "df_extra_x", "group")))
  expect_equal(rownames(df), yule_tree$tip.label)
  expect_equal(df$x, as.vector(BM[, 1]))
  expect_equal(df$y, as.vector(BM[, 2]))
  expect_equal(df$df_extra_x, as.vector(df_extra$x))
  expect_equal(df$group, group)

  names(group) <- 1:20
  expect_warning(df <- clean_eiger(y ~ x + group, BM_df, yule_tree, 10), "match")
  expect_true(setequal(colnames(df), c("x", "y", "z", "group")))
  expect_equal(rownames(df), yule_tree$tip.label)
  expect_equal(df$group, as.vector(group))

  rownames(BM_df) <- 1:20
  expect_warning(df <- clean_eiger(y ~ x, BM_df, yule_tree, 10), "match")
  expect_true(setequal(colnames(df), c("x", "y", "z")))
  expect_equal(rownames(df), yule_tree$tip.label)
})

test_that("clean_eiger() fails for invalid inputs", {
  s1 <- ape::stree(10, type = "star")
  s2 <- ape::stree(10, type = "star")
  s1$edge.length <- rep(1, 10)
  s2$edge.length <- rep(1, 10)
  s1$root.edge <- 5
  s2$root.edge <- 5
  felsenstein_tree <- s1 + s2
  BM <- withr::with_seed(42, phytools::fastBM(felsenstein_tree, 1, nsim = 2))
  x <- BM[, 1]
  y <- BM[, 2]
  expect_error(clean_eiger(tree = felsenstein_tree, n_eigenvectors = 5, x = x, y = y), "tip labels")

  yule_tree <- withr::with_seed(17, TreeSim::sim.bd.taxa(10, 1, 1, 0, 1, complete = FALSE)[[1]])
  expect_error(clean_eiger(tree = yule_tree, n_eigenvectors = 20, x = 1:10, y = 3:12), "dimensions")
  expect_error(clean_eiger(tree = yule_tree, n_eigenvectors = 8, x = 1:9, y = 3:12), "same number")
  expect_snapshot(clean_eiger(tree = yule_tree, n_eigenvectors = 8, x = 1:10, y = 5:12), error = TRUE)
  expect_snapshot(clean_eiger(tree = yule_tree, n_eigenvectors = 7, x = 1:10, y = rep("1", 10)), error = TRUE)
})

test_that("prepare_eiger() passes for valid inputs", {
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
  x <- BM[, 1]
  y <- BM[, 2]
  df_expected <- prepare_eiger(tree = felsenstein_tree, n_eigenvectors = 20, x = x, y = y)
  tol <- 1e-8
  for (i in 3:22) {
    v <- as.vector(df_expected[, i])
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
  x <- BM[, 1]
  y <- BM[, 2]
  df <- prepare_eiger(tree = felsenstein_tree, n_eigenvectors = 1, x = x, y = y)
  expect_no_error(run_eiger(data = df, tree = felsenstein_tree, n_eigenvectors = 1, x = x, y = y, prepared = TRUE))
  expect_no_error(run_eiger(tree = felsenstein_tree, n_eigenvectors = 1, x = x, y = y))
  expect_no_error(run_eiger(data = df, tree = felsenstein_tree, n_eigenvectors = 0, x = x, y = y, prepared = TRUE))
  expect_no_error(run_eiger(tree = felsenstein_tree, n_eigenvectors = 0, x = x, y = y))
  expect_no_error(run_eiger(data = df, tree = felsenstein_tree, n_eigenvectors = 2, x = x, y = y, prepared = TRUE, intercept = FALSE))
  expect_no_error(run_eiger(tree = felsenstein_tree, n_eigenvectors = 2, x = x, y = y, intercept = FALSE))
})

test_that("run_eiger() passes for valid inputs for Yule tree", {
  yule_tree <- withr::with_seed(17, TreeSim::sim.bd.taxa(100, 1, 1, 0, 1, complete = FALSE)[[1]])
  yule_tree$root.edge <- 0
  BM <- withr::with_seed(17, phytools::fastBM(yule_tree, 1, nsim = 3))
  x <- BM[, 1]
  y <- BM[, 2]
  z <- BM[, 3]
  BM_df <- as.data.frame(BM)
  colnames(BM_df) <- c("x", "y", "z")

  df <- prepare_eiger(tree = yule_tree, n_eigenvectors = 10, x = x, y = y)
  expect_no_error(run_eiger(data = df, tree = yule_tree, n_eigenvectors = 30, x = x, y = y, prepared = TRUE))
  expect_no_error(run_eiger(tree = yule_tree, n_eigenvectors = 30, x = x, y = y))
  expect_no_error(run_eiger(data = df, tree = yule_tree, n_eigenvectors = 30, x = x, y = y, prepared = TRUE, intercept = FALSE))
  expect_no_error(run_eiger(tree = yule_tree, n_eigenvectors = 30, x = x, y = y, intercept = FALSE))
  expect_no_error(run_eiger(y ~ x + z, BM_df, yule_tree, 30))
  expect_no_error(run_eiger(y ~ x + z, tree = yule_tree, n_eigenvectors = 30))
})

test_that("run_eiger() fails for too many eigenvectors", {
  yule_tree <- withr::with_seed(17, TreeSim::sim.bd.taxa(100, 1, 1, 0, 1, complete = FALSE)[[1]])
  yule_tree$root.edge <- 0
  BM <- withr::with_seed(17, phytools::fastBM(yule_tree, 1, nsim = 2))
  x <- BM[, 1]
  y <- BM[, 2]
  df <- prepare_eiger(tree = yule_tree, n_eigenvectors = 10, x = x, y = y)
  expect_error(run_eiger(data = df, tree = yule_tree, n_eigenvectors = 95, x = x, y = y, prepared = TRUE), "successfully")
  expect_error(run_eiger(tree = yule_tree, n_eigenvectors = 95, x = x, y = y), "successfully")
  expect_error(run_eiger(data = df, tree = yule_tree, n_eigenvectors = 100, x = x, y = y, prepared = TRUE, intercept = FALSE), "successfully")
  expect_error(run_eiger(tree = yule_tree, n_eigenvectors = 100, x = x, y = y, intercept = FALSE), "successfully")
})
