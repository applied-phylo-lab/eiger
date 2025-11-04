test_that("check_eiger() passes for valid inputs", {
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
  expect_silent(check_eiger(x, y, felsenstein_tree, 20))

  yule_tree <- withr::with_seed(17, TreeSim::sim.bd.taxa(10, 1, 1, 0, 1, complete = FALSE)[[1]])
  expect_silent(check_eiger(1:10, 3:12, yule_tree, 5))
})

test_that("check_eiger() fails for invalid inputs", {
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
  expect_error(check_eiger(x, y, felsenstein_tree, 20), "tip labels")

  yule_tree <- withr::with_seed(17, TreeSim::sim.bd.taxa(10, 1, 1, 0, 1, complete = FALSE)[[1]])
  expect_error(check_eiger(1:10, 3:12, yule_tree, 20), "dimensions")
  expect_error(check_eiger(1:9, 3:12, yule_tree, 8), "length")
  expect_error(check_eiger(1:10, 5:12, yule_tree, 8), "length")
  expect_error(check_eiger(1:10, rep("1", 10), yule_tree, 7), "numeric")
  expect_error(check_eiger(rep("1", 10), 1:10, yule_tree, 7), "numeric")
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
  df_expected <- prepare_eiger(x, y, felsenstein_tree, n_eigenvectors = 20)
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

test_that("run_eiger() passes for valid inputs", {
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
  df <- prepare_eiger(x, y, felsenstein_tree, 1)
  expect_no_error(run_eiger(x, y, felsenstein_tree, 1, df = df))
  expect_no_error(run_eiger(x, y, felsenstein_tree, 1))
  expect_no_error(run_eiger(x, y, felsenstein_tree, 0, df = df))
  expect_no_error(run_eiger(x, y, felsenstein_tree, 0))
  expect_no_error(run_eiger(x, y, felsenstein_tree, 2, df = df, intercept = FALSE))
  expect_no_error(run_eiger(x, y, felsenstein_tree, 2, intercept = FALSE))
})

test_that("run_eiger() passes for valid inputs", {
  yule_tree <- withr::with_seed(17, TreeSim::sim.bd.taxa(100, 1, 1, 0, 1, complete = FALSE)[[1]])
  yule_tree$root.edge <- 0
  BM <- withr::with_seed(17, phytools::fastBM(yule_tree, 1, nsim = 2))
  x <- BM[, 1]
  y <- BM[, 2]
  df <- prepare_eiger(x, y, yule_tree, 10)
  expect_no_error(run_eiger(x, y, yule_tree, 30, df = df))
  expect_no_error(run_eiger(x, y, yule_tree, 30))
  expect_no_error(run_eiger(x, y, yule_tree, 30, df = df, intercept = FALSE))
  expect_no_error(run_eiger(x, y, yule_tree, 30, intercept = FALSE))
})

test_that("run_eiger() fails for too many eigenvectors", {
  yule_tree <- withr::with_seed(17, TreeSim::sim.bd.taxa(100, 1, 1, 0, 1, complete = FALSE)[[1]])
  yule_tree$root.edge <- 0
  BM <- withr::with_seed(17, phytools::fastBM(yule_tree, 1, nsim = 2))
  x <- BM[, 1]
  y <- BM[, 2]
  df <- prepare_eiger(x, y, yule_tree, 10)
  expect_error(run_eiger(x, y, yule_tree, 95, df = df), "many")
  expect_error(run_eiger(x, y, yule_tree, 95), "many")
  expect_error(run_eiger(x, y, yule_tree, 100, df = df, intercept = FALSE), "many")
  expect_error(run_eiger(x, y, yule_tree, 100, intercept = FALSE), "many")
})
