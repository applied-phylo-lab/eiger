test_that("check_phylo() passes for valid input trees", {
  yule_tree <- withr::with_seed(17, TreeSim::sim.bd.taxa(4, 1, 1, 0, 1, complete = FALSE)[[1]])
  expect_silent(check_phylo(yule_tree))

  s1 <- ape::stree(3, type = "star")
  s2 <- ape::stree(3, type = "star")
  s1$root.edge <- 1
  s2$root.edge <- 1
  felsenstein_tree <- s1 + s2
  expect_silent(check_phylo(felsenstein_tree))
})

test_that("check_phylo() raises errors for invalid inputs", {
  broken_tree <- list(edge = matrix(1:4, 2, 2), tip.label = letters[1:3])
  expect_error(check_phylo(broken_tree), "valid")

  broken_tree <- list(edge = matrix(c(4L, 1L, 4L, 5L, 5L, 2L, 5L, 3L),
                                    4, 2, byrow = TRUE),
                      tip.label = letters[1:3],
                      Nnode = 1L)
  expect_error(check_phylo(broken_tree), "valid")

  broken_tree <- list(edge = matrix(c(4L, 1L, 4L, 5L, 5L, 2L, 5L, 3L),
                                    4, 2, byrow = TRUE),
                      tip.label = letters[1:3],
                      Nnode = 3L)
  expect_error(check_phylo(broken_tree), "valid")

  broken_tree <- list(edge = matrix(c(4L, 1L, 4L, 5L, 5L, 2L, 5L, 3L),
                                    4, 2, byrow = TRUE),
                      tip.label = letters[1:4],
                      Nnode = 1L)
  expect_error(check_phylo(broken_tree), "valid")

  broken_tree <- list(edge = matrix(c(4L, 1L, 4L, 5L, 5L, 2L, 5L, 3L),
                                    4, 2, byrow = TRUE),
                      tip.label = letters[1:2],
                      Nnode = 3L)
  expect_error(check_phylo(broken_tree), "valid")

  broken_tree <- list(edge = matrix(c(4L, 1L, 4L, 5L, 4L, 2L, 4L, 3L),
                                    4, 2, byrow = TRUE),
                      tip.label = letters[1:3],
                      Nnode = 2L)
  expect_error(check_phylo(broken_tree), "valid")

  broken_tree <- list(edge = matrix(c(4L, 1L, 4L, 5L, 5L, 2L, 5L, 3L, 5L, 1L),
                                    5, 2, byrow = TRUE),
                      tip.label = letters[1:3],
                      Nnode = 2L)
  expect_error(check_phylo(broken_tree), "valid")
})

test_that("compute_descendancy_matrix() passes for valid input trees", {
  yule_tree <- withr::with_seed(17, TreeSim::sim.bd.taxa(4, 1, 1, 0, 1, complete = FALSE)[[1]])
  yule_descendancy_matrix <- matrix(c(0, 0, 0, 1,
                                      1, 1, 1, 0,
                                      0, 0, 1, 0,
                                      1, 1, 0, 0,
                                      0, 1, 0, 0,
                                      1, 0, 0, 0), 6, 4, byrow = TRUE)
  expect_equal(compute_descendancy_matrix(yule_tree),
               yule_descendancy_matrix)

  s1 <- ape::stree(3, type = "star")
  s2 <- ape::stree(3, type = "star")
  s1$root.edge <- 1
  s2$root.edge <- 1
  felsenstein_tree <- s1 + s2
  felsenstein_descendancy_matrix <- matrix(c(1, 1, 1, 0, 0, 0,
                                             1, 0, 0, 0, 0, 0,
                                             0, 1, 0, 0, 0, 0,
                                             0, 0, 1, 0, 0, 0,
                                             0, 0, 0, 1, 1, 1,
                                             0, 0, 0, 1, 0, 0,
                                             0, 0, 0, 0, 1, 0,
                                             0, 0, 0, 0, 0, 1), 8, 6, byrow = TRUE)
  expect_equal(compute_descendancy_matrix(felsenstein_tree),
               felsenstein_descendancy_matrix)
})

test_that("compute_descendancy_matrix() raises errors for invalid inputs", {
  broken_tree <- list(edge = matrix(1:4, 2, 2), tip.label = letters[1:3])
  expect_error(compute_descendancy_matrix(broken_tree), "valid")

  broken_tree <- list(edge = matrix(c(4L, 1L, 4L, 5L, 5L, 2L, 5L, 3L),
                                    4, 2, byrow = TRUE),
                      tip.label = letters[1:3],
                      Nnode = 1L)
  expect_error(compute_descendancy_matrix(broken_tree), "valid")

  broken_tree <- list(edge = matrix(c(4L, 1L, 4L, 5L, 5L, 2L, 5L, 3L),
                                    4, 2, byrow = TRUE),
                      tip.label = letters[1:3],
                      Nnode = 3L)
  expect_error(compute_descendancy_matrix(broken_tree), "valid")

  broken_tree <- list(edge = matrix(c(4L, 1L, 4L, 5L, 5L, 2L, 5L, 3L),
                                    4, 2, byrow = TRUE),
                      tip.label = letters[1:4],
                      Nnode = 1L)
  expect_error(compute_descendancy_matrix(broken_tree), "valid")

  broken_tree <- list(edge = matrix(c(4L, 1L, 4L, 5L, 5L, 2L, 5L, 3L),
                                    4, 2, byrow = TRUE),
                      tip.label = letters[1:2],
                      Nnode = 3L)
  expect_error(compute_descendancy_matrix(broken_tree), "valid")

  broken_tree <- list(edge = matrix(c(4L, 1L, 4L, 5L, 4L, 2L, 4L, 3L),
                                    4, 2, byrow = TRUE),
                      tip.label = letters[1:3],
                      Nnode = 2L)
  expect_error(compute_descendancy_matrix(broken_tree), "valid")
})

test_that("compute_branch_vcv() passes for valid binary vectors", {
  expected_matrix <- matrix(c(0, 0, 0, 0, 0,
                              0, 0, 0, 0, 0,
                              0, 0, 1, 0, 1,
                              0, 0, 0, 0, 0,
                              0, 0, 1, 0, 1), 5, 5, byrow = TRUE)
  expect_equal(compute_branch_vcv(c(0, 0, 1, 0, 1)), expected_matrix)
  expected_matrix <- matrix(c(1, 1, 1, 1,
                              1, 1, 1, 1,
                              1, 1, 1, 1,
                              1, 1, 1, 1), 4, 4, byrow = TRUE)
  expect_equal(compute_branch_vcv(c(1, 1, 1, 1)), expected_matrix)
})

test_that("compute_branch_egrm() passes for short valid binary vectors", {
  expected_matrix <- matrix(c(0.5, 0.5, -1,
                              0.5, 0.5, -1,
                              -1, -1, 2), 3, 3, byrow = TRUE)
  expect_equal(compute_branch_egrm(c(1, 1, 0)), expected_matrix)
  expected_matrix <- matrix(c(0.5, -1, 0.5,
                              -1, 2, -1,
                              0.5, -1, 0.5), 3, 3, byrow = TRUE)
  expect_equal(compute_branch_egrm(c(0, 1, 0)), expected_matrix)
})

test_that("compute_branch_egrm() passes for long valid binary vectors", {
  x <- c(1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 1)
  egrm <- compute_branch_egrm(x)
  expect_equal(egrm, compute_branch_egrm(1 - x))
  id_zeros <- which(x == 0)
  id_ones <- which(x == 1)
  value_00 <- egrm[id_zeros[1], id_zeros[1]]
  value_01 <- egrm[id_zeros[1], id_ones[1]]
  value_11 <- egrm[id_ones[1], id_ones[1]]
  for (i in id_zeros) {
    for (j in id_zeros) {
      expect_equal(egrm[i, j], value_00)
    }
  }
  for (i in id_zeros) {
    for (j in id_ones) {
      expect_equal(egrm[i, j], value_01)
    }
  }
  for (i in id_ones) {
    for (j in id_zeros) {
      expect_equal(egrm[i, j], value_01)
    }
  }
  for (i in id_ones) {
    for (j in id_ones) {
      expect_equal(egrm[i, j], value_11)
    }
  }
})

test_that("check_phylo_branches() passes for valid input trees", {
  yule_tree <- withr::with_seed(17, TreeSim::sim.bd.taxa(4, 1, 1, 0, 1, complete = FALSE)[[1]])
  expect_silent(check_phylo_branches(yule_tree))

  s1 <- ape::stree(5, type = "star")
  s2 <- ape::stree(5, type = "star")
  s1$edge.length <- rep(1, 5)
  s2$edge.length <- rep(1, 5)
  s1$root.edge <- 5
  s2$root.edge <- 5
  felsenstein_tree <- s1 + s2
  expect_silent(check_phylo_branches(felsenstein_tree))
})

test_that("compute_phylo_branches() raises errors for invalid input trees", {
  broken_tree <- list(edge = matrix(c(4L, 1L, 4L, 5L, 5L, 2L, 5L, 3L),
                                    4, 2, byrow = TRUE),
                      tip.label = letters[1:3],
                      Nnode = 2L)
  class(broken_tree) <- "phylo"
  expect_error(check_phylo_branches(broken_tree), "branch lengths")

  broken_tree$edge.length <- c(1, 2)
  expect_error(check_phylo_branches(broken_tree), "all")
  broken_tree$edge.length <- c(1, 2, NA, 1)
  expect_error(check_phylo_branches(broken_tree), "all")
  broken_tree$edge.length <- c(1, 2, -1, 1)
  expect_error(check_phylo_branches(broken_tree), "non-negative")

  s1 <- ape::stree(3, type = "star")
  s2 <- ape::stree(3, type = "star")
  s1$root.edge <- 1
  s2$root.edge <- 1
  felsenstein_tree <- s1 + s2
  expect_error(check_phylo_branches(felsenstein_tree), "branch lengths")
})

test_that("compute_cov_matrices() and compute_cov_matrix() pass for vcv of yule tree", {
  yule_tree <- withr::with_seed(17, TreeSim::sim.bd.taxa(3, 1, 1, 0, 1, complete = FALSE)[[1]])
  yule_tree$edge.length <- c(2, 1, 3, 4)

  expected_yule_vcvs <- matrix(0, nrow = 9, ncol = 5)
  expected_yule_vcvs[c(1, 2, 4, 5), 1] <- 2
  expected_yule_vcvs[5, 2] <- 1
  expected_yule_vcvs[1, 3] <- 3
  expected_yule_vcvs[9, 4] <- 4
  expected_yule_vcvs[c(1, 2, 4, 5, 9), 5] <- c(5, 2, 2, 3, 4)
  expected_yule_vcv <- matrix(c(5, 2, 0,
                                2, 3, 0,
                                0, 0, 4), nrow = 3, byrow = TRUE)
  results <- compute_cov_matrices(yule_tree, "vcv")
  result_vcvs <- results$cov_matrices
  result_vcv <- results$cov_matrix_tree

  expect_equal(unname(as.matrix(result_vcvs)), expected_yule_vcvs)
  expect_equal(result_vcv, expected_yule_vcv)

  result_vcv1 <- compute_cov_matrix(yule_tree, "vcv")
  expect_equal(result_vcv1, expected_yule_vcv)
})

test_that("compute_cov_matrices() and compute_cov_matrix() pass for vcv of felsenstein's worst case tree", {
  s1 <- ape::stree(5, type = "star")
  s2 <- ape::stree(5, type = "star")
  s1$edge.length <- rep(1, 5)
  s2$edge.length <- rep(1, 5)
  s1$root.edge <- 5
  s2$root.edge <- 5
  felsenstein_tree <- s1 + s2

  expected_felsenstein_vcvs <- matrix(0, nrow = 100, ncol = 13)
  id_branch_1 <- as.vector(sapply(0:4, function(i) 10 * i + 1:5))
  expected_felsenstein_vcvs[id_branch_1, c(1, 13)] <- 5
  id_branch_2 <- as.vector(sapply(5:9, function(i) 10 * i + 6:10))
  expected_felsenstein_vcvs[id_branch_2, c(7, 13)] <- 5
  expected_felsenstein_vcvs[cbind((2:6) * 11 - 21, 2:6)] <- 1
  expected_felsenstein_vcvs[cbind((8:12) * 11 - 32, 8:12)] <- 1
  id_branch_3 <- (1:10) * 11 - 10
  expected_felsenstein_vcvs[id_branch_3, 13] <- expected_felsenstein_vcvs[id_branch_3, 13] + 1

  expected_felsenstein_vcv <- matrix(0, nrow = 10, ncol = 10)
  expected_felsenstein_vcv[1:5, 1:5] <- 5
  expected_felsenstein_vcv[6:10, 6:10] <- 5
  diag(expected_felsenstein_vcv) <- diag(expected_felsenstein_vcv) + 1

  results <- compute_cov_matrices(felsenstein_tree, "vcv")
  result_vcvs <- results$cov_matrices
  result_vcv <- results$cov_matrix_tree

  expect_equal(unname(as.matrix(result_vcvs)), expected_felsenstein_vcvs)
  expect_equal(result_vcv, expected_felsenstein_vcv)

  result_vcv1 <- compute_cov_matrix(felsenstein_tree, "vcv")
  expect_equal(result_vcv1, expected_felsenstein_vcv)
})

test_that("compute_cov_matrices() and compute_cov_matrix() pass for egrm of yule tree", {
  yule_tree <- withr::with_seed(17, TreeSim::sim.bd.taxa(3, 1, 1, 0, 1, complete = FALSE)[[1]])
  yule_tree$edge.length <- c(40, 60, 60, 100)

  expected_yule_egrms <- matrix(0, nrow = 9, ncol = 5)
  expected_yule_egrms[c(1, 2, 4, 5), 1] <- 0.5 * 40 / 260
  expected_yule_egrms[c(3, 6, 7, 8), 1] <- -1 * 40 / 260
  expected_yule_egrms[9, 1] <- 2 * 40 / 260
  expected_yule_egrms[c(1, 3, 7, 9), 2] <- 0.5 * 60 / 260
  expected_yule_egrms[c(2, 4, 6, 8), 2] <- -1 * 60 / 260
  expected_yule_egrms[5, 2] <- 2 * 60 / 260
  expected_yule_egrms[1, 3] <- 2 * 60 / 260
  expected_yule_egrms[c(2, 3, 4, 7), 3] <- -1 * 60 / 260
  expected_yule_egrms[c(5, 6, 8, 9), 3] <- 0.5 * 60 / 260
  expected_yule_egrms[c(1, 2, 4, 5), 4] <- 0.5 * 100 / 260
  expected_yule_egrms[c(3, 6, 7, 8), 4] <- -1 * 100 / 260
  expected_yule_egrms[9, 4] <- 2 * 100 / 260
  val_1 <- (0.5 * 200 + 2 * 60) / 260
  val_2 <- (0.5 * 140 + -1 * 120) / 260
  val_3 <- (-1 * 200 + 0.5 * 60) / 260
  val_4 <- (2 * 140 + 0.5 * 120) / 260
  expected_yule_egrms[c(1, 5), 5] <- val_1
  expected_yule_egrms[c(2, 4), 5] <- val_2
  expected_yule_egrms[c(3, 6, 7, 8), 5] <- val_3
  expected_yule_egrms[9, 5] <- val_4
  expected_yule_egrm <- matrix(c(val_1, val_2, val_3,
                                 val_2, val_1, val_3,
                                 val_3, val_3, val_4), nrow = 3, byrow = TRUE)
  results <- compute_cov_matrices(yule_tree, "egrm")
  result_egrms <- results$cov_matrices
  result_egrm <- results$cov_matrix_tree

  expect_equal(unname(as.matrix(result_egrms)), expected_yule_egrms)
  expect_equal(result_egrm, expected_yule_egrm)

  result_egrm1 <- compute_cov_matrix(yule_tree, "egrm")
  expect_equal(result_egrm1, expected_yule_egrm)
})

test_that("compute_cov_matrices() and compute_cov_matrix() pass for dcvcv and egrm of random trees", {
  yule_tree <- withr::with_seed(42, TreeSim::sim.bd.taxa(100, 1, 1, 0, 1, complete = FALSE)[[1]])

  results <- compute_cov_matrices(yule_tree, "dcvcv")
  result_dcvcvs <- results$cov_matrices
  result_dcvcv <- results$cov_matrix_tree
  result_dcvcvs_vals <- unname(as.matrix(result_dcvcvs))

  expect_equal(colSums(result_dcvcvs_vals), rep(0, ncol(result_dcvcvs_vals)))
  expect_equal(colSums(result_dcvcv), rep(0, ncol(result_dcvcv)))
  expect_equal(rowSums(result_dcvcv), rep(0, nrow(result_dcvcv)))
  expect_equal(result_dcvcv, t(result_dcvcv))

  result_dcvcv1 <- compute_cov_matrix(yule_tree, "dcvcv")
  expect_equal(colSums(result_dcvcv1), rep(0, ncol(result_dcvcv1)))
  expect_equal(rowSums(result_dcvcv1), rep(0, nrow(result_dcvcv1)))
  expect_equal(result_dcvcv1, t(result_dcvcv1))

  results <- compute_cov_matrices(yule_tree, "egrm")
  result_egrms <- results$cov_matrices
  result_egrm <- results$cov_matrix_tree
  result_egrms_vals <- unname(as.matrix(result_egrms))

  expect_equal(colSums(result_egrms_vals), rep(0, ncol(result_egrms_vals)))
  expect_equal(colSums(result_egrm), rep(0, ncol(result_egrm)))
  expect_equal(rowSums(result_egrm), rep(0, nrow(result_egrm)))
  expect_equal(result_egrm, t(result_egrm))

  result_egrm1 <- compute_cov_matrix(yule_tree, "egrm")
  expect_equal(colSums(result_egrm1), rep(0, ncol(result_egrm1)))
  expect_equal(rowSums(result_egrm1), rep(0, nrow(result_egrm1)))
  expect_equal(result_egrm1, t(result_egrm1))
})

test_that("compute_cov_matrices() and compute_cov_matrix() raise errors for invalid input trees", {
  broken_tree <- list(edge = matrix(1:4, 2, 2), tip.label = letters[1:3])
  expect_error(compute_cov_matrices(broken_tree), "valid")
  expect_error(compute_cov_matrix(broken_tree), "valid")

  broken_tree <- list(edge = matrix(c(4L, 1L, 4L, 5L, 5L, 2L, 5L, 3L),
                                    4, 2, byrow = TRUE),
                      tip.label = letters[1:3],
                      Nnode = 2L)
  class(broken_tree) <- "phylo"
  expect_error(compute_cov_matrices(broken_tree), "branch lengths")
  expect_error(compute_cov_matrix(broken_tree), "branch lengths")

  broken_tree$edge.length <- c(1, 2)
  expect_error(compute_cov_matrices(broken_tree), "all")
  expect_error(compute_cov_matrix(broken_tree), "all")
  broken_tree$edge.length <- c(1, 2, NA, 1)
  expect_error(compute_cov_matrices(broken_tree), "all")
  expect_error(compute_cov_matrix(broken_tree), "all")
  broken_tree$edge.length <- c(1, 2, -1, 1)
  expect_error(compute_cov_matrices(broken_tree), "non-negative")
  expect_error(compute_cov_matrix(broken_tree), "non-negative")
})

test_that("compute_cov_matrices() and compute_cov_matrix() raise errors for invalid input cov_matrix", {
  yule_tree <- withr::with_seed(42, TreeSim::sim.bd.taxa(10, 1, 1, 0, 1, complete = FALSE)[[1]])
  expect_error(compute_cov_matrices(yule_tree, "kinship"), "arg")
  expect_error(compute_cov_matrix(yule_tree, "kinship"), "arg")
})

test_that("check_dimensions() passes for valid input dimensions", {
  expect_silent(check_dimensions(1, 1, 5))
  expect_silent(check_dimensions(1:9, 1, 10))
  expect_silent(check_dimensions(c(5, 4, 9, 8, 1, 3), 1, 10))
})

test_that("check_dimensions() fails for invalid input dimensions", {
  expect_error(check_dimensions(-7, 1, 10), "between")
  expect_error(check_dimensions(0, 1, 8), "between")
  expect_error(check_dimensions(c(3, 2, 11, 4, 6), 1, 8), "between")
  expect_error(check_dimensions(c(0.5, 2, 1, 8), 1, 8), "integer")
  expect_error(check_dimensions(c("1", "2", "5", "7"), 1, 8), "integer")
})

test_that("compute_branch_contributions() passes for felsenstein's worst case tree", {
  s1 <- ape::stree(5, type = "star")
  s2 <- ape::stree(5, type = "star")
  s1$edge.length <- rep(1, 5)
  s2$edge.length <- rep(1, 5)
  s1$root.edge <- 5
  s2$root.edge <- 5
  felsenstein_tree <- s1 + s2

  branch_contributions_vcv <- compute_branch_contributions(felsenstein_tree, "vcv", 1)
  dim_1 <- branch_contributions_vcv$dim_1
  branch_max_contribution_1 <- which(dim_1 == max(dim_1))
  expect_true(branch_max_contribution_1 %in% c(1, 7))

  branch_contributions_vcv <- compute_branch_contributions(felsenstein_tree, "vcv", 2)
  dim_2 <- branch_contributions_vcv$dim_2
  branch_max_contribution_2 <- which(dim_2 == max(dim_2))
  expect_true(branch_max_contribution_2 %in% c(1, 7))

  branch_contributions_vcv <- compute_branch_contributions(felsenstein_tree, "vcv", 1:3)
  dim_1 <- branch_contributions_vcv$dim_1
  branch_max_contribution_1 <- which(dim_1 == max(dim_1))
  expect_true(branch_max_contribution_1 %in% c(1, 7))
  dim_2 <- branch_contributions_vcv$dim_2
  branch_max_contribution_2 <- which(dim_2 == max(dim_2))
  expect_true(branch_max_contribution_2 %in% c(1, 7))
  dim_3 <- branch_contributions_vcv$dim_3
  branch_max_contribution_3 <- which(dim_3 == max(dim_3))
  expect_false(branch_max_contribution_3 %in% c(1, 7))

  branch_contributions_dcvcv <- compute_branch_contributions(felsenstein_tree, "dcvcv", 1:9)
  dim_1 <- branch_contributions_dcvcv$dim_1
  branch_max_contribution_1 <- sort(which(dim_1 == max(dim_1)))
  expect_equal(branch_max_contribution_1, c(1, 7))
  expect_equal(dim_1[1], dim_1[7])

  for (i in 2:9) {
    dim_i <- branch_contributions_dcvcv[, i]
    branch_max_contribution_i <- sort(which(abs(dim_i) == max(abs(dim_i))))
    expect_false(any(branch_max_contribution_i %in% c(1, 7)))
    expect_equal(dim_i[1], dim_i[7])
  }

  dims <- c(4, 1, 6, 8, 3)
  branch_contributions_egrm <- compute_branch_contributions(felsenstein_tree, "egrm", dims)
  dim_1 <- branch_contributions_egrm$dim_1
  branch_max_contribution_1 <- sort(which(dim_1 == max(dim_1)))
  expect_equal(branch_max_contribution_1, c(1, 7))
  expect_equal(dim_1[1], dim_1[7])

  for (i in dims[-2]) {
    name_i <- paste0("dim_", i)
    dim_i <- branch_contributions_egrm[[name_i]]
    branch_max_contribution_i <- sort(which(abs(dim_i) == max(abs(dim_i))))
    expect_false(any(branch_max_contribution_i %in% c(1, 7)))
    expect_equal(dim_i[1], dim_i[7])
  }
})

test_that("compute_branch_contributions() passes for some yule tree", {
  yule_tree <- withr::with_seed(42, TreeSim::sim.bd.taxa(50, 1, 1, 0, 1, complete = FALSE)[[1]])

  branch_contributions_dcvcv <- compute_branch_contributions(yule_tree, dims = 1:50)
  for (i in 1:50) {
    dim_i <- branch_contributions_dcvcv[, i]
    expect_equal(dim_i[1], dim_i[92])
  }

  dims <- c(5:10, c(4, 20, 37, 1), 40:45, c(21, 3, 23))
  branch_contributions_egrm <- compute_branch_contributions(yule_tree, "egrm", dims)

  for (i in dims) {
    name_i <- paste0("dim_", i)
    dim_i <- branch_contributions_egrm[[name_i]]
    expect_equal(dim_i[1], dim_i[92])
  }
})

test_that("compute_branch_contributions() raises errors for invalid input trees", {
  broken_tree <- list(edge = matrix(1:4, 2, 2), tip.label = letters[1:3])
  expect_error(compute_branch_contributions(broken_tree, "vcv", 1), "valid")

  class(broken_tree) <- "phylo"
  expect_error(compute_branch_contributions(broken_tree, "vcv", 1), "valid")

  broken_tree <- list(edge = matrix(c(4L, 1L, 4L, 5L, 5L, 2L, 5L, 3L),
                                    4, 2, byrow = TRUE),
                      tip.label = letters[1:3],
                      Nnode = 2L)
  class(broken_tree) <- "phylo"
  expect_error(compute_branch_contributions(broken_tree, "vcv", 1), "branch lengths")

  broken_tree$edge.length <- c(1, 2)
  expect_error(compute_branch_contributions(broken_tree, "vcv", 1), "all")
  broken_tree$edge.length <- c(1, 2, NA, 1)
  expect_error(compute_branch_contributions(broken_tree, "vcv", 1), "all")
  broken_tree$edge.length <- c(1, 2, -1, 1)
  expect_error(compute_branch_contributions(broken_tree, "vcv", 1), "non-negative")
})

test_that("compute_branch_contributions() raises errors for invalid input cov_matrix", {
  yule_tree <- withr::with_seed(42, TreeSim::sim.bd.taxa(10, 1, 1, 0, 1, complete = FALSE)[[1]])
  expect_error(compute_branch_contributions(yule_tree, "kinship", 1), "arg")
})
