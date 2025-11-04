test_that("create_color_palette() works for valid inputs", {
  expect_no_error(create_color_palette(c("red", "blue"), 30))
  expect_no_error(create_color_palette(c("#BBCCEE", "#CCEEFF", "#CCDDAA", "#EEEEBB", "#FFCCCC"), 50))
  expect_no_error(create_color_palette("Viridis", 80))
  expect_no_error(create_color_palette("Purple-Orange", 100))
  expect_no_error(create_color_palette())
  expect_no_error(create_color_palette(n_colors = 50))
  expect_no_error(create_color_palette("Inferno"))
})

test_that("create_color_palette() fails for invalid inputs", {
  expect_error(create_color_palette("red", 30), "available")
  expect_error(create_color_palette("CoolColors", 80), "available")
  expect_error(create_color_palette(c("apple", "blueberry"), 30), "valid")
  expect_error(create_color_palette(c("#BBCCEE", "#CCEEFF", "#CCDAA"), 50), "valid")
  expect_error(create_color_palette(c("#BBC", "CCE", "#CCD", "#EEE"), 50), "valid")
  expect_error(create_color_palette(1, 50), "string")
  expect_error(create_color_palette("Viridis", 53.284), "integer")
  expect_error(create_color_palette("Viridis", 1), "greater than 1")
  expect_error(create_color_palette("Viridis", 0), "greater than 1")
  expect_error(create_color_palette("Viridis", -8), "greater than 1")
})

test_that("map_colors() passes for valid inputs", {
  observed <- map_colors(-3:3, c("blue", "red"))
  observed_breaks <- observed$color_breaks
  observed_colors <- observed$mapped_colors
  expected_breaks <- c(-3, 0, 3)
  expected_colors <- c(rep("blue", 3), rep("red", 4))
  expect_equal(observed_breaks, expected_breaks)
  expect_equal(observed_colors, expected_colors)

  observed <- map_colors(-3:3, c("blue", "yellow", "red"))
  observed_breaks <- observed$color_breaks
  observed_colors <- observed$mapped_colors
  expected_breaks <- c(-3, -1, 1, 3)
  expected_colors <- c(rep("blue", 2), rep("yellow", 2), rep("red", 3))
  expect_equal(observed_breaks, expected_breaks)
  expect_equal(observed_colors, expected_colors)

  observed <- map_colors(c(9, 1, 0, -2, 4, -1, 3), c("blue", "yellow", "red"))
  observed_breaks <- observed$color_breaks
  observed_colors <- observed$mapped_colors
  expected_breaks <- c(-9, -3, 3, 9)
  expected_colors <- c("red", rep("yellow", 3), "red", "yellow", "red")
  expect_equal(observed_breaks, expected_breaks)
  expect_equal(observed_colors, expected_colors)
})

test_that("check_plotting_data() passes for valid input data", {
  yule_tree <- withr::with_seed(17, TreeSim::sim.bd.taxa(20, 1, 1, 0, 1, complete = FALSE)[[1]])
  branch_contributions <- compute_branch_contributions(yule_tree, "egrm", 1:10)
  expect_silent(check_plotting_data(yule_tree, branch_contributions, 8))

  s1 <- ape::stree(5, type = "star")
  s2 <- ape::stree(5, type = "star")
  s1$edge.length <- rep(1, 5)
  s2$edge.length <- rep(1, 5)
  s1$root.edge <- 5
  s2$root.edge <- 5
  felsenstein_tree <- s1 + s2
  branch_contributions <- compute_branch_contributions(felsenstein_tree, "vcv", 1:3)
  expect_silent(check_plotting_data(felsenstein_tree, branch_contributions, 2))
})

test_that("check_plotting_data() fails for invalid input data", {
  yule_tree_1 <- withr::with_seed(17, TreeSim::sim.bd.taxa(20, 1, 1, 0, 1, complete = FALSE)[[1]])
  yule_tree_2 <- withr::with_seed(42, TreeSim::sim.bd.taxa(7, 1, 1, 0, 1, complete = FALSE)[[1]])
  s1 <- ape::stree(5, type = "star")
  s2 <- ape::stree(5, type = "star")
  s1$edge.length <- rep(1, 5)
  s2$edge.length <- rep(1, 5)
  s1$root.edge <- 5
  s2$root.edge <- 5
  felsenstein_tree <- s1 + s2

  branch_contributions_yule_1 <- compute_branch_contributions(yule_tree_1, "dcvcv", 1:15)
  branch_contributions_yule_2 <- compute_branch_contributions(yule_tree_2, "dcvcv", 1:5)
  branch_contributions_felsenstein <- compute_branch_contributions(felsenstein_tree, "dcvcv", 1:8)

  expect_error(check_plotting_data(yule_tree_1, branch_contributions_felsenstein, 3), "rows")
  expect_error(check_plotting_data(felsenstein_tree, branch_contributions_yule_1, 3), "rows")

  expect_error(check_plotting_data(yule_tree_2, branch_contributions_felsenstein, 3), "column")

  expect_error(check_plotting_data(yule_tree_1, branch_contributions_yule_1, 19), "included")
  expect_error(check_plotting_data(yule_tree_2, branch_contributions_yule_2, 7), "included")
})

test_that("add_colorbar() runs without error", {
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par))

  color_palette_1 <- c("#BBCCEE", "#CCEEFF", "#CCDDAA", "#EEEEBB", "#FFCCCC")
  graphics::par(mar = c(4, 4, 4, 8))
  expect_silent(add_colorbar(1:6, color_palette_1, 10))

  color_palette_2 <- create_color_palette()
  graphics::par(mar = c(4, 4, 4, 8))
  expect_silent(add_colorbar(1:101, color_palette_2, 15))

  color_palette_3 <- create_color_palette("Inferno", 80)
  graphics::par(mar = c(4, 4, 4, 8))
  expect_silent(add_colorbar(seq(-30, 30, length.out = 81), color_palette_3, 4))

  color_palette_4 <- create_color_palette(color_palette_1, 100)
  graphics::par(mar = c(4, 4, 4, 8))
  expect_silent(add_colorbar(seq(-60, 60, length.out = 100),
                             color_palette_4,
                             13,
                             text_args = list(cex = 2),
                             axis_args = list(side = 2, cex.axis = 2)))
})

test_that("plot_branch_contributions() runs without error", {
  yule_tree_1 <- withr::with_seed(17, TreeSim::sim.bd.taxa(20, 1, 1, 0, 1, complete = FALSE)[[1]])
  yule_tree_2 <- withr::with_seed(42, TreeSim::sim.bd.taxa(7, 1, 1, 0, 1, complete = FALSE)[[1]])
  s1 <- ape::stree(10, type = "star")
  s2 <- ape::stree(10, type = "star")
  s1$edge.length <- rep(1, 10)
  s2$edge.length <- rep(1, 10)
  s1$root.edge <- 5
  s2$root.edge <- 5
  felsenstein_tree <- s1 + s2

  df_1 <- compute_branch_contributions(yule_tree_1, "egrm", 1:10)
  df_2 <- compute_branch_contributions(yule_tree_2, "vcv", 1:5)
  df_3 <- compute_branch_contributions(felsenstein_tree, "dcvcv", 1:3)

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par))

  expect_silent(plot_branch_contributions(yule_tree_1, df_1, "egrm", 3))
  expect_silent(plot_branch_contributions(yule_tree_1,
                                          df_1,
                                          "egrm",
                                          8,
                                          color_scheme = "Inferno"))
  expect_silent(plot_branch_contributions(yule_tree_1,
                                          cov_matrix = "egrm",
                                          dim = 8,
                                          color_scheme = "Viridis",
                                          n_colors = 80,
                                          edge.width = 8))


  color_list <- c("#BBCCEE", "#CCEEFF", "#CCDDAA", "#EEEEBB", "#FFCCCC")
  expect_silent(plot_branch_contributions(yule_tree_2,
                                          df_2,
                                          "vcv",
                                          1,
                                          text_args = list(cex = 1),
                                          axis_args = list(cex.axis = 1),
                                          color_scheme = color_list,
                                          n_colors = 90))
  expect_silent(plot_branch_contributions(yule_tree_2,
                                          cov_matrix = "dcvcv",
                                          dim = 3,
                                          text_args = list(cex = 2),
                                          color_scheme = color_list))

  expect_silent(plot_branch_contributions(felsenstein_tree, df_3, "dcvcv", 1))
  expect_silent(plot_branch_contributions(felsenstein_tree,
                                          df_3,
                                          "dcvcv",
                                          1,
                                          type = "cladogram",
                                          direction = "downwards"))
})
