#' Create the default list of colors
#'
#' `get_default_colors()` creates the default list of colors for downstream
#' plotting.
#'
#' @returns A vector of strings representing colors from dark blue to light blue
#'   to orange to dark red.
#'
#' @keywords internal
get_default_colors <- function() {
  color_list <- c("#364B9A", "#4A7BB7", "#6EA6CD", "#98CAE1",
                  "#C2E4EF", "#EAECCC", "#FEDA8B", "#FDB366",
                  "#F67E4B", "#DD3D2D", "#A50026")
  color_list
}

#' Create a color palette from a color-generating function or a set of colors
#'
#' `create_color_palette()` creates a new color palette by extracting colors
#' from from a color-generating function or interpolating a set of given colors.
#'
#' @param color_scheme A string representing a color-generating function or
#'   vector of strings representing the set of colors to interpolate to create
#'   the new palette. Default to `NULL`.
#' @param n_colors An integer representing the number of colors in the color
#'   palette. Default to `100`.
#'
#' @returns A vector of strings representing the set of colors in the new color
#'   palette. Raises error if the name of the color-generating function is not
#'   valid or if the list of colors provided are not valid.
#'
#' @examples
#' \dontrun{
#' create_color_palette(c("red", "blue"), 30)
#' create_color_palette(c("#BBCCEE", "#CCEEFF", "#CCDDAA", "#EEEEBB", "#FFCCCC"), 50)
#' create_color_palette("Viridis", 80)
#' try(create_color_palette("red", 30))
#' try(create_color_palette(c("apple", "blueberry"), 30))
#' try(create_color_palette(c("red", "blue"), 3.14))
#' try(create_color_palette("CoolColors", 80))
#' }
#'
#' @keywords internal
create_color_palette <- function(color_scheme = NULL, n_colors = 100) {
  # check n_colors
  if (n_colors <= 1 || !rlang::is_integerish(n_colors)) {
    stop("The input number of colors must an integer greater than 1.",
         call. = FALSE)
  }
  # user provided no color scheme
  if (is.null(color_scheme)) {
    color_scheme <- get_default_colors()
    color_palette <- grDevices::colorRampPalette(color_scheme)(n_colors)
  } else if (is.character(color_scheme)) {
    # user provided a color-generating function
    if (length(color_scheme) == 1 && color_scheme %in% grDevices::hcl.pals()) {
      color_palette <- grDevices::hcl.colors(n_colors, color_scheme)
    } else if (length(color_scheme) > 1) {
      # user provided a vector of customized colors
      tryCatch({
        grDevices::col2rgb(color_scheme)
        color_palette <- grDevices::colorRampPalette(color_scheme)(n_colors)
      }, error = function(e) {
        stop("The input list of colors must all represent valid colors.",
             call. = FALSE)
        })
    } else {
      stop("The input color scheme must an available color-generating function in grDevices::hcl.pals().",
           call. = FALSE)
    }
  } else {
    stop("The input color scheme must be in the form of a string or a vector of strings.",
         call. = FALSE)
  }
  color_palette
}

#' Map a vector of values to colors in the color palette
#'
#' `map_colors()` maps a vector of values to colors in the color palette. The
#' mapping is performed in a way such that the values close to `0` would be
#' mapped to colors in the middle of the color palette. Depending on the
#' distribution of the values, the first or the last color in the color palette
#' might not necessarily have a value being mapped to.
#'
#' @param x A vector of values to be mapped to colors.
#' @param color_palette A list of strings representing the color palette for
#'   mapping.
#'
#' @returns A list of two values.
#'
#'   The first value `color_breaks` is a vector representing the boundaries of
#'   value intervals so that values falling in the same interval would be
#'   assigned the same color.
#'
#'   The second value `mapped_colors` is a vector of strings representing the
#'   colors each value is mapped to.
#'
#' @examples
#' \dontrun{
#' map_colors(-8:8, c("blue", "red"))
#' x <- c(-3.5, 9.8, 0.3, -1.1, -4.6, 2.1)
#' color_palette <- c("#BBCCEE", "#CCEEFF", "#CCDDAA", "#EEEEBB", "#FFCCCC")
#' map_colors(x, color_palette)
#' }
#'
#' @keywords internal
map_colors <- function(x, color_palette) {
  max_abs <- max(abs(x))
  color_breaks <- seq(-max_abs, max_abs, length.out = length(color_palette) + 1)
  x_colors <- color_palette[findInterval(x, color_breaks, rightmost.closed = TRUE)]
  list(color_breaks = color_breaks, mapped_colors = x_colors)
}

#' Check if the data for plotting is valid
#'
#' `check_plotting_data()` checks if the data frame for branch contributions is
#' consistent with the phylogenetic tree and if the dimension for plotting is
#' included in the data frame.
#'
#' @inheritParams check_phylo
#' @param branch_contributions A data frame representing the contributions of
#'   branches of the phylogenetic tree to the eigenvectors of some version of
#'   variance-covariance matrix for the whole tree. The data frame of should
#'   have `m` rows and `n` columns, where `m` is the number of branches, and `n`
#'   is the number of dimensions of eigenvectors for which the branch
#'   contributions are computed.
#' @param dim The dimension of eigenvector to plot.
#'
#' @returns Invisible returns `NULL` if all the following requirements are met:
#'   * The data frame for branch contributions has the same number of rows as
#'   the number of branches in the tree.
#'   * The data frame for branch contributions has the column names representing
#'   the dimensions of eigenvectors that are less than the number of tips in the
#'   tree.
#'   * The dimension for plotting is included in the data frame for branch
#'   contributions.
#'   Otherwise raises an error.
#'
#' @keywords internal
check_plotting_data <- function(tree, branch_contributions, dim) {
  # the number of rows in the data frame should be equal to the number of
  # branches of the phylogenetic tree
  n_branches <- length(tree$edge[, 1])
  if (nrow(branch_contributions) != n_branches) {
    stop("The number of rows in the data frame is inconsistent with the phylogenetic tree.",
         call. = FALSE)
  }
  # the column dimensions should be compatible with the number of tips in the
  # phylogenetic tree
  n_tips <- length(tree$tip.label)
  col_names <- colnames(branch_contributions)
  col_dims <- as.numeric(sub(".*_", "", col_names))
  tryCatch({
    check_dimensions(col_dims, 1, n_tips)
  }, error = function(e) {
    stop("The column dimensions in the data frame is inconsistent with the phylogenetic tree.",
         call. = FALSE)
  })
  # the dimension for plotting must be included in the data frame
  if (!(dim %in% col_dims)) {
    stop("The dimension for plotting is not included in the data frame.",
         call. = FALSE)
  }
  invisible(NULL)
}

#' Add title text to the color bar
#'
#' `add_text()` adds a title text to the color bar.
#'
#' @param dim An integer representing the dimension of eigenvector for plotting.
#' @param side See [graphics::mtext]. Default to `4`.
#' @param line See [graphics::mtext]. Default to `5`.
#' @param cex See [graphics::mtext]. Default to `1.5`.
#' @param ... Additional parameters passed to [graphics::mtext].
#'
#' @returns No value, but produces the side effect of writing the title text
#'   into the color bar.
#'
#' @keywords internal
#'
add_text <- function(dim, side = 4, line = 5, cex = 1.5, ...) {
  plot_text <- paste0("Contributions to eigenvector ", dim)
  graphics::mtext(plot_text, side = side, line = line, cex = cex, ...)
}

#' Add axis to the color bar
#'
#' `add_axis()` adds an axis to the color bar.
#'
#' @param side See [graphics::axis]. Default to `4`.
#' @param cex.axis See [graphics::axis]. Default to `1.5`.
#' @param ... Additional parameters passed to [graphics::axis].
#'
#' @returns No value, but produces the side effect of adding an axis to the
#'   color bar.
#'
#' @keywords internal
#'
add_axis <- function(side = 4, cex.axis = 1.5, ...) {
  graphics::axis(side = side, cex.axis = cex.axis, ...)
}

#' Add color bar to the plot
#'
#' `add_colorbar()` adds a color bar to the plot.
#'
#' @param color_breaks A string vector representing the boundaries of value
#'   intervals so that values falling in the same interval have the same color.
#' @param color_palette A string vector representing the color palette for the
#'   color bar.
#' @param dim An integer representing the dimension of eigenvector for plotting.
#' @param text_args A named list of arguments passed to an internal helper that
#'   adds title text to the color bar. Elements include:
#'   \describe{
#'     \item{side}{See [graphics::mtext]. Default to `4`.}
#'     \item{line}{See [graphics::mtext]. Default to `5`.}
#'     \item{cex}{See [graphics::mtext]. Default to `1.5`.}
#'     \item{...}{Additional parameters passed to [graphics::mtext].}
#'   }
#' @param axis_args A named list of arguments passed to an internal helper that
#'   adds axis to the color bar.
#'   \describe{
#'     \item{side}{See [graphics::axis]. Default to `4`.}
#'     \item{cex.axis}{See [graphics::axis]. Default to `1.5`.}
#'     \item{...}{Additional parameters passed to [graphics::axis].}
#'   }
#' @param ... Parameters passed to [graphics::image].
#'
#' @returns No value, but produces the side effect of adding a color bar to the
#'   plot.
#'
#' @keywords internal
#'
add_colorbar <- function(color_breaks, color_palette,
                         dim, text_args = list(), axis_args = list(), ...) {
  interval_half_size <- (color_breaks[2] - color_breaks[1]) / 2
  colorbar_vals <- color_breaks[-1] - interval_half_size

  # deal with image() arguments
  # arguments that should not be overridden
  protected_args <- list(x = 1, y = colorbar_vals,
                         z = matrix(colorbar_vals, nrow = 1),
                         col = color_palette,
                         axes = FALSE, xlab = "", ylab = "")
  # user arguments
  user_args <- list(...)
  colorbar_args <- utils::modifyList(user_args, protected_args)

  do.call(graphics::image, colorbar_args)
  do.call(add_axis, axis_args)
  do.call(add_text, c(list(dim = dim), text_args))
}

#' Plot branch contributions to an eigenvector of variance-covariance matrices
#'
#' `plot_branch_contributions()` plots the contributions of branches of a
#' phylogenetic tree to some eigenvector using some version of
#' variance-covariance matrix for the whole tree.
#'
#' @inheritParams check_plotting_data
#' @param branch_contributions A data frame representing the contributions of
#'   branches of the phylogenetic tree to the eigenvectors of some version of
#'   variance-covariance matrix for the whole tree. The data frame of should
#'   have `m` rows and `n` columns, where `m` is the number of branches, and `n`
#'   is the number of dimensions of eigenvectors for which the branch
#'   contributions are computed. Default to `NULL`, in which case
#'   `branch_contributions` will be calculated using `cov_matrix`.
#' @param cov_matrix A string representing the version of variance-covariance
#'   matrix used to obtain the data frame `branch_contributions`. The options
#'   are `"dcvcv"` (double-centered phylogenetic variance-covariance matrix),
#'   `"vcv"` (non-centered phylogenetic variance-covariance matrix), and
#'   `"egrm"` (expected genetic relatedness matrix). If `branch_contributions`
#'   is `NULL`, `cov_matrix` is used to calculate `branch_contributions`.
#'   Default to `"dcvcv"`.
#' @param text_args A named list of arguments passed to an internal helper that
#'   adds title text to the color bar. Elements include:
#'   \describe{
#'     \item{side}{See [graphics::mtext]. Default to `4`.}
#'     \item{line}{See [graphics::mtext]. Default to `5`.}
#'     \item{cex}{See [graphics::mtext]. Default to `1.5`.}
#'     \item{...}{Additional parameters passed to [graphics::mtext].}
#'   }
#' @param axis_args A named list of arguments passed to an internal helper that
#'   adds axis to the color bar.
#'   \describe{
#'     \item{side}{See [graphics::axis]. Default to `4`.}
#'     \item{cex.axis}{See [graphics::axis]. Default to `1.5`.}
#'     \item{...}{Additional parameters passed to [graphics::axis].}
#'   }
#' @param colorbar_args Parameters passed to [graphics::image] for making the
#'   color bar.
#' @inheritParams create_color_palette
#' @param edge.width See [ape::plot.phylo]. Default to `5`.
#' @param show.tip.label See [ape::plot.phylo]. Default to `FALSE`.
#' @param ... Other parameters passed to [ape::plot.phylo].
#'
#' @returns No value, but produces the side effect of plotting the contributions
#'   of branches of a phylogenetic tree to the eigenvector of dimension `dim`
#'   using some version of variance-covariance matrix for the whole tree. A
#'   color bar is also included with the plot.
#'
#' @examples
#' set.seed(42)
#' yule_tree <- TreeSim::sim.bd.taxa(20, 1, 1, 0, 1, complete = FALSE)[[1]]
#' df <- compute_branch_contributions(yule_tree, "egrm", 1:10)
#' plot_branch_contributions(yule_tree, df, "egrm", 3)
#' plot_branch_contributions(yule_tree, df, "egrm", 5, n_colors = 50)
#' plot_branch_contributions(yule_tree, cov_matrix = "dcvcv", dim = 5, n_colors = 80)
#'
#' @export
plot_branch_contributions <- function(tree,
                                      branch_contributions = NULL,
                                      cov_matrix = c("dcvcv", "vcv", "egrm"),
                                      dim,
                                      text_args = list(),
                                      axis_args = list(),
                                      colorbar_args = list(),
                                      color_scheme = NULL,
                                      n_colors = 100,
                                      edge.width = 5,
                                      show.tip.label = FALSE, ...) {
  # check input data
  check_phylo(tree)
  check_phylo_branches(tree)
  cov_matrix <- match.arg(cov_matrix)
  if (is.null(branch_contributions)) {
    need_compute <- TRUE
    branch_contributions <- compute_branch_contributions(tree, cov_matrix, dim)
  } else {
    need_compute <- FALSE
    check_plotting_data(tree, branch_contributions, dim)
    }

  # create the color palette
  color_palette <- create_color_palette(color_scheme, n_colors)

  # get the values needed for plotting
  if (!need_compute) {
    col_name <- paste0("dim_", dim)
    branch_contributions_dim <- as.vector(branch_contributions[[col_name]])
    } else {
      branch_contributions_dim <- as.vector(branch_contributions[, 1])
      }

  branch_color_map <- map_colors(branch_contributions_dim, color_palette)
  color_breaks <- branch_color_map$color_breaks
  branch_colors <- branch_color_map$mapped_colors

  # plotting
  # save current par settings
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par))

  # modify the settings
  graphics::layout(matrix(1:2, nrow = 1, byrow = TRUE), widths = c(3, 35))

  graphics::par(mar = c(4, 1, 4, 0))
  do.call(add_colorbar, c(list(color_breaks = color_breaks,
                               color_palette = color_palette,
                               dim = dim),
                          text_args, axis_args, colorbar_args))

  # deal with ape::plot.phylo() arguments
  # arguments that should not be overridden
  protected_args <- list(edge.color = branch_colors)
  # default args that can be overridden
  default_args <- list(edge.width = 5, show.tip.label = FALSE)
  # user arguments
  user_args <- list(...)
  plot_args <- utils::modifyList(default_args, user_args)
  plot_args <- utils::modifyList(plot_args, protected_args)

  graphics::par(mar = c(0, 6, 1, 1))
  do.call(ape::plot.phylo, c(list(x = tree), plot_args))
}
