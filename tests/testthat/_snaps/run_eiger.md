# check_list() passes for bad but valid inputs

    Code
      ls6_result <- check_list(ls6, "ls6")
    Condition
      Warning:
      The data list ls6 contains fields that do not have names.
      i The names for the first field of the data list ls6 that has names will be used.
      Warning:
      The data list ls6 do not have the same names across different fields.
      i The names for the first field of the data list ls6 that has names will be used.

# clean_eiger() passes and raises warnings for valid but bad inputs

    Code
      df <- clean_eiger(tree = felsenstein_tree, n_eigenvectors = 5, x = x, y = y)
    Condition
      Warning:
      The names of variable `x` do not exist.
      i The order of the elements must be the same as the order of tip labels of the tree.
      Warning:
      The names of variable `y` do not exist.
      i The order of the elements must be the same as the order of tip labels of the tree.

---

    Code
      df <- clean_eiger(tree = felsenstein_tree, n_eigenvectors = 5, x = x, y = y)
    Condition
      Warning:
      The variable `x` do not match the tip labels of the tree.
      i The order of the variable values must be the same as the order of tip labels of the tree.
      Warning:
      The names of variable `y` do not exist.
      i The order of the elements must be the same as the order of tip labels of the tree.

---

    Code
      df <- clean_eiger(tree = felsenstein_tree, n_eigenvectors = 5, x = x, y = y)
    Condition
      Warning:
      The variable `x` do not match the tip labels of the tree.
      i The order of the variable values must be the same as the order of tip labels of the tree.
      Warning:
      The variable `y` do not match the tip labels of the tree.
      i The order of the variable values must be the same as the order of tip labels of the tree.

---

    Code
      df <- clean_eiger(y ~ x + I(df_extra$y^2) + df_extra$x - 1, BM_df, tree = yule_tree,
      n_eigenvectors = 3)
    Condition
      Warning:
      The row names of the data frame df_extra do not match the tip labels of the tree.
      i The order of the rows must be the same as the order of tip labels of the tree.
      Warning:
      The row names of the data frame df_extra do not match the tip labels of the tree.
      i The order of the rows must be the same as the order of tip labels of the tree.

---

    Code
      df <- clean_eiger(y ~ x + df_extra$x + group, BM_df, tree = yule_tree,
      n_eigenvectors = 3)
    Condition
      Warning:
      The row names of the data frame df_extra do not match the tip labels of the tree.
      i The order of the rows must be the same as the order of tip labels of the tree.
      Warning:
      The names of variable group do not exist.
      i The order of the elements must be the same as the order of tip labels of the tree.

# clean_eiger() fails for invalid inputs

    Code
      clean_eiger(tree = yule_tree, n_eigenvectors = 8, x = 1:10, y = 5:12)
    Condition
      Warning:
      The names of variable `x` do not exist.
      i The order of the elements must be the same as the order of tip labels of the tree.
      Error in `check_plain()`:
      ! The variable `y` must have the same number of elements as the number of tips in the tree.
      i The variable `y` has 8 elements.
      i The tree has 10 tips.

---

    Code
      clean_eiger(tree = yule_tree, n_eigenvectors = 7, x = 1:10, y = rep("1", 10))
    Condition
      Warning:
      The names of variable `x` do not exist.
      i The order of the elements must be the same as the order of tip labels of the tree.
      Warning:
      The names of variable `y` do not exist.
      i The order of the elements must be the same as the order of tip labels of the tree.
      Error in `check_numeric_response()`:
      ! Response variable `y` must be a numeric vector.
      x You've supplied a <character> vector.

