test_that("Tests that compute the lp distances works correctly", {

  context('Test lp distances')

  # Set seed for reproductivity
  set.seed(292313)

  # Use Iris Data
  test_idx <- sample(nrow(iris), 11)
  x_train <- iris[-test_idx, -1]
  y_train <- iris[-test_idx, 1]
  x_test <- iris[test_idx, -1]

  # Create a random forest
  rf <- forestry(x = x_train, y = y_train, nthread = 1)

  # Compute the l1 distances in the "Species" dimension
  distances_1 <- compute_lp(object = rf,
                            feature.new = x_test,
                            feature = "Species",
                            p = 1)

  # Compute the l2 distances in the "Petal.Length" dimension
  distances_2 <- compute_lp(object = rf,
                            feature.new = x_test,
                            feature = "Petal.Length",
                            p = 2)

  expect_identical(length(distances_1), nrow(x_test))
  expect_identical(length(distances_2), nrow(x_test))

  #set tolerance
  skip_if_not_mac()

  expect_equal(distances_1,
               c(0.7412765, 0.5626915, 0.6670021, 0.4814331,
                 0.4269154, 0.7936147, 0.6906481, 0.6000588,
                 0.7773134, 0.5397050, 0.6732839),
               tolerance = 1e-6)
  expect_equal(distances_2,
               c(2.372681, 2.497261, 2.704748, 1.900080,
                 1.638488, 2.406346, 2.101205, 2.427264,
                 3.078544, 2.412146, 2.297884),
               tolerance = 1e-6)
})

