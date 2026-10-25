test_that("ridgereg validates its inputs", {
  dat <- data.frame(y = 1:10, x = seq_len(10))
  expect_error(ridgereg("y ~ x", data = dat, lambda = 1), "formula")
  expect_error(ridgereg(y ~ x, data = matrix(1:10), lambda = 1), "data.frame")
  expect_error(ridgereg(y ~ x, data = dat, lambda = -1), "lambda")
  expect_error(ridgereg(y ~ x, data = dat, lambda = c(0, 1)), "lambda")
  expect_error(ridgereg(y ~ x, data = dat, lambda = Inf), "lambda")
})

test_that("QR ridge coefficients match MASS::lm.ridge", {
  set.seed(123)
  dat <- data.frame(
    y = rnorm(80),
    x1 = rnorm(80),
    x2 = rnorm(80),
    x3 = rnorm(80)
  )
  formula <- y ~ x1 + x2 + x3

  fit <- ridgereg(formula, data = dat, lambda = 0.75)
  reference <- MASS::lm.ridge(formula, data = dat, lambda = 0.75)

  expect_s3_class(fit, "ridgereg")
  expect_equal(unname(coef(fit)), unname(stats::coef(reference)),
               tolerance = 1e-7)
  expect_identical(names(coef(fit))[-1], names(stats::coef(reference))[-1])
})

test_that("lambda zero gives the ordinary least-squares solution", {
  fit <- ridgereg(Petal.Length ~ Sepal.Width + Sepal.Length,
                  data = iris, lambda = 0)
  reference <- stats::lm(Petal.Length ~ Sepal.Width + Sepal.Length,
                         data = iris)
  expect_equal(coef(fit), stats::coef(reference), tolerance = 1e-10)
  expect_equal(unname(predict(fit)), unname(stats::fitted(reference)),
               tolerance = 1e-10)
})

test_that("ridge predictions reuse training transformations", {
  fit <- ridgereg(Petal.Length ~ Species + Sepal.Width,
                  data = iris, lambda = 1)
  newdata <- iris[c(1, 60, 120), ]

  expect_equal(predict(fit), fit$fitted.values)
  expect_length(predict(fit, newdata = newdata), nrow(newdata))
  expect_true(all(is.finite(predict(fit, newdata = newdata))))
})

test_that("QR ridge supports more predictors than observations", {
  set.seed(10)
  x <- as.data.frame(matrix(rnorm(8 * 12), nrow = 8))
  names(x) <- paste0("x", seq_len(ncol(x)))
  x$y <- rnorm(nrow(x))
  fit <- ridgereg(y ~ ., data = x, lambda = 2)

  expect_true(all(is.finite(coef(fit))))
  expect_true(all(is.finite(predict(fit))))
})

test_that("lambda zero rejects a rank-deficient design", {
  dat <- data.frame(y = 1:8, x = rep(1, 8))
  expect_error(ridgereg(y ~ x, data = dat, lambda = 0),
               "full column rank")
})
