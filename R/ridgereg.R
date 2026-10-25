#' Fit a ridge regression model
#'
#' Fits a ridge regression model by solving the penalized least-squares
#' problem with a QR decomposition. Predictor columns are centered and scaled
#' using the training data; the intercept, when present, is not penalized.
#' The augmented system is
#' \deqn{\begin{bmatrix}X_s\\\sqrt{\lambda}P\end{bmatrix}\beta_s =
#'       \begin{bmatrix}y\\0\end{bmatrix},}
#' where \eqn{P} is the identity matrix except for a zero at the intercept.
#'
#' The returned coefficients are transformed back to the original predictor
#' scale. This makes them directly comparable to \code{coef(MASS::lm.ridge())}.
#'
#' @param formula A two-sided model formula with a numeric response.
#' @param data A data frame containing the variables in \code{formula}.
#' @param lambda A single finite, non-negative ridge penalty.
#'
#' @return An object of S3 class \code{ridgereg}, containing the original-scale
#'   coefficients, fitted values, tuning parameter, and the transformations
#'   needed for prediction.
#'
#' @examples
#' fit <- ridgereg(medv ~ lstat + rm, data = MASS::Boston, lambda = 1)
#' coef(fit)
#' predict(fit, newdata = MASS::Boston[1:3, ])
#'
#' @seealso \code{\link{predict.ridgereg}}, \code{\link[MASS]{lm.ridge}}
#' @importFrom stats delete.response model.frame model.matrix model.offset model.response na.omit na.pass
#' @export
ridgereg <- function(formula, data, lambda) {
  if (!inherits(formula, "formula")) {
    stop("'formula' must be an object of class 'formula'.", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.", call. = FALSE)
  }
  if (!is.numeric(lambda) || length(lambda) != 1L ||
      is.na(lambda) || !is.finite(lambda) || lambda < 0) {
    stop("'lambda' must be one finite, non-negative number.", call. = FALSE)
  }

  data_name <- deparse(substitute(data))
  model_frame <- stats::model.frame(
    formula = formula,
    data = data,
    na.action = stats::na.omit,
    drop.unused.levels = TRUE
  )
  terms <- attr(model_frame, "terms")
  response <- stats::model.response(model_frame)
  if (!is.numeric(response) || is.matrix(response)) {
    stop("The response must be a numeric vector.", call. = FALSE)
  }
  if (!all(is.finite(response))) {
    stop("The response must contain only finite values.", call. = FALSE)
  }

  offset <- stats::model.offset(model_frame)
  if (is.null(offset)) {
    offset <- rep(0, length(response))
  } else if (!is.numeric(offset) || !all(is.finite(offset))) {
    stop("The model offset must contain only finite numeric values.",
         call. = FALSE)
  }

  design <- stats::model.matrix(terms, model_frame)
  if (!all(is.finite(design))) {
    stop("The model matrix must contain only finite values.", call. = FALSE)
  }
  n <- nrow(design)
  p <- ncol(design)
  if (p == 0L) {
    stop("The formula must produce at least one coefficient.", call. = FALSE)
  }

  intercept_index <- if (attr(terms, "intercept") == 1L) {
    match("(Intercept)", colnames(design))
  } else {
    integer()
  }
  predictor_index <- setdiff(seq_len(p), intercept_index)

  centers <- stats::setNames(rep(0, p), colnames(design))
  scales <- stats::setNames(rep(1, p), colnames(design))
  if (length(predictor_index) > 0L) {
    if (length(intercept_index) > 0L) {
      centers[predictor_index] <- colMeans(
        design[, predictor_index, drop = FALSE]
      )
    }
    centered_predictors <- sweep(
      design[, predictor_index, drop = FALSE],
      2L,
      centers[predictor_index],
      FUN = "-"
    )
    # RMS scaling matches MASS::lm.ridge's parameterization.
    scales[predictor_index] <- sqrt(colMeans(centered_predictors^2))
    zero_scale <- !is.finite(scales[predictor_index]) |
      scales[predictor_index] <= sqrt(.Machine$double.eps)
    scales[predictor_index[zero_scale]] <- 1
  }

  scaled_design <- sweep(
    sweep(design, 2L, centers, FUN = "-"),
    2L,
    scales,
    FUN = "/"
  )

  penalty <- diag(sqrt(lambda), p)
  if (length(intercept_index) > 0L) {
    penalty[intercept_index, intercept_index] <- 0
  }
  augmented_design <- rbind(scaled_design, penalty)
  augmented_response <- c(response - offset, rep(0, p))
  decomposition <- qr(augmented_design)
  if (decomposition$rank < p) {
    if (lambda == 0) {
      stop(
        "With lambda = 0 the design matrix must have full column rank.",
        call. = FALSE
      )
    }
    stop("The penalized design matrix could not be solved.", call. = FALSE)
  }

  scaled_coefficients <- as.vector(qr.coef(decomposition, augmented_response))
  names(scaled_coefficients) <- colnames(design)
  coefficients <- scaled_coefficients / scales
  if (length(intercept_index) > 0L) {
    coefficients[intercept_index] <- scaled_coefficients[intercept_index] -
      sum(centers[predictor_index] * coefficients[predictor_index])
  }
  names(coefficients) <- colnames(design)

  fitted_values <- as.vector(design %*% coefficients + offset)
  factor_columns <- vapply(model_frame, is.factor, logical(1))
  xlevels <- lapply(model_frame[factor_columns], levels)

  structure(
    list(
      coefficients = coefficients,
      scaled_coefficients = scaled_coefficients,
      fitted.values = fitted_values,
      lambda = lambda,
      centers = centers,
      scales = scales,
      terms = terms,
      xlevels = xlevels,
      contrasts = attr(design, "contrasts"),
      formula = formula,
      call = match.call(),
      data_name = data_name
    ),
    class = "ridgereg"
  )
}

#' Extract ridge regression coefficients
#'
#' Returns the coefficients on the original predictor scale.
#'
#' @param object A \code{ridgereg} model object.
#' @param ... Further arguments (currently unused).
#' @return A named numeric vector of coefficients.
#' @examples
#' fit <- ridgereg(medv ~ lstat + rm, data = MASS::Boston, lambda = 1)
#' coef(fit)
#' @export
coef.ridgereg <- function(object, ...) {
  object$coefficients
}

#' Print a ridge regression model
#'
#' Prints the model call and named coefficient estimates.
#'
#' @param x A \code{ridgereg} model object.
#' @param ... Further arguments (currently unused).
#' @return \code{x}, invisibly.
#' @examples
#' fit <- ridgereg(medv ~ lstat + rm, data = MASS::Boston, lambda = 1)
#' print(fit)
#' @export
print.ridgereg <- function(x, ...) {
  cat("\nCall:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print.default(
    format(x$coefficients, digits = max(3L, getOption("digits") - 3L)),
    print.gap = 2L,
    quote = FALSE
  )
  invisible(x)
}

#' Predict from a ridge regression model
#'
#' Returns fitted values when \code{newdata} is omitted, or predictions for
#' rows in \code{newdata}. Training-data centering and scaling are reused.
#'
#' @param object A \code{ridgereg} model object.
#' @param newdata An optional data frame with predictor values.
#' @param ... Further arguments (currently unused).
#' @return A numeric vector of predictions.
#' @examples
#' fit <- ridgereg(medv ~ lstat + rm, data = MASS::Boston, lambda = 1)
#' predict(fit, newdata = MASS::Boston[1:3, ])
#' @export
predict.ridgereg <- function(object, newdata, ...) {
  if (missing(newdata)) {
    return(object$fitted.values)
  }
  if (!is.data.frame(newdata)) {
    stop("'newdata' must be a data.frame.", call. = FALSE)
  }

  predictor_terms <- stats::delete.response(object$terms)
  model_frame <- stats::model.frame(
    predictor_terms,
    data = newdata,
    na.action = stats::na.pass,
    xlev = object$xlevels
  )
  design <- stats::model.matrix(
    predictor_terms,
    model_frame,
    contrasts.arg = object$contrasts
  )
  coefficient_names <- names(object$scaled_coefficients)
  missing_columns <- setdiff(coefficient_names, colnames(design))
  if (length(missing_columns) > 0L) {
    stop(
      "Missing predictor columns in 'newdata': ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  design <- design[, coefficient_names, drop = FALSE]
  scaled_design <- sweep(
    sweep(design, 2L, object$centers, FUN = "-"),
    2L,
    object$scales,
    FUN = "/"
  )
  predictions <- as.vector(scaled_design %*% object$scaled_coefficients)
  offset <- stats::model.offset(model_frame)
  if (!is.null(offset)) {
    predictions <- predictions + offset
  }
  predictions
}
