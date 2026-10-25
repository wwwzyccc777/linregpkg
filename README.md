# linregpkg

<!-- badges: start -->
[![R CMD check](https://github.com/wwwzyccc777/linregpkg/actions/workflows/r.yml/badge.svg)](https://github.com/wwwzyccc777/linregpkg/actions/workflows/r.yml)
<!-- badges: end -->

`linregpkg` is an R package for fitting multiple linear regression models.
The model is estimated with linear algebra, using the QR decomposition of the
design matrix, and the results are stored in an object of class `linreg`
with methods for printing, plotting and summarising the fit.

The package was developed as part of Computer Lab 4 in the course
**732A94 Advanced R Programming** at Linköping University.

## Installation

Install the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("wwwzyccc777/linregpkg", build_vignettes = TRUE)
```

## Usage

Fit a model with `linreg()`, which takes a `formula` and a `data.frame`:

``` r
library(linregpkg)

mod <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
```

The following methods are available for `linreg` objects:

| Method          | Description                                                        |
|-----------------|--------------------------------------------------------------------|
| `print(mod)`    | Prints the call and the estimated coefficients                     |
| `summary(mod)`  | Coefficients with standard errors, t-values, p-values, σ̂ and df   |
| `coef(mod)`     | Returns the coefficients as a named vector                         |
| `resid(mod)`    | Returns the residuals                                              |
| `pred(mod)`     | Returns the fitted values                                          |
| `plot(mod)`     | Residuals vs Fitted and Scale-Location plots (ggplot2)             |

``` r
print(mod)
summary(mod)
coef(mod)
head(resid(mod))
head(pred(mod))
plot(mod)
```

## Method

For a model **y = Xβ + ε**, the design matrix is decomposed as **X = QR**,
where Q has orthonormal columns and R is upper triangular. The coefficients
are obtained by solving **Rβ̂ = Qᵀy**, and the variance of the estimates is

**Var(β̂) = σ̂² (RᵀR)⁻¹**, with **σ̂² = eᵀe / (n − p)**.

This is numerically more stable than computing (XᵀX)⁻¹ directly.

## Vignette

A walkthrough of the package on the `iris` dataset is available as a
vignette:

``` r
browseVignettes("linregpkg")
```

## Ridge regression

The package also provides ridgereg(formula, data, lambda). It centers and
scales predictors, leaves an intercept unpenalized, and solves the augmented
ridge least-squares problem with QR decomposition. The print(), coef(), and
predict() methods are available for ridge models.

~~~r
ridge_model <- ridgereg(
  Petal.Length ~ Sepal.Width + Sepal.Length,
  data = iris,
  lambda = 1
)
print(ridge_model)
coef(ridge_model)
predict(ridge_model, newdata = iris[1:3, ])
~~~

visualize_airport_delays() uses dplyr to summarize mean arrival delays by
destination and plots them at the airports' longitude and latitude.

## Ridge regression vignette

The ridgereg vignette demonstrates a reproducible BostonHousing prediction
workflow with a training/test split, linear and forward-selected models, and
10-fold cross-validation to choose the ridge penalty. Run
browseVignettes("linregpkg") to open both package vignettes.

## Authors

- Viet Tien Trinh — LiU ID: `vietr933` — GitHub: [@Tvths](https://github.com/Tvths)
- Zhengyu Wang — LiU ID: `zhewa470` — GitHub: [@wwwzyccc777](https://github.com/wwwzyccc777)

## License

MIT © linregpkg authors
