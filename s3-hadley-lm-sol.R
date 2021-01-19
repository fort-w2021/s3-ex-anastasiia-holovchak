new_lm <- function(coefficients, residuals, effects, rank, fitted.values, assign,
  qr, df.residual, xlevels, call, terms, model
) {
  
assert_double(coefficients, residuals, effects, fitted.values)
assert_integer(rank, assign, df.residual)
assert_list(qr, xlevels, model)
if (typeof(call) != "language") {
  stop("Assertion on 'call' failed: Must be of type 'language'")
}
if (typeof(terms) != "language") {
  stop("Assertion on 'terms' failed: Must be of type 'language'")
}

  structure(list(coefficients = coefficients, residuals = residuals,
      effects = effects, rank = rank, fitted.values = fitted.values,
      assign = assign, qr = qr, df.residual = df.residual, xlevels = xlevels,
      call = call, terms = terms, model = model),
    class = "lm")
}
