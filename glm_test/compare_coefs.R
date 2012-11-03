#' A function to check correspondence between model parameters and fit
compare.coefs <- function(data, fit) {
  coefs <- data$params$coefs
  intercept <- data$params$intercept
  merge(
    data.frame(feature.name=c('(Intercept)', names(coefs)),
               param=c(`(Intercept)`=intercept, coefs)),
    subset(
      data.frame(feature.name=rownames(coef(fit)),
                 fit=coef(fit)[,1]),
      coef!=0),
    all=TRUE)
}
