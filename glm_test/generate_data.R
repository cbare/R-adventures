#' Generate random data for a bunch of features, some of which yield a
#' response. Many of the features are unrelated to the response. Of
#' course, we'll inject some noise data, too.
generate.data <- function(n=1000, m=5, sig.features=1:5, noise.level=0.10) {
  
  # create bogus feature matrix
  features <- matrix(runif(n*m), nrow=n, ncol=m)
  rownames(features) <- sprintf("ex%04d",seq(n))
  colnames(features) <- sprintf("feat%04d",seq(m))
  
  # generate random model parameters
  intercept <- rnorm(1)
  coefs <- rnorm(length(sig.features))
  names(coefs) <- colnames(features)[sig.features]
  
  # create response data
  response <- features[,sig.features] %*% coefs + intercept + rnorm(n, sd=noise.level)
  
  # return generated data
  list(n=n, m=m,
       sig.features=sig.features,
       noise.level=noise.level,
       features=features,
       params=list(intercept=intercept, coefs=coefs),
       response=response)
}
