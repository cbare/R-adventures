#' Feature selection and linear modeling

#+ library1, results='hide', message=FALSE, warning=FALSE
library(glmnet)

#' Let's give the glmnet package a little workout. We'll generate data for
#' a bunch of features, some of which yield a response. Many of the features
#' are unrelated to the responnse. Of course, we'll inject some noise into
#' the data, too.
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
  list(n=n, m=m, sig.features=sig.features, noise.level=noise.level,
       features=features, params=list(intercept=intercept, coefs=coefs), response=response)
}

#' A function to check correspondence between model parameters and fit
compare.coefs <- function(data, fit) {
  merge(
    data.frame(feature.name=c('(Intercept)',names(data$params$coefs)),
               param=c(`(Intercept)`=data$params$intercept, data$params$coefs)),
    subset(
      data.frame(feature.name=rownames(coef(fit)),
                 coef=coef(fit)[,1]),
      coef!=0),
    all=TRUE)
}

#' Make predicted vs actual plots
plot.predicted.vs.actual <- function(data, predicted, actual, noise.level, label=NULL) {
  correlation.predicted.actual <- cor(predicted, actual)
  order.by.predicted <- order(predicted)
  
  ##  create a plot of predicted vs actual
  plot(actual[order.by.predicted],
       pch=21, col="#aaaaaaaa", bg="#cc000030",
       ylab="response", xlab="sample")
  
  title(main="predicted vs. actual", col.main="#666666")
  
  lines(predicted[order.by.predicted], col='blue', lwd=2)
  
  legend("topleft", pch=c(NA, 21), lwd=c(2,NA), 
         col=c("blue", "#aaaaaa"),
         pt.bg=c(NA,"#cc000030"),
         legend=c('predicted','actual'))
  
  if (!is.null(label)) mtext(label, padj=-0.5)
  
  legend("bottomright",
         legend=c(sprintf('corr=%0.3f', correlation.predicted.actual),
                  if (abs(noise.level) >= 2.0)
                    sprintf('noise=%0.1fx', noise.level)
                  else
                    noise.string <- sprintf('noise=%0.0f%%', noise.level*100)
         ))
}

#' Generate data
n <- 1000
noise.level <- 0.50
data <- generate.data(n, m=20, sig.features=1:5, noise.level)

#' Select training and testing sets
train <- sample.int(n, n*0.85)
test <- setdiff(seq(n), train)

#' Fit model using elastic net
fit <- cv.glmnet(data$features[train,], 
                 data$response[train, drop=FALSE],
                 alpha=0.7)
compare.coefs(data, fit)

#' Check our ability to model training data
p <- predict(fit, data$features[train,])
plot.predicted.vs.actual(data$features[train,], p, data$response[train,], noise.level, "training")
cor(p, data$response[train,])

#' Check our ability to predict test data
p <- predict(fit, data$features[test,])
plot.predicted.vs.actual(data$features[test,], p, data$response[test,], noise.level, "testing")
cor(p, data$response[test,])

