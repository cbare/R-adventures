#' Feature selection and linear modeling

#+ set-options, echo=FALSE, cache=FALSE
#options(width=44)
#opts_chunk$set(comment = "", warning = FALSE, message = FALSE, echo = TRUE, tidy = FALSE, size="small")

#+ library1, results='hide', message=FALSE, warning=FALSE
library(glmnet)

#' Generate data
n <- 1000
noise.level <- 0.50
data <- generate.data(n, m=1500, sig.features=1:100, noise.level)

#' Select training and testing sets
train <- sample.int(n, n*0.85)
test <- setdiff(seq(n), train)

generate_folds <- function(n, folds) {
  sample(
    rep(seq(folds), ceiling(n/folds)),
    size=n, replace=FALSE)
}

#' Find the alpha that generates best correlation with test data
#' by crossvalidation
alphas <- seq(0.9,1,0.01)
cors <- double(length=length(alphas))
foldid <- generate_folds(length(train), 10)

for (i in seq(along=alphas)) {
  fit <- cv.glmnet(data$features[train,], 
                   data$response[train, drop=FALSE],
                   foldid=foldid,
                   alpha=alphas[i])
  
  p <- predict(fit, data$features[test,])
  cors[i] <- cor(p, data$response[test,])[1,1]
}

alpha.max <- alphas[which.max(cors)]



#' Fit model using elastic net
fit <- cv.glmnet(data$features[train,], 
                 data$response[train, drop=FALSE],
                 alpha=alpha.max)
compare.coefs(data, fit)

#' Check our ability to model training data
#+ plot1, fig.width=6, fig.height=5
p <- predict(fit, data$features[train,])
plot.predicted.vs.actual(data$features[train,], p, data$response[train,], noise.level, "training")

#' Correlation with training data
cor(p, data$response[train,])

#' Check our ability to predict test data
#+ plot2, fig.width=6, fig.height=5
p <- predict(fit, data$features[test,])
plot.predicted.vs.actual(data$features[test,], p, data$response[test,], noise.level, "testing")

#' Correlation with testing data
cor(p, data$response[test,])





#' There's other fun stuff we could do with this:
#'   - test the effect on the ability to model of
#'     - noise characteristics
#'     - data distribution
#'     - correlated features
#'     - missing features
#'     - aggregate features: x^2, x1*x2, if (x1>0) x2 else x3, etc.
#'     - number of significant features
#'  - sweep alpha parameter values
#'     - plot correlation as a function of alpha
#'     
#'     