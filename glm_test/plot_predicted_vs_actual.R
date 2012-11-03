#' Make predicted vs actual plots
plot.predicted.vs.actual <-
  function(data, predicted, actual, noise.level, label=NULL) {
    
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
                      sprintf('noise=%0.0f%%', noise.level*100)
           ))
}

