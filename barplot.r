
### ANOVA analysis
aov.comparison <- function(data, predictor, response) {
  
  ## summary data
  data2 <- data[!is.na(data[,response]),] ## drop NAs
  
  ## calculate means
  summary.data <- aggregate(data2[,response], by=list(data2[,predictor]),mean)
  colnames(summary.data) <- c(predictor,response)
  
  ##calculate confidence intervals
  se.data <- aggregate(data2[,response], by=list(data2[,predictor]),se)
  summary.data[,"ci"] <- se.data[,2]*1.96
  
  ## figures
  y.min <- summary.data[,response]-summary.data[,"ci"]
  y.max <- summary.data[,response]+summary.data[,"ci"]
  plot1 <- ggplot(summary.data, aes(x=summary.data[,predictor], y=summary.data[,response])) + geom_bar(stat="identity", fill="grey80", color="black", width=0.6)+ theme_bw()+ theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())+geom_errorbar(aes(ymin=y.min,ymax=y.max, width=0))+ xlab(predictor) + ylab(response)+theme(text=element_text(size=16))
  
  Response <- data2[,response]
  Predictors <- data2[,predictor]
  
  summary.stats <- data.frame(summary.data[,1:2],y.min,y.max)
  colnames(summary.stats) <- c(predictor,"mean","lower95","upper95")
  
  ## statistics
  fit1 <- aov(Response~Predictors)
  print(summary(fit1))
  print(TukeyHSD(fit1))
  print(plot1)
  print(summary.stats)
}
