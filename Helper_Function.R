# Helper Functions
library("gplots")
library("randomForest")

plotmean_prop = function(dataset,colname){
  if(is.numeric(unlist(dataset[colname])) == TRUE){
    dataset$temp = binVariable(unlist(dataset[colname]), bins = 4,
                                      method = "proportions",
                                      labels = NULL)
    plotmeans(APURCH.num ~ temp, data = dataset, xlab = colname, las = 2)

  } else {
    plotmeans(APURCH.num ~ unlist(dataset[colname]), data= dataset, xlab = colname, las = 2)
  }
}

plotmean_int = function(dataset,colname){
  if(is.numeric(unlist(dataset[colname])) == TRUE){
    dataset$temp = binVariable(unlist(dataset[colname]), bins = 4,
                               method = "interval",
                               labels = NULL)
    plotmeans(APURCH.num ~ temp, data = dataset, xlab = colname, las = 2)
    
  } else {
    plotmeans(APURCH.num ~ unlist(dataset[colname]), data= dataset, xlab = colname, las =2 )
  }
}

dplot = function(model, colname){
  partial(model, pred.var = colname, 
          prob = TRUE,
          which.class = 2,
          plot = TRUE,
          rug = TRUE,
          plot.engine = "ggplot2")
}
