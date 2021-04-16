#P3 V2
#setwd("C:/Users/edwin/Desktop/BUS 445/Scripts/")
library("dplyr")
library("car")
library("forcats")
library("rpart")
library("rpart.plot")
library("nnet")
library("foreign")
library("corrplot")
library("randomForest")
library("pdp")
library("ggplot2")
library("gplots")
#source("BCA_functions_source_file.R")
#source("Helper_Function.R")
#RSP = read.csv("../Datasets/vcRSP2017.csv")

### Data Cleaning ###
row.names(RSP) = RSP$unique

RSP$Sample <- create.samples(RSP, est = 0.6, val = 0.4, rand.seed = 1)
RSP$APURCH.num = if_else(RSP$APURCH == "Y", 1,0)

RSP$gender = ifelse(RSP$gendf == 1, "F", "M")
RSP$unique = NULL
RSP$gendf = NULL
RSP$gendm = NULL 
RSP$pcode = NULL
RSP$atmcrd = NULL
RSP$NEWLOC = NULL
RSP$NEWMRGG = NULL
RSP$numcon_1 = NULL
RSP$N_IND_INC_ = NULL

RSP$BALCHQ = ifelse(is.na(RSP$BALCHQ), 0, RSP$BALCHQ)
RSP$BALSAV = ifelse(is.na(RSP$BALSAV), 0, RSP$BALSAV)
RSP$BALLOC = ifelse(is.na(RSP$BALLOC), 0, RSP$BALLOC)
RSP$BALMRGG = ifelse(is.na(RSP$BALMRGG), 0, RSP$BALMRGG)
RSP$BALLOAN = ifelse(is.na(RSP$BALLOAN), 0, RSP$BALLOAN)
RSP$CH_NM_SERV = ifelse(is.na(RSP$CH_NM_SERV), 0, RSP$CH_NM_SERV)
RSP$CH_NM_PRD = ifelse(is.na(RSP$CH_NM_PRD), 0, RSP$CH_NM_PRD)
RSP$numrr_1 = ifelse(is.na(RSP$numrr_1), 0, RSP$numrr_1)
variable.summary(RSP)

RSP2 = RSP[!RSP$valsegm == "",]
RSP2 = RSP2[!is.na(RSP2$avginc_1),]

RSP2$APURCH = as.factor(RSP2$APURCH)
RSP2$valsegm = as.factor(RSP2$valsegm)
RSP2$gender = as.factor(RSP2$gender)
variable.summary(RSP2)


RSPForest4 <- randomForest(formula = APURCH ~ age + paydep + BALCHQ + BALSAV + TOTDEP + BALLOAN + BALLOC + 
                                     BALMRGG + TXBRAN + TXATM + TXPOS + TXCHQ + TXWEB + TXTEL + TOTSERV + 
                                    CH_NM_SERV + CH_NM_PRD + valsegm + numrr_1 + avginc_1 + avginv_1 + gender,
                                   data = filter(RSP2, Sample =="Estimation"),
                                   importance = TRUE,
                                   ntree = 1000, mtry = 4)
RSPForest6 <- randomForest(formula = APURCH ~ age + paydep + BALCHQ + BALSAV + TOTDEP + BALLOAN + BALLOC + 
                             BALMRGG + TXBRAN + TXATM + TXPOS + TXCHQ + TXWEB + TXTEL + TOTSERV + 
                             CH_NM_SERV + CH_NM_PRD + valsegm + numrr_1 + avginc_1 + avginv_1 + gender,
                           data = filter(RSP2, Sample =="Estimation"),
                           importance = TRUE,
                           ntree = 1000, mtry = 6)
RSPForest8 <- randomForest(formula = APURCH ~ age + paydep + BALCHQ + BALSAV + TOTDEP + BALLOAN + BALLOC + 
                             BALMRGG + TXBRAN + TXATM + TXPOS + TXCHQ + TXWEB + TXTEL + TOTSERV + 
                             CH_NM_SERV + CH_NM_PRD + valsegm + numrr_1 + avginc_1 + avginv_1 + gender,
                           data = filter(RSP2, Sample =="Estimation"),
                           importance = TRUE,
                           ntree = 1000, mtry = 8)


varImpPlot(RSPForest4,type = 2,
           main="RSPForest4",
           cex =0.7)
varImpPlot(RSPForest6,type = 2,
           main="RSPForest6",
           cex =0.7)
varImpPlot(RSPForest8,type = 2,
           main="RSPForest8",
           cex =0.7)

corrMatrix <- cor(select_if(RSP2, is.numeric))
corrplot(corrMatrix,method="number",type="lower",
         diag = FALSE,number.cex = 0.7)

RSP2logis = glm(formula = APURCH ~ age + paydep + BALCHQ + BALSAV + TOTDEP + BALLOAN + BALLOC + 
                  BALMRGG + TXBRAN + TXATM + TXPOS + TXCHQ + TXWEB + TXTEL + TOTSERV + 
                  CH_NM_SERV + CH_NM_PRD + valsegm + numrr_1 + avginc_1 + avginv_1 + gender,
               data = filter(RSP2, Sample =="Estimation"),
               family = binomial(logit))
summary(RSP2logis)
Anova(RSP2logis)

RSP2Step = step(RSP2logis, direction = "both")
summary(RSP2Step)

lift.chart(modelList = c("RSPForest4","RSPForest6", "RSPForest8", "RSP2logis", "RSP2Step"),
           data = filter(RSP2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.022, type = "cumulative",
           sub = "Validation")

RSP2NNstep <- Nnet(formula = APURCH ~ age + paydep + BALCHQ + BALSAV + TOTDEP + 
                     BALLOAN + BALLOC + BALMRGG + TXBRAN + TXTEL + TOTSERV + CH_NM_SERV + 
                     valsegm,
                  data = filter(RSP2, Sample =="Estimation"),
                  decay = 0.3, size = 4)

RSP2NNall = Nnet(formula = APURCH ~ age + paydep + BALCHQ + BALSAV + TOTDEP + BALLOAN + BALLOC + 
                   BALMRGG + TXBRAN + TXATM + TXPOS + TXCHQ + TXWEB + TXTEL + TOTSERV + 
                   CH_NM_SERV + CH_NM_PRD + valsegm + numrr_1 + avginc_1 + avginv_1 + gender,
                data = filter(RSP2, Sample =="Estimation"),
                decay = 0.3, size = 4)


lift.chart(modelList = c("RSPForest4", "RSP2logis", "RSP2Step", "RSP2NNstep", "RSP2NNall"),
           data = filter(RSP2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.022, type = "cumulative",
           sub = "Validation")


### Non linearity ###
plotmean_prop(RSP2, "TOTDEP")
plotmean_prop(RSP2, "age")
plotmean_prop(RSP2, "BALCHQ")
plotmean_prop(RSP2, "valsegm")
plotmean_prop(RSP2, "avginc_1")
plotmean_prop(RSP2, "TXBRAN")


### Transformations ### 
summary(RSP2$age)
summary(RSP2$BALCHQ)
summary(RSP2$TXBRAN)

RSP2$age.log = log(RSP2$age + 1)
RSP2$TXBRAN.log = log(RSP2$TXBRAN + 1)
RSP2$BALCHQ.sq = (RSP2$BALCHQ)^2

logRF <- randomForest(formula = APURCH ~ age.log + paydep + BALCHQ + BALSAV + BALCHQ.sq + TOTDEP + BALLOAN + BALLOC + 
                        BALMRGG + TXBRAN.log + TXATM + TXPOS + TXCHQ + TXWEB + TXTEL + TOTSERV + 
                        CH_NM_SERV + CH_NM_PRD + valsegm + numrr_1 + avginc_1 + avginv_1 + gender,
                      data = filter(RSP2, Sample =="Estimation"),
                      importance = TRUE,
                      ntree = 1000, mtry = 4)

varImpPlot(logRF, type = 2,
           main = "logRF",
           cex = 0.7)

logRSPlogis = glm(formula = APURCH ~ age.log + paydep + BALCHQ + BALSAV + BALCHQ.sq + TOTDEP + BALLOAN + BALLOC + 
                    BALMRGG + TXBRAN.log + TXATM + TXPOS + TXCHQ + TXWEB + TXTEL + TOTSERV + 
                    CH_NM_SERV + CH_NM_PRD + valsegm + numrr_1 + avginc_1 + avginv_1 + gender,
                  data = filter(RSP2, Sample =="Estimation"),
                  family = binomial(logit))


lift.chart(modelList = c("logRF", "RSP2logis","RSP2Step", "RSPForest4", "logRSPlogis"),
           data = filter(RSP2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.022, type = "cumulative",
           sub = "Validation")

### Final Lift Chart ### 
lift.chart(modelList = c("RSPForest4"),
           data = filter(RSP2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.022, type = "cumulative",
           sub = "Validation")

varImpPlot(RSPForest4, type = 2,
           main = "RSPForest4",
           cex = 0.8)

dplot(RSPForest4, "TOTDEP")
dplot(RSPForest4, "age")
dplot(RSPForest4, "avginv_1")
dplot(RSPForest4, "avginc_1")
dplot(RSPForest4, "BALCHQ")
dplot(RSPForest4, "TXBRAN")

RSPForest.trim <- partial(RSPForest4, pred.var = "BALCHQ",
                          prob = TRUE,
                          which.class = 2,
                          quantiles = TRUE, probs = seq(from = 0.0, to = 0.9, by = 0.02),
                          plot= FALSE)
plotPartial(RSPForest.trim,
            rug = TRUE,
            train = filter(RSP2, Sample == "Estimation"))

RSPForest.trim <- partial(RSPForest4, pred.var = "TOTDEP",
                          prob = TRUE,
                          which.class = 2,
                          quantiles = TRUE, probs = seq(from = 0.0, to = 0.9, by = 0.02),
                          plot= FALSE)
plotPartial(RSPForest.trim,
            rug = TRUE,
            train = filter(RSP2, Sample == "Estimation"))

RSPForest.trim <- partial(RSPForest4, pred.var = "avginc_1",
                          prob = TRUE,
                          which.class = 2,
                          quantiles = TRUE, probs = seq(from = 0.1, to = 0.9, by = 0.02),
                          plot= FALSE)
plotPartial(RSPForest.trim,
            rug = TRUE,
            train = filter(RSP2, Sample == "Estimation"))

RSPForest.trim <- partial(RSPForest4, pred.var = "avginv_1",
                          prob = TRUE,
                          which.class = 2,
                          quantiles = TRUE, probs = seq(from = 0.1, to = 0.9, by = 0.02),
                          plot= FALSE)
plotPartial(RSPForest.trim,
            rug = TRUE,
            train = filter(RSP2, Sample == "Estimation"))

RSPForest.trim <- partial(RSPForest4, pred.var = "BALCHQ",
                          prob = TRUE,
                          which.class = 2,
                          quantiles = TRUE, probs = seq(from = 0.0, to = 0.9, by = 0.02),
                          plot= FALSE)
plotPartial(RSPForest.trim,
            rug = TRUE,
            train = filter(RSP2, Sample == "Estimation"))

RSPForest.trim <- partial(RSPForest4, pred.var = "TXBRAN",
                          prob = TRUE,
                          which.class = 2,
                          quantiles = TRUE, probs = seq(from = 0.0, to = 0.9, by = 0.02),
                          plot= FALSE)
plotPartial(RSPForest.trim,
            rug = TRUE,
            train = filter(RSP2, Sample == "Estimation"))

