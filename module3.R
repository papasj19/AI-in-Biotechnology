############################################################################
# Install packages
############################################################################
if (!require(recipes)) install.packages("recipes")
if (!require(cpp11)) install.packages("cpp11")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(readxl)) install.packages("readxl")
if (!require(knitr)) install.packages("knitr")
if (!require(ggplot2)) install.packages("ggplot2t")
if (!require(clubridate)) install.packages("lubridate")
if (!require(arules)) install.packages("arulest")
#if (!require(arulesViz)) install.packages("arulesViz")
if (!require(plyr)) install.packages("plyr")
if (!require(cdplyr)) install.packages("dplyr")
if (!require(caret)) install.packages("caret")
if (!require(ellipse)) install.packages("ellipse")
if (!require(e1071)) install.packages("e1071")
if (!require(kernlab)) install.packages("kernlab")
if (!require(randomForest)) install.packages("randomForest")
if (!require(mlbench)) install.packages("mlbench")
if (!require(caretEnsemble)) install.packages("caretEnsemble")
if (!require(pROC)) install.packages("pROC")
if (!require(GGally)) install.packages("GGally")
if (!require(auditor)) install.packages("auditor")



#libraries we had to do seperately bc they didnt work
install.packages("rsample")
install.packages("rJava")
install.packages("RWeka", dependencies=TRUE)
install.packages("randomForest")
install.packages("forecast")

library(forecast)
library(RWeka)
library(rJava)
library(randomForest)

# Call libraries
library(caret)
library(plyr)
library(recipes)
library(dplyr)
!library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
#library(arulesViz)
library(plyr)
library(dplyr)
library(randomForest)
library(kernlab)
library(e1071)
library(caret)
library(ellipse)
library(tidyverse)
library (fpp2)
library (forecast)
library (MARSS)
library(tidyquant)
library(plotly)
library(parsnip)
library(rsample)
#library(yardstick)
library(broom)
library(rpart)
library(rpart.plot)
library(xgboost)
library(ggplot2)
library(mlbench)
library(caretEnsemble)
library(lars)
library(relaxo)
library(pROC)
!library(tidyverse)
library(neuralnet)
library(GGally)
library(auditor)


#rm(list = ls())
getwd()

# working directory
#setwd ("C:\\Users\\JoanCarles\\iCloudDrive\\Documents\\_doctorat_versio_treball\\machine_learning")

#setwd ("C:\\Users\\papasj\\Downloads")

#thingy about session-->set working directory to get the thing


# load data

training_tbl = read.table("training_tbl_5min_test3.csv", header = TRUE, sep = ";", dec = ",")
colnames(training_tbl) <- c("date_hora_sync", "Insu_5min", "Carbs_5min", "Sgv_5min")
training_tbl$date_hora_sync <- as.POSIXct(training_tbl$date_hora_sync, format = "%Y%m%dT%H%M%S")

split_obj <- rsample::initial_time_split(training_tbl, prop = 0.75)


train_tbl <- split_obj %>% training()
test_tbl  <- split_obj %>% testing()

test_start <- "2019-08-01 02:00:00"
test_end <- "2019-08-30 02:00:00"
test_tbl <- training_tbl[training_tbl$date_hora_sync >=test_start & training_tbl$date_hora_sync < test_end,]
# ---------------------------------------------------------------
# regresions dels models de simulacio i prediccio de insulina
# ----------------------------------------------------------------

fit.model.lm1 <- lm (Insu_5min ~ Carbs_5min+Sgv_5min, data = train_tbl )
fit.model.rf <- train(Insu_5min ~ Carbs_5min+Sgv_5min, data = train_tbl, method="qrf")
fit.model.lm2 <- train(Insu_5min ~ Carbs_5min+Sgv_5min, data = train_tbl, method="lm")
fit.model.cb <- train(Insu_5min ~ Carbs_5min+Sgv_5min, data = train_tbl, method="cubist")
#couldnt get the rm5 to work
#fit.model.rm5 <- train(Insu_5min ~ Carbs_5min+Sgv_5min, data = train_tbl, method="M5")
fit.model.rf2 <- randomForest(Insu_5min ~ Carbs_5min+Sgv_5min, data = train_tbl)


print(summary (fit.model.lm1))
print(summary (fit.model.rf))
print(summary (fit.model.lm2))
print(summary (fit.model.cb))
#print(summary (fit.model.rm5))
print(summary (fit.model.rf2))



# -------------------------------------------------------------------
# comparativa dels residuals dels models de entrenament vs els reals
# -------------------------------------------------------------------

checkresiduals (fit.model.lm1)
checkresiduals (fit.model.rf)
checkresiduals (fit.model.rf2)
checkresiduals (fit.model.lm2)
checkresiduals (fit.model.cb)
#checkresiduals (fit.model.rm5)

#----------------------------------------------------------------------
# Models de prediccio amb els models de regressio de entrenament
# contra els valors reals de test
#----------------------------------------------------------------------

rlm1 <- predict(fit.model.lm1,test_tbl)
rlrf <- predict(fit.model.rf,test_tbl)
rlrf2 <- predict(fit.model.rf2,test_tbl)
rlm2 <- predict(fit.model.lm2,test_tbl)
rlcb <- predict(fit.model.cb,test_tbl)
#rlrm5 <- predict(fit.model.rm5,test_tbl)


# -------------------------------------------------------------------------
# Imprimir les Grafiques de les dades de la predicio contra les dades
# reals de les quantitats de Insulina vs les dotacions de Carb i lectures CGM
# ----------------------------------------------------------------------------

dev.off()
plot.new()

plot(rlm1, type="p",xlab ="time",ylab="Insu_5min",col="blue")
op<-par(pty="m",mfrow=c(1,1), new = TRUE)
plot (test_tbl$Insu_5min, type ="l", xlab ="time",ylab="Insu_5min", main="Lm",col="red")

plot(rlrf, type="p",xlab ="time",ylab="Insu_5min", main="Rf",col="green")
op<-par(pty="m",mfrow=c(1,1), new = TRUE)
plot (test_tbl$Insu_5min, type ="l",xlab ="time",ylab="Insu_5min", main="Rf", col="red")

plot(rlrf2, type="p",xlab ="time",ylab="Insu_5min", main="Rf",col="green")
op<-par(pty="m",mfrow=c(1,1), new = TRUE)
plot (test_tbl$Insu_5min, type ="l",xlab ="time",ylab="Insu_5min", main="Rf", col="red")


plot(rlcb, type ="p",xlab ="time",ylab="Insu_5min", main="cb",col="orange")
op<-par(pty="m",mfrow=c(1,1), new = TRUE)
plot (test_tbl$Insu_5min, type ="l",xlab ="time",ylab="Insu_5min", main="cb", col="red")

#plot(rlrm5, type ="p", xlab ="time",ylab="Insu_5min",col="black")
op<-par(pty="m",mfrow=c(1,1), new = TRUE)
plot (test_tbl$Insu_5min, type ="l",xlab ="time",ylab="Insu_5min", main="Model Tree", col="red")


dev.off()
par(op)

# -------------------------------------------------------------------------
#  Imprimir Sumaris
# --------------------------------------------------------------------------

print(summary (fit.model.lm1))
print(summary (fit.model.rf))
print(summary (fit.model.lm2))
print(summary (fit.model.cb))
#print(summary (fit.model.rm5))


# ------------------------------------------------------------------
#  Modelar les prediccions fent servir el test_tbl
#-------------------------------------------------------------------

rlm1 <- predict(fit.model.lm1,test_tbl,type=c("response"))
rlrf <- predict(fit.model.rf,test_tbl,type=c("raw"))
rlrf2 <- predict(fit.model.rf,test_tbl,type=c("raw"))

rlm2 <- predict(fit.model.lm2,test_tbl,type=c("raw"))
rlcb <- predict(fit.model.cb,test_tbl,type=c("raw"))
#rlrm5 <- predict(fit.model.rm5,test_tbl,type=c("raw"))



install.packages("pROC")
library(pROC)



# ---------------------------------------------------------------
# Impimir curves ROC i AUC
#----------------------------------------------------------------
dev.off()
plot.new()
par(op)

op<-par(pty="s",mfrow=c(2,2), new = FALSE)

roc (test_tbl$Insu_5min,rlm1, plot=TRUE, smooth=TRUE,
     col=rainbow(7), main="ROC Linear Regression", xlab="Specificity",
     ylab="Sensitivity", ylim=c(0,1),xlim=c(1,0),
     print.auc=TRUE, auc.polygon=TRUE,auc.polygon.col="#377eb822",lwd=4)

roc (test_tbl$Insu_5min,rlrf, plot=TRUE, smooth=TRUE,
     col=rainbow(7), main="ROC Random Forest", xlab="Specificity",
     ylab="Sensitivity", ylim=c(0,1),xlim=c(1,0),
     print.auc=TRUE, auc.polygon=TRUE,auc.polygon.col="#377eb822",lwd=4)

roc (test_tbl$Insu_5min,rlrf2, plot=TRUE, smooth=TRUE,
     col=rainbow(7), main="ROC Random Forest", xlab="Specificity",
     ylab="Sensitivity", ylim=c(0,1),xlim=c(1,0),
     print.auc=TRUE, auc.polygon=TRUE,auc.polygon.col="#377eb822",lwd=4)

roc (test_tbl$Insu_5min,rlcb, plot=TRUE, smooth=TRUE,
     col=rainbow(7), main="ROC Cubist", xlab="Specificity",
     ylab="Sensitivity", ylim=c(0,1),xlim=c(1,0),
     print.auc=TRUE, auc.polygon=TRUE,auc.polygon.col="#377eb822",lwd=4)

#roc (test_tbl$Insu_5min,rlrm5, plot=TRUE, smooth=TRUE,
     col=rainbow(7), main="ROC Model Tree", xlab="Specificity",
     ylab="Sensitivity", ylim=c(0,1),xlim=c(1,0),
     print.auc=TRUE, auc.polygon=TRUE,auc.polygon.col="#377eb822",lwd=4)

par(op)

#auc(rlm1)
#auc(rlrf)
#auc(rlcb)
#auc(rlrm5)


#########################################################################
#   Calcul MSE Mean Square Error
#########################################################################

summary(fit.model.lm1)
MSE.lm1 <- sum((rlm1 - test_tbl$Insu_5min)^2)/nrow(test_tbl)

summary(fit.model.rf)
MSE.rf <- sum((rlrf - test_tbl$Insu_5min)^2)/nrow(test_tbl)

summary(fit.model.rf2)
MSE.rf2 <- sum((rlrf2 - test_tbl$Insu_5min)^2)/nrow(test_tbl)


summary(fit.model.lm2)
MSE.lm2 <- sum((rlm2 - test_tbl$Insu_5min)^2)/nrow(test_tbl)

summary(fit.model.cb)
MSE.cb <- sum((rlcb - test_tbl$Insu_5min)^2)/nrow(test_tbl)

#summary(fit.model.rm5)
#MSE.rm5 <- sum((rlrm5 - test_tbl$Insu_5min)^2)/nrow(test_tbl)

# -------------------------------------------------------------------------------------
# Quant mes baix MSE (Mean Square Error) millor
# -------------------------------------------------------------------------------------

print(paste("MSE Lm:",MSE.lm1,"MSE Rf:",MSE.rf,"MSE Rf2:",MSE.rf2))
print(paste("MSE Cb:",MSE.cb))


########################################################################
#   Fi  Regression  Analysis
#######################################################################
