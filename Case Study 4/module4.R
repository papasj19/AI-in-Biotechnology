
#=================================================================================
#  Week 4  Neural Networks
#
#================================================================================

getwd()

install.packages("rsample")
library(dplyr)


training_tbl = read.table("training_tbl_5min_test3.csv", header = TRUE, sep = ";", dec = ",")


test_start <- "2019-08-01 02:00:00"
test_end <- "2019-08-30 02:00:00"


set.seed (25000)
index <- sample(1:nrow(training_tbl[2:4]),round(0.75*nrow(training_tbl[2:4])))
maxs <- apply(training_tbl[2:4], 2, max) 
mins <- apply(training_tbl[2:4],2, min)
scaled <- as.data.frame(scale(training_tbl[2:4], center = mins, scale = maxs - mins))
train_ <- scaled[index,]

test_tbl <- training_tbl[training_tbl$date_hora_sync >=test_start & training_tbl$date_hora_sync < test_end,]
test_ <- as.data.frame(scale(test_tbl[2:4], center = mins, scale = maxs - mins))

n <- names(train_)
f <- as.formula(paste("Insu_5min ~", paste(n[!n %in% "Insu_5min"], collapse = " + ")))


###################################################################################
# 1-Hidden Layers, Layer-1 2-neurons, , TANH activation
##################################################################################
install.packages("neuralnet")
library(neuralnet)


nn1 <- neuralnet(f,data=train_,hidden=c(2),stepmax=1e+06, learningrate=0.01,linear.output=TRUE, act.fct = "tanh")
plot(nn1,rep='best')

###################################################################################
# 1-Hidden Layers, Layer-1 3-neurons, , TANH activation
##################################################################################

nn2 <- neuralnet(f,data=train_,hidden=c(5),stepmax=1e+07, learningrate=0.01,linear.output=TRUE, act.fct = "tanh")
plot(nn2,rep='best')

###################################################################################
# 2-Hidden Layers, Layer-1 2-neurons, Layer-2 1-Neuron, TANH activation
##################################################################################

nn3 <- neuralnet(f,data=train_,hidden=c(2,1),stepmax=1e+06, learningrate=0.01,linear.output=TRUE, act.fct = "tanh")
plot(nn3,rep='best')

###################################################################################
# 2-Hidden Layers, Layer-1 5-neurons, Layer-2 1-Neuron, TANH activation
##################################################################################

nn4 <- neuralnet(f,data=train_,hidden=c(5,1),stepmax=1e+07, learningrate=0.01,linear.output=TRUE, act.fct = "tanh")
plot(nn4,rep='best')

###################################################################################
# Calcul Errors
###################################################################################

# MSE NN1
pr.nn1 <- compute(nn1,test_[,1:3])
pr.nn1_ <- pr.nn1$net.result*(max(training_tbl$Insu_5min)-min(training_tbl$Insu_5min))+min(training_tbl$Insu_5min)
test.r1 <- (test_$Insu_5min)*(max(training_tbl$Insu_5min)-min(training_tbl$Insu_5min))+min(training_tbl$Insu_5min)
MSE.nn1 <- sum((test.r1 - pr.nn1_)^2)/nrow(test_)

## Training Error NN1
NN1_Train_sse <- sum((nn1$net.result - train_[1])^2)/2
paste ("Train SSE NN1:", round(NN1_Train_sse,4))

## Test Error NN1
Test_NN1_Output <- compute (nn1, test_[,1:3])$net.result
NN1_Test_sse <-sum((Test_NN1_Output - test_ [,1])^2)/2
paste ("Test SSE NN1:", round(NN1_Test_sse,4))

# MSE NN2 NN1
pr.nn2 <- compute(nn2,test_[,1:3])
pr.nn2_ <- pr.nn2$net.result*(max(training_tbl$Insu_5min)-min(training_tbl$Insu_5min))+min(training_tbl$Insu_5min)
test.r2 <- (test_$Insu_5min)*(max(training_tbl$Insu_5min)-min(training_tbl$Insu_5min))+min(training_tbl$Insu_5min)
MSE.nn2 <- sum((test.r2 - pr.nn2_)^2)/nrow(test_)

## Training Error NN2
NN2_Train_sse <- sum((nn2$net.result - train_[1])^2)/2
paste ("Train SSE NN2:", round(NN2_Train_sse,4))

## Test Error NN2
Test_NN2_Output <- compute (nn2, test_[,1:3])$net.result
NN2_Test_sse <-sum((Test_NN2_Output - test_ [,1])^2)/2
paste ("Test SSE NN2:", round(NN2_Train_sse,4))

# MSE NN3
pr.nn3 <- compute(nn3,test_[,1:3])
pr.nn3_ <- pr.nn3$net.result*(max(training_tbl$Insu_5min)-min(training_tbl$Insu_5min))+min(training_tbl$Insu_5min)
test.r3 <- (test_$Insu_5min)*(max(training_tbl$Insu_5min)-min(training_tbl$Insu_5min))+min(training_tbl$Insu_5min)
MSE.nn3 <- sum((test.r3 - pr.nn3_)^2)/nrow(test_)

## Training Error NN3
NN3_Train_sse <- sum((nn3$net.result - train_[1])^2)/2
paste ("Train SSE NN3:", round(NN3_Train_sse,4))

## Test Error NN3
Test_NN3_Output <- compute (nn3, test_[,1:3])$net.result
NN3_Test_sse <-sum((Test_NN3_Output - test_ [,1])^2)/2
paste ("Test SSE NN3:", round(NN3_Test_sse,4))

# MSE NN4
pr.nn4 <- compute(nn4,test_[,1:3])
pr.nn4_ <- pr.nn4$net.result*(max(training_tbl$Insu_5min)-min(training_tbl$Insu_5min))+min(training_tbl$Insu_5min)
test.r4 <- (test_$Insu_5min)*(max(training_tbl$Insu_5min)-min(training_tbl$Insu_5min))+min(training_tbl$Insu_5min)
MSE.nn4 <- sum((test.r4 - pr.nn4_)^2)/nrow(test_)

## Training Error NN4
NN4_Train_sse <- sum((nn4$net.result - train_[1])^2)/2
paste ("Train SSE NN4:", round(NN4_Train_sse,4))

## Test Error NN4
Test_NN4_Output <- compute (nn4, test_[,1:3])$net.result
NN4_Test_sse <-sum((Test_NN4_Output - test_ [,1])^2)/2
paste ("Test SSE NN4:", round(NN4_Train_sse,4))

print(paste("MSE Lm:",MSE.lm1))
print(paste("MSE Rf:",MSE.rf))
print(paste("MSE Rf2:",MSE.rf2))
print(paste("MSE Cb:",MSE.cb))
print(paste("MSE M5:",MSE.rm5))
print(paste("MSE NN1:",MSE.nn1))
print(paste("MSE NN2:",MSE.nn2))
print(paste("MSE NN3:",MSE.nn3))
print(paste("MSE NN4:",MSE.nn4))

#################################################################################
# Plot Xarxes Neuronals
#################################################################################
dev.off()
plot.new()

par(op)
op<-par(pty="s",mfrow=c(2,2), new = FALSE)
plot(nn1,rep='best')
plot(nn2,rep='best')
plot(nn3,rep='best')
plot(nn4,rep='best')
par(op)

##################################################################################
#  ROC Plots NN1, NN2, NN3, NN4
##################################################################################

# If not installed:
install.packages("pROC")

# Then load the package:
library(pROC)

dev.off()
par(op)
plot.new()

op<-par(pty="s",mfrow=c(2,2), new = FALSE)

roc (test.r1,pr.nn1_[,1], plot=TRUE, smooth=TRUE,
     col=rainbow(7), main="ROC NN1", xlab="Specificity", 
     ylab="Sensitivity", ylim=c(0,1),xlim=c(1,0),
     print.auc=TRUE, auc.polygon=TRUE,auc.polygon.col="#377eb822",lwd=4)

roc (test.r2,pr.nn2_[,1], plot=TRUE, smooth=TRUE,
     col=rainbow(7), main="ROC NN2", xlab="Specificity", 
     ylab="Sensitivity", ylim=c(0,1),xlim=c(1,0),
     print.auc=TRUE, auc.polygon=TRUE,auc.polygon.col="#377eb822",lwd=4)

roc (test.r3,pr.nn3_[,1], plot=TRUE, smooth=TRUE,
     col=rainbow(7), main="ROC NN3", xlab="Specificity", 
     ylab="Sensitivity", ylim=c(0,1),xlim=c(1,0),
     print.auc=TRUE, auc.polygon=TRUE,auc.polygon.col="#377eb822",lwd=4)  

roc (test.r4,pr.nn4_[,1], plot=TRUE, smooth=TRUE,
     col=rainbow(7), main="ROC NN4", xlab="Specificity", 
     ylab="Sensitivity", ylim=c(0,1),xlim=c(1,0),
     print.auc=TRUE, auc.polygon=TRUE,auc.polygon.col="#377eb822",lwd=4) 

par(op)

########################################################################
#  Impressio RROCs
########################################################################
install.packages("mplot") 
library(mplot)  

install.packages("auditor")  # Install from CRAN
library(auditor)

exp_lm <- DALEX::explain(fit.model.lm1, data=train_tbl, y=train_tbl$Insu_5min)
mr_lm <- model_residual (exp_lm)
exp_rf <- DALEX::explain(fit.model.rf, data=train_tbl, y=train_tbl$Insu_5min)
mr_rf <- model_residual (exp_rf)
exp_cb <- DALEX::explain(fit.model.cb, data=train_tbl, y=train_tbl$Insu_5min)
mr_cb <- model_residual (exp_cb)

#didnt work!!!!!!!!!!!!
exp_rm5 <- DALEX::explain(fit.model.rm5, data=train_tbl, y=train_tbl$Insu_5min)
mr_rm5 <- model_residual (exp_rm5)

exp_rf2 <- DALEX::explain(fit.model.rf2, data=train_tbl, y=train_tbl$Insu_5min)
mr_rf2 <- model_residual (exp_rf2)


exp_nn1 <- DALEX::explain(nn1, data=train_tbl, y=train_tbl$Insu_5min)
mr_nn1 <- model_residual (exp_nn1)
exp_nn2 <- DALEX::explain(nn2, data=train_tbl, y=train_tbl$Insu_5min)
mr_nn2 <- model_residual (exp_nn2)
exp_nn3 <- DALEX::explain(nn3, data=train_tbl, y=train_tbl$Insu_5min)
mr_nn3 <- model_residual (exp_nn3)
exp_nn4 <- DALEX::explain(nn4, data=train_tbl, y=train_tbl$Insu_5min)
mr_nn4 <- model_residual (exp_nn4)

#plot result

install.packages("mplot")
library(mplot)


dev.off()
par(op)

op<-par(pty="s",mfrow=c(2,2), new = TRUE)
#not work 
plot (mr_lm,mr_nn1, plot_type="rroc")

plot (mr_lm,mr_rf, type="rroc")
plot (mr_lm,mr_rf2, type="rroc")

plot (mr_cb,mr_nn1, type="rroc")
plot (mr_rf,mr_nn1, type="rroc")
plot (mr_lm,mr_rf2, type="rroc")
plot (mr_rf,mr_rf2, type="rroc")
par(op)

s_lm <- score_rroc (exp_lm)
s_cb <- score_rroc (exp_cb)
s_rf <- score_rroc (exp_rf)
s_rf2 <- score_rroc (exp_rf2)
s_nn1 <- score_rroc (exp_nn1)
s_nn2 <- score_rroc (exp_nn2)
s_nn3 <- score_rroc (exp_nn3)
s_nn4 <- score_rroc (exp_nn4)

print (paste("Lm:",s_lm, "Cb",s_cb))
print (paste("Lm:",s_lm, "Rf1",s_rf, "Rf2",s_rf2, "NN1",s_nn1))
print (paste("Lm:",s_lm, "NN1",s_nn1))

#################################################################################33
# Bar plot of results
#################################################################################33

install.packages("ggplot2")
library(ggplot2)


Regression_NN_Errors <- tibble(Network = rep(c("NN1", "NN2", "NN3", "NN4"), each = 2), 
                               DataSet = rep(c("Train", "Test"), time = 4), 
                               SSE = c(
                                 NN1_Train_sse, NN1_Test_sse, 
                                 NN2_Train_sse, NN2_Test_sse, 
                                 NN3_Train_sse, NN3_Test_sse, 
                                 NN4_Train_sse, NN4_Test_sse
                               ))

dev.off()
plot.new()

Regression_NN_Errors %>% 
  ggplot(aes(Network, SSE, fill = DataSet)) + 
  geom_col(position = "dodge") + 
  ggtitle("Regressi? ANN's SSE")

dev.off()
plot.new()

#######################################################################################
#  Fi xarxa neuronal
######################################################################################

# -------------------------------------------------------------------------
# Imprimir les Grafiques de les dades de la predicio contra les dades
# reals de les quantitats de Insulina vs les dotacions de Carb i lectures CGM
# ----------------------------------------------------------------------------

# prediccio del dia 22/09

test_start2 <- "2019-08-22 02:00:00"
test_end2 <- "2019-08-23 02:00:00"

test_tbl2 <- training_tbl[training_tbl$date_hora_sync >=test_start2 & training_tbl$date_hora_sync < test_end2,]
test_2 <- as.data.frame(scale(test_tbl2[2:4], center = mins, scale = maxs - mins))

test_2b <- cbind(test_tbl2$date_hora_sync,test_2)
colnames(test_2b) <- c("date_hora_sync","Insu_5min","Carbs_5min","Sgv_5min")

install.packages("RWeka")
library(RWeka)


# Regressions #################################
rlm12 <- predict(fit.model.lm1,test_tbl2)
rlrf2 <- predict(fit.model.rf,test_tbl2)
rlrf22 <- predict(fit.model.rf2,test_tbl2)
rlm22 <- predict(fit.model.lm2,test_tbl2)
rlcb2 <- predict(fit.model.cb,test_tbl2)
rlrm52 <- predict(fit.model.rm5,test_tbl2)

# Regressio Xarxa Neuronal #####################
pr.nn1_2 <- compute(nn1,test_2[,1:3])
pr.nn1_2_ <- pr.nn1_2$net.result*(max(training_tbl$Insu_5min)-min(training_tbl$Insu_5min))+min(training_tbl$Insu_5min)
#pr.nn1_2_ <- pr.nn1_2$net.result

dev.off()
plot.new()

# PLot Linear Regression vs test #################################3
plot(rlm12, type="s",xlab ="time",ylab="Insu_5min",yaxt="none", 
     xaxt="none",col="blue", lwd=2,lty=4, ylim=c(0,1))
      op<-par(pty="m",mfrow=c(1,1), new = TRUE)
plot (test_tbl2$date_hora_sync,test_tbl2$Insu_5min, type ="s",
      yaxt="none",xlab ="time",ylab="Insu_5min", 
      main="Linear Regression v. Test",col="red", lwd=2,
      ylim=c(0,1))
      axis(2,las=2)
      legend ("topright",legend=c("Test","Pred"), col=c("red","blue"), lty=1:2,cex=0.8)

# PLot Random Forest vs test #################################3
plot(rlrf2, type="s",xlab ="time",ylab="Insu_5min",yaxt="none", 
     xaxt="none",col="blue", lwd=2,lty=4, ylim=c(0,1))
     op<-par(pty="m",mfrow=c(1,1), new = TRUE)
plot (test_tbl2$date_hora_sync,test_tbl2$Insu_5min, type ="s",
      yaxt="none",xlab ="time",ylab="Insu_5min", 
      main="Random Forest v. Test",col="red", lwd=2,
      ylim=c(0,1))
      axis(2,las=2)
      legend ("topright",legend=c("Test","Pred"), col=c("red","blue"), lty=1:2,cex=0.8)

# PLot Random Forest2 vs test #################################3
      plot(rlrf22, type="s",xlab ="time",ylab="Insu_5min",yaxt="none", 
           xaxt="none",col="blue", lwd=2,lty=4, ylim=c(0,1))
      op<-par(pty="m",mfrow=c(1,1), new = TRUE)
      plot (test_tbl2$date_hora_sync,test_tbl2$Insu_5min, type ="s",
            yaxt="none",xlab ="time",ylab="Insu_5min", 
            main="Random Forest 2 v. Test",col="red", lwd=2,
            ylim=c(0,1))
      axis(2,las=2)
      legend ("topright",legend=c("Test","Pred"), col=c("red","blue"), lty=1:2,cex=0.8)
      
# PLot Cubist Regression vs test #################################3
plot(rlcb2, type="s",xlab ="time",ylab="Insu_5min",yaxt="none", 
           xaxt="none",col="blue", lwd=2,lty=4, ylim=c(0,1))
      op<-par(pty="m",mfrow=c(1,1), new = TRUE)
plot (test_tbl2$date_hora_sync,test_tbl2$Insu_5min, type ="s",
            yaxt="none",xlab ="time",ylab="Insu_5min", 
            main="Cubist Regression v. Test",col="red", lwd=2,
            ylim=c(0,1))
      axis(2,las=2)
      legend ("topright",legend=c("Test","Pred"), col=c("red","blue"), lty=1:2,cex=0.8)
      
# PLot Model Tree Regression vs test #################################3
plot(rlrm52, type="s",xlab ="time",ylab="Insu_5min",yaxt="none", 
           xaxt="none",col="blue", lwd=2,lty=4, ylim=c(0,1))
      op<-par(pty="m",mfrow=c(1,1), new = TRUE)
plot (test_tbl2$date_hora_sync,test_tbl2$Insu_5min, type ="s",
            yaxt="none",xlab ="time",ylab="Insu_5min", 
            main="Model Tree Regression v. Test",col="red", lwd=2,
            ylim=c(0,1))
      axis(2,las=2)
      legend ("topright",legend=c("Test","Pred"), col=c("red","blue"), lty=1:2,cex=0.8)
      
# PLot Xarxa Neuronal NN1 Regression vs test #################################3
      plot(pr.nn1_2_[,1], type="s",xlab ="time",ylab="Insu_5min",yaxt="none", 
           xaxt="none",col="blue", lwd=2,lty=4, ylim=c(0,1))
      op<-par(pty="m",mfrow=c(1,1), new = TRUE)
      plot (test_tbl2$date_hora_sync,test_tbl2$Insu_5min, type="s",
            yaxt="none",xlab ="time",ylab="Insu_5min", 
            main="Xarxa Neuronal NN1 v. Test",col="red", lwd=2,
            ylim=c(0,1))
      axis(2,las=2)
      legend ("topright",legend=c("Test","Pred"), col=c("red","blue"), lty=1:2,cex=0.8)      
  
      
      # PLot Linear Regression & NN1 vs test #################################3
      plot(rlm12, type="s",xlab ="time",ylab="Insu_5min",yaxt="none", 
           xaxt="none",col="blue", lwd=2,lty=4, ylim=c(0,1))
      op<-par(pty="m",mfrow=c(1,1), new = TRUE)
      plot(pr.nn1_2_[,1], type="s",xlab ="time",ylab="Insu_5min",yaxt="none", 
           xaxt="none",col="green", lwd=2,lty=4, ylim=c(0,1))
      op<-par(pty="m",mfrow=c(1,1), new = TRUE)
      
      plot (test_tbl2$date_hora_sync,test_tbl2$Insu_5min, type ="s",
            yaxt="none",xlab ="time",ylab="Insu_5min", 
            main="Linear Regression & Neural Netwrk v. Test",col="red", lwd=2,
            ylim=c(0,1))
      axis(2,las=2)
      legend ("topright",legend=c("Test","Pred LM", "Pred NN"), col=c("red","blue","green"), lty=1:2,cex=0.8)  
      

      dev.off()
      plot.new()
########################################################################
#   Fi  Analysis
#######################################################################
