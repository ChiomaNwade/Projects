

library(ggplot2)
library(ggcorrplot)
library(psych)
## 
## Attaching package: 'psych'
## The following objects are masked from 'package:ggplot2':
## 
##     %+%, alpha
library(lattice)
library(caret)
library(neuralnet)
library(writexl)
library(readxl)
library(dplyr)
## 
## Attaching package: 'dplyr'
## The following object is masked from 'package:neuralnet':
## 
##     compute
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
library(faux)
## 
## ************
## Welcome to faux. For support and examples visit:
## https://debruine.github.io/faux/
## - Get and set global package options with: faux_options()
## ************
library(tidyr)

data <- read_excel("dea-to-NN-corr.xlsx")
# pairs.panels(originaldata[c(1:7)])

#pairs.panels(data[c(2:8)])

pairs.panels(data[c(2:8)], 
             method = "pearson",
             hist.col = "darkseagreen4",
             density = TRUE,
             ellipses = TRUE
             )

originaldata <- select(data, -last_col())
originaldata <- select(originaldata, -1)
summary_df <- as.data.frame(summary(originaldata))


write_xlsx(summary_df, "General statistics.xlsx")

summary(originaldata)
#####featureplot with efficiency
#data <- read_excel("dea-to-NN-corr.xlsx")

# pairs.panels(originaldata[c(1:7)])
#pairs.panels(data[c(2:8)])
#ccrcorr <- select(data, -1)
#ccrfeatureplot <- as.data.frame(cor(ccrcorr))
#write_xlsx(summary_df, "General statistics.xlsx")
#summary(originaldata)

library(readxl)
library(ggplot2)
library(ggcorrplot)
library(psych)
## 
## Attaching package: 'psych'
## The following objects are masked from 'package:ggplot2':
## 
##     %+%, alpha
library(lattice)
library(caret)
library(neuralnet)

data <- read_excel("dea-to-NN.xlsx")


# ***************** Regression ******************

Data_with_Supplier_Number <- data[,c(1,2:9)]
Data <- data[,c(1,2:9)]

# To use normalised data uncomment the below line
Data[,2:(ncol(Data)-1)] <- scale(Data[,2:(ncol(Data)-1)])


training_data_percentages <- seq(from = 0.4, to = 0.9, length.out = 1) # creating sequence to represent training data ratio
# the below loop iterates to vary training data amount and check the model performance 
for (t in training_data_percentages){
  
  indx_Partition <- createDataPartition(Data$Efficiency, p=t, list=FALSE) # index of training data
  training_data <- Data[indx_Partition,] # training dataset
  testing_data <- Data[-indx_Partition,] # testing dataset
  
  set.seed(12345)
  # Creating the model
  TrainedNeuralNet1 <- neuralnet(Efficiency ~ EDC + NSPM + TCS + WSLHC + NBWE + NSAT + NNDP, data = training_data, hidden = 2)
  
  plot(TrainedNeuralNet1)
  
  #Predicting on test data
  Predicted_Parameters <- neuralnet::compute(TrainedNeuralNet1, testing_data)
  Predicted_Net_Results <- Predicted_Parameters$net.result

  # Converting predcited outcomes as data frame
  Predicted_Data <- data.frame(Predicted_Net_Results)
  
  # *******************************************
  # Step 4 evaluating model performance
  # *******************************************
  
  par(mfrow=c(4,1))
  
  corln <- cor(Predicted_Data$Predicted_Net_Results, testing_data$Efficiency)
  cat("Correlation between Actual and Predicted Efficiency",corln,"\n")
  #plot(c(1:nrow(testing_data)), testing_data$Efficiency, type="l", col="green", lwd=5, xlab="Suppliers", ylab="Efficiency")
  plot(testing_data$Supplier, testing_data$Efficiency, type="l", col="green", lwd=5, xlab="Suppliers", ylab="Efficiency")
  #lines(c(1:nrow(testing_data)), Predicted_Data$Predicted_Net_Results, col="red", lwd=2)
  lines(testing_data$Supplier, Predicted_Data$Predicted_Net_Results, col="red", lwd=2)
  grid(nx = NULL, ny = NULL,
       lty = 2, col = "gray", lwd = 2)
  title("Suppliers' Efficiency Prediction")
  legend(x = "bottomright", bg="transparent", inset = 0.05,c("Actual Efficiencies","Predicted Efficiencies"), lwd=c(5,2), col=c("green","red"), y.intersp=0.5, cex=0.8)
  
  Efficiency_Table=cbind(testing_data[,c(1,9)], Predicted_Data)
  names(Efficiency_Table)[2]<-paste("Actual Efficiency")  
  names(Efficiency_Table)[3]<-paste("Predicted Efficiency") 
  
  # ***********************************************************
  # *********** Performance improvement ***********************
  # ***********************************************************
  # MORE HIDDEN LAYERS

  set.seed(12345)
  # Creating the model
  TrainedNeuralNet2 <- neuralnet(Efficiency ~ EDC + NSPM + TCS + WSLHC + NBWE + NSAT + NNDP, data = training_data, hidden = c(2,2,2,2))

  plot(TrainedNeuralNet2)

  #Predicting on test data
  Predicted_Parameters <- neuralnet::compute(TrainedNeuralNet2, testing_data)
  Predicted_Net_Results <- Predicted_Parameters$net.result
  # Converting predcited outcomes as data frame
  Predicted_Data <- data.frame(Predicted_Net_Results)

  corln <- cor(Predicted_Data$Predicted_Net_Results, testing_data$Efficiency)
  cat("Correlation between Actual and Predicted Efficiency",corln,"\n")

  #plot(c(1:nrow(testing_data)), testing_data$Efficiency, type="l", col="green", lwd=5, xlab="Suppliers", ylab="Efficiency")
  plot(testing_data$Supplier, testing_data$Efficiency, type="l", col="green", lwd=5, xlab="Suppliers", ylab="Efficiency")
  #lines(c(1:nrow(testing_data)), Predicted_Data$Predicted_Net_Results, col="red", lwd=2)
  lines(testing_data$Supplier, Predicted_Data$Predicted_Net_Results, col="red", lwd=2)
  grid(nx = NULL, ny = NULL,
       lty = 2, col = "gray", lwd = 2)
  title("Suppliers' Efficiency Prediction - More Hidden Layer")
  legend(x = "bottomright", bg="transparent", inset = 0.05,c("Actual Efficiencies","Predicted Efficiencies"), lwd=c(5,2), col=c("green","red"), y.intersp=0.5, cex=0.8)
  
  Efficiency_Table=cbind(Efficiency_Table, Predicted_Data)
  names(Efficiency_Table)[4]<-paste("Predicted Efficiencies with More Hidden Layers")  


  # ***********************************************************
  # *********** Performance improvement ***********************
  # ***********************************************************
  # WITH SOFTPLUS FUNCTION

  softplus <- function(x) {
    log(1 + exp(x))
  }

  set.seed(12345)
  # Creating the model
  TrainedNeuralNet3 <- neuralnet(Efficiency ~ EDC + NSPM + TCS + WSLHC + NBWE + NSAT + NNDP, data = training_data, hidden = 1, act.fct = softplus)

  plot(TrainedNeuralNet3)

  #Predicting on test data
  Predicted_Parameters <- neuralnet::compute(TrainedNeuralNet3, testing_data)
  Predicted_Net_Results <- Predicted_Parameters$net.result
  # Converting predcited outcomes as data frame
  Predicted_Data <- data.frame(Predicted_Net_Results)

  corln <- cor(Predicted_Data$Predicted_Net_Results, testing_data$Efficiency)
  cat("Correlation between Actual and Predicted Efficiency",corln,"\n")

  #plot(c(1:nrow(testing_data)), testing_data$Efficiency, type="l", col="green", lwd=5, xlab="Suppliers", ylab="Efficiency")
  plot(testing_data$Supplier, testing_data$Efficiency, type="l", col="green", lwd=5, xlab="Suppliers", ylab="Efficiency")
  #lines(c(1:nrow(testing_data)), Predicted_Data$Predicted_Net_Results, col="red", lwd=2)
  lines(testing_data$Supplier, Predicted_Data$Predicted_Net_Results, col="red", lwd=2)
  grid(nx = NULL, ny = NULL,
       lty = 2, col = "gray", lwd = 2)
  title("Suppliers' Efficiency Prediction - Softplus Activation Function")
  legend(x = "bottomright", bg="transparent", inset = 0.05,c("Actual Efficiencies","Predicted Efficiencies"), lwd=c(5,2), col=c("green","red"), y.intersp=0.5, cex=0.8)
  
  Efficiency_Table=cbind(Efficiency_Table, Predicted_Data)
  names(Efficiency_Table)[5]<-paste("Predicted Efficiencies with Softplus Activation")

  # ***********************************************************
  # *********** Performance improvement ***********************
  # ***********************************************************
  # WITH UNNORMALISATION

  unnormalize <- function(x) {
    return((x * (max(testing_data$Efficiency)) - min(testing_data$Efficiency)) + min(testing_data$Efficiency))
  }

  set.seed(12345)
  # Creating the model
  TrainedNeuralNet4 <- neuralnet(Efficiency ~ EDC + NSPM + TCS + WSLHC + NBWE + NSAT + NNDP, data = training_data, hidden = 2)

  plot(TrainedNeuralNet4)

  #Predicting on test data
  Predicted_Parameters <- neuralnet::compute(TrainedNeuralNet4, testing_data)
  Predicted_Net_Results <- Predicted_Parameters$net.result
  Predicted_Net_Results <- unnormalize(Predicted_Net_Results)
  # Converting predcited outcomes as data frame
  Predicted_Data <- data.frame(Predicted_Net_Results)

  corln <- cor(Predicted_Data$Predicted_Net_Results, testing_data$Efficiency)
  cat("Correlation between Actual and Predicted Efficiency",corln,"\n")

  #plot(c(1:nrow(testing_data)), testing_data$Efficiency, type="l", col="green", lwd=5, xlab="Suppliers", ylab="Efficiency")
  plot(testing_data$Supplier, testing_data$Efficiency, type="l", col="green", lwd=5, xlab="Suppliers", ylab="Efficiency")
  #lines(c(1:nrow(testing_data)), Predicted_Data$Predicted_Net_Results, col="red", lwd=2)
  lines(testing_data$Supplier, Predicted_Data$Predicted_Net_Results, col="red", lwd=2)
  grid(nx = NULL, ny = NULL,
       lty = 2, col = "gray", lwd = 2)
  title("Suppliers' Efficiency Prediction - Unnormalization")
  legend(x = "bottomright", bg="transparent", inset = 0.05,c("Actual Efficiencies","Predicted Efficiencies"), lwd=c(5,2), col=c("green","red"), y.intersp=0.5, cex=0.8)

  Efficiency_Table=cbind(Efficiency_Table, Predicted_Data)
  names(Efficiency_Table)[6]<-paste("Predicted Efficiencies with Unnormalisation")

}
## Correlation between Actual and Predicted Efficiency 0.7171497
## Correlation between Actual and Predicted Efficiency 0.6580913
## Correlation between Actual and Predicted Efficiency 0.7874179
## Correlation between Actual and Predicted Efficiency 0.7171497
Efficiency_Table_1 <- cbind(Efficiency_Table[,1:2]) 
Efficiency_Table_1 <- Efficiency_Table_1[order(-Efficiency_Table_1$`Actual Efficiency`),]

Efficiency_Table_2 <- cbind(Efficiency_Table[,c(1,5)]) 
Efficiency_Table_2 <- Efficiency_Table_2[order(-Efficiency_Table_2$`Predicted Efficiencies with Softplus Activation`),]

Final_Efficiency_Ranking <- cbind(Efficiency_Table_1, Efficiency_Table_2)

