# Working Directory and Libraries ----

setwd("C:/Users/alpsr/Desktop/IE 48B Assignment 2/CBF")

library(data.table)
library(ggplot2)
library(genlasso)
library(rpart)
library(rattle)

# Functions ----
# Converts the a data frame in the wide format to the long format pivoting o n the id and class columns.
WidetoLong <- function(x){
  long_x <- melt(x, id.vars = c('ID', 'Class'))
  return(long_x)
}

# Data Manipulation ----
raw_data <- read.table("CBF_Train.txt", header = F ,
                       na.strings ="", stringsAsFactors= F)

raw_data <- setnames(raw_data, "V1", "Class")
raw_data <- data.table(raw_data)
raw_data[, "ID" := 1:.N]
head(raw_data)

setcolorder(raw_data, c("ID", "Class"))
cbf <- raw_data
cbf_long <- WidetoLong(raw_data)
cbf_long[,variable := as.numeric(gsub('V','',variable))-1]
head(cbf)

# Class 1 - Cylinder
ggplot(cbf_long[ID == 1])+
  geom_point(aes(x = variable, y = value))

# Class 2 - Bell
ggplot(cbf_long[ID == 5])+
  geom_point(aes(x = variable, y = value))

# Class 3 - Funnel
ggplot(cbf_long[ID == 4])+
  geom_point(aes(x = variable, y = value))

# Task 1 - 1d Fused Lasso ----
FusedLasso1D <- function(dataset, id){
  
  # Getting the time series
  trial = dataset[id,c(-1,-2)]
  trial
  out = trendfilter(unlist(trial), ord = 0)
  cv = cv.trendfilter(out, k = 10, verbose = FALSE)
  
  minlambda = cv$lambda.min
  minlambdaindex = which(out$lambda == cv$lambda.min)
  fitted = out$fit[,minlambdaindex]
  
  ret <- list(minlambda, fitted)

  return(ret)
}

RunFusedLasso1D <- function(dataset, k){
  

  FittedValues <- data.table()
  Lambda <- 0
  
  for(i in seq(1,30)){
    
    out <- 0
    out <- FusedLasso1D(dataset,i)
    
    FittedValues <- rbind(FittedValues, t(unlist(out[2])))
    Lambda[i] <- unlist(out[1])
    
  }
  
  ret <- list(Lambda, FittedValues)
  
  return(ret)
  
}

lasso <- RunFusedLasso1D(cbf, 10)

plot_real <- unlist(cbf[1,-c("ID","Class")])
plot_fit_lasso <- unlist(lasso[[2]][1,])
plot_lasso <- data.frame(seq(1,128),plot_fit_lasso, plot_real)

ggplot(plot_lasso)+
  geom_line(aes(x = seq.1..128., y = plot_fit_lasso, color = '1D Fused Lasso Fit'), size = 1)+
  geom_line(aes(x = seq.1..128., y = plot_real, color = 'Real Series'), size = 1)+
  labs(x = 'Time', y = 'Values', title = 'Real vs Fitted Values (1D Fused Lasso)', colour = 'Legend')

# Task 2 - Regression Trees ----

RegressionTrees <- function(dataset, id, k){
  
  series <- dataset[ID == id,]

  # Forming the test indices
  test_index = seq(1,120, by = k)
  remove_index = -test_index # Indices to be removed
  
  error <- Matrix(0,nrow = 10, ncol = 4)
  
  for(i in seq(1,10)){
    for(j in c(1,2,3,4)){
      train = series[remove_index,] # Training Series
      fit <- rpart(value~variable, train, control = rpart.control(cp = 0, minsplit = 20, minbucket = 10, maxdepth = j))
      series[, predict := predict(fit, series)]
      error[i,j] <- sum(series[variable %in% test_index,(value-predict)^2])
    }
    test_index = test_index + 1
    remove_index = -test_index
  }
  
  depth = colSums(error)
  final_fit = rpart(value~variable, series, control = rpart.control(cp = 0, minsplit = 20, minbucket = 10, maxdepth = which.min(depth)))
  
  return(list(error,depth,which.min(depth),predict(final_fit,series)))
}


RunRegressionTrees <- function(dataset, k){
  
  BestDepth <- 0
  Fitted <- matrix(,nrow = 30, ncol = 128)
  Errors <- matrix(,nrow = 30, ncol = 4)
  for(i in seq(1,30)){
    fit <- RegressionTrees(dataset,i,k)
    BestDepth[i] <- unlist(fit[3])
    Fitted[i,] <- unlist(fit[4]) #rbind(Fitted, (unlist(fit[3])))
    Errors[i,] <- unlist(fit[2]) #rbind(Errors, (unlist(fit[2])))
  }
  return(list(BestDepth, Fitted, Errors))
}

regtree <- RunRegressionTrees(cbf_long, 10)

plot_real <- unlist(cbf[1,-c("ID","Class")])
plot_fit_tree <- unlist(lasso[[2]][1,])
plot_tree <- data.frame(seq(1,128),plot_fit_tree, plot_real)

ggplot(plot_tree)+
  geom_line(aes(x = seq.1..128., y = plot_fit_tree, color = 'Regression Tree Fit'), size = 1)+
  geom_line(aes(x = seq.1..128., y = plot_real, color = 'Real Series'), size = 1)+
  labs(x = 'Time', y = 'Values', title = 'Real vs Fitted Values (Regression Trees)', colour = 'Legend')

# Task 3 - MSE Values ----

# MSE for the Lasso
fitted_lasso <- as.matrix(lasso[[2]])
real_series <- as.matrix(cbf[,-c("ID","Class")])
squared_errors_lasso  <- (fitted_lasso - real_series)^2
mse_lasso <- rowSums(squared_errors_lasso)/128

# MSE for the Regression Trees
fitted_trees <- as.matrix(regtree[[2]])
#real_series <- as.matrix(cbf[,-c("ID","Class")])
squared_errors_trees  <- (fitted_trees - real_series)^2
mse_trees <- rowSums(squared_errors_trees)/128

#Boxplot
ID <- c(rep("1D Fused Lasso",times = 30),rep("Regression Trees",times = 30))
MSE <- c(mse_lasso,mse_trees)
df1 <- data.frame(ID, MSE)

hist(mse_lasso)
hist(mse_trees)

ggplot(df1,aes(y = MSE, fill = ID))+
  geom_boxplot(aes(x = as.factor(ID)))+
  labs(title = 'Boxplot of MSE values of two representations', x = 'Representation')

# Task 4 - Predictions ----

class <- cbf$Class

# Classification Using Raw Series
# 1-NN Classifier Using the Raw Data

raw_data_dist <- dist(as.matrix(cbf[,c(-1,-2)]))
raw_data_dist <- as.matrix(raw_data_dist)
diag(raw_data_dist) <- 1000

raw_data_dist_order <- apply(raw_data_dist, 1, order)
closest_raw <- raw_data_dist_order[1,]
predicted_raw <- class[closest_raw]

table(class,predicted_raw)
acc_raw <- sum(class==predicted_raw)/length(predicted_raw)

# Classification Using FusedLasso Representation
# 1-NN Classifier Using the FusedLasso Representation

lasso_dist <- dist(as.matrix(fitted_lasso))
lasso_dist <- as.matrix(lasso_dist)
diag(lasso_dist) <- 1000

lasso_dist_order <- apply(lasso_dist, 1, order)
closest_lasso <- lasso_dist_order[1,]
predicted_lasso <- class[closest_lasso]

table(class,predicted_lasso)
acc_lasso <- sum(class==predicted_lasso)/length(predicted_lasso)

# Classification Using Regression Trees
# 1-NN Classifier Using Regression Trees

tree_dist <- dist(as.matrix(fitted_trees))
tree_dist <- as.matrix(tree_dist)
diag(tree_dist) <- 1000

tree_dist_order <- apply(tree_dist, 1, order)
closest_tree <- tree_dist_order[1,]
predicted_tree <- class[closest_tree]

table(class,predicted_tree)
acc_tree <- sum(class==predicted_tree)/length(predicted_tree)


