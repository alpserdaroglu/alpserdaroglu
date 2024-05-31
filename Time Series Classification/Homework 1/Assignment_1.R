library(data.table)
library(ggplot2)
library(plotly)
library(scatterplot3d)
library(plotly)

setwd("C:/Users/alpsr/Desktop/Assignment 1/UWave_TRAIN")

# Functions ----

# Function to get the cumulative sum of a data table. i.e Acceleration -> Velocity -> Position.
# Takes the data in the wide format and returns it in the wide format.
getCumSum <- function(x){
  asd <- transpose(x[,c(-1,-2)])
  def <- transpose(cumsum(asd))
  fgh <- data.table(cbind(ID = 1:nrow(x), x[,2], def))
  return(fgh)
}

# Converts the a data frame in the wide format to the long format pivoting o n the id and class columns.
WidetoLong <- function(x){
  long_x <- melt(x, id.vars = c('ID', 'Class'))
  return(long_x)
}

# Reading the Data ----
x_raw <- read.table("uWaveGestureLibrary_X_TRAIN.txt", header = F ,
                    na.strings ="", stringsAsFactors= F)
y_raw <- read.table("uWaveGestureLibrary_Y_TRAIN.txt", header = F ,
                    na.strings ="", stringsAsFactors= F)
z_raw <- read.table("uWaveGestureLibrary_Z_TRAIN.txt", header = F ,
                    na.strings ="", stringsAsFactors= F)

# Acceleration ----

# add the id column and convert to data table
x_acc <- data.table(cbind(ID = 1:nrow(x_raw),x_raw))
y_acc <- data.table(cbind(ID = 1:nrow(y_raw),y_raw))
z_acc <- data.table(cbind(ID = 1:nrow(z_raw),z_raw))
colnames(x_acc)[2] <- 'Class'
colnames(y_acc)[2] <- 'Class'
colnames(z_acc)[2] <- 'Class'

x_acc_long <- WidetoLong(x_acc)
y_acc_long <- WidetoLong(y_acc)
z_acc_long <- WidetoLong(z_acc)

x_acc_long[,variable := as.numeric(gsub('V','',variable))-1]
y_acc_long[,variable := as.numeric(gsub('V','',variable))]
z_acc_long[,variable := as.numeric(gsub('V','',variable))]

setnames(x_acc_long, "value", "X_Acc")
setnames(y_acc_long, "value", "Y_Acc")
setnames(z_acc_long, "value", "Z_Acc")

# ordering the tables just in case
x_acc_long <- x_acc_long[order(ID, variable),]
y_acc_long <- y_acc_long[order(ID, variable),]
z_acc_long <- z_acc_long[order(ID, variable),]

acceleration <- x_acc_long[]
acceleration[,Y_Acc := y_acc_long[,Y_Acc]]
acceleration[,Z_Acc := z_acc_long[,Z_Acc]]

#Velocity ----
x_vel <- getCumSum(x_acc)
y_vel <- getCumSum(y_acc)
z_vel <- getCumSum(z_acc)

x_vel_long <- WidetoLong(x_vel)
y_vel_long <- WidetoLong(y_vel)
z_vel_long <- WidetoLong(z_vel)

x_vel_long <- x_vel_long[order(ID, variable),]
y_vel_long <- y_vel_long[order(ID, variable),]
z_vel_long <- z_vel_long[order(ID, variable),]

x_vel_long[,variable := as.numeric(gsub('V','',variable))]
y_vel_long[,variable := as.numeric(gsub('V','',variable))]
z_vel_long[,variable := as.numeric(gsub('V','',variable))]

setnames(x_vel_long, "value", "X_Vel")
setnames(y_vel_long, "value", "Y_Vel")
setnames(z_vel_long, "value", "Z_Vel")

velocity <- x_vel_long[]
velocity[,Y_Vel := y_vel_long[,Y_Vel]]
velocity[,Z_Vel := z_vel_long[,Z_Vel]]

#Position ----
x_pos <- getCumSum(x_vel)
y_pos <- getCumSum(y_vel)
z_pos <- getCumSum(z_vel)

x_pos_long <- WidetoLong(x_pos)
y_pos_long <- WidetoLong(y_pos)
z_pos_long <- WidetoLong(z_pos)

x_pos_long <- x_pos_long[order(ID, variable),]
y_pos_long <- y_pos_long[order(ID, variable),]
z_pos_long <- z_pos_long[order(ID, variable),]

x_pos_long[,variable := as.numeric(gsub('V','',variable))]
y_pos_long[,variable := as.numeric(gsub('V','',variable))]
z_pos_long[,variable := as.numeric(gsub('V','',variable))]

setnames(x_pos_long, "value", "X_Pos")
setnames(y_pos_long, "value", "Y_Pos")
setnames(z_pos_long, "value", "Z_Pos")

position <- x_pos_long[]
position[,Y_Pos := y_pos_long[,Y_Pos]]
position[,Z_Pos := z_pos_long[,Z_Pos]]

# Combining all the tables
temp <- merge(acceleration, velocity, by = c('ID', 'Class', 'variable'))
Gesture <- merge(temp, position, by = c('ID', 'Class', 'variable'))

# Task1 ---

# 3D Plots

# getting the first instances of each class
Top1_Gesture <- Gesture[,head(.SD, 315), Class]

scatterplot3d(x = Top1_Gesture[Class == 1, X_Pos], y = Top1_Gesture[Class == 1, Y_Pos], z = Top1_Gesture[Class == 1, Z_Pos], box = FALSE)
scatterplot3d(x = Top1_Gesture[Class == 2, Y_Pos], y = Top1_Gesture[Class == 2, X_Pos], z = Top1_Gesture[Class == 2, Z_Pos], box = FALSE)
scatterplot3d(x = Top1_Gesture[Class == 3, X_Pos], y = Top1_Gesture[Class == 3, Z_Pos], z = Top1_Gesture[Class == 3, Y_Pos], box = FALSE)
scatterplot3d(x = Top1_Gesture[Class == 4, X_Pos], y = Top1_Gesture[Class == 4, Z_Pos], z = Top1_Gesture[Class == 4, Y_Pos], box = FALSE)
scatterplot3d(x = Top1_Gesture[Class == 5, X_Pos], y = Top1_Gesture[Class == 5, Z_Pos], z = Top1_Gesture[Class == 5, Y_Pos], box = FALSE)
scatterplot3d(x = Top1_Gesture[Class == 6, X_Pos], y = Top1_Gesture[Class == 6, Z_Pos], z = Top1_Gesture[Class == 6, Y_Pos], box = FALSE)
scatterplot3d(x = Top1_Gesture[Class == 7, X_Pos], y = Top1_Gesture[Class == 7, Y_Pos], z = Top1_Gesture[Class == 7, Z_Pos], box = FALSE)
scatterplot3d(x = Top1_Gesture[Class == 8, X_Pos], y = Top1_Gesture[Class == 8, Y_Pos], z = Top1_Gesture[Class == 8, Z_Pos], box = FALSE)

fig <- plot_ly(Top1_Gesture[Class == 2], x = ~X_Pos, y = ~Y_Pos, z = ~Z_Pos, color = ~Class)
fig <- add_markers(fig)
fig

fig2 <- plot_ly(Gesture[Class == 2 & ID <= 90], x = ~X_Pos, y = ~Y_Pos, z = ~Z_Pos, color = ~ID)
fig2 <- add_markers(fig2)
fig2

# Representation 1 - Using Average X, Y, Z positions ----

Idea1 <- Gesture[,.(X_Avg = mean(X_Pos), Y_Avg = mean(Y_Pos), Z_Avg = (mean(Z_Pos))), by = .(ID, Class)]
Idea1[,"X_GrandAvg" := mean(X_Avg), by = .(Class)]
Idea1[,"Y_GrandAvg" := mean(Y_Avg), by = .(Class)]

ClassAvg <- Gesture[,.(X_Avg = mean(X_Pos), Y_Avg = mean(Y_Pos)), by = .(Class)]

ggplot(Idea1, aes(x = X_Avg, y = Y_Avg, color = as.factor(Class)))+
  geom_point()

ggplot(Idea1, aes(x = X_GrandAvg, y = Y_GrandAvg, color = as.factor(Class)))+
  geom_point()

fig2 <- plot_ly(Idea1, x = ~X_Avg, y = ~Y_Avg, z = ~Z_Avg, color = ~as.factor(Class))
fig2 <- add_markers(fig2)
fig2

# Representation 2 - Using Maximum Velocity and Acceleration ----

ggplot(Top1_Gesture, aes(x = variable, y = sqrt(Y_Vel^2+X_Vel^2+Z_Vel^2), color = as.factor(Class)))+
  geom_point()

ggplot(Top1_Gesture, aes(x = variable, y = sqrt(Y_Acc^2+X_Acc^2+Z_Acc^2), color = as.factor(Class)))+
  geom_point()

Idea6 <- Gesture[, .(MaxVelocity = max(sqrt(Y_Vel^2+X_Vel^2+Z_Vel^2)), MaxAcc = max(sqrt(Y_Acc^2+X_Acc^2+Z_Acc^2))), by = .(ID, Class)]

ggplot(Idea6[MaxAcc < 6], aes(x = MaxAcc, y = MaxVelocity, color = as.factor(Class)))+
  geom_point()

# Other Representation Ideas ----

#1
Idea3 <- Gesture[,.(Ratio_X = sum(X_Vel>0) / .N, Ratio_Y = sum(Y_Vel > 0) / .N), by = .(ID, Class)]
ggplot(Idea3, aes(x = Ratio_X, y = Ratio_Y, color = as.factor(Class)))+
  geom_point()

RatioAvg <- Gesture[,.(Ratio_X = sum(X_Vel>0) / .N, Ratio_Y = sum(Y_Vel > 0) / .N), by = .(Class)]
ggplot(RatioAvg, aes(x = Ratio_X, y = Ratio_Y, color = as.factor(Class)))+
  geom_point()


#2

Idea2 <- Gesture[variable == 315, .(Displacement = sqrt((X_Pos^2)+(Y_Pos^2)+(Z_Pos^2))), by = .(ID, Class)]
temp_Dist <- Gesture[,.(Distance = sum(abs(X_Vel)+abs(Y_Vel)+abs(Z_Vel))), by = .(ID, Class)]
Idea2 <- merge(Idea2, temp_Dist, by = c("Class", "ID"))

ggplot(Idea2, aes(x = Displacement, y = Distance, color = as.factor(Class)))+
  geom_point()

#3
  
ggplot(Top1_Gesture, aes(x = variable, y = sqrt(Y_Vel^2+X_Vel^2), color = as.factor(Class)))+
  geom_point()

Gesture[,Half := (variable <=157)]

Idea4 <- Gesture[Half == 0, .(M1 = mean(Y_Vel^2+X_Vel^2)), by = .(ID, Class)]
temp_int <- Gesture[Half == 1 , .(M2 = mean(Y_Vel^2+X_Vel^2)), by = .(ID, Class)]
Idea4 <- merge(Idea4, temp_int, by = c("ID", "Class"))

ggplot(Idea4, aes(x = M1, y = M2, color = as.factor(Class)))+
  geom_point()

IntervalMeans <- Gesture[, .(Mean = mean(Y_Vel^2+X_Vel^2)),  by = .(Class, Half)]
IntervalMeans <- dcast(IntervalMeans, Class ~ Half )

setnames(IntervalMeans, "TRUE", "M1")
setnames(IntervalMeans, "FALSE", "M2")

ggplot(IntervalMeans, aes(x = M1, y = M2, color = as.factor(Class)))+
  geom_point()

