library(caret)
library(randomForest)
setwd("~/Coursera/Practical Machine Learning/Project")
training <- read.csv("pml-training.csv", header=TRUE)
testing <- read.csv("pml-testing.csv", header=TRUE)

# Split the training set into another training and testing set
inTrain <- createDataPartition(y=training$classe, p=0.7, list=FALSE)
subTraining <- training[inTrain,]
subTesting <- training[-inTrain,]

qplot(roll_belt, pitch_belt, colour=classe, data=subTraining)
qplot(accel_belt_x, accel_belt_y, colour=classe, data=subTraining)
qplot(magnet_belt_x, magnet_belt_y, colour=classe, data=subTraining)
qplot(magnet_arm_x, magnet_arm_y, colour=classe, data=subTraining)

modFit <- randomForest(classe ~ roll_belt + pitch_belt + yaw_belt + total_accel_belt +
                  accel_belt_x + accel_belt_y + accel_belt_z +
                  magnet_belt_x + magnet_belt_y + magnet_belt_z +
                  roll_arm + pitch_arm + yaw_arm + total_accel_arm +
                  accel_arm_x + accel_arm_y + accel_arm_z +
                  magnet_arm_x + magnet_arm_y + magnet_arm_z +
                  roll_dumbbell + pitch_dumbbell + yaw_dumbbell +
                  gyros_arm_x + gyros_arm_y + gyros_arm_z +
                  gyros_belt_x + gyros_belt_y + gyros_belt_z, data=subTraining)

confusionMatrix(subTesting$classe, predict(modFit, subTesting))
