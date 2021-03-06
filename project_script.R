library(caret)
library(randomForest)
setwd("~/Coursera/Practical Machine Learning/Project")
training <- read.csv("pml-training.csv", header=TRUE)
testing <- read.csv("pml-testing.csv", header=TRUE)

# Split the training set into another training and testing set
inTrain <- createDataPartition(y=training$classe, p=0.7, list=FALSE)
subTraining <- training[inTrain,] # we will be training the model on this dataset
subTesting <- training[-inTrain,] # we will test the model on this dataset

# View plots of some of the variables
qplot(roll_belt, pitch_belt, colour=classe, data=subTraining)
qplot(accel_belt_x, accel_belt_y, colour=classe, data=subTraining)
qplot(magnet_belt_x, magnet_belt_y, colour=classe, data=subTraining)
qplot(magnet_arm_x, magnet_arm_y, colour=classe, data=subTraining)

# Create the Random Forest Model
modFit <- randomForest(classe ~ roll_belt + pitch_belt + yaw_belt + total_accel_belt +
                  accel_belt_x + accel_belt_y + accel_belt_z +
                  magnet_belt_x + magnet_belt_y + magnet_belt_z +
                  roll_arm + pitch_arm + yaw_arm + total_accel_arm +
                  accel_arm_x + accel_arm_y + accel_arm_z +
                  magnet_arm_x + magnet_arm_y + magnet_arm_z +
                  roll_dumbbell + pitch_dumbbell + yaw_dumbbell +
                  gyros_arm_x + gyros_arm_y + gyros_arm_z +
                  gyros_belt_x + gyros_belt_y + gyros_belt_z, data=subTraining)

# View the confusion matrix of how the model did predicting against the subTraining dataset
confusionMatrix(subTraining$classe, predict(modFit, subTraining))

# View the confusion matrix of how the model did predicting against the subTesting dataset
confusionMatrix(subTesting$classe, predict(modFit, subTesting))


# The following code is for predicting the 20 test cases in the testing dataset
setwd("~/Coursera/Practical Machine Learning/Project/test_cases")

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

predictions <- predict(modFit, testing)
pml_write_files(predictions)

