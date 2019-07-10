## Merges the training and the test sets to create one data set.

setwd("~/UCI HAR Dataset/train")

SubjectTrain <- read.table("subject_train.txt")
XTrain <- read.table("X_train.txt")
YTrain <- read.table("y_train.txt")

FullDataTrain <- cbind (XTrain, YTrain, SubjectTrain)


setwd("~/UCI HAR Dataset/test")

SubjectTest <- read.table("subject_test.txt")
XTest <- read.table("X_test.txt")
YTest <- read.table("y_test.txt")

FullDataTest <- cbind (XTest, YTest, SubjectTest)


FullDataSet <- rbind (FullDataTrain, FullDataTest)

## Extracts only the measurements on the mean and standard deviation for each measurement.

setwd("~/UCI HAR Dataset")
Features <- read.table ("features.txt", stringsAsFactors = FALSE)

FeaturesMeanStd <- rbind (data.frame (colnumber=grep("mean", Features$V2), colname=grep("mean", Features$V2, value=TRUE)), 
                          data.frame (colnumber=grep("std", Features$V2), colname=grep("std", Features$V2, value=TRUE)))
library (dplyr)
FeaturesMeanStd <- arrange (FeaturesMeanStd, colnumber)

## Uses descriptive activity names to name the activities in the data set

Colnumbers <- c (FeaturesMeanStd$colnumber, 562, 563)
Colnames <- c(as.character (FeaturesMeanStd$colname), "Training_labels", "Subject_id")

library(data.table)
FullDataSet <- as.data.frame(FullDataSet)
FullDataSet <- FullDataSet [,Colnumbers]
setnames (FullDataSet, Colnames)

## Appropriately labels the data set with descriptive variable names.
TrainLabels <- function (x) {
  if (x == 1) {
   return ("WALKING")
  }
  else if ( x == 2) {
    return ("WALKING_UPSTAIRS")
  }
  else if (x == 3) {
    return ("WALKING_DOWNSTAIRS")
  }
  else if (x == 4) {
    return ("SITTING")
  }
  else if (x == 5) {
   return ("STANDING")
  }
  else if (x == 6) {
    return ("LAYING")
  }
}

FullDataSet <- mutate (FullDataSet, Training_labels = sapply(FullDataSet$Training_labels, TrainLabels))

## From the data set in step 4, creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject.
TidyDataSet <- FullDataSet %>%
  group_by(Subject_id, Training_labels) %>%
  summarise_all(funs(mean))
write.table(TidyDataSet, "TidyDataSet.txt", row.name=FALSE)

## View(TidyDataSet)
