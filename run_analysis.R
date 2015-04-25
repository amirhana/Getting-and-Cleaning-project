library(data.table)
library(dplyr)
setwd('D:/cleaningdata')
FeatureNames<-read.table('UCI HAR Dataset/features.txt')
activityLabels<-read.table('UCI HAR Dataset/activity_labels.txt',header=F)
subjectTrain<-read.table('UCI HAR Dataset/train/subject_train.txt',header=F)
activityTrain<-read.table('UCI HAR Dataset/train/y_train.txt',header=F)
featuresTrain<-read.table('UCI HAR Dataset/train/x_train.txt',header=F)
subjectTest<-read.table('UCI HAR Dataset/test/subject_test.txt',header=F)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = F)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = F)
#============================part1
subject<-rbind(subjectTrain,subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)
colnames(features)<-t(FeatureNames[2])
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)
#==================================================part2
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)
requiredColumns <- c(columnsWithMeanSTD, 562, 563)

extractedData <- completeData[,requiredColumns]
dim(extractedData)
#===============================================part3
extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}
extractedData$Activity <- as.factor(extractedData$Activity)
#======================part4
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))
#==========================part5
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)
