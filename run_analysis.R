#Data: 
##https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#Data Explanation: 
##http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

##You should create one R script called run_analysis.R that does the following. 
##1. Merges the training and the test sets to create one data set.
##2. Extracts only the measurements on the mean and standard deviation for each measurement. 
##3. Uses descriptive activity names to name the activities in the data set
##4. Appropriately labels the data set with descriptive variable names. 
##5. Creates a second, independent tidy data set with the average of each variable 
##    for each activity and each subject.

#Grab the working directory files for easier viewing
wdFiles <- list.files()
testFile <- list.files(wdFiles[5]) #This is the 'test' file
trainFile <- list.files(wdFiles[6]) #This is the 'train' file

#Read the activities and features into tables
activity <- read.table("activity_labels.txt")
colnames(activity) <- c("id", "activity")

feature <- read.table("features.txt")
colnames(feature) <- c("id", "measurement")

#Grab testing/training data and merge them
##Test
test_measure <- read.table("test/X_test.txt")
test_activity <- read.table("test/y_test.txt")
test_subject <- read.table("test/subject_test.txt")
testCol <- rep("test", nrow(test_measure))

###Pretty names
colnames(test_activity) <- "activity_id"
colnames(test_subject) <- "subject_id"

####Created new table with all the test data
testSet <- cbind(test_measure, test_activity, test_subject)
testSet$set <- testCol

##Train
train_measure <- read.table("train/X_train.txt")
train_activity <- read.table("train/y_train.txt")
train_subject <- read.table("train/subject_train.txt")
trainCol <- rep("train", nrow(train_measure))

###Pretty names
colnames(train_activity) <- "activity_id"
colnames(train_subject) <- "subject_id"

####Created new table with all the train data
trainSet <- cbind(train_measure, train_activity, train_subject)
trainSet$set <- trainCol

#Merge
mergedSet <- rbind(trainSet, testSet)

#Use regular expressions to grab list of variables involving standard deviation and mean
colnum <- c(grep("mean( | std(",feature$measurement), 562:564)
relevant <- mergedSet[,colnum]
merged2 <- merge(relevant,activity,by.x="activity",by.y="id")
colnames(merged2) <- c("activity_id", grep("mean() | std()",feature$measurement, value=T), "subject", "set", "activity")

#tidy'r up!
tidy <- aggregate(merged2[,!(names(merged2) %in% c("subject_id","activity", "activity_id", "set"))],by=list(merged2$subject_id,merged2$activity),FUN=mean,na.rm=T)

#create that table!
write.table(tidy,"tidy_mean.txt",row.names=FALSE)