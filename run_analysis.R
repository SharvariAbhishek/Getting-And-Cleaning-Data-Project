library(data.table)
library(dplyr)
library(qdapTools)


#Downloading the data from url mentioned
#-----------------------------------------
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile="./data/Dataset.zip", method = "curl")
unzip("./data/Dataset.zip", exdir="./data")



#Path to dataset downloaded 
#-------------------------------------
path_url <- "./data/UCI HAR Dataset"
path_train <- file.path(path_url, "train")
path_test <- file.path(path_url, "test")



#Step 1: Merging  the training and the test sets to create one data set.
#=================================================================================================

features <- read.table(file.path(path_url, "features.txt"), header = FALSE)
colnames(features) <- c( "Index", "feature_Name")

activity <- read.table(file.path(path_url, "activity_labels.txt"), header = FALSE)
colnames(activity) <- c("ActivityId", "Activity")

# Reading training and test data
#------------------------------------------------------------------------------------
subject_train <- read.table(file.path(path_train, "subject_train.txt"), header = FALSE)
subject_test <- read.table(file.path(path_test, "subject_test.txt"), header = FALSE)
subject_data <- rbind(subject_train, subject_test)
colnames(subject_data) <- "Subject" # Renaming Subject dataset column name


# Reading X test and train data
#-----------------------------------------------------------------------------
X_train <- read.table(file.path(path_train, "X_train.txt"), header = FALSE)
X_test <- read.table(file.path(path_test, "X_test.txt"), header = FALSE)
X_data <- rbind(X_train, X_test)

colnames(X_data) <- features$feature_Name


#Reading Y test and training data
#------------------------------------------------------------------------
Y_Train <- read.table(file.path(path_train, "y_train.txt"), header = FALSE)
Y_Test <- read.table(file.path(path_test, "y_test.txt"), header = FALSE)
Y_data <- rbind(Y_Train, Y_Test)
colnames(Y_data) <- "Activity"



final_data <- cbind(subject_data, Y_data)
final_data <- cbind(X_data, final_data)



#========================================================================================================
#Step 2:Extracts only the measurements on the mean and standard deviation for each measurement

#Using subset() function in data.table package to subset only the mean and std for each measurement
#(?=.*(mean|std)) in grep is postivie look ahead which asserts that the match must contain string mean and std
#(?!.*meanFreq) is negative look ahead which asserts that match should not contain string meanFreq

final_data <- subset(final_data, select = c(grep("^(?=.*(mean|std))(?!.*meanFreq)", names(final_data), value = TRUE, perl = TRUE), "Subject", "Activity"))



#========================================================================================================
#Step 3: Uses descriptive activity names to name the activities in the data set

#Using lookup function in qdap package for the below:

final_data$Activity <- lookup(final_data$Activity, activity)

#Also a function call factor can be used:
#final_data$Activity <- factor(final_data$Activity, levels = activity$Value, labels = activity$Activity)



#========================================================================================================
#Step 4: Appropriately labels the data set with descriptive variable names

colnames(final_data) <- sub("^t", "time", colnames(final_data))
colnames(final_data) <- sub("^f", "frequency", colnames(final_data))
colnames(final_data) <- sub("*Acc", "Accelerometer", colnames(final_data))
colnames(final_data) <- sub("*BodyBody*", "Body", colnames(final_data))
colnames(final_data) <- sub("*Gyro*", "Gyroscope", colnames(final_data))
colnames(final_data) <- sub("*Mag", "Magnitude", colnames(final_data))



#=========================================================================================================
#Step 5 : Create a second, independent tidy data set with the average of each variable for each activity and each subject


average_data<-aggregate(. ~Subject + Activity, final_data, mean)
average_data <- average_data[order(average_data$Subject, average_data$Activity),]

write.table(average_data, file = "TidyData.txt", row.names = FALSE)




