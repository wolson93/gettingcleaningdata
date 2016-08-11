
#Download and unzip file
if(!file.exists("./uci_data")){dir.create("./uci_data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./uci_data/Dataset.zip",method="curl")
unzip(zipfile="./uci_data/Dataset.zip",exdir="./uci_data")
setwd("./uci_data/UCI HAR Dataset")

#Read training data
x_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/y_train.txt")
subject_train <- read.table("./train/subject_train.txt")

#Read test data
x_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/y_test.txt")
subject_test <- read.table("./test/subject_test.txt")

#Read features and activity labels
features <- read.table("./features.txt")
activityLabels <- read.table("./activity_labels.txt")

#Label columns with appropriate names
colnames(x_test) = features[,2]
colnames(x_train) = features[,2]
colnames(activityLabels) = c("activityId","activityName")
colnames(y_test) = "activityId"
colnames(y_train) = "activityId"
colnames(subject_test) = "subjectId"
colnames(subject_train) = "subjectId"

#Merge data sets
train_data <- cbind(subject_train,y_train,x_train)
test_data <- cbind(subject_test,y_test,x_test)
combined_data <- rbind(train_data,test_data)

#Extract mean and standard deviation measurements only
columnNames <- colnames(combined_data)
mean_stddev <- (grepl("activityId",columnNames) | grepl("subjectId",columnNames) | grepl("mean..",columnNames) | grepl("std..",columnNames))
mean_stddev_data <- combined_data[ ,mean_stddev==TRUE]

#Aggregate data such that we have the average of each variable for each subject and each activity
tidy_data <- aggregate(. ~ subjectId + activityId,mean_stddev_data,mean)

#Add descriptive activity labels
tidy_data_activityLabels <- merge(tidy_data,activityLabels,by="activityId",all=TRUE)

#Tidy variable names
columnNames = colnames(tidy_data_activityLabels)
for (i in 1:length(columnNames))
{  
  columnNames[i] = gsub("\\()","",columnNames[i])
  columnNames[i] = gsub("-std","StdDev",columnNames[i])
  columnNames[i] = gsub("-mean","Mean",columnNames[i])
  columnNames[i] = gsub("^(t)","time",columnNames[i])
  columnNames[i] = gsub("^(f)","freq",columnNames[i])
  columnNames[i] = gsub("Mag","Magnitude",columnNames[i])
}

colnames(tidy_data_activityLabels) = columnNames

#Reorder data variables
tidy_data_final <- tidy_data_activityLabels[order(tidy_data_activityLabels$subjectId, tidy_data_activityLabels$activityId),]

#Write table
write.table(tidy_data_final, "tidy_data_final.txt", row.name=FALSE)
