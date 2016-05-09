# Getting-and-Cleaning-Data Class Project
##Script Code
#Get the data from the website. 
fileURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
download.file(fileURL,"Alldata.zip", mode = "wb")
unzip("Alldata.zip")

#Read 6 files [x_*.txt, y_*.txt, subject_*.txt] into their own data frames. '*' = one of
# train or test.
# Output vector of 561 measurements per subject per activity
traindata <- read.table("UCI HAR Dataset/train/x_train.txt")
testdata <- read.table("UCI HAR Dataset/test/x_test.txt")

# Activity for each row of measurements
train_act <- read.table("UCI HAR Dataset/train/y_train.txt",colClasses = "character")
test_act <- read.table("UCI HAR Dataset/test/y_test.txt")

# Subject for each row of measurements.
train_subject <- read.table("UCI HAR Dataset/train/subject_train.txt")
test_subject <- read.table("UCI HAR Dataset/test/subject_test.txt")

# Use rbind to coalesce training and test data frames vertically. "merge" as an R funtion
# has a much narrower meaning than as a verb in the English language.
All_activities <- rbind(train_act, test_act)
All_vectors <- rbind(traindata,testdata)
All_subjects <- rbind(train_subject,test_subject)


ActivityNames <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
# There must be a more elegant way of assigning meaniful actifity rownames than this, but at
# least it works.
All_activities <- (gsub("1","WALKING", All_activities[,1] ))
namesAsStrings     <- data.frame(All_activities)
namesAsStrings[,1] <- gsub("2","WALKING_UPSTAIRS", namesAsStrings[,1] )
namesAsStrings[,1] <- gsub("3","WALKING_DOWNSTAIRS", namesAsStrings[,1] )
namesAsStrings[,1] <- gsub("4","SITTING", namesAsStrings[,1] )
namesAsStrings[,1] <- gsub("5","STANDING", namesAsStrings[,1] )
namesAsStrings[,1] <- gsub("6","LAYING", namesAsStrings[,1] )
colnames(namesAsStrings)<- 'Activity'
colnames(All_subjects) <- 'Subject'
#Select only measurements of means and sigmas, and give measurements meaningful names.
mean_std <- select(tbl_df(All_vectors),1:6,41:46,81:86,121:126,161:166,201:202,214,215,227,228,240,241,253,254,266:271,345:350,424:429,503,504,516,517,529,530,542,543)
colnames(mean_std) <- c("tBodyAcc-mean()-X", "tBodyAcc-mean()-Y", "tBodyAcc-mean()-Z", "tBodyAcc-std()-X", "tBodyAcc-std()-Y", "tBodyAcc-std()-Z", "tGravityAcc-mean()-X", "tGravityAcc-mean()-Y", "tGravityAcc-mean()-Z", "tGravityAcc-std()-X", "tGravityAcc-std()-Y","tGravityAcc-std()-Z", "tBodyAccJerk-mean()-X", "tBodyAccJerk-mean()-Y", "tBodyAccJerk-mean()-Z", "tBodyAccJerk-std()-X", "tBodyAccJerk-std()-Y", "tBodyAccJerk-std()-Z", "tBodyGyro-mean()-X", "tBodyGyro-mean()-Y", "tBodyGyro-mean()-Z", "tBodyGyro-std()-X", "tBodyGyro-std()-Y", "tBodyGyro-std()-Z", "tBodyGyroJerk-mean()-X", "tBodyGyroJerk-mean()-Y", "tBodyGyroJerk-mean()-Z", "tBodyGyroJerk-std()-X", "tBodyGyroJerk-std()-Y", "tBodyGyroJerk-std()-Z", "tBodyAccMag-mean()", "tBodyAccMag-std()", "tGravityAccMag-mean()", "tGravityAccMag-std()", "tBodyAccJerkMag-mean()", "tBodyAccJerkMag-std()", "tBodyGyroMag-mean()", "tBodyGyroMag-std()",  "tBodyGyroJerkMag-mean()", "tBodyGyroJerkMag-std()","fBodyAcc-mean()-X", "fBodyAcc-mean()-Y", "fBodyAcc-mean()-Z", "fBodyAcc-std()-X", "fBodyAcc-std()-Y", "fBodyAcc-std()-Z", "fBodyAccJerk-mean()-X", "fBodyAccJerk-mean()-Y", "fBodyAccJerk-mean()-Z","fBodyAccJerk-std()-X", "fBodyAccJerk-std()-Y", "fBodyAccJerk-std()-Z", "fBodyGyro-mean()-X", "fBodyGyro-mean()-Y", "fBodyGyro-mean()-Z", "fBodyGyro-std()-X", "fBodyGyro-std()-Y", "fBodyGyro-std()-Z", "fBodyAccMag-mean()", "fBodyAccMag-std()", "fBodyBodyAccJerkMag-mean()","fBodyBodyAccJerkMag-std()","fBodyBodyGyroMag-mean()", "fBodyBodyGyroMag-std()","fBodyBodyGyroJerkMag-mean()", "fBodyBodyGyroJerkMag-std()"
)
# use dyplyr for easy manipulation
 AC_outputs <- tbl_df(mean_std)
 Activities<-tbl_df(namesAsStrings)
 Subjects<-tbl_df(All_subjects)

#Combine the selected measurements with their activity and subject identifiers
by_Activity <- cbind(Activities,AC_outputs)
by_Subject <- cbind(Subjects,AC_outputs)
# This completes step 4 of building the project assignment.
# Step 5 creates a second, independent tidy data set with the average of each variable for 
#each activity and each subject.
# 1) Select only those columns where the variable is the mean, not the standard deviation, of
# of a measurmed variable.
# 2) Group rows by activity
# 3) Take the mean of each group and store the results in rows of the new data set (data_frame).
# 4) Repeat 1 - 3 with subject instead of variable.
#  Here's where chanining might come in handy.

# Mean per activity, over all subjects
# "summarize_each()" is a fantastic addition to our arsenal of methods!
mean_by_Activity <- by_Activity %>%
        group_by(Activity) %>%
        summarize_each(funs(mean))
# Mean per subject, over all activities
mean_by_Subject <- by_Subject %>%
        group_by(Subject) %>%
        summarize_each(funs(mean))
#Finally write the new tidy data set to permanent (disk) storage.
write.table(mean_by_Subject, "mean_by_Subject.txt",row.names = FALSE)
write.table(mean_by_Activity, "mean_by_Activity.txt",row.names = FALSE)
## Comments
This script is so simple that it doesn't really required description beyond that provided by the comments in the code.

A few words are necessary to explain why some variables with "mean" in their names were not included in the analysis.  The variables which *were* included are the means and standard deviations of measured quantities.  The angle variables are single values computed from these means of measurements.

