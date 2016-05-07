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
stuff1 <- rbind(train_act, test_act)
stuff2 <- rbind(traindata,testdata)
stuff3 <- rbind(train_subject,test_subject)


ActivityNames <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
# There must be a more elegant way of assigning meaniful actifity rownames than one row at a time, but at
# least it works. 
stuff1 <- (gsub("1","WALKING", stuff1[,1] ))
namesAsStrings     <- data.frame(stuff1)
namesAsStrings[,1] <- gsub("2","WALKING_UPSTAIRS", namesAsStrings[,1] )
namesAsStrings[,1] <- gsub("3","WALKING_DOWNSTAIRS", namesAsStrings[,1] )
namesAsStrings[,1] <- gsub("4","SITTING", namesAsStrings[,1] )
namesAsStrings[,1] <- gsub("5","STANDING", namesAsStrings[,1] )
namesAsStrings[,1] <- gsub("6","LAYING", namesAsStrings[,1] )
colnames(namesAsStrings)<- 'Activity'
colnames(stuff3) <- 'Subject'
#Select only measurements of means and sigmas, and give measurements meaningful names.
mean_std <- select(tbl_df(stuff2),1:6,41:46,81:86,121:126,161:166,201:202,214,215,227,228,240,241,253,254,266:271,345:350,424:429,503,504,516,517,529,530,542,543)
colnames(mean_std) <- c("tBodyAcc-mean()-X", "tBodyAcc-mean()-Y", "tBodyAcc-mean()-Z", "tBodyAcc-std()-X", "tBodyAcc-std()-Y", "tBodyAcc-std()-Z", "tGravityAcc-mean()-X", "tGravityAcc-mean()-Y", "tGravityAcc-mean()-Z", "tGravityAcc-std()-X", "tGravityAcc-std()-Y","tGravityAcc-std()-Z", "tBodyAccJerk-mean()-X", "tBodyAccJerk-mean()-Y", "tBodyAccJerk-mean()-Z", "tBodyAccJerk-std()-X", "tBodyAccJerk-std()-Y", "tBodyAccJerk-std()-Z", "tBodyGyro-mean()-X", "tBodyGyro-mean()-Y", "tBodyGyro-mean()-Z", "tBodyGyro-std()-X", "tBodyGyro-std()-Y", "tBodyGyro-std()-Z", "tBodyGyroJerk-mean()-X", "tBodyGyroJerk-mean()-Y", "tBodyGyroJerk-mean()-Z", "tBodyGyroJerk-std()-X", "tBodyGyroJerk-std()-Y", "tBodyGyroJerk-std()-Z", "tBodyAccMag-mean()", "tBodyAccMag-std()", "tGravityAccMag-mean()", "tGravityAccMag-std()", "tBodyAccJerkMag-mean()", "tBodyAccJerkMag-std()", "tBodyGyroMag-mean()", "tBodyGyroMag-std()",  "tBodyGyroJerkMag-mean()", "tBodyGyroJerkMag-std()","fBodyAcc-mean()-X", "fBodyAcc-mean()-Y", "fBodyAcc-mean()-Z", "fBodyAcc-std()-X", "fBodyAcc-std()-Y", "fBodyAcc-std()-Z", "fBodyAccJerk-mean()-X", "fBodyAccJerk-mean()-Y", "fBodyAccJerk-mean()-Z","fBodyAccJerk-std()-X", "fBodyAccJerk-std()-Y", "fBodyAccJerk-std()-Z", "fBodyGyro-mean()-X", "fBodyGyro-mean()-Y", "fBodyGyro-mean()-Z", "fBodyGyro-std()-X", "fBodyGyro-std()-Y", "fBodyGyro-std()-Z", "fBodyAccMag-mean()", "fBodyAccMag-std()", "fBodyBodyAccJerkMag-mean()","fBodyBodyAccJerkMag-std()","fBodyBodyGyroMag-mean()", "fBodyBodyGyroMag-std()","fBodyBodyGyroJerkMag-mean()", "fBodyBodyGyroJerkMag-std()"
)
# use dyplyr for easy manipulation
 AC_outputs <- tbl_df(stuff2)
 Activities<-tbl_df(namesAsStrings)
 Subjects<-tbl_df(stuff3)

#Combine the selected measurements with their activity and subject identifiers
ids <- cbind(Activities,Subjects)
alldata <- tbl_df(cbind(ids, mean_std))
# This completes step 4 of building the project assignment.
# Step 5 creates a second, independent tidy data set with the average of each variable for 
#each activity and each subject.
# 1) Select only those columns where the variable is the mean, not the standard deviation, of
# of a measurmed variable.
# 2) Group rows by activity
# 3) Take the mean of each group and store the results in rows of the new data set (data_frame).
# 4) Repeat 1 - 3 with subject instead of variable.
#  Here's where chanining might come in handy.

# Mean per activity, over all subjects(don't want summarize to give meaningless mean of Subject)
# "summarize_each()" is a fantastic addition to our arsenal of methods!
mean_by_Activity <- alldata %>%
        group_by(Activity) %>%
        summarize_each(funs(mean),-Subject)
# Mean per subject, over all activities(don't want summarize to give meaningless mean of Activity)
mean_by_Subject <- alldata %>%
        group_by(Subject) %>%
        summarize_each(funs(mean),-Activity)
#Finally write the new tidy data sets to permanent (disk) storage.
write.table(mean_by_Subject, "mean_by_Subject.txt",row.names = FALSE)
write.table(mean_by_Activity, "mean_by_Activity.txt",row.names = FALSE)


