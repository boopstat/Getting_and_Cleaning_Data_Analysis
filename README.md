#Course Project
(find all project-related materials in the UCI HAR Dataset directory, however, copies of the important files have been put into this main directory to fulfill the submission requirement)

  Unzip the source (https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip) into a folder on your local drive, say C:\Users\yourname\Documents\R\
  
  Put run_analysis.R into C:\Users\yourname\Documents\R\UCI HAR Dataset\
  
  In RStudio: setwd("C:\\Users\\yourname\\Documents\\R\\UCI HAR Dataset\\"), followed by: source("run_analysis.R")
  
  Use data <- read.table("data_set_with_the_averages.txt") to read the data. It is 180x68 because there are 30 subjects and 6 activities, thus "for each activity and each subject" means 30 * 6 = 180 rows. Note that the provided R script has no assumptions on numbers of records, only on locations of files.
 
 # The following R script create tidy data set (run_analysis.R)

# 1. Merges the training and the test sets to create one data set.

exp1 <- read.table("train/X_train.txt")
exp2 <- read.table("test/X_test.txt")
X <- rbind(exp1, exp2)

exp1 <- read.table("train/subject_train.txt")
exp2 <- read.table("test/subject_test.txt")
S <- rbind(exp1, exp2)

exp1 <- read.table("train/y_train.txt")
exp2 <- read.table("test/y_test.txt")
Y <- rbind(exp1, exp2)

# 2  Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table("features.txt")
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, indices_of_good_features]
names(X) <- features[indices_of_good_features, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

# 3. Uses descriptive activity names to name the activities in the data set.

activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"

# 4. Appropriately labels the data set with descriptive activity names.

names(S) <- "subject"
cleaned <- cbind(S, Y, X)
write.table(cleaned, "merged_clean_data.txt")

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(S)[,1]
numSubjects = length(unique(S)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
    for (a in 1:numActivities) {
        result[row, 1] = uniqueSubjects[s]
        result[row, 2] = activities[a, 2]
        tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
        result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
        row = row+1
    }
}
write.table(result, "tidy_data_set.txt")
