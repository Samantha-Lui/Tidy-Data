# 
# Downloading and extracting the files provided for the analysis.
#
# Sets the destination for the download to be the local directory
localDir <- getwd()
# Sets the download file name to be "courseProject.zip".
fname = paste(localDir,"/courseProject.zip", sep="")
furl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(furl, destfile=fname)
# Exacts locally the file, "UCI HAR Dataset", which contains all of the data files.
unzip(fname)




#
# Reading in the data from the relevant files and store them as tables.
#
# Links the class labels with their activity name.
activity <- read.table("UCI HAR Dataset/activity_labels.txt", stringsAsFactors=FALSE) 
features <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors=FALSE) # List of all features.

# Each row identifies the subject who performed 
# the activity for the test window sample.
subTest <- read.table("UCI HAR Dataset/test/subject_test.txt", stringsAsFactors=FALSE) 
yTest <- read.table("UCI HAR Dataset/test/y_test.txt", stringsAsFactors=FALSE) # Test labels.
xTest <- read.table("UCI HAR Dataset/test/X_test.txt", stringsAsFactors=FALSE) # Test set.

# Each row identifies the subject who performed 
# the activity for the train window sample.
subTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", stringsAsFactors=FALSE)
yTrain <- read.table("UCI HAR Dataset/train/y_train.txt", stringsAsFactors=FALSE)  # Train labels.
xTrain <- read.table("UCI HAR Dataset/train/X_train.txt", stringsAsFactors=FALSE) # Train set.





#
# Merging the training and the test sets and extracting the measurements on the mean and 
# standard deviation for each measurement from the merged data set.
#
# Merges the training and the test sets to create one data set.
combinedDataSet <- rbind(xTrain,xTest)
# Extracts only the measurements on the mean and standard deviation for each measurement. 
targetColNums <- union(grep("mean()", features$V2), grep("std()", features$V2))
targetColNums <- sort(targetColNums)
targetDataSet <- combinedDataSet[,targetColNums]





#
# Creating a data set with descriptive activity names, subject id's, and the measurements
# on the `mean` and standard deviation `for` each measurement. 
#
# Converts the activity labels into descriptive activity names for the combined data set.
combinedLabels <- rbind(yTrain,yTest)
act <- sapply(combinedLabels$V1, function(x) activity$V2[x])

combinedSubject <- rbind(subTrain,subTest)

# Creates a list of descriptive variable names for the data set.
featureNames <- features$V2[targetColNums]
featureNames <- sapply(featureNames, function(x) as.character(x))
# There is an inconsistency in the features_info and features.
# Apparently the terms with `BodyBody` are artifacts.
# The following line `fixes` the problem.
featureNames <- gsub("BodyBody","Body", featureNames, fixed=TRUE)

# Creates a data set with descriptive activity names, subject id's,
# and only the measurements on the mean and standard deviation for each measurement.
preprocessedDataSet <- data.frame(act, combinedSubject, targetDataSet)
names(preprocessedDataSet) <- c("activity","subject", featureNames)





#
# Creating a tidy data set with the average of each variable for each activity and each subject.
#
# Calculates the average of each variable for each activity and each subject.
result <- aggregate(preprocessedDataSet, 
                    by=list(preprocessedDataSet$subject,preprocessedDataSet$activity), 
                    FUN=mean, na.rm=TRUE)

# Adds descriptive variable names to the tidy data set.
featureNamesTidy <- lapply(featureNames, function(x) paste("avg(",x,")",sep="",collapse = NULL))

tidyData <- data.frame(result$Group.2,result$Group.1,result[,5:83]) # The tidy data set.
names(tidyData) <- c("activity","subject",featureNamesTidy)





#
# Outputs the tidy data set in a text file
#
write.table(format(tidyData,"digits"=8), "tidyData.txt", row.names = FALSE)
