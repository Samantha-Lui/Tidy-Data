#Tidy Data
###Summary
> The script "run_analysis.R" prepares a tidy data set that can be used for later analysis from a set of data collected from the accelerometers from the Samsung Galaxy S smartphone for wearable computing. A full description is available at the site where the data was obtained:

> [http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones).

> The script was produced using R x64 3.1.1 on Windows 7. The resulted tidy data set "tidyData.txt" and the Code Book are available in the main repo. 


__Note__: The terms containing `BodyBody` in "features.txt" are inconsistent to the information provided in "features_info.txt" and are apparently some artifacts. We adhered to the terms defined in "features_info.txt" as we referred to the feature names.



####The script is divided into the following sections:

#####1. Downloading and extracting the files.

>- _localDir_ : The local directory where the script was stored.
>- _fname_ : The full name of the file to be download.
>- _furl_ : The url of the site from where the data was downloaded.


#####2. Loading files "activity_labels.txt", "features.txt", "./test/subject_test.txt", "./test/y_test.txt", "./test/X_test.txt", "./train/subject_train.txt", "./train/y_train.txt" and "./train/X_train.txt" from the base directory "UCI HAR Dataset" and storing them as tables.

>- _activity_ : Links the class labels with their activity name.
>- _features_ : List of all features measured.
>- _subTest_ : Each row identifies the subject who performed the activity for the test window sample.
>- _yTest_ : Test labels.
>- _xTest_ : Test set.
>- _subTrain_ : Each row identifies the subject who performed the activity for the train window sample.
>- _yTrain_ : Train labels.
>- _xTrain_ : Train set.


#####3.  Merging the training and the test sets and extracting the measurements on the mean and standard deviation for each measurement from the merged data set.

>- _combinedDataSet_ : Merged data set from the training and the test sets.
>- _targetColNums_ : Ordered list of the column numbers in _combinedDataSet_ where the columns correspond to the measurements on the mean and standard deviation.
>- _targetDataSet_ : Merged data set from the training and the test sets containing only the measurements on the mean and standard deviation for each measurement.


#####4. Creating a data set with descriptive activity names, subject id's, and the measurements on the mean and standard deviation for each measurement. 

>- _combinedLabels_ : Merged data set from the training labels and the test labels.
>- _act_ : Each row identifies the activity performed with the corresponding record of measurements in _targetDataSet_.
>- _combinedSubject_ : Each row identifies the subject who performed the activity in _act_.
>- _featureNames_ : List of descriptive variable names for the data set.
>- _preprocessedDataSet_ : The data set ready to be processed for the designated tidy data set.


#####5. Creating a tidy data set with the average of each variable for each activity and each subject. 
>- _result_ : Each row contains the name of an activity, the id of the subject performing the activity, and the averages of each of the means and standard deviations extracted above with respect to the subject.
>- _featureNamesTidy_ : List of descriptive variable names.
>- _tidyData_ : The tidy data set.


#####6. Outputing the tidy data set.
>- The tidy data set is output as a table in "tidyData.txt" in where each quantitative entity was rounded to eight significant digits and expressed in scientific notation.
