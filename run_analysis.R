
#Date - 02/26/2017
#Author - Doug Puccetti

#Steps Followed
#1- Merges the training and the test sets to create one data set.
#2- Extracts only the measurements on the mean and standard deviation for each measurement.
#3- Uses descriptive activity names to name the activities in the data set
#4- Appropriately labels the data set with descriptive variable names.
#5- From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Creating a working directory where the UCI HAR Dataset was unzipped
wkdir('/Users/Doug/desktop/UCI HAR Dataset/');

#Read in the data from associated files within the HAR Dataset
x_Train = read.table('/Users/Doug/desktop/UCI HAR Dataset/train/x_train.txt',header=FALSE); #Importing the x_train.txt document
x_Test = read.table('/Users/Doug/desktop/UCI HAR Dataset/test/X_test.txt',header=FALSE); #Importing the x_test.txt document
y_Train = read.table('/Users/Doug/desktop/UCI HAR Dataset/train/y_train.txt',header=FALSE); #Importing the y_train.txt document
y_Test = read.table('/Users/Doug/desktop/UCI HAR Dataset/test/y_test.txt',header=FALSE); #Importing the y_test.txt document
act_Type = read.table('/Users/Doug/desktop/UCI HAR Dataset/activity_labels.txt',header=FALSE); #Importing the activity_lables.txt document
sub_Train = read.table('/Users/Doug/desktop/UCI HAR Dataset/train/subject_train.txt',header=FALSE) #Importing the subject_train.txt document
sub_Test = read.table('/Users/Doug/desktop/UCI HAR Dataset/test/subject_test.txt',header=FALSE); #Importing the subject_test.txt document
feat_tbl = read.table('/Users/Doug/desktop/UCI HAR Dataset/features.txt',header=FALSE); #Importing the features.txt document

#Assigin column names to the data imported above
colnames(act_Type) = c('activityId','activityType');
colnames(sub_Train) = "subjectId";
colnames(x_Train) = feat_tbl[,2]; 
colnames(y_Train) = "activityId";

training_Dataset = cbind(y_Train,sub_Train,x_Train); #Combining the sub_Train, y_Train, and x_Train files

#Assign column names to testing data
colnames(sub_Test) = "subjectId";
colnames(x_Test) = feat_tbl[,2]; 
colnames(y_Test) = "activityId";

test_Dataset = cbind(y_Test,sub_Test,x_Test); #Combine y_Test, x_Test and sub_Test data

combined_Dataset = rbind(training_Dataset,test_Dataset); #Creating a final combined data set

colNames  = colnames(combined_Dataset); #Using combined_Dataset to create a vector which will be used to select/segment the mean() & stddev() columns

#Create logical_Vector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logical_Vector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

#Subset the combined_Dataset table based on the logical_Vector to keep only desired columns
combined_Dataset = fcombined_Dataset[logical_Vector==TRUE];

#Merge the combined_Dataset set with the act_Type table to include activity names
combined_Dataset = merge(combined_Dataset,act_Type,by='activityId',all.x=TRUE);

#Updating the colNames vector to include the new column names after merge
colNames  = colnames(combined_Dataset); 

#Create a new table, no_Activity_Type without the act_Type column
no_Activity_Type = combined_Dataset[,names(combined_Dataset) != 'activityType'];

#Summarizing the no_Activity_Type table to include just the mean of each variable for each activity and each subject
tidy_Data = aggregate(no_Activity_Type[,names(no_Activity_Type) != c('activityId','subjectId')],by=list(activityId=no_Activity_Type$activityId,subjectId = no_Activity_Type$subjectId),mean);

#Merging the tidyData with act_Type to include descriptive acitvity names
tidy_Data= merge(tidy_Data,act_Type,by='activityId',all.x=TRUE);

#Export the tidyData set as tab seperated value.
write.table(tidy_Data, '/Users/Doug/desktop/UCI HAR Dataset/tidyData.txt',row.names=TRUE,sep='\t');

