
###
### To work this script must be executed in the directory "UCI HAR Dataset"
### created by the zip file of the downloaded data set
###


###
###   Load libraries needed
###
 
library(dplyr)
library(tidyr)
library(sqldf)

###
###	Read the data set
### 	Merges the training and the test sets to create one data set.
###

features_df  <- read.table("features.txt", header=FALSE)
train_df  <- read.table("train/X_train.txt", header=FALSE)
test_df <- read.table("test/X_test.txt", header=FALSE)

###
### 	Merges the training and the test sets to create one data set.
###

DF <- rbind(train_df, test_df)

###
### The data sets are merged, transform to tbl_df to apply dplyr and tidyr
### DF and features_df  
###

DF <- tbl_df(DF)
features_df  <- tbl_df(features_df)


###
### Create data frame with only the columns needed
### Extracts only the measurements on the mean and standard deviation for each measurement.
### We use the filter function to grep variables with mean() and std() exluding 'Freq'
### in features_df which contains variables
### the function filter is used
###
  
col_to_select <- filter(features_df, grepl('mean()|std()', V2), !grepl('Freq', V2))

### 
###  select only the mesurement requested, the columns select are in col_to_select$V1 vector
###

 
DF2 <- select(DF, col_to_select$V1)

###
###  Read the subject  "subject_train.txt" and activity "y_train.txt" data sets for train
###

subject_train  <- read.table("train/subject_train.txt", header=FALSE)
y_train <- read.table("train/y_train.txt", header=FALSE)

###
###  Read the subject  "subject_test.txt" and activity "y_test.txt" data sets for train
###

subject_test  <- read.table("test/subject_test.txt", header=FALSE)
y_test  <- read.table("test/y_test.txt", header=FALSE)


##
## transforms the data frames to tbl_df
##

subject_train <- tbl_df(subject_train)
y_train <- tbl_df(y_train)
subject_test <- tbl_df(subject_test )
y_test <- tbl_df(y_test)
 
###
### Merging Activity , y_train & y_test and put them in Y
###

Y <- rbind(y_train , y_test)

###
###   modify row value for the six activities
###

Y  <-  mutate( Y , V1 = ifelse(V1 == 1 , "WALKING",V1))
Y  <-  mutate(Y , V1 = ifelse(V1 == 2 , "WALKING_UPSTAIRS",V1))
Y  <-  mutate(Y , V1 = ifelse(V1 == 3 , "WALKING_DOWNSTAIRS",V1))
Y  <-  mutate(Y , V1 = ifelse(V1 == 4 , "SITTING",V1))
Y  <-  mutate(Y , V1 = ifelse(V1 == 5 , "STANDING",V1))
Y  <-  mutate(Y , V1 = ifelse(V1 == 6, "LAYING",V1))


###
###  Merging subject:   subject_train & subject_test 
### must be in order train first
###

subject <- rbind(subject_train, subject_test )

###
###  Mergin activity  and subject by column, subject on the left 
###  the order don't matter
### 

subject_activiy  <- cbind (subject , Y)

###
###  rename column of  subject_activiy  
###

colnames(subject_activiy ) <- c("subject", "activity")

###
###   Uses descriptive activity names to name the activities in the data set
###   

colnames(DF2) <- col_to_select$V2

###
###  Mergin DF2   & subject_activity by columns
###

DF2  <- cbind(subject_activiy  , DF2)

###
### for problèmes with some characters in the column i rename the columns eliminating the characters: (, ), -
### it's not elegant but works
###

colnames(DF2) <-
c("subject", "activity","tBodyAcc_mean_X", "tBodyAcc_mean_Y", "tBodyAcc_mean_Z", "tBodyAcc_std_X", "tBodyAcc_std_Y", "tBodyAcc_std_Z", "tGravityAcc_mean_X",  "tGravityAcc_mean_Y", 
"tGravityAcc_mean_Z", "tGravityAcc_std_X", "tGravityAcc_std_Y", "tGravityAcc_std_Z", "tBodyAccJerk_mean_X", "tBodyAccJerk_mean_Y",   "tBodyAccJerk_mean_Z", "tBodyAccJerk_std_X", 
"tBodyAccJerk_std_Y", "tBodyAccJerk_std_Z", "tBodyGyro_mean_X", "tBodyGyro_mean_Y", "tBodyGyro_mean_Z",         
"tBodyGyro_std_X", "tBodyGyro_std_Y", "tBodyGyro_std_Z", "tBodyGyroJerk_mean_X", "tBodyGyroJerk_mean_Y", "tBodyGyroJerk_mean_Z", "tBodyGyroJerk_std_X",      
"tBodyGyroJerk_std_Y", "tBodyGyroJerk_std_Z", "tBodyAccMag_mean", "tBodyAccMag_std", "tGravityAccMag_mean", "tGravityAccMag_std", "tBodyAccJerkMag_mean",     
"tBodyAccJerkMag_std", "tBodyGyroMag_mean", "tBodyGyroMag_std", "tBodyGyroJerkMag_mean", "tBodyGyroJerkMag_std", "fBodyAcc_mean_X", "fBodyAcc_mean_Y" ,         
"fBodyAcc_mean_Z", "fBodyAcc_std_X", "fBodyAcc_std_Y", "fBodyAcc_std_Z", "fBodyAccJerk_mean_X", "fBodyAccJerk_mean_Y",  "fBodyAccJerk_mean_Z",       
"fBodyAccJerk_std_X", "fBodyAccJerk_std_Y", "fBodyAccJerk_std_Z", "fBodyGyro_mean_X", "fBodyGyro_mean_Y", "fBodyGyro_mean_Z", "fBodyGyro_std_X",         
"fBodyGyro_std_Y", "fBodyGyro_std_Z", "fBodyAccMag_mean", "fBodyAccMag_std", "fBodyBodyAccJerkMag_mean", "fBodyBodyAccJerkMag_std", "fBodyBodyGyroMag_mean",    
"fBodyBodyGyroMag_std", "fBodyBodyGyroJerkMag_mean",  "fBodyBodyGyroJerkMag_std" )

###
### From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
###

TIDY_DF <- sqldf( "select  activity, subject,
avg(tBodyAcc_mean_X), avg(tBodyAcc_mean_Y), avg(tBodyAcc_mean_Z), avg(tBodyAcc_std_X), avg(tBodyAcc_std_Y), avg(tBodyAcc_std_Z), avg(tGravityAcc_mean_X),  avg(tGravityAcc_mean_Y),
avg(tGravityAcc_mean_Z), avg(tGravityAcc_std_X), avg(tGravityAcc_std_Y), avg(tGravityAcc_std_Z), avg(tBodyAccJerk_mean_X), avg(tBodyAccJerk_mean_Y),  avg(tBodyAccJerk_mean_Z), avg(tBodyAccJerk_std_X),
avg(tBodyAccJerk_std_Y), avg(tBodyAccJerk_std_Z), avg(tBodyGyro_mean_X), avg(tBodyGyro_mean_Y), avg(tBodyGyro_mean_Z),         
avg(tBodyGyro_std_X), avg(tBodyGyro_std_Y), avg(tBodyGyro_std_Z), avg(tBodyGyroJerk_mean_X), avg(tBodyGyroJerk_mean_Y), avg(tBodyGyroJerk_mean_Z), avg(tBodyGyroJerk_std_X),      
avg(tBodyGyroJerk_std_Y), avg(tBodyGyroJerk_std_Z), avg(tBodyAccMag_mean), avg(tBodyAccMag_std), avg(tGravityAccMag_mean), avg(tGravityAccMag_std), avg(tBodyAccJerkMag_mean),     
avg(tBodyAccJerkMag_std), avg(tBodyGyroMag_mean) , avg(tBodyGyroMag_std), avg(tBodyGyroJerkMag_mean), avg(tBodyGyroJerkMag_std), avg(fBodyAcc_mean_X), avg(fBodyAcc_mean_Y) ,         
avg(fBodyAcc_mean_Z), avg(fBodyAcc_std_X), avg(fBodyAcc_std_Y), avg(fBodyAcc_std_Z), avg(fBodyAccJerk_mean_X), avg(fBodyAccJerk_mean_Y),  avg(fBodyAccJerk_mean_Z),       
avg(fBodyAccJerk_std_X), avg(fBodyAccJerk_std_Y), avg(fBodyAccJerk_std_Z), avg(fBodyGyro_mean_X), avg(fBodyGyro_mean_Y), avg(fBodyGyro_mean_Z), avg(fBodyGyro_std_X),         
avg(fBodyGyro_std_Y), avg(fBodyGyro_std_Z), avg(fBodyAccMag_mean), avg(fBodyAccMag_std), avg(fBodyBodyAccJerkMag_mean), avg(fBodyBodyAccJerkMag_std), avg(fBodyBodyGyroMag_mean),    
avg(fBodyBodyGyroMag_std), avg(fBodyBodyGyroJerkMag_mean),  avg(fBodyBodyGyroJerkMag_std)
from DF2
group by activity, subject
order by activity, subject ;" )


###
### Write tidy data to file "tidy_df.txt"
###

write.table(TIDY_DF, "tidy_df.txt")





