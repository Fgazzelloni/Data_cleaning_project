## Final Project Course Getting Cleaning data
## Data collected from the accelerometers from the Samsung Galaxy S smartphone

# Source:
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones


## LIBRARIES---------------------------------------
#library(data.table)
#library(tidyverse)
#library(plyr)

library(dplyr)


## LOAD DATA------------------------------------------
# The data-sets will be downloaded in a chosen filename folder
filename <- "samsung_data.zip"

# Download Samsung data from the web in ".zip" format and unzip
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}

unzip(filename)


## LIST DATA--------------------------------------------------------------
#list all the files inside the specified folder
list.files("UCI HAR Dataset" )

### FEATURES VECTOR----------------------------
features<-read.table("UCI HAR Dataset/features.txt",col.names= c("id","features_names"))
dim(features)

## ACTIVITY----------------------------------
activity<-read.table("UCI HAR Dataset/activity_labels.txt", col.names= c("activity_code", "activity"))
dim(activity)

## TRAIN AND TEST SETS------------------------------------------
# list all the files inside "train" and "test" folders
list.files("UCI HAR Dataset/train/Inertial Signals")#same content as "./test/Inertial Signals"
list.files("UCI HAR Dataset/train")
list.files("UCI HAR Dataset/test")

# 1- MERGE TRAINING AND TEST SETS to create one data set---------------------------------------

# read SUBJECT for train and test sets
train_subj<-read.table("UCI HAR Dataset/train/subject_train.txt",col.names= "subject")
test_subj<-read.table("UCI HAR Dataset/test/subject_test.txt",col.names= "subject")

# check of the proportion of the subjects divided by 30% and 70% in the training and test groups
require(plyr)
count(train_subj);
count(test_subj)

# merge SUBJECT for train and test sets----
merged_subj<-rbind(train_subj,test_subj)

# remove unwanted sets in the environment
rm(train_subj); rm(test_subj)

# read VALUES for train and test sets
train_values<- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$features_names)
test_values<- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$features_names)

# check of the structure of the sets
head(train_values,c(2,3));head(test_values,c(2,3))
dim(train_values);dim(test_values)

# assure the sets have the same variable names
table(names(train_values)==names(test_values))

# merge VALUES for train and test sets-----
merged_values<-rbind(train_values,test_values)

# remove of the initial training and test sets:
rm(train_values);rm(test_values)

# read LABELS for train and test
train_lab<- read.table("UCI HAR Dataset/train/y_train.txt",col.names= c("activity_code"))
test_lab<- read.table("UCI HAR Dataset/test/y_test.txt", col.names= c("activity_code"))

# check of the structure of the sets
head(train_lab);head(test_lab)
dim(train_lab);dim(test_lab)

# merge LABELS for train and test----
merged_lab<-rbind(train_lab,test_lab)

# remove of the training_lab and test_lab:
rm(train_lab);rm(test_lab)


# TRAINING AND TEST MERGED----
training_and_test<-cbind(merged_subj,merged_lab,merged_values)

# check of the structure of the sets
head(training_and_test,c(2,4))
dim(training_and_test)

# remove of the merged_lab and merged_sets
rm(merged_subj);rm(merged_lab);rm(merged_values)


# 2- MEAN and STANDARD DEVIATION for each measurement-----

# Extraction of selected descriptive statistics for each measurement
# select only mean and standard deviation, no angle() function measurements or others mean frequencies
training_and_test_stats<-training_and_test%>%
  select(subject,activity_code,!contains(c("meanFreq", "angle"),ignore.case = TRUE))%>%
  select(subject,activity_code,contains(c("mean","std"), ignore.case = TRUE))

# check of the structure of the sets
head(training_and_test_stats,c(2,6))
dim(training_and_test_stats)

#remove the original
rm(training_and_test)

# 3- Add the description of the activities in the data set----

training_and_test_stats$activity_code <- activity[training_and_test_stats$activity_code, 2]


# check of the structure
head(training_and_test_stats,c(2,4))

# 4- Adjust data labels with descriptive variable names----

# list of the names vector
names(training_and_test_stats)
# change the strings as needed
names(training_and_test_stats)[2]<-"Activity"
names(training_and_test_stats)<-sub("^t","Time",names(training_and_test_stats))
names(training_and_test_stats)<-sub("^f","Frequency",names(training_and_test_stats))
names(training_and_test_stats)<-gsub("(^[[:alpha:]])", "\\U\\1", names(training_and_test_stats), perl=TRUE)
names(training_and_test_stats)<-sub("Acc","Accelerometer",names(training_and_test_stats))
names(training_and_test_stats)<-sub("Gyro","Gyroscope",names(training_and_test_stats))
names(training_and_test_stats)<-sub("Mag","Magnitude",names(training_and_test_stats))
names(training_and_test_stats)<-sub(".tBody","TimeBody",names(training_and_test_stats))
names(training_and_test_stats)<-gsub("([a-z])([A-Z])", "\\1_\\2", names(training_and_test_stats))#add an underscore sign
names(training_and_test_stats)<-sub("Body_Body","Body",names(training_and_test_stats))
names(training_and_test_stats)<-sub("\\.","_",names(training_and_test_stats))
names(training_and_test_stats)<-gsub("[[:punct:]]","_",names(training_and_test_stats) )
names(training_and_test_stats)<-gsub("[[:punct:]]*$","",names(training_and_test_stats) )
names(training_and_test_stats)<-gsub("(_[XYZ])(.*)","\\2\\1", names(training_and_test_stats))
names(training_and_test_stats)<-gsub("gravity","Gravity", names(training_and_test_stats))
names(training_and_test_stats)<-gsub("___","_", names(training_and_test_stats))
names(training_and_test_stats)<-gsub("([Mm]ean)","MEAN", names(training_and_test_stats))
names(training_and_test_stats)<-gsub("(std)","STD", names(training_and_test_stats))


# 5- Create a second independent tidy data set--------
# From the data set in step 4, creates a second, independent tidy data set with the average of
# each variable for each activity and each subject

# to create a tidy and independent data set, the "training_and_test_stats" with only selected descriptive statistics
# has been reshaped using "melt" , "dcast" and "melt" again

# the data set is reshaped in a four-column data-set
require(reshape2)
tidy_data_melt<-melt(training_and_test_stats,id=c("Subject","Activity"),measure.vars=3:68)
# taken the mean of all the values per "Activity" and "subject"
tidy_data_cast<-dcast(tidy_data_melt,Subject+Activity~variable,mean)

tidy_data_melt2<-melt(tidy_data_cast,id=c("Subject","Activity"),measure.vars=3:68)

# round of the value statistics
tidy_data_melt2$value<-round(tidy_data_melt2$value,3)

head(tidy_data_melt2)
dim(tidy_data_melt2)

# remove of the sets that are not needed
rm(tidy_data_melt,tidy_data_cast)

# check of the unique variables in the set
plyr::count(tidy_data_melt2$variable)

# separate the string in the column "variable" into six identifier variables
tidy_data<-tidy_data_melt2 %>%
  tidyr::separate(variable,
                  into = c("Domain",
                           "Component",
                           "Sensor",
                           "Signal",
                           "Stats",
                           "Direction"),
                  sep = "\\_")

head(tidy_data)

# remove melted dataset
rm(tidy_data_melt2)

# check that the column's names contains the correct data
plyr::count(tidy_data$Domain)
plyr::count(tidy_data$Component)
plyr::count(tidy_data$Sensor)
# these columns are not sorted correctly
plyr::count(tidy_data$Signal)#col 6
plyr::count(tidy_data$Stats)#col 7
plyr::count(tidy_data$Direction)#col 8

# relocate the row variables in the correct column
require(tidyverse)
final<-tidy_data%>%
  as_tibble()%>%
  select(Signal,Stats,Direction)%>% #selection of the columns to be fixed
  mutate(id=row_number())%>%         #add of an index to identify the column
  pivot_longer(-id)%>%               #long list of all the variables
  group_by(id)%>%
  summarize(                         #relocate variables in the correct column
    Signal = paste(value%>%keep(~.x %in% c("Jerk","Magnitude")),collapse="_"),
    Stats = paste(value%>%keep(~.x %in% c("MEAN","STD")),collapse=""),
    Direction = paste(value%>%keep(~.x %in% c("X","Y","Z")),collapse="")
  )%>%
  select(-id)


# rebuild the data set with the original set and the fixed columns
final1<-tidy_data%>%
  select(-c(Signal,Stats,Direction))

tidy_data<-cbind(final1,final)%>%
  select(Subject,
         Activity,
         Domain,
         Component,
         Sensor,
         Signal,
         Stats,
         Direction,
         Value="value")

# remove "final1" and "final"
rm(final1,final)

# final "tidy_data" set composed of nine columns
# each column has a variable and each row has an observation value
head(tidy_data);tail(tidy_data)


# TIDY DATA #########################################################################

write.table(tidy_data, "tidydata.txt", row.name=FALSE)

tidy_data<-read.table("tidydata.txt")



