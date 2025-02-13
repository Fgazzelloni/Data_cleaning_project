---
title: "Variables Codebook"
subtitle: "Data cleaning project - Getting Cleaning Data - JHU"
author: "Federica Gazzelloni"
date: "23/12/2020 - 22/01/2021"
output:
  html_document:
    toc: TRUE
    number_sections: FALSE
    toc_float:
      collapsed: TRUE
      smooth_scroll: FALSE
    keep_md: yes
---


```{r}
library(rmarkdown)
library(knitr)
```


# Data Cleaning Project Description

Randomized sample training and test analysis of a group of 30 volunteers within an age bracket of 19-48 years.
Individuals performed six activities while wearing a smartphone (Samsung Galaxy S II) on the waist to capture:

- acceleration with an accelerometer 
- velocity with a gyroscope 

## GitHub Repository
The project require a new dataset with selected descriptive statistics and a repository in GitHub:

- https://github.com/Fgazzelloni/Data_cleaning_project

##### 

# Study design and data processing 

##### The experiment randomized a group of 30 volunteers into two subgroup in proportions:

- TRAINING 70%
- TEST     30%

To measure the variation in Time and Frequency of linear acceleration and angular velocity with sensor signals accelerometer and gyroscope by applying noise filters and then sampled.

##### 

## Measurement of Variation

##### The variations is measured in DOMAINS:

- Time
- Frequency

##### The Motion COMPONENTS are:

- Body
- Gravity

##### The type of SIGNALS:

- Jerk      ( rate at which an object's acceleration changes with respect to time, 
              having both magnitude and direction )
- Magnitude ( maximum positive value of the height of the wave )

##### The SENSORS used to measure the variation:

- Accelerometer         
- Gyroscope



### Time

##### The measure capture "t" time-domain variation through sensor signals and takes consideration of the "-XYZ" 3-axial signals in the X, Y and Z directions, as indicated below:

The measurements in Time domain also consider the two type of signal measurements of amplitude Jerk and Magnitude

- "tAcc-XYZ"  ( Time accelerometer 3-axial signals ) subgroups:
                              
                              "tBodyAcc-XYZ"      ( Time Body Accelerometer 3-axial signals )
                              "tGravityAcc-XYZ"   ( Time Gravity accelerometer 3-axial signals )
                              
- "tGyro-XYZ" ( Time Gyroscope 3-axial signals )

###### 

#### Jerk signals
##### Obtained deriving the body linear acceleration and the angular velocity:

- tBodyAccJerk-XYZ  ( Time Body Accelerometer Jerk 3-axial signals )
- tBodyGyroJerk-XYZ ( Time Body Gyroscope Jerk 3-axial signals )

#### Magnitude
##### The magnitude of these three-dimensional signals were calculated using the Euclidean norm:

- tBodyAccMag      ( Time Body Accelerometer Magnitude )
- tGravityAccMag   ( Time Gravity Accelerometer Magnitude )

- tBodyGyroMag     ( Time Body Gyroscope Magnitude )

- tBodyAccJerkMag  ( Time Body Accelerometer Jerk Magnitude )
- tBodyGyroJerkMag ( Time Body Gyroscope Jerk Magnitude )


### Frequency
##### The Fast Fourier Transform (FFT) was applied to some of these signals producing,"f" frequency domain signals:

The measurements in Frequency domain also consider the two type of signal measurements of amplitude Jerk and Magnitude

- fBodyAcc-XYZ     ( Frequency Body Accelerometer 3-axial signals )
- fBodyGyro-XYZ    ( Frequency Body Gyroscope 3-axial signals )

#### Jerk signals
- fBodyAccJerk-XYZ ( Frequency Body Accelerometer Jerk 3-axial signals )

#### Magnitude
- fBodyAccMag      ( Frequency Body Accelerometer Magnitude )
- fBodyGyroMag     ( Frequency Body Gyroscope Magnitude )
- fBodyAccJerkMag  ( Frequency Body Accelerometer Jerk Magnitude )
- fBodyGyroJerkMag ( Frequency Body Gyroscope Jerk Magnitude )


##### 

## Descriptive statistics
##### Included descriptive statistics calculated to analyze results of measurements of interest in this project:  

- mean(): Mean value
- std(): Standard deviation

##### Excluded descriptive statistics:

- mad(): Median absolute deviation 
- max(): Largest value in array
- min(): Smallest value in array
- sma(): Signal magnitude area
- energy(): Energy measure. Sum of the squares divided by the number of values. 
- iqr(): Interquartile range 
- entropy(): Signal entropy
- arCoeff(): Autorregresion coefficients with Burg order equal to 4
- correlation(): correlation coefficient between two signals
- maxInds(): index of the frequency component with largest magnitude
- meanFreq(): Weighted average of the frequency components to obtain a mean frequency
- skewness(): skewness of the frequency domain signal 
- kurtosis(): kurtosis of the frequency domain signal 
- bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
- angle(): Angle between to vectors.

##### 

## Collection of the raw data
The experiment collection of data-sets were downloaded under .zip format at the following address:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

The folder "UCI HAR Dataset" included two more "test" and "train" folders and four .txt files with information about the variables and variables labels. All the files were listed, read and subset as requested selecting only Mean and Standard Deviation as descriptive statistics. 

##### 

# Creating the tidy datafile

### Guide to create the tidy data file
1. download the data and unzip it in the project folder
2. check of the original "Readme.txt" and "features_info.txt" files for information about how variables are listed and what is their meaning
3. identify the location of the variables of interest for the project
4. check the structure of the data sets for merging column names with content
5. extracting needed information by the selection of the columns containing the descriptive statistics as requested.
6. split the columns to selected information to obtain a tidy data set


### Cleaning of the data
After having downloaded the raw data, unzipped the files, selected the requested information, the sets were assembled in one unique dataset and the variables cleaned to have one variable per column and one observation per row. To do this the set were melt and dcast twice to obtain a clean four column set of mean data grouped per "Activity" and "Subject". The long-string of measurements renamed appropriately to make the variables name standing out more clearly and then separated within six variables:

- Domain      (Time/Frequency)
- Component   (Body/Gravity)
- Sensor      (Accelerometer/Gyroscope)
- Signal      (Jerk/Magnitude)
- Stats       (Mean/Standard Deviation)
- Direction   (X/Y/Z)


```{r}
#source("README.md")
```

##### 

# Description of the variables
General description of the file includes :

##### Dimensions of the datasets
 
```{r message=FALSE, warning=FALSE}
source("run_analysis.R")
```


##### Activity sets:
```{r}
dim(activity);
head(activity)
```


##### Features sets:
```{r}
dim(features);
head(features)
```

##### Training and Test selected descriptive Statistics sets:

```{r message=FALSE, warning=FALSE}
dim(training_and_test_stats);
head(training_and_test_stats,c(4,4))
```

##### Final independent Tidy Data set:

```{r message=FALSE, warning=FALSE}
dim(tidy_data);
head(tidy_data)
```
 
##### Summary of the data:
 
```{r}
summary(tidy_data)
```
 
## Variables present in the dataset:

##### Variable 1- Subject 
Thirty subjects volunteers for the experiment represented as integers 1 to 30

##### Variable 2 - Activity 
##### Six activities per thirty subject makes 180 outcomes for each measurement
- WALKING
- WALKING_UPSTAIRS
- WALKING_DOWNSTAIRS
- SITTING
- STANDING
- LAYING

##### Variable 3 - Domain 
##### Two domains as character:
- Time
- Frequency

```{r}
plyr::count(tidy_data$Domain)
```

##### Variable 4 - Component 
- Body
- Gravity
```{r}
plyr::count(tidy_data$Component)
```

##### Variable 5 - Sensor 
- Accelerometer         
- Gyroscope
```{r}
plyr::count(tidy_data$Sensor)
```


##### Variable 6 - Signal 
- Jerk      
- Magnitude
```{r}
plyr::count(tidy_data$Signal)
```

##### Variable 7 - Stats 
- Mean
_ Standard Deviation
```{r}
plyr::count(tidy_data$Stats)
```


##### Variable 8 - Direction
- X
- Y
- Z

```{r}
plyr::count(tidy_data$Direction)
```

##### Variable 9 - Value

0 to 1 rates as double numeric values identifying the results of the measurements of combination of variables

# Source of the Project
Getting Cleaning Data final Coursera Project by JHU: 
- "https://www.coursera.org/learn/data-cleaning"





















