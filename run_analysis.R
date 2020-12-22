## Final Project Course Getting Cleaning data


#getdata_projectfiles_UCI HAR Dataset.zip
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones


filename <- "samsung_data.zip"


if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}



# Checking if folder exists
if (!file.exists("samsung_dataset")) {
  unzip(filename)
}

