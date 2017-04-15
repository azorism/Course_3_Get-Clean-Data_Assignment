run_analysis <- function(){
  
  library(dplyr)
  library(plyr)
  
  # Read text files into R
  x_test <- read.table("X_test.txt")
  x_train <- read.table("X_train.txt")
  label_test <- read.table("y_test.txt")
  label_train <- read.table("y_train.txt")
  label_desc <- read.table("activity_labels.txt")
  features <- read.table("features.txt")
  sub_test <- read.table("subject_test.txt")
  sub_train <- read.table("subject_train.txt")
  
  # Rename columns "V1" into "Label" and "V2" into "Description" for the activity
  label_test <- dplyr::rename(label_test, Label = V1)
  label_train <- dplyr::rename(label_train, Label = V1)
  label_desc <- dplyr::rename(label_desc, Label = V1, Description = V2)
  
  # Assign description of activities to labels; use join to avoid an unwanted reordering of data rows caused by merge
  label_test <- join(label_test,label_desc,type="left")
  label_train <- join(label_train,label_desc,type="left")
  
  
  # Exercise 1: "Merges the training and the test sets to create one data set"
  merged_data <- rbind(x_test,x_train)
  
  
  # Exercies 4: "Appropriately labels the data set with descriptive variable names" 
  # -->  Change column names according to the features
  names(merged_data) <- features$V2
  
  
  # Exercise 2: "Extracts only the measurements on the mean and standard deviation for each measurement."
  # --> Regular expression checking "mean()" OR "std()" substrings in the column names
  merged_data <- merged_data[ , grepl("mean()|std()" , names(merged_data))]
  
  
  # Exercise 3: "Uses descriptive activity names to name the activities in the data set"
  # Append description for test and train data set into one aggregated data frame; then bind the column to the consolidated data set
  labeling <- rbind(label_test,label_train)
  description <- labeling$Description
  merged_data <- cbind(description, merged_data)
  
  
  # Exercise 5: From the data set in step 4, create a second, independent tidy data set with the average of each variable for 
  # each activity and each subject.
  # Append subject into the consolidated data set
  subjects <- rbind(sub_test,sub_train)
  subjects <- dplyr::rename(subjects, Subject = V1)
  merged_data <- cbind(subjects, merged_data)
  
  # How many columns has the consolidated data frame
  column_count <- dim(merged_data)[[2]]
  
  # Create tidy data set <- Aggregate function to calculate the mean on columns 3-81; i.e. on the mean and std values; grouped by subject & description
  tidy_data <- aggregate(merged_data[, 3:column_count], list(merged_data$Subject,merged_data$description), mean)
  tidy_data <- dplyr::rename(tidy_data, Subject = Group.1, Description = Group.2)
  write.table(tidy_data, file="Assignment_4_tidy_data.txt", append=FALSE, row.names = FALSE)  

  # return tidy_date set for test purposes
  tidy_data
}