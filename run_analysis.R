library(data.table)
library(tidyverse)
library(glue)

# location of the folder where the necessary text files are stored
setwd("location/of/the/folder/where/the/necessary/text/files/are/stored")

run_analysis <- function() {
  # 1. Read in the training and test data; rename the columns; rbind train and test together
  .read_rename_rbind <- function(file.type, names) {
    # a simple function to read in a text file and set the column names for the created data frame
    .read_rename <- function(path, names) setNames(fread(path), names)
    # get paths of the form c("train/X_train.txt", "test/X_test.txt")
    .get_paths <- function(file.type) {s <- c("train", "test"); glue("{s}/{file.type}_{s}.txt")}
    # apply .read_data to both training and test paths from .get_paths and rbind the results together
    map_dfr(.get_paths(file.type), .read_rename, names)
  }
  
  # 2. Read in the column names and activity labels we'll need.
  # the names of the features for the data
  feature.names <- fread("features.txt")$V2
  # just the indices for the mean and std features
  relevant.feature.indices <- grep("mean\\(|std\\(", feature.names)
  # the labels mapping
  activity.map <- fread("activity_labels.txt")$V2
  
  # 3. Call 1. for subjects, activities, and the feature data; get rid of extraneous features; 
  # map activities to human-readable labels; cbind the three together to get the final data frame
  cbind(.read_rename_rbind("X", feature.names)[, relevant.feature.indices],
        transmute(.read_rename_rbind("y", "activity"), activity = activity.map[activity]),
        .read_rename_rbind("subject", "subject"))
}

app.data <- run_analysis()

# calculate the mean for each feature for each subject/activity
app.data %>% group_by(subject, activity) %>%
  summarize_all(mean) %>%
  write.table("results.txt", row.names = FALSE)
