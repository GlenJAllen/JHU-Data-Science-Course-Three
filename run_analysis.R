library(data.table)
library(tidyverse)
library(glue)

# location of the folder where the necessary text files are stored
setwd("location/of/the/folder/where/the/necessary/text/files/are/stored")

run_analysis <- function() {
  # 1. Read in the training and test data; rename the columns; rbind train and test together
  .read_rename_rbind <- function(file.type, names) {
    # read in a table and set the column names
    .read_rename <- function(path, names) setNames(fread(path, data.table = FALSE), names)
    # get paths of the form c("train/X_train.txt", "test/X_test.txt")
    .get_paths <- function(file.type) {s <- c("train", "test"); glue("{s}/{file.type}_{s}.txt")}
    # apply .read_data to training and test paths from .get_paths and rbind the results
    map_dfr(.get_paths(file.type), .read_rename, names)
  }
  
  # 2. Read in the feature names and activity labels we'll need.
  feature.names <- fread("features.txt")$V2
  activity.map <- fread("activity_labels.txt")$V2
  
  # 3. Call 1. for subjects, activities, and the feature data; map activities to human-readable 
  # labels; drop extraneous features; cbind the three to get the final data frame
  cbind(.read_rename_rbind("subject", "subject"), 
        transmute(.read_rename_rbind("y", "activity"), activity = activity.map[activity]),
        select(.read_rename_rbind("X", feature.names), grep("mean\\(|std\\(", feature.names)))
}

app.data <- run_analysis()

# 4. Calculate the mean for each feature for each subject/activity
app.data %>% group_by(subject, activity) %>%
  summarize_all(mean) %>%
  write.table("results.txt", row.names = FALSE)
