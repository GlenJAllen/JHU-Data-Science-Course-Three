library(data.table)
library(tidyverse)
library(magrittr)
library(glue)

# location of the folder where the necessary text files are stored
setwd("location/of/the/folder/where/the/necessary/text/files/are/stored")

run_analysis <- function() {
  # read in the training and test data rename the columns rbind train and test together
  .read_rename_rbind <- function(file.type, names) {
    # A simple function to read in a text file and set the column names for the created data frame
    .read_rename <- function(path, names) set_colnames(fread(path, data.table = FALSE), names)
    
    # get paths of the form c("train/X_train.txt", "test/X_test.txt")
    .get_paths <- function(file.type) {s <- c("train", "test"); glue("{s}/{file.type}_{s}.txt")}
    
    # Apply .read_data to both training and test paths from .get_paths and rbind the results together
    map_dfr(.get_paths(file.type), .read_rename, names)
  }
  
  # The names of the features for the data
  feature.names <- fread("features.txt")$V2
  # Just the indices for the mean and std features
  relevant.feature.indices <- setdiff(grep("mean|std", feature.names), grep("Freq", feature.names))
  # The labels mapping
  activity.map <- fread("activity_labels.txt")$V2
  
  # Do the above for subjects, activities, and the feature data
  # Map activities to human-readable labels
  # cbind the three together to get the final data frame
  cbind(.read_rename_rbind("X", feature.names)[, relevant.feature.indices],
        transmute(.read_rename_rbind("y", "activity"), activity = activity.map[activity]),
        .read_rename_rbind("subject", "subject"))
}

app.data <- run_analysis()

# Calculate the mean for each feature for each subject/activity
app.data %>% group_by(subject, activity) %>%
  summarize_all(mean) %>%
  write.table("results.txt", row.names = FALSE)
