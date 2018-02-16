library(data.table)
library(tidyverse)
library(glue)

setwd("location/of/the/folder/where/the/necessary/text/files/are/stored")

run_analysis <- function() {
  # 1. Read, rename, rbind
  .read_rename_rbind <- function(prefix, names) {
    # Create paths of the form c("train/{prefix}_train.txt", "test/{prefix}_test.txt")
    .create_paths <- function(prefix) glue("{train.test}/{prefix}_{train.test}.txt")
    # read in the train and test data, rbind the results and rename the columns
    .create_paths(prefix) %>%
      map_dfr(fread, data.table = FALSE) %>%
      setNames(names)
  }

  # 2. Read in the feature names and activity labels we'll need.
  feature.names <- fread("features.txt")$V2
  activity.map <- fread("activity_labels.txt")$V2
  # For creating paths of the form c("train/{prefix}_train.txt", "test/{prefix}_test.txt")
  train.test <- c("train", "test")

  # 3. Call 1. for subjects, activities, and the feature data
  #    cbind the three to get the final data frame
  cbind(.read_rename_rbind("subject", "subject"),
        # map activities to human-readable labels
        transmute(.read_rename_rbind("y", "activity"), activity = activity.map[activity]),
        # drop extraneous features
        .read_rename_rbind("X", feature.names)[, grep("mean\\(|std", feature.names)])
}

# This completes the first part of the instructions.
app.data <- run_analysis()

# 4. Calculate the mean for each feature for each subject/activity
app.data %>% group_by(subject, activity) %>%
  summarize_all(mean) %>%
  write.table("results.txt", row.names = FALSE)
