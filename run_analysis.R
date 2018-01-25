library(data.table)
library(tidyverse)
library(glue)

setwd("location/of/the/folder/where/the/necessary/text/files/are/stored")

run_analysis <- function() {
  # 1. Read, rename, rbind
  .read_rename_rbind <- function(prefix, names) {
    # create paths of the form c("train/{prefix}_train.txt", "test/{prefix}_test.txt")
    .create_paths <- function(prefix) {s <- c("train", "test"); glue("{s}/{prefix}_{s}.txt")}
    # read in the train and test data, rename the columns and rbind the results
    map_dfr(.create_paths(prefix), ~setNames(fread(., data.table = FALSE), names))
  }
  
  # 2. Read in the feature names and activity labels we'll need.
  feature.names <- fread("features.txt")$V2
  activity.map <- fread("activity_labels.txt")$V2
  
  # 3. Call 1. for subjects, activities, and the feature data; map activities to human-readable 
  # labels; drop extraneous features; cbind the three to get the final data frame
  cbind(.read_rename_rbind("subject", "subject"), 
        transmute(.read_rename_rbind("y", "activity"), activity = activity.map[activity]),
        select(.read_rename_rbind("X", feature.names), grep("mean\\(|std", feature.names)))
}

app.data <- run_analysis()

# 4. Calculate the mean for each feature for each subject/activity
app.data %>% group_by(subject, activity) %>%
  summarize_all(mean) %>%
  write.table("results.txt", row.names = FALSE)
