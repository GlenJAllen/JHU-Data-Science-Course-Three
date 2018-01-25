library(data.table)
library(tidyverse)
library(magrittr)
library(glue)

# location of the folder where the necessary text files are stored
base.path <- "C:/Users/u55a20/Downloads/johns_hopkins_course_3_project_data/UCI HAR Dataset"
# Silly convenience function for getting base.path/path
file_path <- function(path) file.path(base.path, path)
# The names of the features for the data
feature.names <- fread(file_path("features.txt"))$V2
# Just the indices for the mean and std features
relevant.feature.indices <- setdiff(grep("mean|std", feature.names), grep("Freq", feature.names))
# Their names
relevant.feature.names <- feature.names[relevant.feature.indices]
# The labels mapping
activity.map <- fread(file_path("activity_labels.txt"))$V2

# read in the training and test data
# (possibly) extract relevant columns
# rename the columns
# rbind train and test together
read_and_rbind <- function(file.type, names, ...) {
  # A simple function to read in a text file from base.path/path
  # (possibly) extract certain columns
  # and set the column names for the created data frame
  .read_data <- function(path, names, indices = NULL) {
    df <- fread(file_path(path), data.table = FALSE)
    if(!is.null(indices)) df %<>% extract(indices)
    set_colnames(df, names)
  }

  # get paths of the form c("train/X_train.txt", "test/X_test.txt")
  .get_paths <- function(file.type) {s <- c("train", "test"); glue("{s}/{file.type}_{s}.txt")}

  # Apply .read_data to both training and test paths from .get_paths
  map_dfr(.get_paths(file.type), .read_data, names, ...)
}

# Do the above for subjects, activities, and the feature data
# Map activities to human-readable labels
# cbind the three together to get the final data frame
app.data <- cbind(read_and_rbind("X", relevant.feature.names, relevant.feature.indices),
                  transmute(read_and_rbind("y", "activity"), activity = activity.map[activity]),
                  read_and_rbind("subject", "subject"))

# Calculate the mean for each feature for each subject/activity
app.data %>% group_by(subject, activity) %>%
  summarize_all(mean) %>%
  write.table(file_path("results.txt"), row.names = FALSE)
