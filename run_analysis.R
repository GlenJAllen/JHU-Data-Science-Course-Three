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

# A simple function to read in a text file from base.path/path
# (possibly) extract certain columns
# and set the column names for the created data frame
read_data <- function(path, names, indices = NULL) {
  df <- fread(file_path(path), data.table = FALSE)
  if(!is.null(indices)) df %<>% extract(indices)
  set_colnames(df, names)
}

# read in the training and test sets, subjects and activities
# (possibly) extract relevant columns
# rename their columns
# (map activity codes to human-readable labels)
# rbind train and test together for each
# then cbind the three data frames together
read_and_rbind <- function(file.type, names, ...) {
  .get_paths <- function(file.type) {s <- c("train", "test"); glue("{s}/{file.type}_{s}.txt")}
  map_dfr(.get_paths(file.type), read_data, names, ...)
}
app.data <- cbind(read_and_rbind("X", relevant.feature.names, relevant.feature.indices),
                  transmute(read_and_rbind("y", "activity"), activity = activity.map[activity]),
                  read_and_rbind("subject", "subject"))

# Calculate the mean for each feature for each subject/activity
app.data %>% group_by(subject, activity) %>%
  summarize_all(mean) %>%
  write.table(file_path("results.txt"), row.names = FALSE)
