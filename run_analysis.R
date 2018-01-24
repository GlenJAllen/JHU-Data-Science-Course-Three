library(data.table)
library(tidyverse)
library(magrittr)

# Just notices that the submission was intended to work for the working directory.
# remove base.path assignment and all references to base.path and setwd(wherever/you/have/the/data)

# location of the folder where the necessary text files are stored
base.path <- "C:/Users/u55a20/Downloads/johns_hopkins_course_3_project_data/UCI HAR Dataset"
# The names of the features for the data
feature.names <- fread(file.path(base.path, "features.txt"))$V2
# Just the indices for the mean and std features
relevant.feature.indices <- setdiff(grep("mean|std", feature.names),
                                    grep("Freq", feature.names))
# Their names
relevant.feature.names <- feature.names[relevant.feature.indices]
# The labels mapping
activity.map <- fread(file.path(base.path, "activity_labels.txt"))

# A simple function to read in a text file from base.path/path
# (possibly) extract certain columns
# and set the column names for the created data frame
read_data <- function(path, names, indices = NULL) {
  df <- fread(file.path(base.path, path), data.table = FALSE)
  if(!is.null(indices)) df %<>% .[, indices]
  set_colnames(df, names)
}

# read in the training and test sets, subjects and activities
# (possibly) extract relevant columns
# rename their columns
# (map activity codes to human-readable labels)
# rbind train and test together for each
# then cbind the three data frames together
app.data <-
  cbind(map_dfr(c("train/X_train.txt", "test/X_test.txt"), read_data,
                relevant.feature.names, relevant.feature.indices),
        map_dfr(c("train/y_train.txt", "test/y_test.txt"), read_data, "activity") %>%
          mutate(activity = activity.map$V2[activity]),
        map_dfr(c("train/subject_train.txt", "test/subject_test.txt"), read_data, "subject"))

# Calculate the mean for each feature for each subject/activity
app.data %>% group_by(subject, activity) %>%
  summarize_all(mean) %>%
  write.table(file.path(base.path, "results.txt"), row.names = FALSE)




