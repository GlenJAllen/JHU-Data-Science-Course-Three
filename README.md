# JHU-Data-Science-Course-Three

This repo contains a single script, run_analysis.R, which merges test and train datasets, renames columns, extracts only the relevant columns, maps activity codes to human-readable labels and then calculates the mean for each feature for each subject-activity combination.

See the codebook for information about the variables.

See the inline comments for greater detail about how the code works.  But here's a brief overview:

0. Generate paths necessary for the relevant files.
1. Read the data in from these files with fread.
2. Row bind the test and training sets together for each pair.
3. Fix the activity labels and feature names.
4. Column bind the three data sets together.
5. Summarize to get the means for each subject/activity.
