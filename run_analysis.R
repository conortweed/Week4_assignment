## Merge the training and test sets to create one dataset

# Test sets 
subj_test_raw <- read.table("subject_test.txt", header = FALSE)
X_test_raw <- read.table("X_test.txt", header = FALSE)
y_test_raw <- read.table("y_test.txt", header = FALSE)

# Create a test dataset
colnames(subj_test_raw) <- c("subjectid")
colnames(y_test_raw) <- c("activity")
test_set <- cbind(subj_test_raw, y_test_raw, X_test_raw)

# Training sets
subj_train_raw <- read.table("subject_train.txt", header = FALSE)
X_train_raw <- read.table("X_train.txt", header = FALSE)
y_train_raw <- read.table("y_train.txt", header = FALSE)

# Create a training dataset
colnames(subj_train_raw) <- c("subjectid")
colnames(y_train_raw) <- c("activity")
train_set <- cbind(subj_train_raw, y_train_raw, X_train_raw)

# Merge them together
merged_data <- rbind(train_set, test_set)


## Extract only the measurements on the mean and standard deviation for each measurement. 
# Create a character vector of column names based on the features.txt file
features <- read.table("features.txt", header = FALSE)
feat_cols <- features[,2]
names(merged_data)[3:563] <- as.character(feat_cols)

# Select out the variables relating to the mean and std using grep
mean_std_cols <- grep(".*mean().*|.*std().*", features[,2], value = TRUE)
mean_std_cols <- c("subjectid", "activity", mean_std_cols)

# Use list of desired variables to subset the dataframe
mean_std_data <- merged_data[,mean_std_cols]


## Label the activities with descriptive terms
# Create table of activities with the levels kept as column 1
activity_labels <- read.table("activity_labels.txt", header = FALSE)
# Activity variable converted to factors and then labelled
mean_std_data$activity <- factor(mean_std_data$activity, levels = activity_labels[,1], labels = activity_labels[,2])      

##Create a second, independent tidy data set with the average of each variable for each activity and each subject.
library(dplyr)
new_means <- mean_std_data %>%      
                    group_by(activity, subjectid) %>%      
                    summarise_all(funs(mean))

write.table(new_means, file = "./Week4/Assignment/new_means.txt", row.names = FALSE, col.names = TRUE)
