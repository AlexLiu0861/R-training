library(reshape2)

# Load activity labels and its features
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")
activityLabels[,2] <- as.character(activityLabels[,2])
features <- read.table("UCI HAR Dataset/features.txt")
features[,2] <- as.character(features[,2])

# Extract only the data on mean and standard deviation
features_Wanted <- grep(".*mean.*|.*std.*", features[,2])
features_Wanted.names <- features[features_Wanted,2]
features_Wanted.names = gsub('-mean', 'Mean', features_Wanted.names)
features_Wanted.names = gsub('-std', 'Std', features_Wanted.names)
features_Wanted.names <- gsub('[-()]', '', features_Wanted.names)

# Load the datasets for both train and test conbination, it's the base of the rest operations.
#There are two main Variables : activities and subjects.
#Activities includes those activityLabels.
#Clean the data by using cbind, to link data and their names.
train <- read.table("UCI HAR Dataset/train/X_train.txt")[features_Wanted]
train_Activities <- read.table("UCI HAR Dataset/train/Y_train.txt")
train_Subjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
train <- cbind(train_Subjects, train_Activities, train)

test <- read.table("UCI HAR Dataset/test/X_test.txt")[features_Wanted]
test_Activities <- read.table("UCI HAR Dataset/test/Y_test.txt")
test_Subjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
test <- cbind(test_Subjects, test_Activities, test)

# merge datasets and add labels
all_Data <- rbind(train, test)
colnames(all_Data) <- c("subject", "activity", features_Wanted.names)

# turn activities & subjects into factors
all_Data$activity <- factor(all_Data$activity, levels = activityLabels[,1], labels = activityLabels[,2])
all_Data$subject <- as.factor(all_Data$subject)
all_Data.melted <- melt(all_Data, id = c("subject", "activity"))
all_Data.mean <- dcast(all_Data.melted, subject + activity ~ variable, mean)

write.table(all_Data.mean, "tidy_data.txt", row.names = FALSE, quote = FALSE)