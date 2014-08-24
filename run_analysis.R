run_analysis <- function() {
     
     ## Reads in and combines data sets
     Xtrain <- read.table("UCI HAR Dataset/train/X_train.txt")
     Xtest <- read.table("UCI HAR Dataset/test/X_test.txt")
     dat <- rbind(Xtrain, Xtest)
     
     ## Reads in labels and applies to data
     labels <- read.table("UCI HAR Dataset/features.txt")
     colnames(dat) <- labels[, 2]
     
     ## Reads in and combines activity types and labels
     Ytrain <- scan("UCI HAR Dataset/train/y_train.txt")
     Ytest <- scan("UCI HAR Dataset/test/y_test.txt")
     type <- c(Ytrain, Ytest)
     
     ## Reads in and combines subject lists
     subject_train <- scan("UCI HAR Dataset/train/subject_train.txt")
     subject_test <- scan("UCI HAR Dataset/test/subject_test.txt")
     subject <- c(subject_train, subject_test)
     
     ## Combines data and selects only means and standard deviations
     alldata <- cbind(Subject = subject, Activity_Type = type, dat)
     toKeep <- sort(c(grep("mean", names(alldata)), grep("std", names(alldata))))
     data <- alldata[, c(1, 2, toKeep)]
     data <- data[, !grepl("Freq", names(data))]
     data <- data[order(data$Subject), ]
     
     ## Labels activity names
     activities <- read.table("UCI HAR Dataset/activity_labels.txt")
     data$Activity_Type <- as.character(activities[data$Activity_Type, 2])
     
     ## Finds averages of each variable for each activity for each subject
     newdata <- split(data, data$Subject)
     final <- data.frame()
     for (subject in newdata) {
          final <- rbind(final, ddply(subject, .(Activity_Type), .fun = numcolwise(mean)))
     }
     write.table(final, file = “tidy”, row.names = FALSE)
}


