library(dplyr)

#read data
trainData <- read.table("./UCIHARDataset/train/X_train.txt")
trainLabels <- read.table("./UCIHARDataset/train/y_train.txt")
testData <- read.table("./UCIHARDataset/test/X_test.txt")
testLabels <- read.table("./UCIHARDataset/test/y_test.txt")

features <- read.table("./UCIHARDataset/features.txt")
activiltyLabels <- read.table("./UCIHARDataset/activity_labels.txt")
trainSubject <- read.table("./UCIHARDataset/train/subject_train.txt")
testSubject <- read.table("./UCIHARDataset/test/subject_test.txt")

#merge data
CompleteData <- rbind(trainData,testData)
totallabels <- rbind(trainLabels,testLabels)
totalSubjects <- rbind(trainSubject,testSubject)

#Extracts only the measurements on the mean and standard deviation 
#for each measurement and place them in a table
meanspos <- grep("mean", features[,2])
stdpos <- grep("std", features[,2])
MeanStdData <- CompleteData[,c(meanspos, stdpos)]

# descriptive activity names to name the activities in the data set
totallabels[,2] <- activiltyLabels[match(totallabels[,1],activiltyLabels[,1]),2]
totallabels[,2] <- as.factor(totallabels[,2])
finalset <- cbind(totallabels, totalSubjects, MeanStdData)

# Appropriately labels the data set with descriptive variable names
meansVal <- grep("mean", features[,2], value = TRUE)
stdVal <- grep("std", features[,2], value = TRUE)
newLabels <- c("CodeActivity", "Activity","Subject", meansVal, stdVal)
names(finalset) <- newLabels

#From the data set in step 4, creates a second, independent tidy data set with 
#the average of each variable for each activity and each subject.

prom <- finalset %>% group_by(Activity,Subject) %>% summarize_all(funs(mean))

write.table(prom, file = "tidyDataRicardoCuervo.txt", row.names = FALSE)






