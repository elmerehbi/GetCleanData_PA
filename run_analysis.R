##################################################################################
################# R3 Getting and Cleaning Data - Course Project ##################
##################################################################################

# archive is read from "./data" directory
if(!file.exists("./data/UCI HAR Dataset")){
        unzip("./data/getdata-projectfiles-UCI HAR Dataset.zip", exdir = "./data/")
}

# # create "processed" directory under "./data" to write tidy sets
# if(!file.exists("./data/processed")){
#         dir.create("./data/processed")
# }

############################# 1 MERGING THE DATA  #############################
## Merges the training and the test sets to create one data set.

# labels: Links the class labels with their activity name.
# fea: List of all features (features.txt)
labels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
fea <- read.table("./data/UCI HAR Dataset/features.txt", colClasses = "character")

# tesub/trsub: Each row identifies the subject who performed the 
# activity for each window sample. Its range is from 1 to 30. (subject_test.txt)
# tex: Test set (X_test.txt)
# tey: Test labels (y_test.txt)
# trx: Training set (X_train.txt)
# try_: Training labels (y_train.txt)

tesub <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
tex <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
tey <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
trsub <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
trx <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
try_ <- read.table("./data/UCI HAR Dataset/train/y_train.txt")

colnames(tesub) <- paste("subject"); colnames(trsub) <- paste("subject")
colnames(labels) <- paste(c("label", "activity"))
colnames(tey) <- paste("label"); colnames(try_) <- paste("label")

# change column names to features names
colnames(tex) <- paste(fea$V2); colnames(trx) <- paste(fea$V2)

# merge "tey" with "labels" according to the label
# ordering problem fixed with "id" column and order()
# another way is to use the plyr function join()
tey$id <- 1:nrow(tey)
act_labels_te <- merge(x = tey, y = labels, by = "label", sort = FALSE, )
act_labels_te <- act_labels_te[order(act_labels_te$id), ]

# similarly merge "try_" with "labels" by label
try_$id <- 1:nrow(try_)
act_labels_tr <- merge(x = try_, y = labels, by = "label", sort = FALSE)
act_labels_tr <- act_labels_tr[order(act_labels_tr$id), ]


# adding two columns: subject & activity from "tesub" & "act_labels_te" respectively
test <- cbind(tex, tesub, activity = act_labels_te$activity)
train <- cbind(trx, trsub, activity = act_labels_tr$activity)

# no longer needed; remove from environment
rm(fea,tex, tesub, act_labels_te, tey, trx, trsub, act_labels_tr, try_, labels)

merged <- rbind(test, train)

rm(test, train)

########################### 2 EXTRACTING MEAN & SD  ###########################
## Extracts only the measurements on the mean and standard deviation for each measurement. 

mean_colnames  <- grep("mean", colnames(merged), value = TRUE)
std_colnames <- grep("std", colnames(merged), value = TRUE)

# subset merged by mean, std, subject, activity
extracted <- cbind(merged[,mean_colnames], merged[,std_colnames], subject = merged$subject, activity = merged$activity)
rm(mean_colnames, std_colnames)

# could possibly be done with a one-liner using melt or dcast (reshape2)


######################## 3 DESCRIPTIVE ACTIVITY NAMES #########################
## Uses descriptive activity names to name the activities in the data set

# activity names: replace underscores with space & turn letters to lower case
extracted$activity <- tolower(gsub("_", " ", extracted$activity))


######################## 4 DESCRIPTIVE VARIABLE NAMES ##########################
## Appropriately labels the data set with descriptive variable names. 

colnames(extracted) <- gsub("BodyBody", "Body", colnames(extracted)) # remove duplicates of "Body
colnames(extracted) <- gsub("^(tBody)", "time_body_", colnames(extracted)) 
colnames(extracted) <- gsub("^(fBody)", "freq_body_", colnames(extracted)) 
colnames(extracted) <- gsub("^(tGravity)", "time_gravity_", colnames(extracted)) 
colnames(extracted) <- gsub("Acc", "acceleration_", colnames(extracted))
colnames(extracted) <- gsub("Gyro", "gyroscope_", colnames(extracted))
colnames(extracted) <- gsub("Jerk", "jerk_", colnames(extracted))
colnames(extracted) <- gsub("Mag", "magnitude_", colnames(extracted))
colnames(extracted) <- gsub("-mean[(][)]", "mean_", colnames(extracted))
colnames(extracted) <- gsub("-std[(][)]", "standard.deviation_", colnames(extracted))
colnames(extracted) <- gsub("-meanFreq[(][)]", "mean.freq", colnames(extracted))
colnames(extracted) <- gsub("-X", "_X.axis", colnames(extracted))
colnames(extracted) <- gsub("-Y", "_Y.axis", colnames(extracted))
colnames(extracted) <- gsub("-Z", "_Z.axis", colnames(extracted))
colnames(extracted) <- gsub("__", "_", colnames(extracted))
colnames(extracted) <- gsub("_$", "", colnames(extracted))

# more readable variable names (refer to "features_info.txt")
# gsub("time_body_acceleration_mean", "Mean_of_Time_Domain_of_Body_Acceleration_along", names(extracted))
# gsub("time_gravity_acceleration_mean", "Mean_of_Time_Domain_of_Gravity_Acceleration_along", names(extracted))
# need 19 of them in addition to above 16


#################### 5 NEW TIDY SET W/ VARIABLES "AVERAGES" ####################
## Creates a second, independent tidy data set with the average of each variable
## for each activity and each subject.

# calculate mean of each variable for each activity and each subject
# should get 30*6 = 180 rows
tidy <- aggregate(. ~ subject + activity, data = extracted, mean) 
tidy <- tidy[order(tidy$subject), ] # order by subjects

# Alternative method: dcast & ddply

###############################################################################
############################# END OF FILE #####################################