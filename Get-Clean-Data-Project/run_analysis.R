## This function returns the tidy data set the project asked. 
getTidyData <- function(){
  m <- mergeDataSet()
  m <- extractDataSet(m)
  m <- renameVar(m)
  m <- getAvg(m)
  m
}

## This function is used to :
## - Merges the training and the test sets to create one data set.
## - Remove duplicated columns in the merged data set.
## - Labels the data set with descriptive variable names.
## It returns a dataset with several variables:
## - Column "ID" is defined for subject.
## - Column "Activity" is defined for activity label.
## - The rest Columns store the data read from X_test.txt and X_train.txt 
##   with depulicated columns removed.
mergeDataSet <- function(fPath1="./test/", fPath2="./train/", fPath3="./features.txt") {
  ##merge subject
  subject_test <- read.table(paste0(fPath1,"subject_test.txt"))
  subject_train <- read.table(paste0(fPath2,"subject_train.txt"))
  subject <- rbind(subject_test,subject_train)
  names(subject) <- "ID"
  
  features <- character()
  if (!file.exists(fPath3)) {
    stop("Please check the path of features.txt.")
  }else {
    features <- as.character(read.table(fPath3)[[2]],stringsAsFactors = FALSE)
  }
  
  ##merge X & drop duplicated column
  filter <- getDistinctCol(features)
  X_test<-read.table(paste0(fPath1,"X_test.txt"))
  X_train<-read.table(paste0(fPath2,"X_train.txt"))
  X <- rbind(X_test,X_train)
  X <- X[filter]
  names(X)<-features[filter]
  
  ##merge Y
  Y_test<-read.table(paste0(fPath1,"Y_test.txt"))
  Y_train<-read.table(paste0(fPath2,"Y_train.txt"))
  Y <- rbind(Y_test,Y_train)
  names(Y)<-"Activity"
  
  ##return merged dataset
  newDataSet <- cbind(ID=subject,Activity=Y,X)
  newDataSet
}

## This function is used to:
## - Extracts only the measurements on the mean and standard deviation 
##   for each measurement.
extractDataSet <- function(source = data.frame()){
  index_ID <- grep("ID",names(source),fixed=TRUE)
  index_Activity <-grep("Activity",names(source),fixed=TRUE)
  index_mean <- grep("mean",names(source),fixed=FALSE)
  index_std <-grep("std",names(source),fixed=FALSE)
  source[c(index_ID, index_Activity, index_mean, index_std)]
}

## This function is used to:
## -Use descriptive activity names to name the activities in the data set.
renameVar <- function(source = data.frame(), fPath="./activity_labels.txt"){
  activityLabels <- data.frame()
  if (!file.exists(fPath)){
    stop("Please check the path of activity_labels.txt.")
  }else {
    activityLabels <- read.table(fPath,stringsAsFactors = FALSE)
  }
  
  for (i in seq(nrow(activityLabels))){
    source$Activity[source$Activity==activityLabels[i,1]] <- activityLabels[i,2]
  }
  source
}

## This function is used to:
## - Creates a second, independent tidy data set with the average of each variable 
##   for each activity and each subject 
getAvg <- function(source = data.frame(),fPath1="./activity_labels.txt", fPath2="./features.txt"){
  subject <- c(1:30)
  activityLabels <- data.frame()
  if (!file.exists(fPath1)){
    stop("Please check the path of activity_labels.txt.")
  }else {
    activityLabels <- read.table(fPath1,stringsAsFactors = FALSE)
  }
  activityLabels<-as.character(activityLabels[[2]])
  features <- character()
  if (!file.exists(fPath2)) {
    stop("Please check the path of features.txt.")
  }else {
    features <- as.character(read.table(fPath2)[[2]],stringsAsFactors = FALSE)
  }
  
  avg_Sub_Act <- data.frame()
  for (i in seq(length(subject))){
    for (j in seq(length(activityLabels))){
      sub <- subset(source, ID == subject[i] & Activity == activityLabels[j],select=(3:(ncol(source))))
      if (nrow(sub)!=0){
        tmp <- data.frame(ID=subject[i],Activity=activityLabels[j],data.frame(lapply(sub,mean)))
        avg_Sub_Act <- rbind(avg_Sub_Act,tmp)
      }
    }
  }
  write.table(avg_Sub_Act, file = "./avg_Sub+Act.txt", row.names = FALSE)
  avg_Sub_Act
}

## This function is used to drop duplicated column.
getDistinctCol <- function(x = character()){
  if (length(x)<=1) return(TRUE)
  distinctRowFilter <- TRUE
  for (i in 2:length(x)){
    if (!is.na(match(x[i],x[1:(i-1)]))){
      distinctRowFilter <- c(distinctRowFilter,FALSE)
    }else {
      distinctRowFilter <- c(distinctRowFilter,TRUE)
    }
  }
  distinctRowFilter
}
