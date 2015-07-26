
data_analysis<-function() {
  library(dplyr)
  
  ## start load raw data
  #features names
  features<-read.table("C:/Courses/Data/Project/features.txt", stringsAsFactors = FALSE)
  #activities names
  activities<-read.table("C:/Courses/Data/Project/activity_labels.txt", stringsAsFactors = FALSE)
  #training label, training set and training subjects
  y_train<-read.table("C:/Courses/Data/Project/train/y_train.txt", stringsAsFactors = FALSE)
  x_train<-read.table("C:/Courses/Data/Project/train/x_train.txt", stringsAsFactors = FALSE)
  subject_train<-read.table("C:/Courses/Data/Project/train/subject_train.txt", stringsAsFactors = FALSE)
  #test labels, test set and test subject
  y_test<-read.table("C:/Courses/Data/Project/test/y_test.txt", stringsAsFactors = FALSE)
  x_test<-read.table("C:/Courses/Data/Project/test/x_test.txt", stringsAsFactors = FALSE)
  subject_test<-read.table("C:/Courses/Data/Project/test/subject_test.txt", stringsAsFactors = FALSE)
  ## end load raw data
  
  ##start tidying data
  #filter only interested data features
  sel_features<-features[grep("mean\\()|std\\()", features$V2),]  
  #select only columns corresponding with interested features (mean(), std())
  sel_x_train<-select(x_train, sel_features$V1)
  sel_x_test<-select(x_test, sel_features$V1)

  #merge data
  #binding training data (training subject, training label, selected training features)
  sel_train<-cbind(subject_train, y_train, sel_x_train)
  #binding test data (test subject, test label, selected test features)
  sel_test<-cbind(subject_test, y_test, sel_x_test)
  #binding train and test data
  sel_data<-rbind(sel_train, sel_test)
  #descriptive column names for data set
  names(sel_data)<-c("subject", "activity_id", sel_features$V2)
  #descriptive column names for activity
  names(activities)<-c("activity_id", "activity")
  data<-select(merge(activities, sel_data, by.x = "activity_id", by.y = "activity_id"), -activity_id)
  ## end tidying data

  ##data set with the average of each variable for each activity and each subject
  #this requires reshape2 package
  library(reshape2)
  #change data to long format
  ldata<-melt(data, id=c("activity", "subject"), na.rm=TRUE)
  #average of each vairable (feature) for each activity and each subject
  data2<-dcast(ldata, activity + subject ~ variable, mean)
  #writing data2 to text file
  write.table(data2, "human_activities.txt", row.name = FALSE)
  data2
}