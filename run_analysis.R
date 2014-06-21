#Reading test data and then labeling it properly
  test_subject<-read.table("./UCI HAR Dataset/test/subject_test.txt",col.names="subject_no.")

  x_test<-read.table("./UCI HAR Dataset/test/X_test.txt")
  feature<-read.table("./UCI HAR Dataset/features.txt",row.names=1,colClasses="character")
  names(x_test)<-feature[,1]
  
  activity_label<-read.table("./UCI HAR Dataset/activity_labels.txt",colClasses=c("integer", "character"))
  
  y_test<-read.table("./UCI HAR Dataset/test/Y_test.txt",col.names="activity")
  for(i in seq_len(nrow(y_test))){
    num<-y_test[i,1]
    y_test[i,1]<-activity_label[num,2]
  }
  test_type<-data.frame(rep("test",nrow(y_test)))
  names(test_type)<-"type"
  test_data<-cbind(y_test,test_type,test_subject,x_test)

#Reading train data and then labeling it properly
  train_subject<-read.table("./UCI HAR Dataset/train/subject_train.txt",col.names="subject_no.")
  
  x_train<-read.table("./UCI HAR Dataset/train/X_train.txt")
  names(x_train)<-feature[,1]
  
  y_train<-read.table("./UCI HAR Dataset/train/Y_train.txt",col.names="activity")
  for(i in seq_len(nrow(y_train))){
    num<-y_train[i,1]
    y_train[i,1]<-activity_label[num,2]
  }
  
  train_type<-data.frame(rep("TRAIN",nrow(y_train)))
  names(train_type)<-"type"
  train_data<-cbind(y_train,train_type,train_subject,x_train)

#Merge test data with train data
  merge<-rbind(test_data,train_data)
  merge<-merge[order(merge$activity,merge$subject_no.),]

#Extract mean and std from the merged data
  library("reshape2")
  feature$check<-grepl(c('mean'),feature[,1])|grepl(c('std'),feature[,1])
  variable<-feature[feature$check,]
  data_melt<-melt(merge,id=c("activity","subject_no.","type"),measure.vars=variable[,1])

#create a tidy data set
  data_cast<-dcast(data_melt,activity + subject_no.+ type ~ variable,mean)

#export the tidy data set in .txt format
  write.table(data_cast,file="tidydata.txt",sep="\t", row.names = F)