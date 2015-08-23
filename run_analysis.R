
# mergedsets:: Function that reads the data in X_test.txt and X_train.txt files
#              and merges the data. Additionally, it also reads the features.txt file
#              and names the columns of the data.frame after the features.
mergedsets<- function(){
     require("stringr")
     # Read 
     features <- read.table("features.txt")[,2]
     
     cleanfeatureslabel <- sapply(features, 
                                      function(x) str_replace_all(x, "\\(\\)|-",""))
     
     test <- read.table("test/X_test.txt", 
                             col.names = cleanfeatureslabel)
     train <- read.table("train/X_train.txt", 
                              col.names = cleanfeatureslabel)
     
#     datatestsubject <- read.table("test/subject_test.txt")
#     datatrainsubject <- read.table("train/subject_train.txt")
     
#     names(datatestsubject)<-"SUBJECT"
#     names(datatrainsubject)<-"SUBJECT"
#     completedatatest <<- cbind(datatestsubject,datatest)
#     completedatatrain <<- cbind(datatrainsubject,datatrain)
     mergedds <- rbind(test,train)

# Find which elemnts correspond to the mean and standard deviation
     idxmean<-grepl("mean[^meanFreq]",names(mergedds))
     idxstd<-grepl("std",names(mergedds))
     mergedmean<-mergedds[,idxmean]
     mergedstd<-mergedds[,idxstd]
     tidyds<<-cbind(mergedmean,mergedstd)
     write.table(tidyds, 'tidydataset.txt')
}

activitylabels<-function(){
     activitynames<- read.table("activity_labels.txt")
     activity<<-activitynames[,2]
}
mergeddesc<-function(){
     activitylabels()
     
     labeltest<- read.table("test/y_test.txt")
     labeltrain<- read.table("train/y_train.txt")

     mergedlabels <- rbind(labeltest,labeltrain)
     mergeddescription <<- data.frame(as.vector(lapply(mergedlabels,
                                             function(x) activity[x])))
     names(mergeddescription)<<-c("ACTIVITY")
}


selct<-function(df){
     idx<-grepl("^-mean()",names(df))
     df[,idx]
}