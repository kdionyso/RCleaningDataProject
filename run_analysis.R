
# mergedsets:: Function that reads the data in X_test.txt and X_train.txt files
#              and merges the data. Additionally, it also reads the features.txt file
#              and names the columns of the data.frame after the features.

readmergesets<- function(){
     require("stringr")
     # Read 
     features <<- read.table("features.txt")[,2]
     
     test <- read.table("test/X_test.txt", 
                             col.names = features)
     train <- read.table("train/X_train.txt", 
                              col.names = features)
     
     mergedds <<- rbind(test,train)
}

selectmeanstd<- function(ds){
     
     # find which elemnts correspond to the mean and standard deviation
     idxmean<<-grepl("mean[^meanFreq]",names(ds))
     idxstd<<-grepl("std",names(ds))
     mergedmean<-ds[,idxmean]
     mergedstd<-ds[,idxstd]
     selectds<-cbind(mergedmean,mergedstd)
     selectds
}

cleanfeaturesdata<-function(idxmean,idxstd,ds,features){
     cleanfeatures <- sapply(features, 
                             function(x) str_replace_all(x, "\\(\\)|-",""))
     cleanfeatureslabelmean<-cleanfeatures[idxmean]
     cleanfeatureslabeldstd<-cleanfeatures[idxstd]
     cleanfeatureslabel<-cbind(cleanfeatureslabelmean,cleanfeatureslabeldstd)
     tidyds<-ds
     names(tidyds)<-cleanfeatureslabel
     tidyds
}

activitylabels<-function(){
     activitynames<- read.table("activity_labels.txt")
     activitylist<<-activitynames[,2]
}

mergeddesc<-function(){
     # read and store the activity labels
     activitylabels()
     # read and store the test and train datasets
     readmergesets()
     
     # select the mean and standard deviation features from datasets test 
     # and train and store indexes corresponding to the appropriate columns.
     selectds<-selectmeanstd(mergedds)
     cleands<-cleanfeaturesdata(idxmean,idxstd,selectds,features)
     
     # read test and train activity datasets     
     labelactivitytest<- read.table("test/y_test.txt")
     labelactivitytrain<- read.table("train/y_train.txt")

     # merge test and train subject datasets after cleaning up column description
     mergedlabels <- rbind(labelactivitytest,labelactivitytrain)
     activity <<- data.frame(as.vector(lapply(mergedlabels,
                                             function(x) activitylist[x])))
     names(activity)<<-c("activity")
     
     # read test and train subject datasets
     labelsubjecttest<- read.table("test/subject_test.txt")
     labelsubjecttrain<- read.table("train/subject_train.txt")
     
     # merge test and train subject datasets
     mergedsubject <- rbind(labelsubjecttest,labelsubjecttrain)
     subject <<- data.frame(mergedsubject)
     names(subject)<<-c("subject")
     
     # create clean datatable with all information about activity, subject and 
     # requested featurese
     datatable<<-cbind(activity,subject,cleands)
     cbind(activity,subject,cleands)
}
indexds<-function(dt,activity,subject){
     # index by activity and remove activity column from indexbyactivity var
     indexbyactivity<-split(dt[2:ncol(dt)], activity)
     
     # group by subject and remove subject column from indexbyactivityandsubject 
     # variable
     indexbyactivityandsubject<-lapply(indexbyactivity, 
                                  function(x) { split(x[2:ncol(x)], x$subject) }) 
     # find the mean/average 
     tidydatatable2<-lapply(indexbyactivityandsubject , 
                        function(x) { lapply(x, function(y) sapply(y, mean) )} )
   
     # update names of new datatable
     cleanfeaturesaverage <- sapply(names(dt)[3:ncol(dt)], 
                             function(x) paste0("AVERAGE_",
                                               str_replace_all(x, "\\(\\)|-","")))
     lapply(tidydatatable2, function(x) lapply(x,
                         function(y) names(y)<-cleanfeaturesaverage))
     write.table(tidydatatable2, 'tidydataset.txt',row.name=FALSE)
     tidydatatable2
}
