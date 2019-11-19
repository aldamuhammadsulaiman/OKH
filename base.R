library(dplyr)

countCourse <- function(datlist) {
  nc<<-0
  for (i in 1:length(datlist)) {
    for (j in 1:length(datlist[[i]]))
      nc<<-pmax(nc,as.integer(datlist[[i]][[j]]))
  }
}
createCM <- function(data) {
  datlist<-list()
  for(i in 1:nrow(data)) {
    k<-strsplit(as.character(data[i,])," ")
    datlist[[i]]<-list()
    for(j in 1:length(k[[1]])){
      datlist[[i]]<-append(datlist[[i]],k[[1]][[j]])
    }
  }
  countCourse(datlist)
  test<-data.frame(matrix(ncol=nc,nrow=nc))
  test<-test %>% mutate_all(~replace(.,is.na(.),0))
  for (i in 1:length(datlist)) {
    for (j in 1:length(datlist[[i]])) {
      for (k in 1:length(datlist[[i]])) {
        if(datlist[[i]][[j]]!=datlist[[i]][[k]])
          test[as.integer(datlist[[i]][[j]]),as.integer(datlist[[i]][[k]])]<-test[as.integer(datlist[[i]][[j]]),as.integer(datlist[[i]][[k]])]+1
      }
    }
  }
  return (test)
}
createBinaryCM <- function (test) {
  testt<-test
  for (i in 1:nc) {
    for (j in 1:nc) {
      if(test[i,j]>0 || i==j)
        testt[i,j]<-1
    }
  }
  return (testt)
}

export <- function (timeslotlist, filename) {
  filename_noext<-sub("\\..*","",filename)
  if(file.exists(paste(filename_noext,".sol",sep = ""))) {
    rem<-file.remove(paste(filename_noext,".sol",sep = ""))
  }
  for (i in 1:length(timeslotlist)) {
    for (j in 1:length(timeslotlist[[i]])) 
      write(paste(timeslotlist[[i]][[j]],i-1),file=paste(filename_noext,".sol",sep = ""),sep="",append=TRUE)
  }
}

export_cm<- function (cm, filename) {
  filename_noext<-sub("\\..*","",filename)
  if(!file.exists(paste(filename_noext,".cm",sep = "")))
    write.csv(cm,file=paste(filename_noext,".cm",sep = ""), row.names = FALSE)
}
