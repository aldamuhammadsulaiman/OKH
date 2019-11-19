library(tibble)
source("D:/alda/okh_3_26_PM_11_19_2019/base.R")

#largest degree first heuristic
sortLargest <- function(testt) {
  testt$sum<-sapply(testt,sum)
  testt<-rownames_to_column(testt,"rname")
  testt<-testt %>% arrange(desc(sum))
  testt<-column_to_rownames(testt,"rname")
  testt<-testt[,-ncol(testt)]
  testt[nrow(testt)+1,]<-sapply(testt,sum)
  testt<-testt[,order(-testt[nrow(testt),])]
  testt<-testt[-nrow(testt),]
  return (testt)
}

createTimeslot <- function(testt) {
  nc2<-ncol(testt)
  timeslotlist<-list()
  timeslotlist[[1]]<-list()
  timeslotlist[[1]]<-append(timeslotlist[[1]],rownames(testt[1,]))
  
  for (i in 2:nc2) {
    j<-length(timeslotlist)
    for (k in 1:j){
      p<-0
      l<-length(timeslotlist[[k]])
      for (m in 1:l)
        p<-pmax(p,testt[timeslotlist[[k]][[m]],i])
      if(p==0) {
        timeslotlist[[k]]<-append(timeslotlist[[k]],rownames(testt[i,]))
        break;
      }
      else if (k==j) {
        timeslotlist[[length(timeslotlist)+1]]<-list()
        timeslotlist[[length(timeslotlist)]][[1]]<-rownames(testt[i,])
      }
    }
  }
  return (timeslotlist)  
}

calculatePen<-function (timeslotlist,test) {
  cost<-0
  for (i in 1:length(timeslotlist)) {
    if(length(timeslotlist[[i]])>1) {
      for (j in 1:(length(timeslotlist[[i]])-1)) {
        l<-j+1
        for (k in l:length(timeslotlist[[i]])) {
          if(test[as.integer(timeslotlist[[i]][[j]]),as.integer(timeslotlist[[i]][[k]])]>0) {
            return(1000)
          }
        }
      }
    }
  }
  for (i in 1:(length(timeslotlist)-1)) {
    m<-i+1
    n<-m+4
    for (j in 1:length(timeslotlist[[i]])) {
      for (l in m:n) {
        if (l<(length(timeslotlist)+1)) {
          for (k in 1:length(timeslotlist[[l]])) {
            cost<-cost+(test[as.integer(timeslotlist[[i]][[j]]),as.integer(timeslotlist[[l]][[k]])]*2^(5-(l-i)))/nrow(data)
          }
        }
        else
          break;
      }
    }
  }
  return (cost)
}

runAll <- function(data,filename) {
  starttime<-Sys.time()
  test<<-createCM(data)
  export_cm(test,filename)
  testt<-createBinaryCM(test)
  testt<-sortLargest(testt)
  timeslotlist<<-createTimeslot(testt)
  print(Sys.time()-starttime)
  print(calculatePen(timeslotlist,test))
  export(timeslotlist,filename)
}

