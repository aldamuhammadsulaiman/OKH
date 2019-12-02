library(tibble)
source("D:/base.R")

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

#largest weighted degree first heuristic
sortLargestW <- function(testt,test) {
  testt$sum<-sapply(testt,sum)
  testt$weight<-sapply(test,sum)
  test$sum<-testt$sum
  test$weight<-testt$weight
  test<-rownames_to_column(test,"rname")
  test<-test[order(-test[,ncol(test)-1],-test[,ncol(test)]),]
  rownames(test)<-test$rname
  test<-test[,-1]
  test<-test[,-ncol(test)]
  test<-test[,-ncol(test)]
  testt<-testt[,-ncol(testt)]
  testt<-testt[,-ncol(testt)]
  test[nrow(test)+1,]<-sapply(test,sum)
  test[nrow(test)+1,]<-sapply(testt,sum)
  test<-test[,order(-test[nrow(test),],-test[nrow(test)-1,])]
  test<-test[-nrow(test),]
  test<-test[-nrow(test),]
  return (test)
}

createTimeslot <- function(testt,test) {
  nc2<-ncol(test)
  timeslotlist<-list()
  timeslotlist[[1]]<-list()
  timeslotlist[[1]]<-append(timeslotlist[[1]],as.integer(rownames(test[1,])))
  test<-test[-1,]
  
  while(nrow(test)>0){
    most_str<-getLeastSaturated(timeslotlist,test)
    if(most_str$tsllist[[1]]>0)  {
      timeslotlist[[as.integer(most_str$tsllist[[1]])]]<-append( timeslotlist[[as.integer(most_str$tsllist[[1]])]],most_str$exam)
    } else {
      timeslotlist[[length(timeslotlist)+1]]<-list()
      timeslotlist[[length(timeslotlist)]][[1]]<-most_str$exam
    }
    test<-test[!row.names(test)==paste(most_str$exam),]
  }
  
  # for (i in 2:nc2) {
  #   j<-length(timeslotlist)
  #   for (k in 1:j){
  #     p<-0
  #     l<-length(timeslotlist[[k]])
  #     for (m in 1:l)
  #       p<-pmax(p,testt[timeslotlist[[k]][[m]],i])
  #     if(p==0) {
  #       timeslotlist[[k]]<-append(timeslotlist[[k]],rownames(testt[i,]))
  #       break;
  #     }
  #     else if (k==j) {
  #       timeslotlist[[length(timeslotlist)+1]]<-list()
  #       timeslotlist[[length(timeslotlist)]][[1]]<-rownames(testt[i,])
  #     }
  #   }
  # }
  return (timeslotlist)  
}
#getLeastSaturated(timeslotlist,test)
getLeastSaturated <- function (tslot,exam) {
  str_table<<-data.frame(exam=numeric(0),TimeslotConf=integer(0),ExamConf=integer(0),EnrollConf=integer(0))
  str_table$tsllist<-list()
  for (i in 1:nrow(exam)) {
    tslconf<-0
    tot.exconf<-0
    enrconf<-0
    tsllist<-list()
    for (j in 1:length(tslot)) {
      exconf<-0
      for (k in 1:length(tslot[[j]])) {
        enrcheck<-exam[i,paste("X",tslot[[j]][[k]],sep = "")]
        if (enrcheck>0) {
          enrconf<-enrconf+enrcheck
          exconf<-exconf+1
        }
      }
      if (exconf>0) {
        tslconf<-tslconf+1
        tot.exconf<-tot.exconf+exconf
      }
      else
        tsllist<-append(tsllist,j)
    }
    str_table[nrow(str_table)+1,]<-c(as.integer(row.names(exam[i,])),tslconf,tot.exconf,enrconf,ifelse(length(tsllist)==0,0,tsllist))
    str_table<-str_table[order(-str_table[,2],-str_table[,3],-str_table[,4]),]
  }
  return(str_table[1,])
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
  test<-sortLargestW(testt,test)
  timeslotlist<<-createTimeslot(testt,test)
  print(Sys.time()-starttime)
  print(calculatePen(timeslotlist,test))
  export(timeslotlist,filename)
}

