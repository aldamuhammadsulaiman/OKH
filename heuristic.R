
library(tibble)
source("D:/okh_fix/base.R")

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
  summed<-test[,ncol(test)]
  test<-test[,-ncol(test)]
  testt<-testt[,-ncol(testt)]
  testt<-testt[,-ncol(testt)]
  test[nrow(test)+1,]<-sapply(test,sum)
  test[nrow(test)+1,]<-sapply(testt,sum)
  test<-test[,order(-test[nrow(test),],-test[nrow(test)-1,])]
  test<-test[-nrow(test),]
  test<-test[-nrow(test),]
  test[,ncol(test)+1]<-summed
  return (test)
}

createTimeslot <- function(test) {
  timeslotlist<-list()
  timeslotlist[[1]]<-list()
  timeslotlist[[1]]<-append(timeslotlist[[1]],as.integer(rownames(test[1,])))
  test<-test[-1,]
  
  while(nrow(test)>0){
    most_str<-getMostSaturated(timeslotlist,test)
    if(most_str[[2]]>0)  {
      timeslotlist[[as.integer(most_str[[2]])]]<-append( timeslotlist[[as.integer(most_str[[2]])]],most_str[[1]])
    } else {
      timeslotlist[[length(timeslotlist)+1]]<-list()
      timeslotlist[[length(timeslotlist)]][[1]]<-most_str[[1]]
    }
    test<-test[!row.names(test)==paste(most_str[[1]]),]
    # timeslotlist
  }
  return (timeslotlist)  
}
getMostSaturated <- function (tslot,exam) {
  satur_mat<-matrix(0,nrow=nrow(exam),ncol=length(tslot)+5)
  nc<-ncol(satur_mat)
  excol<-nc-4
  encol<-nc-3
  scol<-nc-2
  ccol<-nc-1
  for (i in 1:length(tslot)) {
    for (j in 1:length(tslot[[i]])) {
      xx<-exam[,paste("X",tslot[[i]][[j]],sep = "")]
      satur_mat[,excol]<-ifelse(xx>0,satur_mat[,i]+1,satur_mat[,i])
      satur_mat[,i]<-satur_mat[,i]+xx
    }
  }
  satur_mat[,scol]<-rowSums(satur_mat)
  satur_mat[,ccol]<-rowSums(satur_mat[,1:ifelse(nc==6,1,nc-5),drop=FALSE]>0)
  satur_mat[,nc]<-as.integer(row.names(exam))
  satur_mat[,encol]<-exam[paste(satur_mat[,nc]),ncol(exam)]
  p<-max(satur_mat[,ccol])
  if(grepl("kfu",filename))
    maxs<-satur_mat[order(-satur_mat[,ccol],-satur_mat[,scol],-satur_mat[,excol],-satur_mat[,encol]),]
  else
    maxs<-satur_mat[order(-satur_mat[,ccol],-satur_mat[,excol],-satur_mat[,scol],-satur_mat[,encol]),]
  
  if(is.null(ncol(maxs))) 
    maxs<-t(maxs)
  if (nc!=5 & p<(nc-5)) {
    return(list(maxs[1,nc],which(maxs[1,1:(nc-5)]==0)[[1]]))
  }
  return(list(maxs[1,nc],0))
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
  test<-createCM(data)
  cms<-test
  export_cm(test,filename)
  testt<-createBinaryCM(test)
  test<-sortLargestW(testt,test)
  timeslotlist<<-createTimeslot(test)
  print(Sys.time()-starttime)
  print(calculatePen(timeslotlist,cms))
  export(timeslotlist,filename)
}
