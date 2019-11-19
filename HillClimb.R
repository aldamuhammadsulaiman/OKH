source("D:/alda/okh_3_26_PM_11_19_2019/base.R")
source("D:/alda/okh_3_26_PM_11_19_2019/heuristic.R")

hcPrepare <- function(fname) {
  sol<-read.csv(fname,sep=" ",header=FALSE)
  sol$V2<-sol$V2+1
  dlist<-list()
  for (i in 1:length(unique(sol$V2))) {
    dlist[[i]]<-list()
    for (j in 1:length(sol[sol$V2==i,2])) {
      dlist[[i]]<-append(dlist[[i]],sol[sol$V2==i,][j,1])
    }
  }
  countCourse(dlist)
  return(dlist)
}
listIndex <- function (x) {
  index_tab<-data.frame(matrix(nrow=nc,ncol=2))
  for (i in 1:length(x)) {
    for (j in 1:length(x[[i]])){
      index_tab[as.integer(x[[i]][[j]]),1]<-i
      index_tab[as.integer(x[[i]][[j]]),2]<-j
    }
  }
  return(index_tab)
}
stochastic_hillclimb <- function(x,index_tab,cm,maxIter) {
  current_sol<-x
  current_sol_pen<-calculatePen(current_sol,cm)
  starttime<-Sys.time()
  for (i in 1:maxIter) {
    y<-x
    rnd<-round(runif(1,1,nrow(index_tab)))
    rnd_timeslot<-index_tab[rnd,1]
    while (rnd_timeslot==index_tab[rnd,1])
      rnd_timeslot<-round(runif(1,1,length(unique(index_tab$X1))))
    init_ts<-index_tab[rnd,1]
    x[[index_tab[rnd,1]]][[index_tab[rnd,2]]]<-NULL
    x[[rnd_timeslot]]<-append(x[[rnd_timeslot]],rnd)
    preswap_pen<-delta<-calculatePen_swap(y,cm,rnd_timeslot,init_ts,1)
    postswap_pen<-calculatePen_swap(x,cm,rnd_timeslot,init_ts,0)
    candidate_sol_pen<-current_sol_pen+(postswap_pen-preswap_pen)
    if (current_sol_pen>candidate_sol_pen){
      current_sol<-x
      current_sol_pen<-candidate_sol_pen
      index_tab<-listIndex(current_sol)
    }
    else
      x<-current_sol
  }
  print(Sys.time()-starttime)
  print(paste("Penalty for final solution after ",maxIter," iterations: ",current_sol_pen,sep = ""))
  return(current_sol)
}
# calculatePen_swap<-function (timeslotlist,test,x) {
#   cost<-0
#   if(length(timeslotlist[[x]]>1)) {
#     for (j in 1:(length(timeslotlist[[x]])-1)) {
#       l<-j+1
#       for (k in l:length(timeslotlist[[x]])) {
#         if (test[as.integer(timeslotlist[[x]][[j]]),as.integer(timeslotlist[[x]][[k]])]>0) {
#           return (1000)
#         }
#       }
#     }
#   }
#   for (i in 1:(length(timeslotlist)-1)) {
#     m<-i+1
#     n<-m+4
#     for (j in 1:length(timeslotlist[[i]])) {
#       for (l in m:n) {
#         if (l<(length(timeslotlist)+1)) {
#           for (k in 1:length(timeslotlist[[l]])) {
#             cost<-cost+(test[as.integer(timeslotlist[[i]][[j]]),as.integer(timeslotlist[[l]][[k]])]*2^(5-(l-i)))/nrow(data)
#           }
#         }
#         else
#           break;
#       }
#     }
#   }
#   return (cost)
# }

getCM <- function(fname) {
  filename_noext<-sub("\\..*","",fname)
  cmat<-read.csv(paste(filename_noext,".cm",sep = ""),sep=",",header=TRUE)
  return(cmat)
  
}



####
calculatePen_swap<-function (timeslotlist,test,x,init_ts,bypass_hardcheck) {
  cost<-0
  if(bypass_hardcheck==0) {
    if(length(timeslotlist[[x]]>1)) {
      for (j in 1:(length(timeslotlist[[x]])-1)) {
        l<-j+1
        for (k in l:length(timeslotlist[[x]])) {
          if (test[as.integer(timeslotlist[[x]][[j]]),as.integer(timeslotlist[[x]][[k]])]>0) {
            return (1000)
          }
        }
      }
    }
  }
  delta_cost<-0
  for (i in (init_ts-5):(init_ts+5)) {
    if(i<1)
      next
    else if(i<length(timeslotlist)) {
      if(i<init_ts | i>init_ts) {
        for (j in 1:length(timeslotlist[[i]])) {
          for (k in 1:length(timeslotlist[[init_ts]])) {
            delta_cost<-delta_cost+(test[as.integer(timeslotlist[[i]][[j]]),as.integer(timeslotlist[[init_ts]][[k]])]*2^(5-abs(init_ts-i)))/nrow(data)
          } 
        }
      }
      else
        next
    }
  }
  for (i in (x-5):(x+5)) {
    if(i<1)
      next
    else if(i<length(timeslotlist)) {
      if(i<x | i>x) {
        for (j in 1:(length(timeslotlist[[i]]))) {
          for (k in 1:length(timeslotlist[[x]])) {
            delta_cost<-delta_cost+(test[as.integer(timeslotlist[[i]][[j]]),as.integer(timeslotlist[[x]][[k]])]*2^(5-abs(x-i)))/nrow(data)
          }
        }
      }
      else
        next
    }
  }
  return (delta_cost)
}
