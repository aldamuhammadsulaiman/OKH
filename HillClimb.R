source("D:/okh_fix/base.R")
source("D:/okh_fix/heuristic.R")
library(compiler)
enableJIT(3)
hcPrepare <- function(sol) {
  dlist<-list()
  for (i in 1:length(unique(sol[,2]))) {
    dlist[[i]]<-list()
    for (j in 1:length(sol[sol[,2]==i,2])) {
      if (length(sol[sol[,2]==i,2])==1) 
        dlist[[i]]<-append(dlist[[i]],sol[sol[,2]==i,][1])
      else
        dlist[[i]]<-append(dlist[[i]],sol[sol[,2]==i,][j,1])
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
# stochastic_hillclimb <- function(x,index_tab,cm,maxIter) {
#   current_sol<-x
#   current_sol_pen<-calculatePen(current_sol,cm)
#   starttime<-Sys.time()
#   for (i in 1:maxIter) {
#     rnd<-round(runif(1,1,nrow(index_tab)))
#     rnd_timeslot<-index_tab[rnd,1]
#     while (rnd_timeslot==index_tab[rnd,1])
#       rnd_timeslot<-round(runif(1,1,length(unique(index_tab$X1))))
#     x[[index_tab[rnd,1]]][[index_tab[rnd,2]]]<-NULL
#     x[[rnd_timeslot]]<-append(x[[rnd_timeslot]],rnd)
#     candidate_sol_pen<-calculatePen_move(x,cm,rnd_timeslot)
#     if (current_sol_pen>candidate_sol_pen){
#       # if (length(x[[index_tab[rnd,1]]])>((index_tab[rnd,2])-1)){
#       #   for (j in (index_tab[rnd,2]+1):length(x[[index_tab[[rnd,1]]]])) {
#       #     index_tab[]
#       #   }
#       # }
#       # index_tab[rnd,1]<-rnd_timeslot
#       # index_tab[rnd,2]<-length(x[[rnd_timeslot]])
#       current_sol<-x
#       current_sol_pen<-candidate_sol_pen
#       index_tab<-listIndex(current_sol)
#     }
#     else
#       x<-current_sol
#   }
#   print(Sys.time()-starttime)
#   print(paste("Penalty for final solution after ",maxIter," iterations: ",current_sol_pen,sep = ""))
#   return(current_sol)
# }
# calculatePen_move<-function (timeslotlist,test,x) {
#   cost<-0
#   tsxlength<-length(timeslotlist[[x]])
#   if(tsxlength>1) {
#     for (j in 1:(tsxlength-1)) {
#       l<-j+1
#       for (k in l:tsxlength) {
#         if (test[timeslotlist[[x]][[j]],timeslotlist[[x]][[k]]]>0) {
#           return (1000)
#         }
#       }
#     }
#   }
#   tslength<-length(timeslotlist)
#   for (i in 1:(tslength-1)) {
#     m<-i+1
#     n<-m+4
#     for (j in 1:length(timeslotlist[[i]])) {
#       for (l in m:n) {
#         if (l<(tslength+1)) {
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
# 
getCM <- function(fname) {
   filename_noext<-sub("\\..*","",fname)
   cmat<-read.csv(paste(filename_noext,".cm",sep = ""),sep=",",header=TRUE)
   cmat<-data.matrix(cmat)
   return(cmat)   
}
# 
# 
# 

############

stochastic_hillclimb <- function(current_sol,cm,maxiter,stud_amount) {
  start<-Sys.time()
  iteration_pen<-matrix(0,nrow=maxiter,ncol=1)
  pen<-calculatePen(hcPrepare(current_sol),cm)
  for (i in 1:maxiter) {
    rand_exam<-round(runif(1,1,nrow(current_sol)),0)
    curr_ts<-current_sol[current_sol[,1]==rand_exam,2]
    rand_ts<-curr_ts
    lnt<-length(unique(current_sol[,2]))
    while (rand_ts==curr_ts)
      rand_ts<-round(runif(1,1,lnt))
    iter_pen<-calculatePen_move(current_sol,cm,current_sol[current_sol[,1]==rand_exam,2],rand_ts,rand_exam,stud_amount)
    if(iter_pen<0) {
      current_sol[current_sol[,1]==rand_exam,2]<-rand_ts
      pen<-pen+iter_pen
    }
    iteration_pen[i]<-pen
  }
  plot(iteration_pen, type = "l")
  print(Sys.time()-start)
  return (current_sol)
}

calculatePen_move <- function(sol,cm,init,target,exam,stud) {
  sol_target<-sol[sol[,2]==target,1]
  for(i in 1:length(sol_target)) {
    if(cm[sol_target[i],exam]>0) {
      return (1)
    }
  }
  check_ts<-list(init,target)
  delta<-0
  for (i in 1:2) {
    if (i==2) {
      sol[sol[,1]==exam,2]<-target
      delta<-(-delta)
    }
    neighbor<-sol[(sol[,2]<check_ts[[i]] & sol[,2]>(check_ts[[i]]-6)) | (sol[,2]>check_ts[[i]] & sol[,2]<(check_ts[[i]]+6)) ,]
    for (j in 1:nrow(neighbor))
      delta<-delta+(cm[sol[sol[,1]==exam,1],neighbor[j,1]]*2^(5-abs(check_ts[[i]]-neighbor[j,2])))
  }
  return (delta/stud)
}
