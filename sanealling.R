sim_annealing <- function(current_sol,cm,temp,stud_amount,max_iter) {
  start<-Sys.time()
  coolingRate<-0.01
  iteration_pen<-matrix(10,nrow=max_iter,ncol=2)
  pen<-calculatePen(hcPrepare(current_sol),cm)
  for (i in 1:max_iter) {
    k<-round(runif(1,1,5),0)
    rand_exam<-round(runif(1,1,nrow(current_sol)),0)
    if(k<4) {
      if(k==1) {
        curr_ts<-current_sol[current_sol[,1]==rand_exam,2]
        }
      else if(k==2) {
        rand_exam2<-cm[rand_exam,]
        rand_exam2<-rand_exam2[which(rand_exam2==0)]
        rand_exam2<-rand_exam2[round(runif(1,1,length(rand_exam2)),0)]
        rand_exam2<-as.integer(substr(names(rand_exam2),2,5))
        rand_exam<-c(rand_exam,rand_exam2)
        curr_ts<-c(current_sol[current_sol[,1]==rand_exam[1],2],
                   current_sol[current_sol[,1]==rand_exam[2],2])
      }
      else if(k==3) {
        rand_exam2<-cm[rand_exam,]
        rand_exam2<-rand_exam2[which(rand_exam2==0)]
        rand_exam2<-rand_exam2[round(runif(1,1,length(rand_exam2)),0)]
        rand_exam2<-as.integer(substr(names(rand_exam2),2,5))
        rand_exam3<-cm[c(rand_exam,rand_exam2),]
        rand_exam3<-colSums(rand_exam3)
        rand_exam3<-rand_exam3[which(rand_exam3==0)]
        rand_exam3<-rand_exam3[round(runif(1,1,length(rand_exam3)),0)]
        rand_exam3<-as.integer(substr(names(rand_exam3),2,5))
        rand_exam<-c(rand_exam,rand_exam2,rand_exam3)
        curr_ts<-c(current_sol[current_sol[,1]==rand_exam[1],2],
                   current_sol[current_sol[,1]==rand_exam[2],2],
                   current_sol[current_sol[,1]==rand_exam[3],2])
      }
      rand_ts<-getPossibleMove(current_sol,cm,rand_exam)
      if(length(rand_ts)>0) {
        rand_ts<-rand_ts[round(runif(1,1,length(rand_ts)),0)]
      } 
      else {
        iteration_pen[i,1]<-0
        next
      }
      deltapen<-calculatePen_move_sa(current_sol,cm,curr_ts,rand_ts,rand_exam,stud_amount)
    }
    else {
      swap_cand<-round(runif(1,1,length(unique(current_sol[,2]))),0)
      swap_cand<-current_sol[current_sol[,2]==swap_cand,1]
      swap_cand<-swap_cand[round(runif(1,1,length(swap_cand)),0)]
      if (k==4) { rand_exam<-c(rand_exam,swap_cand) }
      else {
        swap_cand2<-round(runif(1,1,length(unique(current_sol[,2]))),0)
        swap_cand2<-current_sol[current_sol[,2]==swap_cand2,1]
        swap_cand2<-swap_cand2[round(runif(1,1,length(swap_cand2)),0)]
        rand_exam<-c(rand_exam,swap_cand,swap_cand2)
      }
      deltapen<-calculatePen_swap_sa(current_sol,cm,rand_exam,stud_amount)
    }
    if(deltapen<0 | (deltapen!=100 & exp(-deltapen/temp)>runif(1,0,1))) {
      iteration_pen[i,1]<-deltapen
      if (k<4) {
        for (i in 1:length(rand_exam))
          current_sol[current_sol[,1]==rand_exam[i],2]<-rand_ts
      }
      else {
        saved<-current_sol[current_sol[,1]==rand_exam[1],2]
        if(length(rand_exam)==2) {
          current_sol[current_sol[,1]==rand_exam[1],2]<-current_sol[current_sol[,1]==rand_exam[2],2]
          current_sol[current_sol[,1]==rand_exam[2],2]<-saved
        } else {
          current_sol[current_sol[,1]==rand_exam[1],2]<-current_sol[current_sol[,1]==rand_exam[2],2]
          current_sol[current_sol[,1]==rand_exam[2],2]<-current_sol[current_sol[,1]==rand_exam[3],2]
          current_sol[current_sol[,1]==rand_exam[3],2]<-saved
        }
      }
    }
    else
      iteration_pen[i,1]<-0
    temp<-temp*(1-coolingRate)
  }
  print(Sys.time()-start)
  for (i in 1:nrow(iteration_pen)) {
    if(i==1) {
      iteration_pen[i,2]<-pen
      next
      }
    iteration_pen[i,2]<-iteration_pen[(i-1),2]+iteration_pen[i,1]
  }
  plot(iteration_pen[,2], type = "l")
  
  return (current_sol)
}
getPossibleMove <- function (sol,cm,exam) {
  cfmatrix<-matrix(0,nrow=length(exam)+1,ncol=length(unique(sol[,2])))
  for (j in 1:(nrow(cfmatrix)-1)) {
    for (i in 1:ncol(cfmatrix)) {
      if (i==sol[sol[,1]==exam[j],2]) {
        cfmatrix[j,i]<-1000
        next
        }
      ts_member<-sol[sol[,2]==i,1]
      for (k in 1:length(ts_member))
        cfmatrix[j,i]<-cfmatrix[j,i]+cm[exam[j],ts_member[k]]
    }
  }
  cfmatrix[nrow(cfmatrix),]<-colSums(cfmatrix)
  possible_move<-which(cfmatrix[nrow(cfmatrix),]==0)
  return(possible_move)
}

checkHardConstViol <- function(sol,cm,target,exam) {
  sol_target<-t(sol[sol[,2]==target,1])
  for(i in 1:length(sol_target)) {
    if(cm[sol_target[i],exam]>0) {
      return (TRUE)
    }
  }
  return (FALSE)
}
calculatePen_move_sa <- function(sol,cm,init,target,exam,stud) {
  check_ts<-list(init,target)
  tot_delta<-0
  for (k in 1:length(exam)) {
    delta<-0
    for (i in 1:2) {
      if (i==2) {
        sol[sol[,1]==exam[[k]],2]<-target
        delta<-(-delta)
        neighbor<-sol[(sol[,2]<check_ts[[i]] & sol[,2]>(check_ts[[i]]-6)) | (sol[,2]>check_ts[[i]] & sol[,2]<(check_ts[[i]]+6)) ,]
        for (j in 1:nrow(neighbor))
          delta<-delta+(cm[sol[sol[,1]==exam[[k]],1],neighbor[j,1]]*2^(5-abs(check_ts[[i]]-neighbor[j,2])))
      }
      else {
        neighbor<-sol[(sol[,2]<check_ts[[i]][[k]] & sol[,2]>(check_ts[[i]][[k]]-6)) | (sol[,2]>check_ts[[i]][[k]] & sol[,2]<(check_ts[[i]][[k]]+6)) ,]
        for (j in 1:nrow(neighbor))
          delta<-delta+(cm[sol[sol[,1]==exam[[k]],1],neighbor[j,1]]*2^(5-abs(check_ts[[i]][[k]]-neighbor[j,2])))
        
      }
    }
    tot_delta<-tot_delta+delta
  }
  return (tot_delta/stud)
}

calculatePen_swap_sa <- function(sol,cm,exam,stud) {
  exams<-c(exam,exam[1])
  for (i in 1:length(exam)) {
    target<-sol[sol[,2]==sol[sol[,1]==exams[i+1],2],1] 
    target<-target[target!=exams[i+1]]
    if(length(target)==0)
      next
    for (j in 1:length(target)) {
      if(cm[exam[i],target[j]]>0) {
        return(100)
      }
    }
  }
  check_ts<-list()
  for (i in 1:length(exam)) 
    check_ts<-append(check_ts,sol[sol[,1]==exam[[i]],2])
  check_ts<-append(check_ts,check_ts[[1]])
  tot_delta<-0
  for (k in 1:length(exam)) {
    delta<-0
    for (i in k:(k+1)) {
      if (i==k+1) {
        sol[sol[,1]==exam[[k]],2]<-check_ts[[i]]
        delta<-(-delta)
      }
      neighbor<-sol[(sol[,2]<check_ts[[i]] & sol[,2]>(check_ts[[i]]-6)) | (sol[,2]>check_ts[[i]] & sol[,2]<(check_ts[[i]]+6)) ,]
      for (j in 1:nrow(neighbor))
        delta<-delta+(cm[sol[sol[,1]==exam[[k]],1],neighbor[j,1]]*2^(5-abs(check_ts[[i]]-neighbor[j,2])))
    }
    tot_delta<-tot_delta+delta
  }
  return (tot_delta/stud)
}
  
sim_annealing2<- function(sol,cm,stud,max_iter,temp) {
  start<-Sys.time()
  coolingRate<-0.01
  pen<-calculatePen(hcPrepare(sol),cm)
  iteration_pen<-matrix(0,nrow=max_iter,ncol=1)
  for (iter in 1:max_iter) {
    k<-round(runif(1,1,2),0)
    rand_exam<-round(runif(1,1,nrow(sol)),0)
    sel_ts<-sol[sol[,1]==rand_exam,2]
    conf_ts<-getNeighboringTS(sol,rand_exam,cm)
    if(length(conf_ts)==0) {
      iteration_pen[iter]<-pen
      next
    }
    rand_ts<-conf_ts[round(runif(1,1,length(conf_ts)),0)]
    sel_ts_member<-sol[sol[,2]==sel_ts,1]
    rand_ts_member<-sol[sol[,2]==rand_ts,1]
    kempe_chain<-findKempeChain(sel_ts_member,rand_ts_member,cm)
    if(length(kempe_chain)>0) {
      deltapen<-calculatePen_kswap_vns(sol,cm,sel_ts,rand_ts,kempe_chain,stud)
      if(deltapen<0 | exp(-deltapen/temp)>runif(1,0,1)) {
        sol<-chainSwap(sol,kempe_chain,sel_ts,rand_ts)
        pen<-pen+deltapen
      }
    }
    iteration_pen[iter]<-pen
    temp<-temp*(1-coolingRate)
  }
  plot(iteration_pen, type = "l")
  print(Sys.time()-start)
  return(sol)
}

