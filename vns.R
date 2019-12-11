vns<- function(sol,cm,stud,max_iter) {
  k<-1
  start<-Sys.time()
  pen<-calculatePen(hcPrepare(sol),cm)
  iteration_pen<-matrix(0,nrow=max_iter,ncol=1)
  for (iter in 1:max_iter) {
    if(k==1) {
      rand_exam<-round(runif(1,1,nrow(sol)),0)
      sel_ts<-sol[sol[,1]==rand_exam,2]
      conf_ts<-getNeighboringTS(sol,rand_exam,cm)
      if(length(conf_ts)==0) {
        k<-k+1
        iteration_pen[iter]<-pen
        next
      }
      rand_ts<-conf_ts[round(runif(1,1,length(conf_ts)),0)]
      sel_ts_member<-sol[sol[,2]==sel_ts,1]
      rand_ts_member<-sol[sol[,2]==rand_ts,1]
      kempe_chain<-findKempeChain(sel_ts_member,rand_ts_member,cm)
      if(length(kempe_chain)>0) {
        deltapen<-calculatePen_kswap_vns(sol,cm,sel_ts,rand_ts,kempe_chain,stud)
        if(deltapen<0) {
          sol<-chainSwap(sol,kempe_chain,sel_ts,rand_ts)
          k<-1
          pen<-pen+deltapen
        }
      }
      else
        k<-k+1
    }
    else if(k==2) {
      move_candidate<-sol[sol[,2]==sel_ts,1]
      move_candidate<-move_candidate[round(runif(1,1,length(move_candidate)),0)]
      best_delta<-0
      moveto<-sol[sol[,1]==move_candidate,2]
      for (i in 1:length(unique(sol[,2]))) {
        if(i==sel_ts) { next }
        curr_delta<-calculatePen_move(sol,cm,sel_ts,i,move_candidate,stud)
        if (curr_delta<best_delta) {
          best_delta<-curr_delta
          moveto<-i
        }
      }
      pen<-pen+best_delta
      sol[sol[,1]==move_candidate,2]<-moveto
      k<-1
    }
    iteration_pen[iter]<-pen
  }
  plot(iteration_pen, type = "l")
  print(Sys.time()-start)
  return(sol)
}

getNeighboringTS <- function (sol, rand_exam, cm) {
  crashing_ts<-numeric(0)
  crashing_exam<-cm[rand_exam,]
  crashing_exam<-as.integer(substr(names(crashing_exam[which(crashing_exam>0)]),2,5))
  if(length(crashing_exam)==0) 
    return(numeric(0))
  for (i in 1:length(crashing_exam)) {
    crashing_ts[i]<-sol[sol[,1]==crashing_exam[i],2]
  }
  return(unique(crashing_ts))
}

findKempeChain <- function (sel_ts_member, rand_ts_member, cm) {
  kempechain<-data.frame(sel=numeric(0),rand=numeric(0))
  for (i in 1:length(sel_ts_member)) {
    for (j in 1:length(rand_ts_member)) {
      if (cm[sel_ts_member[i],rand_ts_member[j]]>0) {
        kempechain<-rbind(kempechain,c(sel_ts_member[i],rand_ts_member[j]))
      }
    }
  }
  return(data.matrix(kempechain))
}

chainSwap <- function (sol,kempe_chain,sel_ts,rand_ts) {
  kempe1<-unique(kempe_chain[,1])
  kempe2<-unique(kempe_chain[,2])
  for (i in 1:length(kempe1))
    sol[sol[,1]==kempe1[i],2]<-rand_ts
  for (i in 1:length(kempe2))
    sol[sol[,1]==kempe2[i],2]<-sel_ts
  return(sol)
}

calculatePen_kswap_vns <- function(sol,cm,init,target,kempe_chain,stud) {
  check_ts<-list(init,target)
  kempe_list<-list(unique(kempe_chain[,1]),unique(kempe_chain[,2]))
  delta<-0
  temp_delta<-0
  for (l in 1:2) {
    if (l==2) { 
     sw<-check_ts[[1]]
     check_ts[[1]]<-check_ts[[2]]
     check_ts[[2]]<-sw
     }
    for (k in 1:length(kempe_list[[l]])) {
      temp_delta<-0
      for (i in 1:2) {
        if (i==2) {
          sol[sol[,1]==kempe_list[[l]][[k]],2]<-check_ts[[i]]
          temp_delta<-(-temp_delta)
        }
        neighbor<-sol[(sol[,2]<check_ts[[i]] & sol[,2]>(check_ts[[i]]-6)) | (sol[,2]>check_ts[[i]] & sol[,2]<(check_ts[[i]]+6)) ,]
        for (j in 1:nrow(neighbor))
          temp_delta<-temp_delta+(cm[sol[sol[,1]==kempe_list[[l]][[k]],1],neighbor[j,1]]*2^(5-abs(check_ts[[i]]-neighbor[j,2])))
      }
      delta<-delta+temp_delta
    }
  }
  return (delta/stud)
}
calculatePen_move_vns <- function(sol,cm,init,target,exam,stud) {
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
# calculatePen_swap_vns <- function(sol,cm,init,target,exam,targ_exam,stud) {
#   check_ts<-list(init,target)
#   exm<-list(exam,targ_exam)
#   delta<-0
#   tot_delta<-0
#   for (k in 1:2) {
#     if (k==2) {
#       sw<-check_ts[[1]]
#       check_ts[[1]]<-check_ts[[2]]
#       check_ts[[2]]<-sw
#       delta<-0
#     }
#     for (i in 1:2) {
#       if (i==2) {
#         sol[sol[,1]==exm[[k]],2]<-target
#         delta<-(-delta)
#       }
#       neighbor<-sol[(sol[,2]<check_ts[[i]] & sol[,2]>(check_ts[[i]]-6)) | (sol[,2]>check_ts[[i]] & sol[,2]<(check_ts[[i]]+6)) ,]
#       for (j in 1:nrow(neighbor))
#         delta<-delta+(cm[sol[sol[,1]==exm[[k]],1],neighbor[j,1]]*2^(5-abs(check_ts[[i]]-neighbor[j,2])))
#     }
#     tot_delta<-tot_delta+delta
#   }
#   return (delta/stud)
# }

vns_2<- function(sol,cm,stud_amount,max_iter) {
  k<-1
  start<-Sys.time()
  pen<-calculatePen(hcPrepare(sol),cm)
  iteration_pen<-matrix(10,nrow=max_iter,ncol=2)
  for (i in 1:max_iter) {
    rand_exam<-round(runif(1,1,nrow(sol)),0)
    if(k<4) {
      if(k==1) {
        curr_ts<-sol[sol[,1]==rand_exam,2]
      }
      else if(k==2) {
        rand_exam2<-cm[rand_exam,]
        rand_exam2<-rand_exam2[which(rand_exam2==0)]
        rand_exam2<-rand_exam2[round(runif(1,1,length(rand_exam2)),0)]
        rand_exam2<-as.integer(substr(names(rand_exam2),2,5))
        rand_exam<-c(rand_exam,rand_exam2)
        curr_ts<-c(sol[sol[,1]==rand_exam[1],2],
                   sol[sol[,1]==rand_exam[2],2])
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
        curr_ts<-c(sol[sol[,1]==rand_exam[1],2],
                   sol[sol[,1]==rand_exam[2],2],
                   sol[sol[,1]==rand_exam[3],2])
      }
      rand_ts<-getPossibleMove(sol,cm,rand_exam)
      if(length(rand_ts)>0) {
        rand_ts<-rand_ts[round(runif(1,1,length(rand_ts)),0)]
      } else {
        iteration_pen[i,1]<-0
        k+1
        next
      }
      deltapen<-calculatePen_move_sa(sol,cm,curr_ts,rand_ts,rand_exam,stud_amount)
    }
    else {
      swap_cand<-round(runif(1,1,length(unique(sol[,2]))),0)
      swap_cand<-sol[sol[,2]==swap_cand,1]
      swap_cand<-swap_cand[round(runif(1,1,length(swap_cand)),0)]
      if (k==4) { rand_exam<-c(rand_exam,swap_cand) }
      else {
        swap_cand2<-round(runif(1,1,length(unique(sol[,2]))),0)
        swap_cand2<-sol[sol[,2]==swap_cand2,1]
        swap_cand2<-swap_cand2[round(runif(1,1,length(swap_cand2)),0)]
        rand_exam<-c(rand_exam,swap_cand,swap_cand2)
      }
      deltapen<-calculatePen_swap_sa(sol,cm,rand_exam,stud_amount)
    }
    if(deltapen<0) {
      iteration_pen[i,1]<-deltapen
      if (k<4) {
        for (i in 1:length(rand_exam))
          sol[sol[,1]==rand_exam[i],2]<-rand_ts
      }
      else {
        saved<-sol[sol[,1]==rand_exam[1],2]
        if(length(rand_exam)==2) {
          sol[sol[,1]==rand_exam[1],2]<-sol[sol[,1]==rand_exam[2],2]
          sol[sol[,1]==rand_exam[2],2]<-saved
        } else {
          sol[sol[,1]==rand_exam[1],2]<-sol[sol[,1]==rand_exam[2],2]
          sol[sol[,1]==rand_exam[2],2]<-sol[sol[,1]==rand_exam[3],2]
          sol[sol[,1]==rand_exam[3],2]<-saved
        }
      }
      k<-1
    }
    else {
      iteration_pen[i,1]<-0
      if (k==5)
        k<-1
      else
        k<-k+1
    }
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
  
  return (sol)
}
