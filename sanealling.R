sim_annealing <- function(current_sol,cm,temp,stud_amount) {
  start<-Sys.time()
  coolingRate<-0.001
  while (temp>1) {
    rand_exam<-round(runif(1,1,nrow(current_sol)),0)
    curr_ts<-current_sol[current_sol[,1]==rand_exam,2]
    rand_ts<-curr_ts+round(runif(1,-curr_ts+1,lnt-curr_ts))
    lnt<-length(unique(current_sol[,2]))
    while (checkHardConstViol(current_sol,cm,rand_ts,rand_exam)) {
      rand_ts<-curr_ts+round(runif(1,-curr_ts+1,lnt-curr_ts))
    }
    deltapen<-calculatePen_swap_sa(current_sol,cm,current_sol[current_sol[,1]==rand_exam,2],rand_ts,rand_exam,stud_amount)
    if(deltapen<0) {
      current_sol[current_sol[,1]==rand_exam,2]<-rand_ts
    }
    else if (exp(deltapen/temp)>runif(1,0,1)) {
      current_sol[current_sol[,1]==rand_exam,2]<-rand_ts
    }
    temp<-temp*(1-coolingRate)
  }
  print(temp)
  print(Sys.time()-start)
  return (current_sol)
}

checkHardConstViol <- function(sol,cm,target,exam) {
  sol_target<-sol[sol[,2]==target,1]
  for(i in 1:length(sol_target)) {
    if(cm[sol_target[i],exam]>0) {
      return (TRUE)
    }
  }
  return (FALSE)
}
calculatePen_swap_sa <- function(sol,cm,init,target,exam,stud) {
  check_ts<-list(init,target)
  delta<-0
  for (i in 1:2) {
    if (i==2) {
      sol[sol[,1]==exam,2]<-target
      delta<-(-delta)
    }
    neighbor<-sol[(sol[,2]<check_ts[[i]] & sol[,2]>(check_ts[[i]]-6)) | (sol[,2]>check_ts[[i]] & sol[,2]<(check_ts[[i]]+6)) ,]
    for (j in 1:nrow(neighbor))
      delta<-delta+(cm[sol[sol[,1]==exam,1],neighbor[j,1]]*2^5-abs(check_ts[[i]]-neighbor[j,2]))
  }
  return (delta/stud)
}

