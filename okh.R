source("D:/okh_fix/base.R")
source("D:/okh_fix/heuristic.R")
source("D:/okh_fix/HillClimb.R")
source("D:/okh_fix/sanealling.R")
source("D:/okh_fix/vns.R")
###################################################################
#construct initial solution
filename<-file.choose()
data<-read.csv(filename,header=FALSE)
runAll(data,filename)

#optimize solution w/ stochastic hill climbing method
# filename2<-file.choose()
#final_sol<-stochastic_hillclimb(data_sol,getCM(filename2),1000,nrow(data))
data_sol<-read.csv(paste(sub("\\..*","",filename),".sol",sep = ""),sep = " ",header=FALSE)
data_sol$V2<-data_sol$V2+1
data_sol<-data.matrix(data_sol)
cm<-getCM(filename)

init_pen<-calculatePen(hcPrepare(data_sol),cm)
print(paste("initial penalty: ",init_pen, sep=""))

# #hilclimb
# final_sol<-stochastic_hillclimb(data_sol,cm,3000,nrow(data))
# hc_pen<-calculatePen(hcPrepare(final_sol),cm)
# print(hc_pen)
# #export_tab(final_sol,filename,"hc")

# move1 move2 move3 swap2 
##sa
final_sol_sa<-sim_annealing(data_sol,cm,400,nrow(data),3000)
sa_pen<-calculatePen(hcPrepare(final_sol_sa),cm)
print(paste("penalty for sa with 5 neighborhood: ",sa_pen,sep = ""))

##vns
final_sol_vns2<-vns_2(data_sol,cm,nrow(data),3000)
vns2_pen<-calculatePen(hcPrepare(final_sol_vns2),cm)
print(paste("penalty for vns with 5 neighborhood: ",vns2_pen,sep = ""))

##improvement
print(paste("improvement over sa: ",round((sa_pen-vns2_pen)/sa_pen,3),sep=""))

#export_tab(final_sol_sa,filename,"sa")

#kempe chain neighborhood & move exam to timeslot with best penalty
##sa
final_sol_sa2<-sim_annealing2(data_sol,cm,nrow(data),3000,400)
sa_pen2<-calculatePen(hcPrepare(final_sol_sa2),cm)
print(paste("penalty for sa with 2 custom neighborhood: ",sa_pen2,sep = ""))

##vns
final_sol_vns<-vns(data_sol,cm,nrow(data),3000)
vns_pen<-calculatePen(hcPrepare(final_sol_vns),cm)
print(paste("penalty for vns with 2 custom neighborhood: ",vns_pen,sep = ""))

##improvement
print(paste("improvement over sa: ",round((sa_pen2-vns_pen)/sa_pen,3),sep=""))

#export_tab(final_sol_vns,filename,"vns")

###################################################################