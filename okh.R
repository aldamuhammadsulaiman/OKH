source("D:/base.R")
source("D:/heuristic.R")
source("D:/HillClimb.R")
source("D:/sanealling.R")
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
cm<-data.matrix(cm)
final_sol<-stochastic_hillclimb(data_sol,cm,100000,nrow(data))
final_sol_sa<-sim_annealing(data_sol,cm,10000,nrow(data))
print(calculatePen(hcPrepare(final_sol),cm))
print(calculatePen(hcPrepare(final_sol_sa),cm))
export_tab(final_sol,filename)
###################################################################