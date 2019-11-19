source("D:/alda/okh_3_26_PM_11_19_2019/base.R")
source("D:/alda/okh_3_26_PM_11_19_2019/heuristic.R")
source("D:/alda/okh_3_26_PM_11_19_2019/HillClimb.R")
###################################################################
#construct initial solution
filename<-file.choose()
data<-read.csv(filename,header=FALSE)
runAll(data,filename)

#optimize solution w/ stochastic hill climbing method
filename2<-file.choose()
data_sol<-hcPrepare(filename2)
final_sol<-stochastic_hillclimb(data_sol,listIndex(data_sol),getCM(filename2),5000)
export(final_sol,filename2)
###################################################################