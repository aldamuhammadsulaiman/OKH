source("D:/alda/base(1)/base.R")
source("D:/alda/base(1)/heuristic.R")
source("D:/alda/base(1)/HillClimb.R")
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
final_sol<-stochastic_hillclimb_c(data_sol,getCM(filename),10000,nrow(data))
print(calculatePen(hcPrepare(final_sol),getCM(filename)))
export_tab(final_sol,filename)
###################################################################