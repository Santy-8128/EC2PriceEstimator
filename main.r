### A small simple R utility to estimate pricing across multiple EC2 instances and show the minimum
### Usage Rscript script.r --NoJobs 10000 --MemoryPerJob 35 --CPUPerJob 16 --DiskSpacePerJob 200 --TimePerJob 3 > OUTPUT.txt

#library("RODBC")
#suppressWarnings(library(RODBC))
#suppressWarnings(suppressMessages(library(RODBC)))

suppressPackageStartupMessages(require(optparse))
suppressPackageStartupMessages(require(knitr))
 

option_list = list(
  make_option(c("-n", "--NoJobs"), action="store", default=NA, type='integer',
              help="Total number of jobs to run"),
  make_option(c("-m", "--MemoryPerJob"), action="store", default=NA, type='integer',
              help="Average (total) memory per job "),
  make_option(c("-c", "--CPUPerJob"), action="store", default=NA, type='integer',
              help="Number of CPUs per job "),
  make_option(c("-d", "--DiskSpacePerJob"), action="store", default=NA, type='integer',
              help="Average disk space required per job "),
  make_option(c("-t", "--TimePerJob"), action="store", default=NA, type='integer',
              help="Average time per job")
)
opt = parse_args(OptionParser(usage = "usage: %prog [options]", option_list=option_list,add_help_option = TRUE,))




NoJobs=as.numeric(opt$NoJobs)
MemoryPerJob=as.numeric(opt$MemoryPerJob)
CPUPerJob=as.numeric(opt$CPUPerJob)
DiskSpacePerJob=as.numeric(opt$DiskSpacePerJob)
TimePerJob=as.numeric(opt$TimePerJob)

if(is.na(opt$NoJobs) || is.na(opt$MemoryPerJob) || is.na(opt$CPUPerJob) || is.na(opt$DiskSpacePerJob) || is.na(opt$TimePerJob))
{
      print(" All input parameters are mandatory. Please type: Rscript main.r --help")
      quit()
}


## READ THE DATA

MyData <- read.csv("Amazon EC2 Instance Comparison.csv",  header=TRUE, sep=",")
Required_Data=apply(MyData[,c("API.Name","Memory","vCPUs","Linux.On.Demand.cost")],2,as.character)


## CALCULATE BASIC PARAMETERS

Memory_Per_Instance = as.numeric(sapply(Required_Data[,c("Memory")],function(x){strsplit(x," ")[[1]][1]}))
CPUs_Per_Instance = as.numeric(sapply(Required_Data[,c("vCPUs")],function(x){strsplit(x," ")[[1]][1]}))
Cost_Per_Instance = suppressWarnings(as.numeric(sapply(sapply(Required_Data[,c("Linux.On.Demand.cost")],function(x){strsplit(x," ")[[1]][1]}),function(x){substr(x,2,1000)})))
Fin_Data=data.frame(cbind(Memory_Per_Instance,CPUs_Per_Instance,rep(1000)))
Requirements=c(MemoryPerJob,CPUPerJob,DiskSpacePerJob)
NoJobs_Per_Instance=floor(apply(apply(Fin_Data,1,function(x){x/Requirements}),2,min))
No_Instance=ceiling(NoJobs/NoJobs_Per_Instance)
Total_Cost=Cost_Per_Instance*No_Instance*TimePerJob

## ESTIMATE FINAL PRICE
FINAL_PRICE=data.frame(cbind(Required_Data[,c("API.Name")],  paste(Memory_Per_Instance," Gib",sep="")    ,  paste(CPUs_Per_Instance," vCPUs",sep="")   ,  paste("$",Cost_Per_Instance," hourly",sep="")   ,   NoJobs_Per_Instance  , No_Instance   ,  paste("$",sprintf("%.2f",Total_Cost) ,sep="") ) )
colnames(FINAL_PRICE)=c("Type","Memory","vCPUs","Cost/Instance","Jobs/Instance","NoInstances","Total_Cost")
FINAL_SORTED_PRICE=FINAL_PRICE[order(Total_Cost),]
print(kable(FINAL_SORTED_PRICE,align="c",row.names=FALSE))





