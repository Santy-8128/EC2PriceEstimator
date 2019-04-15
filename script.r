
Spec=c(35,16,200,10000)

require(knitr)
library(argparse)

parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add an help option 

parser$add_argument("-n", "--NoJobs", type="integer", default=0,
    help="Total number of jobs to run [default %(default)s]",
    metavar="number")
parser$add_argument("-m", "--MemoryPerJob", type="integer", default=0, 
    help="Average (total) memory per job [default %(default)s]",
    metavar="number")
parser$add_argument("-c", "--CPUPerJob", type="integer", default=0, 
    help="Number of CPUs per job [default %(default)s]",
    metavar="number")
parser$add_argument("-d", "--DiskSpacePerJob", type="integer", default=0, 
    help="Average disk space required per job [default %(default)s]",
    metavar="number")
parser$add_argument("-t", "--TimePerJob", type="integer", default=0, 
    help="Average time per job [default %(default)s]",
    metavar="number")
# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults, 
args <- parser$parse_args()


NoJobs=as.numeric(args$NoJobs)
MemoryPerJob=as.numeric(args$MemoryPerJob)
CPUPerJob=as.numeric(args$CPUPerJob)
DiskSpacePerJob=as.numeric(args$DiskSpacePerJob)
TimePerJob=as.numeric(args$TimePerJob)



if(NoJobs*MemoryPerJob*CPUPerJob*DiskSpacePerJob*TimePerJob==0)
{
	parser$print_help()
	quit()
}




MyData <- read.csv("Amazon EC2 Instance Comparison.csv",  header=TRUE, sep=",")

Required_Data=apply(MyData[,c("API.Name","Memory","vCPUs","Linux.On.Demand.cost")],2,as.character)



Memory_Per_Instance = as.numeric(sapply(Required_Data[,c("Memory")],function(x){strsplit(x," ")[[1]][1]}))
CPUs_Per_Instance = as.numeric(sapply(Required_Data[,c("vCPUs")],function(x){strsplit(x," ")[[1]][1]}))
Cost_Per_Instance = as.numeric(sapply(sapply(Required_Data[,c("Linux.On.Demand.cost")],function(x){strsplit(x," ")[[1]][1]}),function(x){substr(x,2,1000)}))
Fin_Data=data.frame(cbind(Memory_Per_Instance,CPUs_Per_Instance,rep(1000)))
Requirements=c(MemoryPerJob,CPUPerJob,DiskSpacePerJob)
NoJobs_Per_Instance=floor(apply(apply(Fin_Data,1,function(x){x/Requirements}),2,min))
No_Instance=ceiling(NoJobs/NoJobs_Per_Instance)
Total_Cost=Cost_Per_Instance*No_Instance*TimePerJob

FINAL_PRICE=data.frame(cbind(Required_Data[,c("API.Name")],  paste(Memory_Per_Instance," Gib",sep="")    ,  paste(CPUs_Per_Instance," vCPUs",sep="")   ,  paste("$",Cost_Per_Instance," hourly",sep="")   ,   NoJobs_Per_Instance  , No_Instance   ,  paste("$",sprintf("%.2f",Total_Cost) ,sep="") ) )
colnames(FINAL_PRICE)=c("Type","Memory","vCPUs","Cost/Instance","Jobs/Instance","NoInstances","Total_Cost")
FINAL_SORTED_PRICE=FINAL_PRICE[order(Total_Cost),]

print(kable(FINAL_SORTED_PRICE,align="c",row.names=FALSE))