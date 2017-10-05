#######################################################################################
#######################################################################################
########### Script started in October 2017 by Heather Kropp                 ###########
########### Script for processing data files from Campbell sensors.         ###########
########### This script accounts for issues with timestamps due to incorrect###########
########### computer times, different time zones on devices, and tracks use ###########
########### of sensors over time. Currently this script assumes that the    ###########
########### incoming data from the logger is appended to each file with     ###########
########### consistent time documentation.                                  ###########
#######################################################################################
#######################################################################################
#load lubridate
library(lubridate)
#load plyr
library(plyr)

#setwd to the folder with compiled files saved as csv
#make sure there are only the compiled files in this folder
setwd("c:\\Users\\hkropp\\Google Drive\\viper_energy\\combined_files\\campbell\\csv_to_process")
#setwd("c:\\Users\\hkropp\\Google Drive\\viper_energy\\combined_files\\ls_toprocess")
#specify an output path
output.path<-"c:\\Users\\hkropp\\Google Drive\\viper_energy\\combined_files\\campbell\\csv_out"
#indicate the date format of the data
dateFormat<-"%m/%d/%Y %H:%M"


#read in data tables with sensor and logger information
datMI<-read.csv("c:\\Users\\hkropp\\Google Drive\\viper_energy\\combined_files\\campbell\\sensor_info\\measurement_info.csv")
datDI<-read.csv("c:\\Users\\hkropp\\Google Drive\\viper_energy\\combined_files\\campbell\\sensor_info\\datatable_desc.csv")
datSI <- read.csv("c:\\Users\\hkropp\\Google Drive\\viper_energy\\combined_files\\campbell\\sensor_info\\sensor_info.csv")
#get unique filenames
datLI <- data.frame(loggerFile = unique(as.character(datDI$filename)))
datLI$loggID <- seq(1, dim(datLI)[1])


##get file names
tofix<-paste0(getwd(), "/", datLI$loggerFile, ".csv")
#read in files
fixmet<-list()
for(i in 1:length(tofix)){
	fixmet[[i]]<-read.csv(tofix[i], na.strings=c("NAN", "NA"))

}


#create a fixed time stamp for each file
#first pull out the day of year and year
#info for each timestamp

fixmetD<-list()
for(i in 1:length(tofix)){
	fixmetD[[i]]<-data.frame(DateI=as.Date(fixmet[[i]]$TIMESTAMP, dateFormat))
	fixmetD[[i]]$doyUN<-yday(fixmetD[[i]]$DateI)
	fixmetD[[i]]$yearUN<-year(fixmetD[[i]]$DateI)
	
}



#function to convert UTC -4 to siberian time which is +15 hours to EDT
CherskiyThour<-function(hour){
	ifelse(hour<9,hour+15,hour-9)
}
CherskiyTdoy<-function(hour,doy){
	ifelse(doy!=366,
			ifelse(hour<9,doy,doy+1 ),
			ifelse(hour<9,doy,1))
}
#need to fix mismatch in year on last day of the year
CherskiyTyear<-function(doy,year, hour){
	ifelse(doy==366, 
			ifelse(hour<9,year,year+1),
			year)
}

#convert to decimal hour
for(i in 1:length(tofix)){
	fixmetD[[i]]$timeUN<-fixmet[[i]]$hour+(fixmet[[i]]$minute/60)
}



#fix the data

for(i in 1:length(tofix)){
	fixmetD[[i]]$hourF<-ifelse(fixmet[[i]]$UTC==-4,
						CherskiyThour(fixmetD[[i]]$timeUN),
						fixmetD[[i]]$timeUN)
	fixmetD[[i]]$doyF1<-ifelse(fixmet[[i]]$UTC==-4,
						CherskiyTdoy(fixmetD[[i]]$timeUN,fixmetD[[i]]$doyUN),
						fixmetD[[i]]$doyUN)
	fixmetD[[i]]$yearF<-ifelse(fixmet[[i]]$UTC==-4,
						CherskiyTyear(fixmetD[[i]]$doyUN,fixmetD[[i]]$yearUN,fixmetD[[i]]$timeUN),
						fixmetD[[i]]$yearUN)
	fixmetD[[i]]$doyF2<- fixmetD[[i]]$doyF1+ fixmet[[i]]$Day.offset
}

Fixout<-list()
#write to file
for(i in 1:length(tofix)){
	#make a data frame with only the correct info
	Fixout[[i]]<-data.frame(doy=fixmetD[[i]]$doyF2,year=fixmetD[[i]]$yearF,
							hour=fixmetD[[i]]$hourF, minute=fixmet[[i]]$minute,
							fixmet[[i]][,6:dim(fixmet[[i]])[2]])
	
}


########################################################################################
#### match sensor info to data
########################################################################################   


#each datatable has the same initialization period because all of the sensors are set up together
#for the data table. Just need to subset Fixout based on the datatable
#just get the unique date for each datatable
dateStartD <- unique(data.frame(loggerFile=datDI$filename, timeoutEnd = datDI$timeoutEnd, 
					dayEnd=datDI$dayEnd, yearEnd=datDI$yearEnd))
					
#now merge with datatable ID
fileStart <- join(datLI, dateStartD, by="loggerFile",type="left")					

#exclude any data in the warm up period before sensor install
Fixout2<- list()
fixStart <- numeric (0)
for(i in 1:length(tofix)){
	#get the starting point for the data
	if(length(which(Fixout[[i]]$doy==fileStart$dayEnd[i]&
				Fixout[[i]]$hour==fileStart$timeoutEnd[i]&
				Fixout[[i]]$year==fileStart$yearEnd[i]))!= 0){
					fixStart[i] <- which(Fixout[[i]]$doy==fileStart$dayEnd[i]&
								Fixout[[i]]$hour==fileStart$timeoutEnd[i]&
								Fixout[[i]]$year==fileStart$yearEnd[i])
	}else{fixStart[i] <- 1}							
	#subset to include starting point							
	Fixout2[[i]] <- Fixout[[i]][fixStart[i]:dim(Fixout[[i]])[1],]
}
					
#start by subsetting the info for each data table
measList <- list()
measList2 <- list()
for(i in 1:length(tofix)){
	#pull out info
	measList[[i]] <- datDI[datDI$filename==datLI$loggerFile[i],]
	#now match up the possible types of measurements in each datatable
	measList2[[i]] <- join(measList[[i]], datMI, by="sensorName", type="left")
}


#now need to pull out all by measurement type
#join measurement type
colnames(datSI)[1] <- "loggerFile" 
fileStart <- join(fileStart, datSI, by="loggerFile", type="left" )




 
#pull out each type into a list
sapflowF <- fileStart[fileStart$measType=="sapflow",]
heatfluxF <- fileStart[fileStart$measType=="heatflux",]
radiationF <- fileStart[fileStart$measType=="radiation",]

#for sapflux variables are best left relatively untouched 
#so just pull out and leave untouched

#directories to save to
dir1 <-c("c:\\Users\\hkropp\\Google Drive\\viper_energy\\combined_files\\campbell\\csv_out\\",
		"c:\\Users\\hkropp\\Google Drive\\Loranty_Lab_Sensor\\campbell\\",
		"c:\\Users\\hkropp\\Google Drive\\viperSensor\\")

sapflowListTemp<-list()
for(k in 1:length(dir1)){
	for(i in 1:dim(sapflowF )[1]){
		sapflowListTemp[[i]] <- Fixout2[[sapflowF$loggID[i]]]
		#clean up column names
		colnames(sapflowListTemp[[i]]) <- gsub("[[:punct:]]", "", colnames(sapflowListTemp[[i]]))
		
		write.table(sapflowListTemp[[i]],
			paste0(dir1[k],"sapflow\\",sapflowF$loggerFile[i], ".csv" ),
			sep=",", row.names=FALSE)
	}

}
#now compile radiation

#just grab the info for the entire radiometer
datDIsub <- data.frame(loggerFile= datDI$filename, site= datDI$site, sensorZ= datDI$sensorZ, loc=datDI$sensorLoc)
radiationF <- join(radiationF, datDIsub, by="loggerFile", type="left")

radiationListTemp<-list()
for(i in 1:dim(radiationF )[1]){
	radiationListTemp[[i]] <- Fixout2[[radiationF$loggID[i]]]
	#add the site info, location, and height 
	radiationListTemp[[i]]$site <- rep(radiationF$site[i], dim(radiationListTemp[[i]])[1])
	radiationListTemp[[i]]$loc <- rep(radiationF$loc[i], dim(radiationListTemp[[i]])[1])
	radiationListTemp[[i]]$sensorZ <- rep(radiationF$sensorZ[i], dim(radiationListTemp[[i]])[1])
}
#now add all together
radiationAll <- ldply(radiationListTemp, data.frame)


			
			
# compile heatflux
#first grab relevant heatflux data
heatfluxListTemp<-list()
heatMeas <- list()
heatfluxListTemp2<-list()
for(i in 1:dim(heatfluxF )[1]){
	heatfluxListTemp[[i]] <- Fixout2[[heatfluxF$loggID[i]]]
	heatMeas[[i]] <- measList2[[heatfluxF$loggID[i]]]
	#restructure to combine all into a dataframe
	heatfluxListTemp2[[i]] <- data.frame(doy=rep(heatfluxListTemp[[i]][,1], times=dim(heatMeas[[i]])[1]),
										year=rep(heatfluxListTemp[[i]][,2], times=dim(heatMeas[[i]])[1]),
										hour=rep(heatfluxListTemp[[i]][,3], times=dim(heatMeas[[i]])[1]),
										shf=as.vector(data.matrix(heatfluxListTemp[[i]][,6:(5+dim(heatMeas[[i]])[1])])),
										site=rep(heatMeas[[i]]$site, each=dim(heatfluxListTemp[[i]])[1]),
										loc=rep(heatMeas[[i]]$sensorLoc, each=dim(heatfluxListTemp[[i]])[1]),
										sensorZ=rep(heatMeas[[i]]$sensorZ, each=dim(heatfluxListTemp[[i]])[1]),
										sensorID=rep(heatMeas[[i]]$sensorID, each=dim(heatfluxListTemp[[i]])[1]))
										}
#combine all together
heatfluxAll <- ldply(heatfluxListTemp2, data.frame)

#write output
for(k in 1:length(dir1)){
write.table(radiationAll, paste0(dir1[k],"\\radiation\\netR.csv"),
			sep=",", row.names=FALSE)
write.table(heatfluxAll,paste0(dir1[k],"\\heatflux\\heatflux.csv"),
			sep=",", row.names=FALSE)		
}			