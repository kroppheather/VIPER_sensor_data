#######################################################################################
#######################################################################################
########### Script started in June 2017 by Heather Kropp                    ###########
########### Script for processing data files from Decgon sensors.           ###########
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
setwd("c:\\Users\\hkropp\\Google Drive\\viper_energy\\combined_files\\decagon\\csv_to_process")
#setwd("c:\\Users\\hkropp\\Google Drive\\viper_energy\\combined_files\\ls_toprocess")
#specify an output path
output.path<-"c:\\Users\\hkropp\\Google Drive\\viper_energy\\combined_files\\decagon\\csv_out"
#indicate the date format of the data
dateFormat<-"%m/%d/%Y %H:%M"

#read in data tables with sensor and logger information
datLI<-read.csv("c:\\Users\\hkropp\\Google Drive\\viper_energy\\combined_files\\decagon\\sensorInfo\\DataloggerInventory.csv")
datSI<-read.csv("c:\\Users\\hkropp\\Google Drive\\viper_energy\\combined_files\\decagon\\sensorInfo\\SensorInventory.csv")
datST<-read.csv("c:\\Users\\hkropp\\Google Drive\\viper_energy\\combined_files\\decagon\\sensorInfo\\sensorTable.csv")



##get file names
tofix<-paste0(getwd(), "/", datLI$loggerFile, ".csv")
#read in files
fixmet<-list()
for(i in 1:length(tofix)){
	fixmet[[i]]<-read.csv(tofix[i])

}

#create a fixed time stamp for each file
#first pull out the day of year and year
#info for each timestamp

fixmetD<-list()
for(i in 1:length(tofix)){
	fixmetD[[i]]<-data.frame(DateI=as.Date(fixmet[[i]]$Timestamp, dateFormat))
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
#### match sensor time frames to data
########################################################################################   


##################################
#### organize sensor information
#### and seperate by consideration
##################################
#first check how many sensor positions had no changes
spTableNA <- unique(data.frame(loggerID=datSI$loggerID,slotN=datSI$slotN,sensorMeas=datSI$sensorMeas,
							sensorName=datSI$sensorName, timeTS=datSI$timeoutEnd,dayTS=datSI$dayEnd, yearTS=datSI$yearEnd))
#omit slots that don't have sensors
spTable <- spTableNA[is.na(spTableNA$sensorMeas)==FALSE,]

#set up two tables for subset
#subset a table for the first sensor placements at the start of the datalogger

#start by getting a count of different sensors in a slot
slcount <- aggregate(spTable$slotN, by=list(spTable$slotN,spTable$loggerID), FUN="length")
colnames(slcount) <- c("slotN", "loggerID", "count")
#only need to use the first start time in slots with only one sensor
slcountI <- slcount[slcount$count==1,]
#join other info
slcountI <- join(slcountI, spTable, by=c("loggerID","slotN"), type="left")


#slots with more than 1 sensor
slcountM <- slcount[slcount$count>1,]


#there are currently no slots with multiple sensors cycled through
#so only do this join if 
if(dim(slcountM)[1]>0){
    #get multiple sensors out
    slmult<-join(spTable,slcountM, by=c("loggerID","slotN"), type="inner")

}


##################################
#### grab output of loggers
#### for each sensor
##################################

#fix loggers where there was never a sensor type switch
#make a list of the sensors for each logger type
	
#pull out all sensor combinations
sensorTemp <- list()
for(i in 1: dim(slcountI)[1]){
	sensorTemp[[i]] <- cbind(Fixout[[slcountI$loggerID[i]]]
	[Fixout[[slcountI$loggerID[i]]]$doy==slcountI$dayTS[i]
		&Fixout[[slcountI$loggerID[i]]]$hour==slcountI$dayTS[i]
		&Fixout[[slcountI$loggerID[i]]]$year==slcountI$dayTS[i]:dim(Fixout[[slcountI$loggerID[i]]])
		,1:4],
	Fixout[[slcountI$loggerID[i]]][Fixout[[slcountI$loggerID[i]]]$doy==slcountI$dayTS[i]
		&Fixout[[slcountI$loggerID[i]]]$hour==slcountI$dayTS[i]
		&Fixout[[slcountI$loggerID[i]]]$year==slcountI$dayTS[i]:dim(Fixout[[slcountI$loggerID[i]]])
		,4+slcountI$slotN[i]])
}
#subset each logger based on the time when it is really starts measurements



#create an id for what table the sensor output goes into
tablesI <- unique(data.frame(tableType=datST$tableType))
tablesI$tableID <- seq(1, dim(tablesI)[1])







