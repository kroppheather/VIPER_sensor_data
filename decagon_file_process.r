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
datST$typeID<- seq(1,dim(datST)[1])
	
#pull out all sensor combinations
sensorTemp <- list()

for(i in 1: dim(slcountI)[1]){
	#find out the row to subset by

	sensorTemp[[i]] <- cbind(Fixout[[slcountI$loggerID[i]]]
	[,1:4],
	s=Fixout[[slcountI$loggerID[i]]][
		,4+slcountI$slotN[i]])
	
}
for(i in 1: dim(slcountI)[1]){
	sensorTemp[[i]]$sensorToinclude<- ifelse(sensorTemp[[i]]$doy<slcountI$dayTS[i]&sensorTemp[[i]]$year==slcountI$yearTS[i],1,
									ifelse(sensorTemp[[i]]$doy==slcountI$dayTS[i]&sensorTemp[[i]]$year==slcountI$yearTS[i]&sensorTemp[[i]]$hour<slcountI$timeTS[i],1,2))	
}
for(i in 1: dim(slcountI)[1]){
	sensorTemp[[i]]<-sensorTemp[[i]][sensorTemp[[i]]$sensorToinclude==2,]								
}
#create an id for what table the sensor output goes into
tablesI <- unique(data.frame(tableType=datST$tableType))
tablesI$tableID <- seq(1, dim(tablesI)[1])

#match the sensor table with what data table it goes to
tableSmatch <- join(datSI, datST, by=c("sensorMeas", "sensorName", "sensorUnit"), type="left")

#createid to keep track with above tbale
slcountI$sensorID <- seq(1, dim(slcountI)[1])

#now join this to the sesnsor actually usedd
sensorALL <- join(tableSmatch, slcountI, by=c("slotN", "loggerID", "sensorMeas", "sensorName"), type="right")
datLname <- data.frame(loggerID=datLI$loggerID, degagonName=datLI$decagonName)
sensorALL <- join( sensorALL, datLname, by="loggerID", type="left")




#need to add additional sensor info with each observations
for(i in 1: dim(sensorALL)[1]){
	sensorTemp[[sensorALL$sensorID[i]]]$site <- rep(sensorALL$site[i],dim(sensorTemp[[i]])[1])
	sensorTemp[[sensorALL$sensorID[i]]]$sensorZ <- rep(sensorALL$sensorZ[i],dim(sensorTemp[[i]])[1])
	sensorTemp[[sensorALL$sensorID[i]]]$sensorMeas <- rep(sensorALL$sensorMeas[i],dim(sensorTemp[[i]])[1])
	sensorTemp[[sensorALL$sensorID[i]]]$sensorName <- rep(sensorALL$sensorName[i],dim(sensorTemp[[i]])[1])
	sensorTemp[[sensorALL$sensorID[i]]]$sensorUnit <- rep(sensorALL$sensorUnit[i],dim(sensorTemp[[i]])[1])
	sensorTemp[[sensorALL$sensorID[i]]]$sensorLoc <- rep(sensorALL$sensorLoc[i],dim(sensorTemp[[i]])[1])
	sensorTemp[[sensorALL$sensorID[i]]]$loggerName <- rep(sensorALL$degagonName[i],dim(sensorTemp[[i]])[1])
	sensorTemp[[sensorALL$sensorID[i]]]$slotN <- rep(sensorALL$slotN[i],dim(sensorTemp[[i]])[1])
	sensorTemp[[sensorALL$sensorID[i]]]$typeID <- rep(sensorALL$typeID[i],dim(sensorTemp[[i]])[1])
	sensorTemp[[sensorALL$sensorID[i]]]$sensorUID <- rep(sensorALL$sensorUID[i],dim(sensorTemp[[i]])[1])
}

#convert to data frame
sensorTemp2 <- ldply(sensorTemp,data.frame)

#now data from each sensor type
MeasTemp <- list()
MeasTemp2 <- list()
for(i in 1:dim(datST)[1]){
	MeasTemp[[i]] <- sensorTemp2[sensorTemp2$typeID==datST$typeID[i],]
	MeasTemp2[[i]] <- data.frame(MeasTemp[[i]][,1:3],s=MeasTemp[[i]][,5], sensorZ=MeasTemp[[i]]$sensorZ, sensInfo=paste0(MeasTemp[[i]]$sensorMeas,".",MeasTemp[[i]]$sensorName),
									sensorUnit=MeasTemp[[i]]$sensorUnit,sensorLoc=MeasTemp[[i]]$sensorLoc,
									site=MeasTemp[[i]]$site, loggerName=MeasTemp[[i]]$loggerName, slotN=MeasTemp[[i]]$slotN,sensorUID=MeasTemp[[i]]$sensorUID )
	
	colnames(MeasTemp2[[i]])[4] <- paste0(datST$sensorMeas[i],".",datST$sensorName[i])
		#colnames(MeasTemp2[[i]])[5:11] <- paste0(datST$sensorMeas[i],".",datST$sensorName[i],".",colnames(MeasTemp2[[i]])[5:11])							
									
	}

	
#now need to group by which table it goes to

soilIDs <- datST[datST$tableType=="soil",]

soilListTemp<-list()
for(i in 1:dim(soilIDs)[1]){
	soilListTemp[[i]] <- MeasTemp2[[soilIDs$typeID[i]]]

}
#now subset lists by type
metIDs <- datST[datST$tableType=="met",]

metListTemp<-list()
for(i in 1:dim(metIDs)[1]){
	metListTemp[[i]] <- MeasTemp2[[metIDs$typeID[i]]]

}

#now subset lists by type
waterIDs <- datST[datST$tableType=="water",]

waterListTemp<-list()
for(i in 1:dim(waterIDs)[1]){
	waterListTemp[[i]] <- MeasTemp2[[waterIDs$typeID[i]]]

}



NDVIIDs <- datST[datST$tableType=="NDVI",]

NDVIListTemp<-list()
for(i in 1:dim(NDVIIDs)[1]){
	NDVIListTemp[[i]] <- MeasTemp2[[NDVIIDs$typeID[i]]]

}



#now write each sensor to the output folder
#save in 3 locations so undergrads can access for their own working folder
#an original copy gets saved in the viper energy folder
#I also have my own working folder
dir1 <-c("c:\\Users\\hkropp\\Google Drive\\viper_energy\\combined_files\\decagon\\csv_out\\",
		"c:\\Users\\hkropp\\Google Drive\\Loranty_Lab_Sensor\\decagon\\sensorData\\",
		"c:\\Users\\hkropp\\Google Drive\\viperSensorOut\\",
		"z:\\student_research\\tobio\\viperSensor\\decagon\\",
		"z:\\data_repo\\field_data\\viperSensor\\decagon\\")
for(k in 1:length(dir1)){
for(i in 1:dim(NDVIIDs)[1]){
	write.table(NDVIListTemp[[i]], 
		paste0(dir1[k],"ndvi\\",colnames(NDVIListTemp[[i]])[4],
				".csv"), sep=",", row.names=FALSE)
}

for(i in 1:dim(soilIDs)[1]){
	write.table(soilListTemp[[i]], 
		paste0(dir1[k],"soil\\",colnames(soilListTemp[[i]])[4],
				".csv"), sep=",", row.names=FALSE)
}

for(i in 1:dim(metIDs)[1]){
	write.table(metListTemp[[i]], 
		paste0(dir1[k],"met\\",colnames(metListTemp[[i]])[4],
				".csv"), sep=",", row.names=FALSE)
}

for(i in 1:dim(waterIDs)[1]){
	write.table(waterListTemp[[i]], 
		paste0(dir1[k],"water\\",colnames(waterListTemp[[i]])[4],
				".csv"), sep=",", row.names=FALSE)
}
}