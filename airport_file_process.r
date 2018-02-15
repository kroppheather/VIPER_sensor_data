#########################################################################################
#########################################################################################
########### Script started in October 2017 by Heather Kropp                   ###########
########### Processes csv file from the Cherskiy weather station:             ###########
########### Weather station Cherskiy, Russia, WMO_ID=25123                    ###########
########### Encoding: ANSI                                                    ###########
########### The data are provided by the website "Reliable Prognosis", rp5.ua ###########
########### If you use the data, please indicate the name of the website.     ###########
########### For meteorological parameters see the address                     ###########
########### http://rp5.ua/archive.php?wmo_id=25123&lang=en                    ###########
#########################################################################################
#########################################################################################
library(lubridate)
library(plyr)
library(imputeTS)

#airport met data
datAD<-read.table("c:\\Users\\hkropp\\Google Drive\\viper_energy\\combined_files\\airport\\airport.csv", sep=";", head=TRUE, skip=6, stringsAsFactors=FALSE)

#get date from each
dateAP<-as.Date(rownames(datAD), "%d.%m.%Y %H:%M")


#header is shifted over 1 
#precip is RRR in reliable prognosis, but here it is in the Td column
Precip1 <- datAD$Td

#no convert precip to numbers
#calling trace precipitation 0.01
#precip is in mm
PrecipF1 <- ifelse(Precip1 == "Trace of precipitation" ,"0.01" ,
			ifelse(Precip1 == "No precipitation", "0",
				ifelse(Precip1 == "",0 ,Precip1)))
				
PrecipN1<-as.numeric(PrecipF1)


#now turn into a dataframe
PrecipDat <- data.frame(doy=yday(dateAP),year=year(dateAP),
						Precip=PrecipN1)
#get daily total precip
PrecipDay <- aggregate(PrecipDat$Precip, 
				by=list(PrecipDat$doy,PrecipDat$year), FUN="sum")
colnames(PrecipDay)<-c("doy","year","Pr.mm")

#calculate daily average pressure
datPhh<-data.frame(doy=yday(dateAP),year=year(dateAP), Pkpa= datAD$T/7.474)

datPj<-aggregate(datPhh$Pkpa, by=list(datPhh$doy, datPhh$year), FUN="mean", na.action="na.omit")
colnames(datPj)<-c("doy","year","Pkpa")


#join together
metAirport <- join(datPj, PrecipDay, by=c("doy", "year"), type="full")

#gapfill missing pressure by interpolating with a linear interpolation
metAirport$Pkpa.gap <- na.interpolation(metAirport$Pkpa, option="linear")

dir1 <-c(#"c:\\Users\\hkropp\\Google Drive\\viper_energy\\combined_files\\airport\\csv_out\\",
		#"c:\\Users\\hkropp\\Google Drive\\Loranty_Lab_Sensor\\airport\\",
		#"c:\\Users\\hkropp\\Google Drive\\viperSensor\\airport\\",
		"z:\\student_research\\tobio\\viperSensor\\airport\\")
for(k in 1:length(dir1)){
write.table(metAirport, paste0(dir1[k],"airport.csv"),
			sep=",", row.names=FALSE)
}			