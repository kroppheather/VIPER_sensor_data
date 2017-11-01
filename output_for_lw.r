#####################################################
## formats and organizes data for a longwave model###
#####################################################
library(plyr)
library(lubridate)
#set wd
sD<- "c:\\Users\\hkropp\\Google Drive\\viperSensor"
outD <- "c:\\Users\\hkropp\\Google Drive\\viperSensor\\for_lw"
#subset the longwave data for just high density

datR <- read.csv(paste0(sD, "\\radiation\\netR.csv"))

datRs <- datR[datR$site=="hd",] 
write.table(datRs, paste0(outD, "\\radiation.csv"), sep=",", row.names=FALSE)
plot(datRs$IR01UpCo_Avg)

datRH <- read.csv(paste0(sD, "\\met\\RH.VP4.csv"))
datT <- read.csv(paste0(sD, "\\met\\TempC.VP4.csv"))

datRHs <- datRH[datRH$site=="hd",1:10]
datTs <- datT[datT$site=="hd",1:4]

datRHs$RH.VP4 <- ifelse(datRHs$RH.VP4 >=1,.999,datRHs$RH.VP4)

datMall <- join(datRHs, datTs, by=c("doy","year","hour"), type="full")

write.table(datMall, paste0(outD, "\\met.csv"), sep=",", row.names=FALSE)

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
#pull out hour info
hourI <- strsplit(rownames(datAD), " ")		
hourI2 <- ldply(hourI)		
hourI3 <- hourI2[,2]		

#convert to a number
hourI4 <- as.numeric(gsub("\\:", ".", hourI3))


#now turn into a dataframe
PrecipDat <- data.frame(doy=yday(dateAP),year=year(dateAP),hour=hourI4,
						Precip=PrecipN1, Wind=datAD$DD, snowDesc=datAD$Tg)
						
write.table(PrecipDat, paste0(outD, "\\airport_met.csv"), sep=",", row.names=FALSE)
