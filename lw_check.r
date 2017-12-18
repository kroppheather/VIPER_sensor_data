

datNR <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\radiation\\netR.csv")

check<-aggregate(datNR$SR01Up_Avg, by=list(datNR$doy,datNR$year,datNR$hour,datNR$site,datNR$loc),FUN="length")
#read in air temp

datTair <-read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\met\\TempC.VP4.csv")

datNR$Time <- datNR$doy+(datNR$hour/24)
datTair$Time <- datTair$doy+(datTair$hour/24)
						
par(mfrow=c(2,1))						
plot(datNR$Time[datNR$site=="hd"&datNR$loc=="canopy"&datNR$year==2016],
	datNR$IR01UpCo_Avg[datNR$site=="hd"&datNR$loc=="canopy"&datNR$year==2016],
		type="p", lwd=2, ylim=c(0,500),pch=16, xlab="Day of year 2016", ylab="Longwave W/m2", xlim=c(180,310),
		main="high density")

points(datNR$Time[datNR$site=="hd"&datNR$loc=="canopy"&datNR$year==2016],
	datNR$IR01DnCo_Avg[datNR$site=="hd"&datNR$loc=="canopy"&datNR$year==2016],
		type="p", lwd=2, col="red",pch=16)		

legend(185, 200, c("Upward","Downward"), col=c("black","red"), pch=16, bty="n")
		
plot(datNR$Time[datNR$site=="ld"&datNR$loc=="canopy"&datNR$year==2016],
	datNR$IR01UpCo_Avg[datNR$site=="ld"&datNR$loc=="canopy"&datNR$year==2016],
		type="p", lwd=2, col="black", pch=16, ylim=c(0,500), xlab="Day of year 2016", 
		ylab="Longwave W/m2", xlim=c(180,310), main="low density")				
		
points(datNR$Time[datNR$site=="ld"&datNR$loc=="canopy"&datNR$year==2016],
	datNR$IR01DnCo_Avg[datNR$site=="ld"&datNR$loc=="canopy"&datNR$year==2016],
		type="p", lwd=2, col="red", pch=16)		
legend(185, 200, c("Upward","Downward"), col=c("black","red"), pch=16, bty="n")		


par(mfrow=c(2,1))						
plot(datNR$Time[datNR$site=="hd"&datNR$loc=="canopy"&datNR$year==2017],
	datNR$IR01UpCo_Avg[datNR$site=="hd"&datNR$loc=="canopy"&datNR$year==2017],
		type="p", lwd=2, ylim=c(0,500),pch=16, xlab="Day of year 2017", ylab="Longwave W/m2",
		main="high density", xlim=c(90,230))

points(datNR$Time[datNR$site=="hd"&datNR$loc=="canopy"&datNR$year==2017],
	datNR$IR01DnCo_Avg[datNR$site=="hd"&datNR$loc=="canopy"&datNR$year==2017],
		type="p", lwd=2, col="red",pch=16)		

legend(85, 100, c("Upward","Downward"), col=c("black","red"), pch=16, bty="n")
		
plot(datNR$Time[datNR$site=="ld"&datNR$loc=="canopy"&datNR$year==2017],
	datNR$IR01UpCo_Avg[datNR$site=="ld"&datNR$loc=="canopy"&datNR$year==2017],
		type="p", lwd=2, col="black", pch=16, ylim=c(0,500), xlab="Day of year 2017", 
		ylab="Longwave W/m2", xlim=c(90,230), main="low density")				
		
points(datNR$Time[datNR$site=="ld"&datNR$loc=="canopy"&datNR$year==2017],
	datNR$IR01DnCo_Avg[datNR$site=="ld"&datNR$loc=="canopy"&datNR$year==2017],
		type="p", lwd=2, col="red", pch=16)		
legend(85, 200, c("Upward","Downward"), col=c("black","red"), pch=16, bty="n")		


#check predicted temp
datNR$Tup <- ((datNR$IR01UpCo_Avg/(.98*(5.67*(10^-8))))^(1/4))-273.15
datNR$Tdn <- ((datNR$IR01DnCo_Avg/(.98*(5.67*(10^-8))))^(1/4))-273.15



par(mfrow=c(2,1))						
plot(datNR$Time[datNR$site=="hd"&datNR$loc=="canopy"&datNR$year==2016],
	datNR$Tup[datNR$site=="hd"&datNR$loc=="canopy"&datNR$year==2016],
		type="p", lwd=2,pch=16,ylim=c(-30,28), xlab="Day of year 2016", ylab="Temp (C) from longwave2", xlim=c(180,310),
		main="high density")

points(datNR$Time[datNR$site=="hd"&datNR$loc=="canopy"&datNR$year==2016],
	datNR$Tdn[datNR$site=="hd"&datNR$loc=="canopy"&datNR$year==2016],
		type="p", lwd=2, col="red",pch=16)		

points(datTair$Time[datTair$site=="hd"&datTair$year==2016],
	datTair$TempC.VP4[datTair$site=="hd"&datTair$year==2016], pch=16, col=rgb(77/255,160/255,243/255,.5))		
		
legend(189, -13, c("Upward","Downward", "Air temp sensor"), 
			col=c("black","red",rgb(77/255,160/255,243/255,.5) ), pch=16, bty="n")
		
plot(datNR$Time[datNR$site=="ld"&datNR$loc=="canopy"&datNR$year==2016],
	datNR$Tup[datNR$site=="ld"&datNR$loc=="canopy"&datNR$year==2016],
		type="p", lwd=2, col="black", pch=16, ylim=c(-30,28), xlab="Day of year 2016", 
		ylab="Temp (C) from longwave2", xlim=c(180,310), main="low density")				
		
points(datNR$Time[datNR$site=="ld"&datNR$loc=="canopy"&datNR$year==2016],
	datNR$Tdn[datNR$site=="ld"&datNR$loc=="canopy"&datNR$year==2016],
		type="p", lwd=2, col="red", pch=16)		
		
points(datTair$Time[datTair$site=="ld"&datTair$year==2016],
	datTair$TempC.VP4[datTair$site=="ld"&datTair$year==2016], pch=16, col=rgb(77/255,160/255,243/255,.5))		
		
legend(189, -13, c("Upward","Downward", "Air temp sensor"), 
			col=c("black","red",rgb(77/255,160/255,243/255,.5) ), pch=16, bty="n")		




par(mfrow=c(2,1))						
plot(datNR$Time[datNR$site=="hd"&datNR$loc=="canopy"&datNR$year==2017],
	datNR$Tup[datNR$site=="hd"&datNR$loc=="canopy"&datNR$year==2017],
		type="p", lwd=2,pch=16,ylim=c(-35,28), xlab="Day of year 2017", ylab="Temp (C) from longwave", 
		xlim=c(90,230),
		main="high density")

points(datNR$Time[datNR$site=="hd"&datNR$loc=="canopy"&datNR$year==2017],
	datNR$Tdn[datNR$site=="hd"&datNR$loc=="canopy"&datNR$year==2017],
		type="p", lwd=2, col="red",pch=16)		
points(datTair$Time[datTair$site=="hd"&datTair$year==2017],
	datTair$TempC.VP4[datTair$site=="hd"&datTair$year==2017], pch=16, col=rgb(77/255,160/255,243/255,.5))
	
	
legend(90, 25,  c("Upward","Downward", "Air temp sensor"), 
			col=c("black","red",rgb(77/255,160/255,243/255,.5) ), pch=16, bty="n")	
		
plot(datNR$Time[datNR$site=="ld"&datNR$loc=="canopy"&datNR$year==2017],
	datNR$Tup[datNR$site=="ld"&datNR$loc=="canopy"&datNR$year==2017],
		type="p", lwd=2, col="black", pch=16, ylim=c(-35,28), xlab="Day of year 2017", 
		ylab="Temp (C) from longwave", xlim=c(90,230), main="low density")				
		
points(datNR$Time[datNR$site=="ld"&datNR$loc=="canopy"&datNR$year==2017],
	datNR$Tdn[datNR$site=="ld"&datNR$loc=="canopy"&datNR$year==2017],
		type="p", lwd=2, col="red", pch=16)
		
points(datTair$Time[datTair$site=="ld"&datTair$year==2017],
	datTair$TempC.VP4[datTair$site=="ld"&datTair$year==2017], pch=16, col=rgb(77/255,160/255,243/255,.5))
		
legend(90, 25,  c("Upward","Downward", "Air temp sensor"), 
			col=c("black","red",rgb(77/255,160/255,243/255,.5) ), pch=16, bty="n")	
			
			
#read in bonanza data to see how it compares
datBR <- read.csv("c:\\Users\\hkropp\\Documents\\lw_check\\bonanza.csv", na.strings=c("NA","NULL"))

unique(datBR$measurement)

library(lubridate)
dateBR <- as.Date(datBR$date, "%m/%d/%Y")
datBR$doy <- yday(dateBR)

datBR$Time <- datBR$doy+(datBR$hour/2400)
datBR$year <- year(dateBR)

#find out what years have the most measurements
datBRna <- datBR[is.na(datBR$value)==FALSE,]

datCount <- aggregate(datBRna$value, by=list(datBRna$year), FUN="length")

datLU <- datBR[datBR$measurement=="Longwave Up",]
datLD <- datBR[datBR$measurement=="Longwave Down",]


datLUna <- datBR[is.na(datLU$value)==FALSE,]

datCount <- aggregate(datLUna$value, by=list(datLUna$year), FUN="length")


par(mfrow=c(2,1))
plot(datLU$Time[datLU$year==2013], datLU$value[datLU$year==2013], pch=16, ylim=c(0,600), xlab="Day of year 2013", ylab="Longwave (W/m2)",
		main="Bonanza Creek LTER data")
points(datLD$Time[datLD$year==2013], datLD$value[datLD$year==2013], pch=16,  col="red")

legend(0, 500, c("Upward","Downward"), col=c("black","red"), pch=16, bty="n")


plot(datLU$Time[datLU$year==2012], datLU$value[datLU$year==2012], pch=16, ylim=c(0,600), xlab="Day of year 2014", ylab="Longwave (W/m2)",
		main="Bonanza Creek LTER data")
points(datLD$Time[datLD$year==2012], datLD$value[datLD$year==2012], pch=16,  col="red")	

legend(0, 500, c("Upward","Downward"), col=c("black","red"), pch=16, bty="n")