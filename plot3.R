## Assumes that 'household_power_consumption.txt' is in your
## current working directory

plot3 <- function() {
  
  power_data          <- read.table("household_power_consumption.txt",header=TRUE,sep=";",stringsAsFactors=FALSE)
  
  power_data$Date     <- as.Date(power_data$Date,format="%d/%m/%Y")
  
  first               <- as.Date("2007-02-01",format="%Y-%m-%d")
  
  second              <- as.Date("2007-02-02",format="%Y-%m-%d")
  
  power_data          <- power_data[power_data$Date == first | power_data$Date == second,]
  
  dateAndTimeStr      <- function(date,timeString) {
    
    paste(date,timeString,sep=" ")
    
  }
  
  power_data$DateTime <- mapply(dateAndTimeStr,power_data$Date,power_data$Time)
  power_data$DateTime <- as.vector(strptime(power_data$DateTime,format="%Y-%m-%d %H:%M:%S"))
  
  power_data$Global_active_power <- as.numeric(power_data$Global_active_power)
  power_data$Sub_metering_1      <- as.numeric(power_data$Sub_metering_1)
  power_data$Sub_metering_2      <- as.numeric(power_data$Sub_metering_2)
  power_data$Sub_metering_3      <- as.numeric(power_data$Sub_metering_3)
  
  png("plot3.png",width=480,height=480,units="px")
  with(power_data,plot(DateTime, Sub_metering_1, type="l", xlab="",ylab="Enery sub metering"))
  with(power_data,lines(DateTime, Sub_metering_2, type="l", col="red"))
  with(power_data,lines(DateTime, Sub_metering_3, type="l", col="blue"))  
  legend("topright",lty=1,col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
  dev.off()
}
## Generate the plot
plot3()
