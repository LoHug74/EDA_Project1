library(dplyr)
library(lubridate)

#### READ txt
dt <-read.csv("household_power_consumption.txt",sep = ";")

# data reduction to relevant dates
dt <- dt %>% mutate (DateTime = paste (dt$Date, dt$Time, sep = " ")
                    , DateT = dmy_hms(DateTime) %>%
                      select (- Date, - Time)
                    )
head(dt)
str(dt)

dt$DateTime <- paste (dt$Date, dt$Time, sep = " ")
dt$DateTime <- strptime(dt$DateTime, "%d/%m/%Y %H:%M:%S")
head(dt$DateTime)
str(dt$DateTime)
dt_short <- dt%>% filter(Date > "2007/01/31", Date < "2007/02/03")

head(dt_short$Date)
# data conversion
dt_short$Global_active_power[is.na(dt_short$Global_active_power)] = 0 
dt_short$Global_active_power <- as.numeric(dt_short$Global_active_power)

min (dt_short$Global_active_power, na.rm = TRUE)
head(dt_short$Global_active_power)

### make plot 1
with (dt_short, hist(Global_active_power
                      , breaks = 15
                      , col = 'red'
                      , main = "Global Active Power"
                      , xlab = "Global Active Power (kilowatts)"
                      )
      )
dev.copy(png, file = "Plot1.png")
dev.off()
?plot

### make plot 2
with (dt_short, plot(x = paste(Date,time), y = Global_active_power
                     , type = 'l'
                     , ylab = "Global Active Power (kilowatts)"
                    )
      )
dev.copy(png, file = "Plot1.png")
dev.off()



png("test.png",width=3.25,height=3.25,units="in",res=1200)
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
plot(perf,avg="vertical",spread.estimate="stddev",col="black",lty=3, lwd=3)
dev.off()
