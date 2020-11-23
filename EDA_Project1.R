library(dplyr)
library(lubridate)

# set or reset chart dimensions
par (mfrow = c (1,1), mar = c(4,4,1,0))

#### READ file
dt <-read.csv("household_power_consumption.txt",sep = ";")

# data reduction to relevant dates
dt <- dt %>%  mutate (Date = dmy(Date)) %>%
              filter (Date > as.Date("2007-01-31"), Date < as.Date("2007-02-03")) %>%
              mutate (
                      DateTime = paste (Date, Time, sep = " ")
                    , DateT = ymd_hms(DateTime) 
                    ) %>%
              mutate (dayW = wday(DateT, label = TRUE, abbr = TRUE)) %>%
              select (-c(Date,Time,DateTime))
              
              
# data conversion
dt$Global_active_power[is.na(dt$Global_active_power)] = 0 
dt$Global_active_power <- as.numeric(dt$Global_active_power)

dt$Sub_metering_1[is.na(dt$Sub_metering_1)] = 0 
dt$Sub_metering_1 <- as.numeric(dt$Sub_metering_1)

dt$Sub_metering_2[is.na(dt$Sub_metering_2)] = 0 
dt$Sub_metering_2 <- as.numeric(dt$Sub_metering_2)

dt$Sub_metering_3[is.na(dt$Sub_metering_3)] = 0 
dt$Sub_metering_3 <- as.numeric(dt$Sub_metering_3)

### make plot 1
with (dt, hist(Global_active_power
                      , breaks = 15
                      , col = 'red'
                      , main = "Global Active Power"
                      , xlab = "Global Active Power (kilowatts)"
                      )
      )
dev.copy(png, file = "Plot1.png")
dev.off()

### make plot 2
with (dt, plot(x = DateT, y = Global_active_power
                , type = 'l'
                , ylab = "Global Active Power (kilowatts)"
                ,  xlab = ""
                )
      )
dev.copy(png, file = "Plot2.png")
dev.off()

### make plot 3
# set max y axis
ylim_val <-max(c(dt$Sub_metering_1,dt$Sub_metering_3,dt$Sub_metering_3))
# plot graph
with (dt, plot(x = DateT, y = Sub_metering_1
               , type = 'l'
               , ylab = "Energy sub metering"
               , xlab = ""
               , ylim = c(0,ylim_val)
               , col = 'black'
              )
)
lines (x = dt$DateT, y = dt$Sub_metering_2, col = 'red' )
lines (x = dt$DateT, y = dt$Sub_metering_3, col = 'blue')
legend("topright", lty=1, cex=1, col = c("black","red","blue"), legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))

dev.copy(png, file = "Plot3.png")
dev.off()

### make plot 4
# set window
par (mfrow = c (2,2), mar = c(4,3,1,0), oma =c(0,0,0,0))
# pot graph
with (dt, {plot(x = DateT, y = Global_active_power
                , type = 'l'
                , ylab = "Global Active Power (kilowatts)"
                ,  xlab = "")
  
            plot(x = DateT, y = Voltage
                 , type = 'l'
                 , ylab = "Voltage"
                 , xlab = "datetime")
            plot(x = DateT, y = Sub_metering_1
                 , type = 'l'
                 , ylab = "Energy sub metering"
                 , xlab = ""
                 , col = 'black'
                )
              lines (x = dt$DateT, y = dt$Sub_metering_2, col = 'red' )
              lines (x = dt$DateT, y = dt$Sub_metering_3, col = 'blue')
              legend("topright", lty=1, cex=1, col = c("black","red","blue"), legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))

            plot(x = DateT, y = Global_reactive_power
                   , type = 'l'
                   , ylab = "Global reactive power"
                   , xlab = "datetime"
                   , col = 'black'
              )
  })

dev.copy(png, file = "Plot4.png")
dev.off()

