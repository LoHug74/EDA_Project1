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