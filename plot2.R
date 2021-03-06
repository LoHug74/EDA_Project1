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

### make plot 2
with (dt, plot(x = DateT, y = Global_active_power
               , type = 'l'
               , ylab = "Global Active Power (kilowatts)"
               ,  xlab = ""
              )
)
dev.copy(png, file = "Plot2.png")
dev.off()