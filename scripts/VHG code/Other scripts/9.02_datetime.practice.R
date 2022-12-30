

stuff = read.csv(file.choose(),header=TRUE)

colnames(stuff)[1] = 'dateTime'

library(lubridate)

stuff$dateTime.fixed = parse_date_time(stuff$dateTime, c("%m/%d/%Y %H:%M", #identify format
                                                         "%Y-%m-%d %H:%M")) # set new format



str(stuff)

stuff$dateTime.fixed = as.Date(stuff$dateTime, format="%m/%d/%Y %h:%m")


write.csv(stuff,"fixedDateTimes.csv" )
str(stuff)
