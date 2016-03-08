#CDNow Case
rm(list = ls())
setwd("/Users/david/Dropbox/R/")
cdnow <- read.csv("cdnow_students_transaction.csv", stringsAsFactors = FALSE)

library(lubridate)

# aggregate to get recency, frequency, and monetary
cdnow$DATE <- as.Date(parse_date_time(cdnow$DATE, "mdy"))
recency <- aggregate(cdnow$DATE, by = list(cdnow$ID), FUN = max)
names(recency) <- c("ID", "R")
recency$R <- as.numeric(max(recency$R)-recency$R)
frequency <- aggregate(cdnow$DATE, by = list(cdnow$ID), FUN = length)
names(frequency) <- c("ID", "F")
monetary <- aggregate(cdnow$DOLLARS, by = list(cdnow$ID), FUN = sum)
names(monetary) <- c("ID", "M")

# merge to single data frame
cdrfm <- merge(recency, frequency, by = intersect(names(recency), names(frequency)))
cdrfm <- merge(cdrfm, monetary, by = intersect(names(cdrfm), names(monetary)))
