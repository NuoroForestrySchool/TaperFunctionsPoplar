# How to filter (in a compact way) when OR is required
# https://stackoverflow.com/questions/21636798/better-way-to-filter-a-data-frame-with-dplyr-using-or


library(dplyr)

data <- data.frame(
  subject1 = c("History", "Biology", "Physics", "Digital Humanities"),
  subject2 = c("Chemistry", "Religion", "Chemistry", "Religion")
)
condition <- c("History", "Religion")
subset <- filter(data, subject1 %in% condition | subject2 %in% condition)

data
condition
filter(data, Reduce('|', lapply(data, '%in%', condition)))
