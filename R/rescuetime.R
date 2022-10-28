library(dplyr)

data20 <- read.csv("./data/rescuetime-data-2020-09-01-2020-09-30.csv")
data21 <- read.csv("./data/rescuetime-data-2021-09-01-2021-09-30.csv")
data22 <- read.csv("./data/rescuetime-data-2022-09-01-2022-09-30.csv")

hourly <- full_join(full_join(data20, data21), data22);

# Parsing
hourly[is.na(hourly)] <- 0
hourly$date <- strptime(hourly$date, format = "%Y-%m-%dT%H:%M:%OS");

# Sum productive time
hourly$productive <- hourly$communication + hourly$development + hourly$learning + hourly$business + hourly$design + hourly$focus
hourly$distracted <- hourly$entertainment + hourly$social + hourly$shopping + hourly$news
hourly$neutral <- hourly$uncategorized + hourly$utilities + hourly$personal + hourly$undefined # Ignoring data$misc as its empty
hourly$aggregate <- hourly$productive - hourly$distracted
hourly$total <- hourly$productive + hourly$distracted + hourly$neutral

# Extract date details into new columns
hourly$year <- as.numeric(format(hourly$date, format = "%Y"))
hourly$day <- as.numeric(format(hourly$date, format = "%d"))
hourly$hour <- as.numeric(format(hourly$date, format = "%H"))

head(hourly)

# Explore total time spent on devices per hour
hist(hourly$total, xlab="Total time spent (seconds)", main="Frequency of total time spent")
range(hourly$total)
boxplot(hourly$total, ylab="Total time spent (seconds)")

# Productivity by hour
hourly2020 <- filter(hourly, year=="2020")
hourly2021 <- filter(hourly, year=="2021")
hourly2022 <- filter(hourly, year=="2022")

par(mfrow=c(3,1))
boxplot(hourly2020$total ~ hourly2020$hour)
boxplot(hourly2021$total ~ hourly2021$hour)
boxplot(hourly2022$total ~ hourly2022$hour)

# Calculate productivity for each day
daily <- aggregate(list(productive=hourly$productive, distracted=hourly$distracted, aggreagate=hourly$aggregate), by=list(year=hourly$year, day=hourly$day), FUN=sum)
head(daily)

par(mfrow=c(1,1))

boxplot(daily$aggreagate, ylab="Aggregate Productivity")

# Plot daily productivity by year
boxplot(daily$aggreagate ~ daily$year, xlab="Year", ylab="Aggregate Productivity")
range(daily$aggreagate)

# Extract data for each year
daily2020 <- filter(daily, year=="2020")
daily2021 <- filter(daily, year=="2021")
daily2022 <- filter(daily, year=="2022")

# Shapiro-Wilk test
shapiro.test(daily2020$aggreagate)
shapiro.test(daily2021$aggreagate)
shapiro.test(daily2022$aggreagate)

# Plot histogram
par(mfrow=c(3,1))
hist(daily2020$aggreagate, xlab = "Aggregate Productivity", main="2020")
hist(daily2021$aggreagate, xlab = "Aggregate productivity", main="2021")
hist(daily2022$aggreagate, xlab = "Aggregate productivity", main="2022")

# perform T-Test
t.test(daily2020$aggreagate, daily2021$aggreagate)
t.test(daily2021$aggreagate, daily2022$aggreagate)
