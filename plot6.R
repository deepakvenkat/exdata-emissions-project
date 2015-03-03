# R script to read the 
library("dplyr")
library("tidyr")
library("ggplot2")
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC_code <- readRDS("data/Source_Classification_Code.rds")


# Separate out Baltimore using fips and motor vehicles using on road
resultBaltimore <- NEI %>%
  filter(fips == "24510", type == "ON-ROAD") %>%
  mutate(city = "Baltimore")

# Separate out LA using fips and motor vehicles using on road
resultLA <- NEI %>%
  filter(fips == "06037", type == "ON-ROAD") %>%
  mutate(city = "Los Angeles")

#Combine LA and Baltimore. 
result <- bind_rows(resultBaltimore, resultLA)

# Find the total emissions by city and year
resultSummary <- result %>%
  mutate(year = factor(year), city = factor(city)) %>%
  group_by(city, year) %>%
  summarize(totalEmissions = sum(Emissions))

# Plot a line graph to indicate the difference
# in slope between the two cities
png(filename = "plot6.png", width = 540, height = 540)
g <- ggplot(resultSummary, aes(x = year, y = totalEmissions, color = city, group = city)) + 
  geom_point() +
  geom_line() +
  xlab("Year") +
  ylab("Total Emissions") +
  ggtitle("Comparing total Emissions for Motor vehicles")
  
print(g)
dev.off




