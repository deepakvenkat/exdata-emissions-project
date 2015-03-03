# R script to read the 
library("dplyr")
library("tidyr")
library("ggplot2")
# NEI <- readRDS("data/summarySCC_PM25.rds")
# SCC_code <- readRDS("data/Source_Classification_Code.rds")

# Get all motor vehicles (ON ROAD) and add a column called baltimore
resultBaltimore <- NEI %>%
  filter(fips == "24510", type == "ON-ROAD") %>%
  mutate(city = "Baltimore")

# SUmmarize the result with summation
resultSummary <- resultBaltimore %>%
  mutate(year = factor(year)) %>%
  group_by(year) %>%
  summarize(totalEmissions = sum(Emissions))
#Plot year vs Total Emissions which shows whether emissions have gone 
# down or up. 
png(filename = "plot5.png", width = 540, height = 540)
g <- ggplot(data = resultSummary, aes(x = year, y = totalEmissions)) + 
  geom_bar(colour = "black", fill = "royalblue1", stat = "identity") +
  xlab("Year") + 
  ylab("Total Emissions") +
  ggtitle("Total Emissions from Motor Vehicles for Baltimore city.")
print(g)
dev.off()
