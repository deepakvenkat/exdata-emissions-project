# library to filter and summarize the data
library("dplyr")

#Read the data
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC_code <- readRDS("data/Source_Classification_Code.rds")

# Transform the year column into factor
NEI <- transform(NEI, year <- factor(year))

# Using the group_by, filter and summarize functions of the dplyr package
# The filter uses the fips of 24510 for Baltimore.
# res1 contains two columns year, total_emissions
res1 <- NEI %>%
  filter(fips == "24510") %>%
  group_by(year) %>%
  summarize(total_emissions = sum(Emissions))

#Using base plot plot the Year vs Total Emissions (Baltimore)
# Draw a straight line through the plot
# the slope (positive) shows that the emissions have gone up
png(filename = "plot2.png", width = 540, height = 540)
plot(res1$year, res1$total_emissions, col = "red", 
     xlab = "Year", 
     ylab = "Total Emissions", 
     main = "Change of Emissions by Year for Baltimore")
model <- lm(total_emissions ~ year, res1)
abline(model, lwd = 2, lty = 4)
dev.off()