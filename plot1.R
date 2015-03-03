# library to filter and summarize the data
library("dplyr")

#Read the data
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC_code <- readRDS("data/Source_Classification_Code.rds")

# Transform the year column into factor
NEI <- transform(NEI, year <- factor(year))

# Using the group_by and summarize functions of the dplyr package
# res1 contains two columns year, total_emissions
res1 <- NEI %>%
  group_by(year) %>%
  summarize(total_emissions = sum(Emissions))

#Using base plot plot the Year vs Total Emissions
# Draw a straight line through the plot
# the slope (positive) shows that the emissions have gone up
png(filename = "plot1.png", width = 540, height = 540)
plot(res1$year, res1$total_emissions, col = "red", 
     xlab = "Year", 
     ylab = "Total Emissions", 
     main = "Total Emissions by Year")
model <- lm(total_emissions ~ year, res1)
abline(model, lwd = 2, lty = 4)
dev.off()