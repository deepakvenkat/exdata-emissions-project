# library to filter and summarize the data
library("dplyr")
library("ggplot2")

#Read the data
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC_code <- readRDS("data/Source_Classification_Code.rds")

# Transform the year and type columns into factor
NEI <- transform(NEI, type = factor(type))

# dplyr with fips == 24510 for Baltimore
# group_by year and type 
res_type <- NEI %>%
  filter(fips == "24510") %>%
  group_by(year, type) %>%
  summarize(total_emissions = sum(Emissions))

png(filename = "plot3.png", width = 540, height = 540)
p <- qplot(year, total_emissions, data = res_type, 
      facets = type ~., method = "lm", 
      color = type, geom = c("point", "smooth")) + 
      xlab("Year") + 
      ylab ("Total Emissions") +
      ggtitle("Chnage in Emissions by type over the years")
print(p)
dev.off()