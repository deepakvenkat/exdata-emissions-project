# R script to read the 
library("dplyr")
library("tidyr")
library("ggplot2")
# NEI <- readRDS("data/summarySCC_PM25.rds")
# SCC_code <- readRDS("data/Source_Classification_Code.rds")

#This function is to remove the outliers from a give data vector. 
# The outliers are as per the definition of outliers in Wikipedia. 
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# create a vector of coal codes
coal_code <- SCC_code %>%
  filter(grepl("[Cc]omb", Short.Name), grepl("Coal", Short.Name)) %>%
  select(SCC)
coal <- as.numeric(coal_code$SCC)

# Pic only those observations which have the SCC equal to a coal code. 
res_coal <- NEI %>%
  filter(SCC %in% coal_code$SCC) %>%
  mutate(year = factor(year)) %>%
  mutate(SCC = factor(SCC))

# create a summary data frame which contains a total emissions columns
# grouped by year and SCC. This will give a row for each city, the total emissions 
# in that SCC
coal_summary <- res_coal %>%
  group_by(year, SCC) %>%
  summarise(totalEmissions = sum(Emissions))


# Use the function above to remove the outliers. 
coal_summary$totalEmissions <- remove_outliers(coal_summary$totalEmissions)

# Sample of the coal summary which does not fall under the outlier
sample_summary <- coal_summary %>%
  filter(!is.na(totalEmissions))

#Creating a box plot of the sample above per year
# shows how the total emissions have changed over the year. 
png(filename = "plot4.png", width = 600, height = 600)
boxplot(totalEmissions ~ year, data = sample, 
        main = "Comparing Coal emissions from combustion for all of US",
        xlab = "Year", col  = "blue")
dev.off()