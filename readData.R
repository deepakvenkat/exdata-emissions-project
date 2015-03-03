# R script to read the 
library("dplyr")
library("tidyr")
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC_code <- readRDS("data/Source_Classification_Code.rds")


NEI <- transform(NEI, year = factor(year))


res1 <- NEI %>%
  group_by(year) %>%
  summarize(total_emissions = sum(Emissions))
png(filename = "plot1.png", width = 480, height = 480)
with(res1, plot(year, total_emissions, col = "red"))
model <- lm(total_emissions ~ year, res1)
abline(model, lwd = 2, lty = 4)
dev.off()

#Question 2
res_baltimore <- NEI %>%
  filter(fips == "24510") %>%
  group_by(year) %>%
  summarize(total_emissions = sum(Emissions))

#Question 3
NEI <- transform(NEI, type = factor(type))

res_type <- NEI %>%
  filter(fips == "24510") %>%
  group_by(year, type) %>%
  summarize(total_emissions = sum(Emissions))
#qplot(Emissions, data = NEI, geom = "density", color = type)
qplot(year, total_emissions, data = res_type, facets = type ~., method = "lm", color = type, geom = c("point", "smooth")) + ylim(0, 2000)

#Question 4

coal_code <- SCC_code %>%
  filter(grepl("Coal", Short.Name)) %>%
  select(SCC)
coal <- as.numeric(coal_code$SCC)
res_coal <- NEI %>%
  filter(SCC %in% coal) %>%
  group_by(year) %>%
  
with(res_coal, plot(year, Emissions))
g <- ggplot(res_coal, aes(x = year, y = Emissions))
g + geom_line()
#Question 5

#Question 6



