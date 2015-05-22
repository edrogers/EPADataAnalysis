# 21 May 2015 - plot1.R - Ed Rogers
#
# Have total emissions from PM2.5 decreased in 
# the United States from 1999 to 2008? Using the 
# base plotting system, make a plot showing the 
# total PM2.5 emission from all sources for each 
# of the years 1999, 2002, 2005, and 2008.

library(dplyr)

# This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

yearlyTotalEmissions <- NEI %>% group_by(year) %>% summarize("Yearly Total Emissions" = sum(Emissions))

png("plot1.png")
#options(scipen=5)
par(mar = c(3,6,4,2)+0.1)
par(mgp = c(4,1,0))
plot(yearlyTotalEmissions$year,yearlyTotalEmissions$"Yearly Total Emissions",
     xlab = "",
     ylab = expression('Tons of PM'[2.5]),
     las=1,
     main=expression("United States Total PM"[2.5]*" Emissions by Year"))
abline(lm(yearlyTotalEmissions$"Yearly Total Emissions" ~ yearlyTotalEmissions$year))
dev.off()
