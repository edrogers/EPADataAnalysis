# 21 May 2015 - plot2.R - Ed Rogers
#
# Have total emissions from PM2.5 decreased in the 
# Baltimore City, Maryland (fips == "24510") from 
# 1999 to 2008?

library(dplyr)

# This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

baltimoreCityEmissions <- NEI[NEI$fips =="24510",c("year","Emissions")]
yearlyBCEmissions <- baltimoreCityEmissions %>% group_by(year) %>% summarize("City Total Emissions" = sum(Emissions))

png("plot2.png")
plot(yearlyBCEmissions$year,yearlyBCEmissions$"City Total Emissions",
     xlab = "",
     ylab = expression('Tons of PM'[2.5]),
     las=1,
     main=expression("Baltimore City Total PM"[2.5]*" Emissions by Year"))
abline(lm(yearlyBCEmissions$"City Total Emissions" ~ yearlyBCEmissions$year))
dev.off()
