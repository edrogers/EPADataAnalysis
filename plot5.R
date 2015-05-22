# 21 May 2015 - plot5.R - Ed Rogers
#
# How have emissions from motor vehicle sources changed 
# from 1999-2008 in Baltimore City?

library(dplyr)
library(ggplot2)

# This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# What SCC codes are associated with motor vehicles?
allVehicles   <- grep("vehicle",levels(SCC$Short.Name),ignore.case = TRUE,value=TRUE)
# Note: Vehicle Cooking is probably not "motor vehicle source. 
# Exclude "vehicle cooking as a source of codes
motorVehicles <- grep("vehicle cooking",allVehicles,ignore.case = TRUE,invert=TRUE,value = TRUE)
# Use the codes to select rows and relevant columns from NEI
motorVehicleCodes <- SCC[ SCC$Short.Name %in% motorVehicles, "SCC" ]
mVehicleEmissions <- NEI[ NEI$SCC %in% motorVehicleCodes,
                              c("year","SCC","fips","Emissions")]
# Subset to Baltimore City
bCityMotorVehicleEmissions <- mVehicleEmissions[ mVehicleEmissions$fips==24510,]
# Sum the emissions by year
yearlyEmissions <- bCityMotorVehicleEmissions %>% group_by(year) %>% summarize("Emissions" = sum(Emissions))

png("plot5.png")
# options(scipen=0)
p = ggplot(data = yearlyEmissions,aes(year,Emissions))+
  geom_point(size=4)+
  xlab("")+
  ylab(expression("Tons of PM"[2.5]))+
  ggtitle(expression("Baltimore City PM"[2.5]*" Emissions from Motor Vehicles by Year"))+
  theme(legend.title=element_blank())+
  stat_smooth(method="lm", se=FALSE)
print(p)
dev.off()
