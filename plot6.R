# 21 May 2015 - plot6.R - Ed Rogers
#
# Compare emissions from motor vehicle sources in 
# Baltimore City with emissions from motor vehicle 
# sources in Los Angeles County, California 
# (fips == "06037"). Which city has seen greater 
# changes over time in motor vehicle emissions?

library(dplyr)
library(ggplot2)
library(gridExtra)
library(scales)

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
# Subset to Baltimore City & Los Angeles County
cityMotorVehicleEmissions <- mVehicleEmissions[ mVehicleEmissions$fips %in% c("24510","06037"),]
cityMotorVehicleEmissions$fips <- factor(cityMotorVehicleEmissions$fips,labels=c("Los Angeles County","Baltimore City"))
# Sum the emissions by year
yearlyEmissions <- cityMotorVehicleEmissions %>% group_by(year,fips) %>% summarize("Emissions" = sum(Emissions))
# Now scale them to the 1999 values, to compare relative change
yearlyEmissionsPercent <- ungroup(yearlyEmissions) %>% group_by(fips) %>% mutate(Emissions = Emissions/first(Emissions))
png("plot6.png",width=1000,height=480)
absolutePlot = ggplot(data = yearlyEmissions,aes(year,Emissions,group=yearlyEmissions$fips,color=yearlyEmissions$fips))+
  geom_point(size=4)+
  geom_line()+
  xlab("")+
  ylab(expression("Tons of PM"[2.5]))+
  ggtitle(expression(atop("PM"[2.5]*" Emissions from Motor Vehicles by Year", atop(italic("Total Yearly Emissions Between Cities"), ""))))+
  theme(legend.title=element_blank())
percentPlot = ggplot(data = yearlyEmissionsPercent,aes(year,Emissions,group=yearlyEmissionsPercent$fips,color=yearlyEmissionsPercent$fips,fill=yearlyEmissionsPercent$fips))+
  geom_point(size=4)+
  geom_line()+
  xlab("")+
  ylab(expression("Percentage PM"[2.5]))+
  ggtitle(expression(atop("PM"[2.5]*" Emissions from Motor Vehicles by Year", atop(italic("Percentage of Emissions Relative to 1999 Measurements"), ""))))+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=percent,limits=c(0,1))
grid.arrange(absolutePlot,percentPlot,ncol=2)
dev.off()
