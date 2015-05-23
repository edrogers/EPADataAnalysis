# 21 May 2015 - plot3.R - Ed Rogers
#
# Of the four types of sources indicated by the type 
# (point, nonpoint, onroad, nonroad) variable, which 
# of these four sources have seen decreases in emissions 
# from 1999-2008 for Baltimore City? Which have seen
# increases in emissions from 1999-2008?

library(dplyr)
library(ggplot2)

# This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

baltimoreCityEmissions <- NEI[NEI$fips =="24510",c("year","type","Emissions")]
yearlyBCEmissions <- baltimoreCityEmissions %>% group_by(year,type) %>% summarize("Emissions" = sum(Emissions))

png("plot3.png")
p = ggplot(data = yearlyBCEmissions,aes(year,Emissions,colour=yearlyBCEmissions$type))+
  geom_point(size=4)+
  geom_line()+
  xlab("")+
  ylab(expression("Tons of PM"[2.5]))+
  ggtitle(expression("Baltimore City PM"[2.5]*" Emissions by Year"))+
  theme(legend.title=element_blank())
print(p)
dev.off()
