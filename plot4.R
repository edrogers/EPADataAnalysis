# 21 May 2015 - plot4.R - Ed Rogers
#
# Across the United States, how have emissions from 
# coal combustion-related sources changed from 1999-2008?

library(dplyr)
library(ggplot2)

# This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# What SCC codes are associated with coal combustion?
coalSectors <- grep("coal",levels(SCC$EI.Sector),ignore.case = TRUE,value=TRUE)
coalCodes <- SCC[ SCC$EI.Sector %in% coalSectors, "SCC" ]
#What are the associated emissions?
coalEmissions <- NEI[ NEI$SCC %in% coalCodes,c("year","SCC","Emissions")]
yearlyCoalEmissions <- coalEmissions %>% group_by(year) %>% summarize("Emissions" = sum(Emissions))

png("plot4.png")
options(scipen=0)
p = ggplot(data = yearlyCoalEmissions,aes(year,Emissions))+
  geom_point(size=4)+
  xlab("")+
  ylab(expression("Tons of PM"[2.5]))+
  ggtitle(expression("United States PM"[2.5]*" Emissions from Coal Combustion by Year"))+
  theme(legend.title=element_blank())+
  stat_smooth(method="lm", se=FALSE)
print(p)
dev.off()
