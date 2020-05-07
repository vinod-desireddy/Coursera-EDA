## The question for which we are trying to answer with the plot is -

# Q3. Of the four types of sources indicated by the 
# type(point, nonpoint, onroad, nonroad) variable, which of these 
# four sources have seen decreases in emissions from 1999-2008 for 
# Baltimore City? Which have seen increases in emissions from 1999-2008? 
# Use the ggplot2 plotting system to make a plot answer this question.

#Reading the data from RDS file
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#filtering for Baltimore City
nei = subset(NEI, fips == "24510")

head(nei)
str(nei)
summary(nei)
head(SCC)

nei$type = as.factor(nei$type)
nei$year = as.factor(nei$year)

#Finding the total emissions from all sources for each year
emissions <- tapply(nei$Emissions, list(nei$year,nei$type), sum, na.rm = T)

#coercing the emissions list to data frame
emissions <- as.data.frame(emissions)
emissions$year <- as.factor(rownames(emissions))
rownames(emissions) <- NULL
emissions = emissions[,c(5,1:4)]

#library(tidyr)
#using the gather function, making the emissions data frame in to the plottable format.
emissions <- gather(emissions, Type, 'Emission_tons', colnames(emissions)[2:5])

#plot3
library(ggplot2)
ggplot(data = emissions,aes(x = year, y = Emission_tons))+
      geom_point()+
      facet_grid(Type~.)+
      ggtitle('Total Emissions in the Baltimore City')+
      coord_cartesian(ylim = c(0, 2300))

## Answer - Yes, as is evident from the graph,
# Of the four types of sources indicated by the 
# type(point, nonpoint, onroad, nonroad) variable,
# point has seen slight increase in emissions from 1999-2008.
# nonpoint, onroad, nonroad have seen decreases in emissions from 1999-2008.

#copying the plot from screen device to png graphic device
dev.copy(png, 'plot3.png')
dev.off()
