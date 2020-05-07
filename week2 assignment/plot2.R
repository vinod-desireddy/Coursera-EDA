## The question for which we are trying to answer with the plot is -

# Q2. Have total emissions from PM2.5 decreased in the Baltimore City, 
#    Maryland (fips=="24510") from 1999 to 2008? 
#    Use the base plotting system to make a plot answering this question.

#Reading the data from RDS file
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

head(NEI)
str(NEI)
summary(NEI)
head(SCC)

#filtering for Baltimore City
nei = subset(NEI, fips == "24510")

#Finding the total emissions from all sources for each year
tot_em_per_year <-  tapply(nei$Emissions, nei$year, sum, na.rm = T)
years <- as.numeric(names(tot_em_per_year))

#plot2
plot(years, tot_em_per_year, type = 'b', pch = 19,
     xlim = c(min(years)-1, max(years)+1),
     ylim = c(0.9*min(tot_em_per_year),1.1*max(tot_em_per_year)),
     #setting the x and y limits little bit wider so that graph looks clean
     xlab = 'Year', ylab = 'Emissions(tons)', 
     main = 'Total Emissions per Year in the Baltimore City')

#Labelling the points with the value of total emissions from all sources in that year
text(x = years+0.5, y = 1.025*tot_em_per_year, labels = round(tot_em_per_year))

## Answer - Yes, as is evident from the graph,
#  total emissions from PM2.5 decreased in the Baltimore City

#copying the plot from screen device to png graphic device
dev.copy(png, 'plot2.png')
dev.off()
