## The question for which we are trying to answer with the plot is -

# Q1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using 
# the base plotting system, make a plot showing the total PM2.5 emission from all sources 
# for each of the years 1999, 2002, 2005, and 2008.

#Reading the data from RDS file
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

head(NEI)
str(NEI)
summary(NEI)
head(SCC)

##Finding the total emissions from all sources for each year
tot_em_per_year <-  tapply(NEI$Emissions, NEI$year, sum, na.rm = T)
years <- as.numeric(names(tot_em_per_year))

#plot1
plot(years, tot_em_per_year, type = 'b', pch = 19,
     xlim = c(min(years)-1, max(years)+1),
     ylim = c(min(tot_em_per_year)-100000,max(tot_em_per_year)+100000),
     #setting the x and y limits little bit wider so that graph looks clean
     xlab = 'Year', ylab = 'Emissions(tons)', 
     main = 'Total Emissions per Year from all Sources')

#Labelling the points with the value of total emissions from all sources in that year
text(x = years+0.5, y = tot_em_per_year+150000, labels = round(tot_em_per_year))

##     Answer - Yes, as is evident from the graph, total emissions from 
#      PM2.5 decreased in the United States from 1999 to 2008.

#copying the plot from screen device to png graphic device
dev.copy(png, 'plot1.png')
dev.off()
