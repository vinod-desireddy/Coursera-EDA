## The question for which we are trying to answer with the plot is -
# Q5.How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

#Reading the data from RDS file
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#filtering for Baltimore City
nei1 = subset(NEI, fips == "24510")

#filtering for sources of motor vehicles
scc = SCC[grep('Mobile - On-Road', SCC$EI.Sector),]
str(scc)
scc$SCC = as.character(scc$SCC)

str(nei1)

#merging the datasets to filter out the emissions from the motor vehicles in baltimore
nei2 = merge(nei1, scc, by.x = 'SCC', by.y = 'SCC')

#filtering out for only relevant columns
nei = nei2[, c("SCC", "Emissions", "year")]
rm(nei1)
rm(nei2)

#Finding the total emissions from motor vehicles in baltimore for each year
emissions <- tapply(nei$Emissions, list(nei$year), sum, na.rm = T)

#coercing the emissions list to data frame
emissions <- as.data.frame(emissions)
emissions$year <- as.numeric(rownames(emissions))
rownames(emissions) <- NULL

#plot5
with(emissions, plot(year, emissions, type = 'b', pch = 19,
                     xlim = c(min(year)-1, max(year)+1),
                     ylim = c(0.9*min(emissions),1.1*max(emissions)),
                     #setting the x and y limits little bit wider so that graph looks clean
                     xlab = 'Year', ylab = 'Emissions(tons)', 
                     main = 'Total Emissions per Year from motor vehicles in baltimore'))

#Labelling the points with the value of emissions from motor vehicles in baltimore in that year
text(x = emissions$year+0.5, y = 1.025*emissions$emissions, labels = round(emissions$emissions))

## Answer - Yes, as is evident from the graph,
#  emissions from motor vehicles decreased from 1999-2008 in Baltimore City.

#copying the plot from screen device to png graphic device
dev.copy(png, 'plot5.png')
dev.off()
