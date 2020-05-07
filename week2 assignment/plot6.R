## The question for which we are trying to answer with the plot is -
# Q6.Compare emissions from motor vehicle sources in Baltimore City 
#     with emissions from motor vehicle sources in Los Angeles County, California 
#     (fips=="06037"). Which city has seen greater changes over time in motor vehicle emissions?

#Reading the data from RDS file
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#filtering for Baltimore City
baltimore = subset(NEI, fips == "24510")

#filtering for LA, California
LA = subset(NEI, fips == "06037")

#filtering for sources of motor vehicles
scc = SCC[grep('Mobile - On-Road', SCC$EI.Sector),]
str(scc)
scc$SCC = as.character(scc$SCC)

#merging the datasets to filter out the emissions from the motor vehicles in baltimore
em_baltimore = merge(baltimore, scc, by.x = 'SCC', by.y = 'SCC')

#merging the datasets to filter out the emissions from the motor vehicles in california
em_LA = merge(LA, scc, by.x = 'SCC', by.y = 'SCC')

#filtering out for only relevant columns
em_baltimore = em_baltimore[, c("SCC", "Emissions", "year")]
em_LA = em_LA[, c("SCC", "Emissions", "year")]

#Finding the total emissions from motor vehicles in baltimore for each year
em_bm_year <- tapply(em_baltimore$Emissions, list(em_baltimore$year), sum, na.rm = T)

#coercing the em_bm_year list to data frame
em_bm_year <- as.data.frame(em_bm_year)
em_bm_year$year <- as.numeric(rownames(em_bm_year))
rownames(em_bm_year) <- NULL

#Finding the total emissions from motor vehicles in LA for each year
em_LA_year <- tapply(em_LA$Emissions, list(em_LA$year), sum, na.rm = T)

#coercing the em_LA_year list to data frame
em_LA_year <- as.data.frame(em_LA_year)
em_LA_year$year <- as.numeric(rownames(em_LA_year))
rownames(em_LA_year) <- NULL

#plot6
par(mfrow = c(1,2))
#plotting the emissions from motor vehicles in baltimore in each year
with(em_bm_year, plot(year, em_bm_year, type = 'b', pch = 19,
                     xlim = c(min(year)-1, max(year)+1),
                     ylim = c(0.9*min(em_bm_year),1.1*max(em_bm_year)),
                     #setting the x and y limits little bit wider so that graph looks clean
                     xlab = 'Year', ylab = 'Emissions(tons)', 
                     main = 'From motor vehicles in baltimore'))

#Labelling the points with the value of emissions from motor vehicles in baltimore in that year
text(x = em_bm_year$year+0.5, y = 1.025*em_bm_year$em_bm_year, labels = round(em_bm_year$em_bm_year))

#plotting the emissions from motor vehicles in LA in each year
with(em_LA_year, plot(year, em_LA_year, type = 'b', pch = 19,
                     xlim = c(min(year)-1, max(year)+1),
                     ylim = c(0.9*min(em_LA_year),1.1*max(em_LA_year)),
                     #setting the x and y limits little bit wider so that graph looks clean
                     xlab = 'Year', ylab = 'Emissions(tons)', 
                     main = 'From motor vehicles in LA'))

#Labelling the points with the value of emissions from motor vehicles in LA in that year
text(x = em_LA_year$year+0.5, y = 1.025*em_LA_year$em_LA_year, labels = round(em_LA_year$em_LA_year))

## Answer - Yes, as is evident from the graph,
#  emissions from motor vehicles decreased from 1999-2008 in Baltimore City.
#  emissions from motor vehicles slightly increased from 1999-2008 in LA.

#copying the plot from screen device to png graphic device
dev.copy(png, 'plot6.png')
dev.off()
