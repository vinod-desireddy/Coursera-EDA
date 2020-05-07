## The question for which we are trying to answer with the plot is -

# Q4.Across the United States, how have emissions from coal combustion-related 
# sources changed from 1999-2008?

#Reading the data from RDS file
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


#filtering for sources of coal
scc = SCC[grep('Coal', SCC$Short.Name),]
str(scc)
scc$SCC = as.character(scc$SCC)

str(NEI)

#merging the datasets to filter out the emissions from the coal related sources
nei = merge(NEI, scc, by.x = 'SCC', by.y = 'SCC')

#filtering out for only relevant columns
nei = nei[, c("SCC", "Emissions", "year")]

#Finding the total emissions from coal sources for each year
emissions <- tapply(nei$Emissions, list(nei$year), sum, na.rm = T)

#coercing the emissions list to data frame
emissions <- as.data.frame(emissions)
emissions$year <- as.numeric(rownames(emissions))
rownames(emissions) <- NULL

#plot4
with(emissions, plot(year, emissions, type = 'b', pch = 19,
                     xlim = c(min(year)-1, max(year)+1),
                     ylim = c(0.9*min(emissions),1.1*max(emissions)),
                     #setting the x and y limits little bit wider so that graph looks clean
                     xlab = 'Year', ylab = 'Emissions(tons)', 
                     main = 'Total Emissions per Year from coal Sources'))

#Labelling the points with the value of total emissions from coal sources in that year
text(x = emissions$year+0.5, y = 1.025*emissions$emissions, labels = round(emissions$emissions))

## Answer - Yes, as is evident from the graph,
#  Across the United States, the emissions from coal combustion-related 
#  sources decreased from 1999-2008

#copying the plot from screen device to png graphic device
dev.copy(png, 'plot4.png')
dev.off()
