#loading the 10rows of data initially
data <- read.table('household_power_consumption.txt', nrows = 10, sep = ';', header = T)
str(data)

colnames(data)#checking for the names of the columns

sapply(data, class)#checking for the class of the each column

#loading the whole data set
data1 <- read.table('household_power_consumption.txt', sep = ';', header = T,
                    col.names = c("Date","Time","Global_active_power","Global_reactive_power",
                                  "Voltage","Global_intensity","Sub_metering_1",
                                  "Sub_metering_2","Sub_metering_3"))

data1$Date = as.Date(data1$Date, format = '%d/%m/%Y')

#filtering the dataset only for the required dates
data = subset(data1, Date == '2007-02-01' | Date == '2007-02-02')

data$Time = strptime(paste(data$Date, data$Time, sep = '-'), format = "%Y-%m-%d-%H:%M:%S")
str(data)

#coercing the character columns to numeric
data[,3:8] = sapply(data[, 3:8], as.numeric)
str(data)
#data$weekday = weekdays(data$Date, abbreviate = T)
data$weekday = as.factor(weekdays(data$Date, abbreviate = T))
data = data[,c(1, 10, 2:9)]


#creating the Graphic Device
png(filename = "plot3.png")

#plotting the graph
with(data, plot(Sub_metering_1, type = 'l', col = 'black', xaxt = 'n', 
                xlab = '', ylab = 'Energy sub metering',
                ylim = c(0,max(max(Sub_metering_1),max(Sub_metering_2),max(Sub_metering_3))+0.1)))
lines(data$Sub_metering_2, col = 'red')
lines(data$Sub_metering_3, col = 'blue')
axis(1, at = c(0,1500,2879), lab = c('Thu','Fri','Sat'))
legend("topright", "(0,0)", lty = 1, lwd = 2, legend = c('Sub_metering_1','Sub_metering_2','Sub_metering_3'),
       col = c('black','red','blue'))

dev.off()#closing the Graphic Device connection