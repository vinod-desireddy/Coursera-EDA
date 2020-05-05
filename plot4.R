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
png(filename = "plot4.png")

#plotting the graph
par(mfrow = c(2,2))
with(data, plot(Global_active_power, type = 'l', col = 'black', 
                xaxt = 'n', xlab = '', ylab = 'Global Active Power'))
axis(1, at = c(0,1500,2880), labels = c('Thu','Fri','Sat'))

with(data, plot(Voltage, type = 'l', col = 'black', 
                xaxt = 'n', xlab = 'datetime', ylab = 'Voltage'))
axis(1, at = c(0,1500,2880), labels = c('Thu','Fri','Sat'))

with(data, plot(Sub_metering_1, type = 'l', col = 'black', 
                xaxt = 'n', xlab = '', ylab = 'Energy sub metering'))
lines(data$Sub_metering_2, type = 'l', col = 'red')
lines(data$Sub_metering_3, type = 'l', col = 'blue')
axis(1, at = c(0,1500,2880), labels = c('Thu','Fri','Sat'))
legend('topright', '(0,0)', lty = 1, lwd = 1, col = c('black', 'red', 'blue'), 
       legend = c('Sub_metering_1','Sub_metering_2','Sub_metering_3'), 
       cex = 0.9, bty = 'n')

with(data,plot(Global_reactive_power, type = 'l', col = 'black', xaxt = 'n', 
               xlab = 'datetime', ylab = 'Global_reactive_power'))
axis(1, at = c(0,1500,2880), labels = c('Thu','Fri','Sat'))

dev.off()#closing the Graphic Device connection