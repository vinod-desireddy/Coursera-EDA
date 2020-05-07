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

#creating the Graphic Device
png(filename = "plot1.png")

#plotting the graph
with(data, hist(Global_active_power, col = 'red', 
                xlab = 'Global Active Power(kilowatts)',
                main = 'Global Active Power'))

dev.off()#closing the Graphic Device connection