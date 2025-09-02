##This script will generate Plot 4: A set of 4 line graphs using Household Power Consumption Data

##Load dplr and data.table libraries
  library(dplyr)
  library(data.table)

##File existence check
  if(!file.exists("household_power_consumption.txt")) {
    stop(paste("File does not exist:", "household_power_consumption.txt"))
  }

##Read/load "household_power_consumption.txt" file
  data <- read.table("household_power_consumption.txt", sep=";", header=TRUE, na.strings="NA", strip.white=TRUE, stringsAsFactors=FALSE)

##Convert date and time to date and time classes. Filter for dates between 2007-02-01 and 2007-02-02. Replace "?" with "NA". Convert columns with characters to numeric.
  data <- data %>% mutate(Date = as.Date(Date, format = "%d/%m/%Y"), Time = strptime(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S")) %>% filter(Date >= "2007-02-01" & Date <= "2007-02-02") %>% mutate(across(where(is.character), ~ na_if(., "?"))) %>% mutate(across(where(is.character), ~ as.numeric(.)))

##Remove NA values
  data <- na.omit(data)

##Create the png file for the plot
  png(filename = "plot4.png", width = 480, height = 480, units = "px")
  
##Define the parameters of the plots
  par(mfrow = c(2, 2))

##1st Plot: Plot Time on the x-axis vs. Global Active Power (kilowatts) on the y-axis as a line graph.
  with(data, {
    plot(Time, Global_active_power, type = "l", xlab = "", ylab = "Global Active Power", xaxt = "n")
  
  ##Create a sequence of dates for x-axis
    date_seq <- seq(min(data$Time), max(data$Time) + 86400, by = "day")
  
  ##Add x-axis tick marks and labels
    axis(1, at = date_seq, labels = c("Thu", "Fri", "Sat"))
    
##2nd Plot: Plot Time on the x-axis vs. Voltage on the y-axis as a line graph.
  plot(Time, Voltage, type = "l", xlab = "datetime", ylab = "Voltage", xaxt = "n")
  
  ##Create a sequence of dates for x-axis
    date_seq <- seq(min(data$Time), max(data$Time) + 86400, by = "day")
  
  ##Add x-axis tick marks and labels
    axis(1, at = date_seq, labels = c("Thu", "Fri", "Sat"))
    
##3rd Plot: Plot Time on the x-axis vs. Sub_metering_1 on the y-axis as a line graph.
  plot(Time, Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering", xaxt = "n")
    
  ##Add Sub_metering_2 and Sub_metering_3 to the graph
    with(data, points(Time, Sub_metering_2, type = "l", col = "red"))
    with(data, points(Time, Sub_metering_3, type = "l", col = "blue"))
    
  ##Create a sequence of dates for x-axis
    date_seq <- seq(min(data$Time), max(data$Time) + 86400, by = "day")
    
  ##Add x-axis tick marks and labels
    axis(1, at = date_seq, labels = c("Thu", "Fri", "Sat"))
    
  ##Add legend
    legend("topright", lty = 1, col = c("black", "red", "blue"), bty = "n", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), cex = .9)

##4th Plot: Plot Time on the x-axis vs. Global_reactive_power on the y-axis as a line graph.
  plot(Time, Global_reactive_power, type = "l", xlab = "datetime", ylab = "Global_reactive_power", xaxt = "n")
    
  ##Create a sequence of dates for x-axis
    date_seq <- seq(min(data$Time), max(data$Time) + 86400, by = "day")
    
  ##Add x-axis tick marks and labels
    axis(1, at = date_seq, labels = c("Thu", "Fri", "Sat"))

  })
  
##Close the png file device.
  dev.off()
