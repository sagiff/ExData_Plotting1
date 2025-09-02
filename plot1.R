##This script will generate Plot 1: Global Active Power vs. Frequency, Histogram

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

##Create the png file for the plot
  png(filename = "plot1.png", width = 480, height = 480, units = "px")
  
##Plot Global Active Power (kilowatts) on the x-axis vs. Frequency on the y-axis as a histogram with red bars.
  hist(data$Global_active_power, xlab = "Global Active Power (kilowatts)", col = "red", main = "Global Active Power")
  
##Close the png file device
  dev.off()
  