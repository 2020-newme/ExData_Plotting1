unzip("exdata_data_household_power_consumption.zip", overwrite = FALSE)

library(readr)

loadHpcBase <- function() {
  if (!is.null(hpcBase)) {
    return()
  }
  
  hpcFull <-
    read_delim(file.path("household_power_consumption.txt"),
               delim = ";",
               na = "?")
  
  hpcBase <<-
    hpcFull[hpcFull$Date == "1/2/2007" |
              hpcFull$Date == "2/2/2007",]
}
hpcBase <- NULL
loadHpcBase()

# The base HPC dataset has two separate Date and Time columns, but that's not convenient
# for the plot that needs to be built. So this function returns the provided dataset,
# adds a dt column corresponding to the Date and Time values combined, and also removes
# the Date and Time columns.
#
# Parameters:
#   hpcBase - the base dataset with the Date and Time columns.
#
# Value:
#   a dataset derived from hpcBase, where the Date and Time columns have been removed,
#   and a dt column has been added in its place.
fixDates <- function(hpcBase) {
  dtStr <- paste(hpcBase$Date, hpcBase$Time)
  dt <- strptime(dtStr, "%d/%m/%Y %H:%M:%S")
  hpcNoDateTime <- hpcBase[, 3:9]
  return(cbind(hpcNoDateTime, dt))
}
hpc <- fixDates(hpcBase)

png("plot4.png", width = 504, height = 504)

par(mfcol=c(2,2))

plot(
  hpc$dt,
  hpc$Global_active_power,
  type = "l",
  xlab = "",
  ylab = "Global Active Power (kilowatts)"
)

plot(
  hpc$dt,
  hpc$Sub_metering_1,
  type = "l",
  ylab = "Energy sub metering",
  xlab = ""
)

lines(hpc$dt,
      hpc$Sub_metering_2, col = "red")

lines(hpc$dt,
      hpc$Sub_metering_3, col = "blue")

legend(
  "topright",
  legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
  col = c("black", "red", "blue"),
  lty = 1,
  bty = "n"
)

plot(
  hpc$dt,
  hpc$Voltage,
  type = "l",
  ylab = "Voltage",
  xlab = "datetime"
)

plot(
  hpc$dt,
  hpc$Global_reactive_power,
  type = "l",
  ylab = "Voltage",
  xlab = "datetime"
)

dev.off()