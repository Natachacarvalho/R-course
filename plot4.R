plot4 <- function() {

 require(lattice) 
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  hospital <- read.csv("hospital-data.csv", colClasses = "character")
  #Then we are going to want to merge the two datasets together to match the
  #Hospital.Ownership variable
  #to the death rate data.
  
  outcome.hospital <- merge(outcome, hospital, by = "Provider.Number")
  
  #From here, we can create the relevant variables that we want to plot.
  death <- as.numeric(outcome.hospital[, 11]) ## Heart attack outcome
  npatient <- as.numeric(outcome.hospital[, 15])
  owner <- factor(outcome.hospital$Hospital.Ownership)
  
  outcome.hospital <- merge(outcome, hospital, by = "Provider.Number")
  #From here, we can create the relevant variables that we want to plot.
  death <- as.numeric(outcome.hospital[, 11]) ## Heart attack outcome
  npatient <- as.numeric(outcome.hospital[, 15])
  owner <- factor(outcome.hospital$Hospital.Ownership)
  #1. Use the xyplot function in the lattice package to make a plot of the relationship between 30-day
  #death rate for heart attack versus the number of patients seen. The number of patients should be on
  #the x-axis. Make sure you run library(lattice) before calling xyplot.
  #2. Set the x-axis label to be “Number of Patients Seen”
  #3. Set the y-axis label to be “30-day Death Rate”
  #4. Set the title of the plot to be “Heart Attack 30-day Death Rate by Ownership”
  #5. In each panel of the plot, add a linear regression line highlighting the relationship
  xyplot( death ~ npatient | owner, 
          main="Heart Attack 30-day Death Rate?panel by Ownership",
          xlab="Number of Patients Seen", 
          ylab="30-day Death Rate",
 #, as.table=T
  
  panel=function(x,y){
        panel.lmline( x, y )
        panel.xyplot( x, y ) 
                      }  
          )
         

}