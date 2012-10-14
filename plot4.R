plot4 <- function() {

  
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
  
  
}