### Query Document Database
###
### Author: Thanki, Mansi Pravin
### Course: CS5200
### Term: Full Summer 2022
### Collaborator: Nahata, Hardik

# assumes that you have set up the database structure by running CreateFStruct.R

# Query Parameters (normally done via a user interface)

######################################################


# setLock
setLock <- function(customer, year, quarter) {
  # path variable takes in the parameters to generate the path for .lock file
  path <- paste("docDB/reports", year, quarter, customer, ".lock", sep="/")
  
  # we check if the file already exists, if it doesn't -> create .lockfile
  if(!(file.exists(path))) {
    file.create(path)  
    # returns  0 if .lock does not already exists
    return(0)
  } 
  else {
    # returns error code -1 if .lock already exists
    return(-1)
  }
}

########################################################

# genReportFName 
genReportFName <- function(customer, year, quarter) {
  
  # paste function generates pdf filename using the parameters
  filename <- paste(customer, year, quarter, "pdf", sep=".")
  return(filename)
}

########################################################
# storeReport

storeReport <- function(customer, year, quarter) {
  lockFilePath <- paste("docDB/reports", year, quarter, customer, ".lock", sep="/")
  
  #getwd gets the current working directory
  cwd <- getwd()
  
  # gets the pdf filename from genReportFName() method
  pdfFileName <- genReportFName(customer, year, quarter)
  
  # path to copy from 
  fromPath <- paste(cwd, pdfFileName, sep="/")
  
  # path to copy to
  toPath <- paste("docDB/reports", year, quarter, customer, sep="/")
  toPath <- paste(toPath, "/", sep="")
  
  # file.copy copies pdf file to the destination folder
  # overwrite the file
  file.copy(fromPath, toPath, overwrite = TRUE)
}

########################################################
# relLock - This function releases the acquired lock 
# just remove lock if it exists
relLock <- function(customer, year, quarter) {
  path <- paste("docDB/reports", year, quarter, customer, ".lock", sep="/")
  
  # if lock exists, remove the lock
  if(file.exists(path)) {
    file.remove(path)
    return("LOCK REMOVED")
  }
  else {
    return("NO LOCK FILE PRESENT")
  }
}

########################################################

# checkLock - checks existence of .lock file
checkLock <- function(customer, year, quarter) {
  path <- paste("docDB/reports", year, quarter, customer, ".lock", sep="/")
  
  # file.exists checks if the path to lock file exists
  if(file.exists(path)) {
    # if path exists, function returns yes
    return("YES")
  }
  else {
    # if path does not exists, function returns no
    return("NO")
  }
}

########################################################
# TESTING

# 1. Tests for setLock function

########### TestCase 1: check Acquiring of Lock when it does not already exist#############

# if file does not have lock, setLock acquires the lock and returns 0
# Expected Behaviour: setLock should be able to acquire lock
quarter <- "Q2"
year <- "2021"
customer <- "Medix"

# test function
test_setLock_Acquired <- function(customer, year, quarter) {
  lockPresent = setLock(customer, year, quarter) 
  if (lockPresent == 0) {
    print("Lock Acquired Sucessfully")
  }
  else 
  {
    print("Lock Already Exists")
  }
}

# Lock does not exist initially
relLock(customer, year, quarter) # removes any lock if it exists
test_setLock_Acquired(customer, year, quarter)


########### TestCase 2: check Acquiring of Lock when already exists#############

# if .lock file already exists, setLock does not acquire lock and returns -1
# Expected Behaviour: setLock should not be able to acquire lock as a lock already exists

quarter <- "Q2"
year <- "2021"
customer <- "Medix"

# test function
test_setLock_LockAlreadyExists <- function(customer, year, quarter) {
  lockPresent = setLock(customer, year, quarter) 
  if (lockPresent == 0) {
    print("Lock Acquired Sucessfully")
  }
  else 
  {
    print("Lock Already Exists")
  }
}

# Lock exists initially
setLock(customer, year, quarter) 
test_setLock_LockAlreadyExists(customer, year, quarter)

##########################################################################################

# 2. Tests for genReportFName function

########### TestCase 1: checking whether filename is generated correctly#############

# checks whether genReportFName returns the correctly generated report full file using the pattern Company.Year.Quarter.pdf
# Expected Behaviour: file name with correct Company.Year.Quarter.pdf is generated 

quarter <- "Q2"
year <- "2021"
customer <- "Medix"

# test function
test_genReportFName <- function(customer, year, quarter, expectedFileName) {
  fileName = genReportFName(customer, year, quarter) 
  if (fileName == expectedFileName) {
    print("File name generated successfully")
  }
  else 
  {
    print("File name generated is incorrect")
  }
}

relLock(customer, year, quarter) # removes any lock if it exists
expectedFileName = "Medix.2021.Q2.pdf" # manually setting expected filename to check
test_genReportFName(customer, year, quarter, expectedFileName)

##########################################################################################

# 3. Tests for storeReport function

########### TestCase 1: checking whether file is stored correctly if lock does not exist#############

# checks whether storeReport stores file correctly if lock does not exist
# Expected behaviour: since lock does not exists, file should be allowed to be copied

quarter <- "Q2"
year <- "2021"
customer <- "Medix"

# test function
teststoreReport_LockNotExist <- function(customer, year, quarter) {
  lockPresent = setLock(customer, year, quarter) 
  if (lockPresent == 0) {
    print("Lock Acquired Sucessfully")
    storeReport(customer, year, quarter)
    print("File Copied Sucessfully")
    relLock(customer, year, quarter)
    print("Lock Released")
  }
  else 
  {
    print("Lock Already Exists. File Not Copied.")
  }
}


# Lock does not exist initially
teststoreReport_LockNotExist(customer, year, quarter)

########### TestCase 2: checking whether file is stored correctly if lock exists#############
# Expected behaviour: since lock exists, file should not be allowed to be copied

quarter <- "Q2"
year <- "2021"
customer <- "Medix"

# test function
teststoreReport_LockExists <- function(customer, year, quarter) {
  lockPresent = setLock(customer, year, quarter) 
  if (lockPresent == 0) {
    print("Lock Acquired Sucessfully")
    storeReport(customer, year, quarter)
    print("File Copied Sucessfully")
    relLock(customer, year, quarter)
    print("Lock Released")
  }
  else 
  {
    print("Lock Already Exists. File Not Copied.")
  }
}

# Lock exists initially
setLock(customer, year, quarter) 
teststoreReport_LockExists(customer, year, quarter)

##########################################################################################

# 4. Tests for relLock function

########### TestCase 1: checking whether lock on file is released if lock exists#############

# checks whether lock is released if lock file already exists
# Expected Behaviour: relLock should be able to release the lock

quarter <- "Q2"
year <- "2021"
customer <- "Medix"

# test function
test_relLock_whenLockExists <- function(customer, year, quarter) {
  relLockStatus = relLock(customer, year, quarter) 
  if (relLockStatus == "LOCK REMOVED") {
    print("Lock Removed Sucessfully")
  }
  else 
  {
    print("Lock does not exist! No lock could be removed")
  }
}

# Lock does not exist initially
setLock(customer, year, quarter) # sets lock on the file
test_relLock_whenLockExists(customer, year, quarter)



########### TestCase 2: checking the condition when lock does not exist#############

# checks whether lock is released if lock file does not already exists
# Expected Behaviour: relLock should not be able to release the lock as it does not exist

quarter <- "Q2"
year <- "2021"
customer <- "Medix"

# test function
test_relLock_whenLockNotExists <- function(customer, year, quarter) {
  relLockStatus = relLock(customer, year, quarter) 
  if (relLockStatus == "LOCK REMOVED") {
    print("Lock Removed Sucessfully")
  }
  else 
  {
    print("Lock does not exist! No lock could be removed")
  }
}

# Lock does not exist 

test_relLock_whenLockNotExists(customer, year, quarter)



