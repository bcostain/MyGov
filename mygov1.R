# MyGov Gmail Contacts
# written by Bret Costain
# April 27, 2015
# Retrieve Users Address based on GoogleID
# Execute curl script to retreiv contacts relvent to User address
# Flatten nested JSON file
# Output CSV file
# Output Google+ Cirlces
# NEXT STEP: CSV File imported in GMail into "myGov" folder within Contacts 
#
# BEGIN
# Set Working Directory
setwd("~/Desktop/MyGov")
# Install Packages
install.packages("RCurl")
library("RCurl")
install.packages("devtools")
library(devtools)
install.packages("rjson")
library("rjson")
install.packages("jsonlite")
library("jsonlite")
install.packages("foreach")
library("foreach")
install.packages("stringr")
library("stringr")

#
# FORMAT ADDRESS AS CURL EXPRESSION TO RETRIEVE USER ADDRESS 
API_KEY = "AIzaSyA7KrZQYcgLzO0c4ObJL9G0f3IIsznpmro"
userGooglePlusID= "102823877180866545771"
expression1 = "https://www.googleapis.com/plus/v1/people/" # First part of getURL command BEFORE userGoglePlusID
expression2= "?fields=placesLived&key=" # First part of getURL command AFTER userGoglePlusID BEFORE API_KEY
addressRequest = paste(expression1, userGooglePlusID, expression2, API_KEY, sep="")
addressTest= getURL(addressRequest)
#
# LOAD AS JSON ADDRESS AND CLEAN
json_address <- fromJSON(addressTest)
userAddress=json_address$placesLived$value
userAddressClean = str_replace_all(userAddress,",","") # REMOVE COMMAS
userAddressClean = str_replace_all(userAddress," ","%20") # REPLACE SPACES with %20 
#
# RETRIVE CONTACTS FROM GOOGLE CIVIC API
a = "https://www.googleapis.com/civicinfo/v2/representatives?key=AIzaSyA7KrZQYcgLzO0c4ObJL9G0f3IIsznpmro&address=" # First part of getURL command BEFORE clean address
c= paste(a, userAddressClean, sep = "")
z= getURL(c)
#
# CREATE LABES
# Requires authorization
# POST https://www.googleapis.com/gmail/v1/users/userId/labels
# h = getURL ("https://www.googleapis.com/gmail/v1/users/102823877180866545771/labels
# ?fields=placesLived&key=AIzaSyA7KrZQYcgLzO0c4ObJL9G0f3IIsznpmro")

# Load JSON File
json_file  <- z
json_data <- fromJSON(json_file)
#
# CLEAN NULL FROM EMAIL
json_data$officials$email[sapply(json_data$officials$email, is.null)] <- NA
json_data$officials$emails[sapply(json_data$officials$emails, is.null)] <- NA
# CLEAN LEVEL
json_data$offices$level[sapply(json_data$offices$level, is.null)] <- "Local" # Change NULL to Local
json_data$offices$level <- gsub("administrativeArea1", "State", json_data$offices$level)# Change administraiveArea1 to State
json_data$offices$level <- gsub("country", "Federal", json_data$offices$level)# Change country to Federal
#
# CREATE OFFICE RELATED VECTORS
officialInd=NULL
officeInd=NULL
officeName=NULL
officeLevel = NULL
q= json_data$offices$officialIndices
imax=length(q)
for (i in 1:imax ) 
{
smax=length(q[[i]])
for (s in 1:smax ) 
{
	officialInd=c(officialInd,q[[i]][s])
	officeInd=c(officeInd,i)
	officeName=c(officeName,json_data$offices$name[i])
	officeLevel=c(officeLevel,json_data$offices$level[i])
	}
}
#
d = (json_data$officials$name) # CREATE OFFICIAL NAME VECTOR
e = (json_data$officials$emails) # CREATE OFFICIAL EMAIL VECTOR
f = as.character(e) # CONVERT EMAIL to CHARACTER
ph = (json_data$official$phone)
ph2 = sapply(ph, "[[", 1) #Select first phone number
circle = as(officeLevel,"character")
photo = json_data$officials$photoUrl
#
# CREATE MASTER DATE FRAME FOR CONTACTS
master = data.frame(d,officeName,f,ph2,circle,photo,officialInd,officeInd)
colnames(master) <- c("First Name","Organization", "Email","phone","Circle","Photo","officialInd","officeInd")
write.csv(master, file = "MyGov.csv") # CREATE CSV File
# PRINT RESULTS 
print("API_KEY:"); API_KEY
print("User's Google+ ID: "); userGooglePlusID
print("User's Primary Adress: "); userAddress
print("User's Contact List:"); master
# END
