####### -- Function to read URLs into R -- ######
# 2018-02-26 : AR

gdriveURL <- function(x){
  x =  
  upURL = sub("^[^=]*", "", x)
  y1 =  "https://docs.google.com/uc?id"
  y2 = "&export=download"
  downURL = paste0(y1,upURL,y2)
  read.csv(downURL, header = TRUE, stringsAsFactors = FALSE)
}

### To choose the proper URL...
# 1) Go into Google Drive FishScapes
# 2) Go to folder with data file you're looking for
# 3) Right click on the datafile; choose 'get shareable link'
# 4) Paste shareable link into function (e.g. exDat <- gdriveURL("https://drive.google.com/open?id=1xi-yiFzP8f9TqZPyBg5xetG9EA3nQrS5"))
# 5) Run function, data shoulr be there

### Examples: Make sure to comment out after running
#exCrlDat <- gdriveURL("https://drive.google.com/open?id=1T_QeJTms9QmG65iT1ul_FC1H1XbhBhd5") 
#Shareable link for creel interview data

#fishLength <- gdriveURL('https://drive.google.com/open?id=1pyCKCcAQZiNZz-tX5U2QnZUc79OQEWX2')
#Shareable link for creel fishLength

