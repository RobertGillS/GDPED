##install.packages("downloader)
##install.packages("readr")
##install.packages("stringr")
library(downloader)
library(readr)
library(stringr)

##download raw data from website
download("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv", destfile="GDP.csv")
download("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv", destfile="EduData.csv")

## Clean GDP data
GDP <- read.csv("GDP.csv", header=FALSE, skip = 5, stringsAsFactors = FALSE)
##trim unwanted NA variables
vars <- c("V1","V2","V4","V5")
GDP2 <- GDP[vars]
colnames(GDP2) <- c("Country Code", "GDP Rank", "Country Name", "GDP")
##remove data that was read even though it is just a glossary of country names and codes
GDP2 <- GDP2[1:190,]
##convert GDP to numeric
GDP2$GDP <- as.numeric(gsub(",","",GDP2$GDP))

##Clean Edu data
Edu <- read.csv("EduData.csv", header=TRUE, stringsAsFactors = FALSE)
##remove unwanted variables
vars2 <- c("CountryCode","Long.Name","Income.Group")
Edu2 <- Edu[vars2]
colnames(Edu2) <- c("Country Code", "Country Name", "Income Group")
##Merge data sets
CleanGDPEdu <- merge(GDP2,Edu2, by="Country Code")

##count number of matching Country Codes
length(intersect(GDP2$`Country Code`,Edu2$`Country Code`))

##sort by GDP
SortedClean <- CleanGDPEdu[order(CleanGDPEdu$GDP),]

##Retrieve the 13th country in sorted Data
SortedClean[13,"Country Name.x"]

##calculatre mean GDP Rank based on Income Group
SortedClean$`GDP Rank` <- as.numeric(SortedClean$`GDP Rank`)
aggregate(SortedClean$`GDP Rank`,list(SortedClean$`Income Group`),mean)

##plot GDP
install.packages("ggplot2")
library(ggplot2)

##Plot GDP by Income Group
DistPlot <- ggplot(data=SortedClean, aes(SortedClean$GDP)) + geom_histogram() + aes(fill=SortedClean$`Income Group`)

##summary statistics
aggregate(SortedClean$`GDP Rank`,list(SortedClean$`Income Group`),summary)
aggregate(SortedClean$GDP ,list(SortedClean$`Income Group`),summary)

##Split into quintiles
SortedClean$Quantile[SortedClean$`GDP Rank` <= 38] <- 1
SortedClean$Quantile[SortedClean$`GDP Rank` > 38 & SortedClean$`GDP Rank` <= 76] <- 2
SortedClean$Quantile[SortedClean$`GDP Rank` > 76 & SortedClean$`GDP Rank` <= 114] <- 3
SortedClean$Quantile[SortedClean$`GDP Rank` > 114 & SortedClean$`GDP Rank` <= 152] <- 4
SortedClean$Quantile[SortedClean$`GDP Rank` > 152 & SortedClean$`GDP Rank` <= 190] <- 5

##create a table showing relationship between quantile and income
table(SortedClean$Quantile,SortedClean$`Income Group`)