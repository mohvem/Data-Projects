library(readr)
gsi <- read_csv("file 1")  #data set 1
long_lat <- read_csv("file 2")  # put in file path of long/lat file
require(dplyr)

# count the number of cases in each city
top_cities <- gsi %>% group_by(company_office_city) %>% summarise(., counts = n_distinct(case_name, na.rm = TRUE))

# change col name so you can do a table merge with latitudes 
colnames(top_cities) <- c("city", "count")

top_cities2 <- left_join(top_cities, long_lat)

top_cities3 <- top_cities2 %>% group_by(city) %>% summarise(., long = mean(longitude), lat = mean(latitude))

top_cities3 <- left_join(top_cities3, top_cities)

### Cleaned Up in Excel

top_cities3 <- read_csv("~/top_cities.csv")

top_cities3 <- top_cities3[order(top_cities3$count, decreasing = TRUE),]

top50_cities <- top_cities3[1:50, ]

## Make the Map
require(maps)
require(RgoogleMaps)
require(ggplot2)
require(ggmap)
require(plotly)
map("state")
points(top50_cities$long, top50_cities$lat, col = "blue", pch = 16, cex = 1) # plot cities on map


## more detailed map

states <- map_data("state")
p <- ggplot()
p <- p + geom_polygon(data = states, aes(x = long, y = lat, group = group), colour = "orange", fill = "white")
p <- p + geom_point(data = top_cities3, aes(x = long , y = lat, size = count))
plotly(p) #interactive HTML map
### remove Anchorage, Honolulu, and Ambler, which are messing with plot

subset_top <- top_cities3[c(top_cities3$long > -145, top_cities3$lat < 50), ]

top <- ggplot() 
top <- top + geom_polygon(data = states, aes(x = long, y = lat, group = group), colour = "grey", fill = "white")
top <- top + geom_point(data = subset_top, aes(x = long , y = lat, size = count))
top <- top + annotate(geom = "text", x = -125.5, y = 38, label = "SF Bay Area", colour = "#e34234", fontface = "bold")
top <- top + annotate(geom = "text", x = -124, y = 34, label = "Los Angeles County", colour = "gold", fontface = "bold") 
top <- top + annotate(geom = "text", x = -119, y = 46.5, label = "Seattle/Portland", colour = "#228b22", fontface = "bold")
top <- top + annotate(geom = "text", x = -98, y = 52, label = "U.S. Cities Unum IDI Is Best Represented In", size = 6, fontface = "bold")

plotly(top)  #interactive HTML map
### BROKER ANALYSIS

require(dplyr)
brokers <- read_csv("file 3") # dataset 3
brok <- brokers[, c(1,2, 15)]
names(brok)[3] <- "city"

brok2 <- brok %>% group_by(., city) %>% summarise(., Brokers = n_distinct(PCES_SORCE_PRODR_ID)) #brokers in each city
brok3 <- left_join(brok2, long_lat) #merge with lat/long dataset
brok3 <- brok3 %>% group_by(., city) %>% summarise(long = mean(longitude), lat = mean(latitude))
brok3$Brokers <- brok2$Brokers


# fill in blanks in Excel
broks <- read_csv("file 4") # dataset 4 


# Add to tops map

top_b <- top + geom_point(data = broks, aes(x = long, y = lat, colour = Brokers), pch = 17, size = 6) +
  scale_color_gradient(low = "red", high = "yellow")

plotly(top_b) #interactive HTML map
# New map

brk <- ggplot() 
brk <- brk + geom_polygon(data = states, aes(x = long, y = lat, group = group), colour = "grey", fill = "white")
brk <- brk + geom_point(data = broks, aes(x = long , y = lat, colour = Brokers), pch = 17, size = 6) +
  scale_color_continuous(low = "red", high = "yellow")



f500 <- read_csv("file 5") # fortune 500 companies #data set 5

### some cleanup to improve the merge

f500$company <- toupper(f500$company) # change the companies 2 upper for matching
f500$company[f500$company == "GENERAL MOTORS"] <- "GENERAL MOTORS CORPORATION"
f500$company[f500$company == "AT&T"] <- "AT&T INC."
f500$company[f500$company == "COSTCO"] <- "COSTCO WHOLESALE CORPORATION"
f500$company[f500$company == "JP MORGAN CHASE"] <- "JPMORGAN CHASE BANK NA (PORTED) F-35%"
f500$company[f500$company == "BANK OF AMERICA CORP."] <- "BANK OF AMERICA CORPORATION (COMMISSIONABLE EE'S)"
f500$company[f500$company == "CARDINAL HEALTH"] <- "CARDINAL HEALTH, INC"
f500$company[f500$company == "ARCHER DANIELS MIDLAND"] <- "ARCHER DANIELS MIDLAND COMPANY"
f500$company[f500$company == "JP MORGAN CHASE"] <- "JPMORGAN CHASE BANK NA (PORTED) F-35%"
f500$company[f500$company == "COMCAST"] <- "COMCAST CABLE COMMUNICATIONS MANAGEMENT, LLC"
f500$company[f500$company == "ENTERPRISE PRODUCTS PARTNERS"] <- "ENTERPRISE PRODUCTS COMPANY"
f500$company[f500$company == "JP MORGAN CHASE"] <- "JPMORGAN CHASE BANK NA (PORTED) F-35%"
f500$company[f500$company == "WORLD FUEL SERVICES"] <- "WORLD FUEL SERVICES CORPORATION"
f500$company[f500$company == "BEST BUY"] <- "BEST BUY ENTERPRISE SERVICES INC (PORTED) 7/40%"
f500$company[f500$company == "TESORO"] <- "TESORO CORPORATION"
f500$company[f500$company == "NEW YORK LIFE INSURANCE"] <- "NEW YORK LIFE INSURANCE COMPANY (PENSION MATURITY)"
f500$company[f500$company == "TWENTY-FIRST CENTURY FOX"] <- "21ST CENTURY FOX AMERICA"
f500$company[f500$company == "EXELON"] <- "EXELON CORP."
f500$company[f500$company == "QUALCOMM"] <- "QUALCOMM INC."
f500$company[f500$company == "RITE AID"] <- "RITE AID CORPORATION"
f500$company[f500$company == "NATIONAL OILWELL VARCO"] <- "NATIONAL OILWELL VARCO, INC."
f500$company[f500$company == "QUALCOMM"] <- "QUALCOMM INC."
f500$company[f500$company == "TIME WARNER CABLE"] <- "TIME WARNER"
f500$company[f500$company == "XEROX"] <- "XEROX BUSINESS SERVICES, LLC"
f500$company[f500$company == "QUALCOMM"] <- "QUALCOMM INC."
f500$company[f500$company == "ABBVIE"] <- "ABBVIE INC (PORTED) X/25%"
f500$company[f500$company == "AES"] <- "THE AES CORPORATION"
f500$company[f500$company == "THERMO FISHER SCIENTIFIC"] <- "THERMO FISHER SCIENTIFIC INC (PORTED) W/40%"
f500$company[f500$company == "APACHE"] <- "APACHE CORPORATION"
f500$company[f500$company == "PARKER-HANNIFIN"] <- "PARKER HANNIFIN"
f500$company[f500$company == "WELLCARE HEALTH PLANS"] <- "WELLCARE HEALTH PLANS, INC."
f500$company[f500$company == "MARSH & MCLENNAN"] <- "MARSH & MCLENNAN AGENCY, LLC"
f500$company[f500$company == "CDW"] <- "CDW LLC"
f500$company[f500$company == "STANLEY BLACK & DECKER"] <- "STANLEY BLACK & DECKER, INC."
f500$company[f500$company == "BLACKROCK"] <- "BLACKROCK, INC"
f500$company[f500$company == "CST BRANDS"] <- "CST BRANDS, INC."
f500$company[f500$company == "QUALCOMM"] <- "QUALCOMM INC."
f500$company[f500$company == "ASSURANT"] <- "ASSURANT INC"
f500$company[f500$company == "PETER KIEWIT SONS'"] <- "PETER KIEWIT SONS - TIC HOLDINGS"
f500$company[f500$company == "COGNIZANT TECHNOLOGY SOLUTIONS"] <- "COGNIZANT TECHNOLOGY SOLUTIONS U.S. CORPORATION"
f500$company[f500$company == "QUALCOMM"] <- "QUALCOMM INC."
f500$company[f500$company == "GROUP 1 AUTOMOTIVE"] <- "GROUP 1 AUTOMOTIVE, INC."
f500$company[f500$company == "AGCO"] <- "AGCO CORPORATION"
f500$company[f500$company == "NGL ENERGY PARTNERS"] <- "NGL ENERGY PARTNERS LP"
f500$company[f500$company == "QUALCOMM"] <- "QUALCOMM INC."
f500$company[f500$company == "OWENS & MINOR"] <- "OWENS AND MINOR, INC."
f500$company[f500$company == "APPLIED MATERIALS"] <- "APPLIED MATERIALS, INC."
f500$company[f500$company == "BALL"] <- "BALL CORPORATION"
f500$company[f500$company == "D.R. HORTON"] <- "D.R. HORTON, INC."
f500$company[f500$company == "W.R. BERKLEY"] <- "W. R. BERKLEY CORPORATION"
f500$company[f500$company == "QUALCOMM"] <- "QUALCOMM INC."
f500$company[f500$company == "PACIFIC LIFE"] <- "PACIFIC LIFE INSURANCE"
f500$company[f500$company == "HUNTINGTON INGALLS INDUSTRIES"] <- "HUNTINGTON INGALLS INDUSTRIES, INC"
f500$company[f500$company == "MUTUAL OF OMAHA INSURANCE"] <- "MUTUAL OF OMAHA"
f500$company[f500$company == "SEABOARD"] <- "SEABOARD CORPORATION"
f500$company[f500$company == "ANIXTER INTERNATIONAL"] <- "ANIXTER, INC."
f500$company[f500$company == "DR PEPPER SNAPPLE GROUP"] <- "DR PEPPER SNAPPLE GROUP, INC."
f500$company[f500$company == "TRACTOR SUPPLY"] <- "TRACTOR SUPPLY COMPANY"
f500$company[f500$company == "UNITED RENTALS"] <- "UNITED RENTALS, INC."
f500$company[f500$company == "QUINTILES TRANSNATIONAL HOLDINGS"] <- "QUINTILES TRANSNATIONAL CORP."
f500$company[f500$company == "CH2M HILL"] <- "CH2M"
f500$company[f500$company == "KINDRED HEALTHCARE"] <- "KINDRED HEALTHCARE, INC."
f500$company[f500$company == "HANOVER INSURANCE GROUP"] <- "THE HANOVER INSURANCE GROUP, INC."
f500$company[f500$company == "FISERV"] <- "FISERV, INC."
f500$company[f500$company == "QUALCOMM"] <- "QUALCOMM INC."


f500$signed <- rep(NA, nrow(f500))

for (i in 1:nrow(f500)){
  if (any(f500$company[i] == unique(gsi$case_name))){
    f500$signed[i] <- "Yes"
  } else {
    f500$signed[i] <- "No"
  }
}

table(f500$signed)

names(f500)[1] <- "long"

# Update New map

## SUBSET ONLY MAINLAND US

f500_2 <- f500[f500$lat > 25, ]
f500$signed <- factor(f500$signed)

brk <- ggplot() 
brk <- brk + geom_polygon(data = states, aes(x = long, y = lat, group = group), colour = "grey", fill = "white")
brk <- brk + geom_point(data = broks, aes(x = long , y = lat, colour = Brokers), pch = 17, size = 6) +
  scale_color_continuous(low = "red", high = "yellow")
brk <- brk + geom_point(data =f500_2[f500_2$signed == "No",], aes(x = long, y = lat), colour = "indianred4", size = 1.5) +
  geom_point(data = f500_2[f500_2$signed == "Yes", ], aes(x = long, y = lat), colour = "green4", size = 1.5) +


plotly(brk) #interactive HTML map

