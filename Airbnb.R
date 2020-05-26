# Load and Clean Airbnb Data set
require(readr)
require(lubridate)
require(stringi)
require(stringr)
### Download listings, calendar, and reviews data from Airbnb
list <- read_csv("listings.csv")
cal <- read_csv("calendar.csv")
rev <- read_csv("reviews.csv")

sum(complete.cases(list)) # all are NA lol so let's just leave as is
sum(complete.cases(cal))  # about half is NA
cal2 <- cal[complete.cases(cal), ] # take out NA rows
sum(complete.cases(rev))  # no NAs

cal2$date  <- as.Date(cal2$date, format = "%m-%d-%Y")
cal2$month <- month(cal2$date)
cal2$year <- year(cal2$date)

### Convert things to numbers and remove the weird characters
list$price <- stri_replace_all_regex(str = list$price, pattern = "[$]", replacement = "")
list$price <- stri_replace_all_regex(str = list$price, pattern = ",", replacement = "")
list$price <- stri_replace_all_regex(str = list$price, pattern = "\\.00", replacement = "")
list$price <- as.numeric(list$price)
summary(list$price)

list$weekly_price <- stri_replace_all_regex(str = list$weekly_price, pattern = "[$]", replacement = "")
list$weekly_price <- stri_replace_all_regex(str = list$weekly_price, pattern = ",", replacement = "")
list$weekly_price<- stri_replace_all_regex(str = list$weekly_price, pattern = "\\.00", replacement = "")
list$weekly_price <- as.numeric(list$weekly_price)
summary(list$weekly_price)

list$monthly_price <- stri_replace_all_regex(str = list$monthly_price, pattern = "[$]", replacement = "")
list$monthly_price <- stri_replace_all_regex(str = list$monthly_price, pattern = ",", replacement = "")
list$monthly_price<- stri_replace_all_regex(str = list$monthly_price, pattern = "\\.00", replacement = "")
list$monthly_price <- as.numeric(list$monthly_price)
summary(list$monthly_price)

list$extra_people <- stri_replace_all_regex(str = list$extra_people, pattern = "[$]", replacement = "")
list$extra_people <- stri_replace_all_regex(str = list$extra_people, pattern = ",", replacement = "")
list$extra_people<- stri_replace_all_regex(str = list$extra_people, pattern = "\\.00", replacement = "")
list$extra_people <- as.numeric(list$extra_people)
summary(list$extra_people)

cal2$price <- stri_replace_all_regex(str = cal2$price, pattern = "[$]", replacement = "")
cal2$price <- stri_replace_all_regex(str = cal2$price, pattern = ",", replacement = "")
cal2$price <- stri_replace_all_regex(str = cal2$price, pattern = "\\.00", replacement = "")
cal2$price <- as.numeric(cal2$price)
summary(cal2$price)

list$host_response_rate <- as.numeric(stri_replace_all_regex(str = list$host_response_rate, pattern = "%", replacement = ""))

### in my personal experience, most listings are less than $1000 per night, and the outliers are probably not going to be representative of the average renter on Airbnb

table(list$city) # just gonna ignore the ones that are weirdly in Chinese
df_city<-data.frame(table(list$city)) 
# need to clean this up
list$city[list$city == "600 Atlantic Blvd #309 Monterey Park CA 91754"] <- "Monterey Park"
list$city[list$city == "Agoura hills"] <- "Agoura Hills"
list$city[list$city == "arcadia" | list$city == "Arcadia, California, US"] <- "Arcadia"
list$city[list$city == "Beverly hills" | list$city == "beverly hills" | list$city == "Beverly Hills/Los Angeles"] <- "Beverly Hills"
list$city[list$city == "burbank" | list$city == "Burbank, Los Angeles"] <- "Burbank"
list$city[list$city == "Canyon country"] <- "Canyon Country"
list$city[list$city == "Canoga park"] <- "Canoga Park"
list$city[list$city == "Culver City, California, US"] <- "Culver City"
list$city[list$city == "Diamond bar"] <- "Diamond Bar"
list$city[list$city == "echo park" | list$city == "Echo Park, Los Angeles"] <- "Echo Park"
list$city[list$city == "ENCINO" | list$city == "Encino, Los Angeles"] <- "Encino"
list$city[list$city == "gardena"] <- "Gardena"
list$city[list$city == "Glendale, California, US"] <- "Glendale"
list$city[list$city == "GRANADA HILLS" | list$city == "granada hills California, US"] <- "Granada Hills"
list$city[list$city == "hacienda"] <- "Hacienda Heights"
list$city[list$city == "Hollywood Hills, Los Angeles"] <- "Hollywood Hills"
list$city[list$city == "hollywood, ca" | list$city == "Hollywood, Los Angeles"] <- "Hollywood"
list$city[list$city == "Hollywood Hills, Los Angeles"] <- "Hollywood Hills"
list$city[list$city == "Inglewood, California, US"] <- "Inglewood"
list$city[list$city == "La Habra" | list$city == "La habra heights"] <- "La Habra Heights"
list$city[list$city == "lake balboa"] <- "Lake Balboa"
list$city[list$city == "LAWNDALE/REDONDO BEACH AREA"] <- "Lawndale"
list$city[list$city == "long beach" | list$city == "LONG BEACH" | list$city == "longbeach"] <- "Long Beach"
list$city[list$city == "Los Angeles, Studio City, Tujunga Village"] <- "Studio City"
list$city[list$city == "Los Angeles, studio city"] <- "Studio City"
list$city[list$city == "Los Angeles90064" | list$city == "Los angles" | list$city == "Los Angeles, California, US" | list$city == "Los Angeles" | list$city == "los angeles" | list$city == "Los angeles"] <- "Los Angeles"
list$city[list$city == "Malibu Beach"] <- "Malibu"
list$city[list$city == "Marina del ray" | list$city == "Marina del Ray" | list$city == "Marina del Rey, Venice, Los Angeles"] <- "Marina Del Ray"
list$city[list$city == "Malibu Beach"] <- "Malibu"
list$city[list$city == "Monterey park"] <- "Monterey Park"
list$city[list$city == "N. HOLLYWOOD" | list$city == "No Hollywood" | list$city == "N Hollywood" | list$city == "north hollywood" | list$city == "North Hollywood Los Angeles Studio City" | list$city == 
            "North Hollywood/Burbank" | list$city == "Los Angeles (North Hollywood)" | list$city == "Los Angeles, NoHo Art District"] <- "Hollywood"
list$city[list$city == "pacific palisades" | list$city == "PACIFIC PALISADES" | list$city == "Pacific Palisades, Los Angeles" | list$city == "Pacific Plsds"] <- "Pacific Palisades"
list$city[list$city == "Redo do beach" | list$city == "Redondo Beac" ] <- "Redondo Beach"
list$city[list$city == "rosemead"] <- "Rosemead"
list$city[list$city == "rowland heights" | list$city == "Rowland height" | list$city == "Rowland heights" | list$city == "Rowland Heights / Hacienda Heights"] <- "Rowland Heights"
list$city[list$city == "Santa monica"] <- "Santa Monica"
list$city[list$city == "San Pedro Los Angeles" | list$city == 
            "San Pedra Los Angelas" | list$city == "San Pedro, Los Angeles"] <- "San Pedro"
list$city[list$city == "sherman oaks" | list$city == "Sherman oaks" | list$city == 
            "Shermam Oaks" | list$city == "SHERMAN OAKS" | list$city == "Sherman Oaks," | list$city == "Sherman Oaks, Los Angeles"] <- "Sherman Oaks"
list$city[list$city == "studio city" | list$city == "Studio city" | list$city == 
            "STUDIO CITY"] <- "Studio City"
list$city[list$city == "Topanga"] <- "Topanga Canyon"
list$city[list$city == "VALLEY VILLAGE" | list$city == "valley village" | list$city == "Valley village"] <- "Valley Village"
list$city[list$city == "venice" | list$city == "VENICE" | list$city == "Venice , Los Angeles" | list$city == 
            "Venice (Los Angeles)" | list$city == "venice beach" | list$city == "Venice Beach" | list$city == "VENICE BEACH" | list$city == "Venice Beach - Los Angeles" | list$city == "venice," |list$city == 
            "Venice, Los Angeles" | list$city == "Venice Los Angeles"] <- "Venice"
list$city[list$city == "Walnut, California, US"] <- "Walnut"
list$city[list$city == "west Hollywood" | list$city == "West hollywood" | list$city == "WEST HOLLYWOOD HILLS" |list$city == "West Hollywood," | list$city == "North Hollywood" | list$city == "West Hollywood"] <- "Hollywood"
list$city[list$city == "woodland hills" | list$city == "Woodland Wills" | list$city == "Woodland Hills, Los Angeles" | list$city == "Woodland Hills (Los Angeles)" | list$city == "Woodland" | list$city == "woodland Hills"] <- "Woodland Hills"
list$city[list$city == "Walnut, California, US"] <- "Walnut"

list2 <- list[!is.na(list$city), ]
names(list2)[1] <- "listing_id"

# Analysis

### General Initial Exploration

names(list)[1] <- "listing_id"

data1 <- data.frame(sort(table(cal2$listing_id), decreasing = TRUE))
names(data1)[1] <- "listing_id"
names(data1)[2] <- "count"
data1$listing_id <- as.character(data1$listing_id)
list$listing_id <- as.character(list$listing_id)
pop <- left_join(data1, list)

#### What are cities with most bookings?

pop2 <- pop[order(pop$count, decreasing = TRUE), ]
dim(pop2)
### top 100
toppop<-pop2[1:100, ]
pop_10 <- subset(x = pop, select = c(city, count, listing_id))
table(toppop$city) #Hollywood, Studio City, Marina Del Rey


# Popular Beaches in Los Angeles area
### From Tourism Data, popular beaches include Santa Monica, Venice, Will Rogers (Pacific Palisades), Redondo Beach, Long Beach, and Malibu
list_beach <- list2[list2$city == "Santa Monica" | list2$city == "Venice" | list2$city == "Redondo Beach" | list2$city == "Pacific Palisades" | list2$city == "Malibu" | list2$city == "Long Beach", ]
summary_city_beach<-list_beach %>% group_by(., city) %>% summarise(price = mean(price, na.rm = TRUE), ppl = mean(extra_people, na.rm = TRUE), response = mean(host_response_rate, na.rm = TRUE), listings_count = mean(host_listings_count, na.rm = TRUE), review = mean(review_scores_rating, na.rm = TRUE), accommodates = mean(accommodates, na.rm = TRUE), freeguests = mean(guests_included, na.rm =TRUE) )
mean(list_beach$review_scores_rating, na.rm = TRUE)

### they're all rated very highly 
### LA Tourism data also shows that beaches rated very similarly, depends on what you're looking for
#### When are these places booked most often

names(list_beach)[1] <- "listing_id" 
cal2$listing_id <-as.character(cal2$listing_id)

df1 <- left_join(list_beach, cal2, by = "listing_id")
df1$month <- month.abb[df1$month]
dim(df1)[1]/dim(cal2)[1] ## 11% of bookings past year in LA are for these locations.
6/228 # only represent 3% of total locations represented in LA listings
summary(df1$date)
### it appears these bookings are all for May 2017 and onwards


sort(table(df1$month), decreasing =TRUE) # October, September, July, and March are big months to book beach house locations
summary(c(64436, 61571, 61517, 57637, 57161, 56692 ,56391, 56118, 55924, 54155 ,51680 ,47611))

######### How does compare by beach location?

#### Santa Monica
df1_SM <- df1[df1$city == "Santa Monica", ]
sort(table(df1_SM$month), decreasing = TRUE)
summary(c(15393, 13963, 15485, 15082, 12797, 14877, 16680, 15122, 17013, 17857, 14241, 14908))
# July, September, October are popular months here (increasing order)

# first half of year is below average number of bookings per month
# SM accounts for about quarter of beach bookings each month

### Venice
df1_V <- df1[df1$city == "Venice", ]
sort(table(df1_V$month), decreasing = TRUE)
summary(c(9909, 9906, 9722 ,9598 ,9549 ,9139 ,9109 ,8772, 8292, 8264, 7887, 6136 ))
# March, October, January, April are big months (decreasing order)


### Pacific Palisades

df1_PP <- df1[df1$city == "Pacific Palisades", ]
sort(table(df1_PP$month), decreasing = TRUE)
summary(c(724 ,664, 658, 655, 648, 589 ,581, 570, 533, 532 ,520, 517 ))
# July, September, October are big months

### Redondo Beach
df1_RB <- df1[df1$city == "Redondo Beach", ]
sort(table(df1_RB$month), decreasing = TRUE)
summary(c(5697 ,5542, 5100, 4898, 4870, 4827, 4819, 4710, 4666, 4657, 4296, 4167 ))
# October, September, August, July (basically peak summer to when it starts cooling down in October)


### Malibu
df1_M <- df1[df1$city == "Malibu", ]
sort(table(df1_M$month), decreasing = TRUE)
summary(c(9581, 9194, 9049, 9015, 8598, 8257, 8255, 8235, 8057, 7940, 7940, 7419))
# October, September, June, July (basically peak summer to when it starts cooling down in October)

### Long Beach
df1_LB <- df1[df1$city == "Long Beach", ]
sort(table(df1_LB$month), decreasing = TRUE)
summary(c(21729 ,20737, 20194 ,19300 ,18960 ,18527, 18403 ,18067 ,18024 ,17519 , 16054 ))

# LB accounts for about a third of beach bookings each month - some can be accounted to business travelers

#### Plotting
states <- map_data("state")
states2 <- states[states$region == "california",]
p1 <- ggplot()
p1 <- p1 + geom_point(data = list_beach, aes(x = longitude , y = latitude, colour = city)) 
ggplotly(p1)
p1

### this graph demonstrates that the beaches are quite close by and personal experience tells me that they're not that different, so Airbnb can use other differentiators between locations to advertise listings
### PP seems to be a lot of entire homes, explaining higher price point
### SM and Venice seems to have good mix between private room and entire home

### A graph of when people travel and colors representing the locations I looked at

### variables - months, city, count of bookings
p2 <- ggplot()
p2 <- p2 + geom_bar(data = df2[!is.na(df2$month), ], aes(x = factor(month), fill = city))
ggplotly(p2)
p2

p3 <- ggplot()
p3 <- p3 + geom_bar(data = df1, aes(x = factor(month), fill = city))
p3

require(ggmap)
require(maps)
require(ggplot2)
require(dplyr)
require(plotly)

states <- map_data("state")
states2 <- states[states$region == "california",]
p <- ggplot()
p <- p + geom_polygon(data = states2[states2$lat < 35, ], aes(x = long, y = lat, group = group), colour = "orange", fill = "white")
p <- p + geom_point(data = list2, aes(x = longitude , y = latitude, colour = price)) 
ggplotly(p)

months <- 1:12
bookings <- 
# Other Popular Locations

### Let's look at Hollywood, Marina Del Rey, Beverly Hills, Studio City

list_popular <- list2[list2$city == "Hollywood" | list2$city == "Beverly Hills" |list2$city == "Studio City", ]
summary_popular<-list_popular %>% group_by(., city) %>% summarise(price = mean(price, na.rm = TRUE), ppl = mean(extra_people, na.rm = TRUE), response = mean(host_response_rate, na.rm = TRUE), listings_count = mean(host_listings_count, na.rm = TRUE), review = mean(review_scores_rating, na.rm = TRUE), accommodates = mean(accommodates, na.rm = TRUE), freeguests = mean(guests_included, na.rm =TRUE) )
names(list_popular)[1] <- "listing_id" 
cal2$listing_id <- as.character(cal2$listing_id)
list_popular$listing_id <- as.character(list_popular$listing_id)
df2 <- left_join(list_popular, cal2, by = "listing_id")


### Hollywood

df2_HW <- df2[df2$city == "Hollywood" | df2$city == "West Hollywood" | df2$city == "North Hollywood", ]
sort(table(df2_HW$month), decreasing = TRUE)
summary(c(21064, 20007, 19487, 18776 ,18234, 16812, 16682, 16394, 16304, 15854, 15633, 15104 ))

# July, October, September, August, March (Oscars???)

### Bev Hillz
df2_BH <- df2[df2$city == "Beverly Hills", ]
sort(table(df2_BH$month), decreasing = TRUE)
summary(c(11175, 10943, 10586, 10446, 10442 , 9250 , 9143 , 9101 , 9018 , 8809 , 8749  ,8232 ))

# July, October, September

### MDR
df2_MDR <- df2[df2$city == "Marina del Rey", ]
sort(table(df2_MDR$month), decreasing = TRUE)
summary(c(9558, 9375, 9145, 8784, 8310, 8182, 7979 ,7872, 7853, 7745, 7651, 7220))

# July, October, September, June

### Studio City
df2_SC <- df2[df2$city == "Studio City", ]
sort(table(df2_SC$month), decreasing = TRUE)
summary(c(1302, 1229, 1202, 1181, 1116, 1114 ,1110, 1080, 1047, 1008,  983,  882))

# July, September, October, August
