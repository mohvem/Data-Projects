#### Condensing Unit Type from 42 levels to 7 levels to simplify analysis

## group 1 = engine 
## group 2 = als rescue ambulance
## group 3 = BLS Rescue Ambulance
# Group 4 = Truck
# group 5 = Battalion Chief
# group 6 = Emergency Medical Supervisor
# group 7 = other

groups <- function(x){
  if (x == "E - Engine"){
    val <- "Engine"
  } else{
    if (x == "RA - ALS Rescue Ambulance"){
      val <- "ALS_Rescue_Ambulance"
    } else {
      if (x == "RA8xx - BLS Rescue Ambulance"){
        val <- "BLS_Rescue_Ambulance"
      } else {
        if (x == "T - Truck"){
          val <- "Truck"
        } else {
          if (x == "BC - Battalion Chief"){
            val <- "Battalion_Chief"
          } else {
            if (x == "EM - Emergency Medical Supervisor"){
              val <- "Emergency_Medical_Supervisor"
            } else {
              val <- "Other"
            }
          }
        }
      }
    }
  }
  return(val)
}

lafd3$unit_groups <- as.factor(sapply(lafd3$Unit.Type, groups))

plot(lafd3$unit_groups, lafd3$transformed)

# Grouping First.in.District ----------------------------------------------
### condensing the district variable into 4 levels

fdgroups <- read.csv("/Users/mvembusubramanian/Downloads/LAFD_First_In_Districts.csv", header = TRUE)
names(fdgroups)[4] <- "First.in.District"
fdgroups$First.in.District <- as.integer(fdgroups$First.in.District)
lafd3$First.in.District <- as.integer(lafd3$First.in.District)

fdgroups <- fdgroups[sort(fdgroups$First.in.District, decreasing = TRUE), ] 
fdgrps2 <- fdgroups[, c(2,3,4)]
for (i in 1:102){
  if (fdgrps2$BATTALION_[i] == 1 || fdgrps2$BATTALION_[i] == 2 || fdgrps2$BATTALION_[i] == 11){
    fdgrps2$DIVISION_N[i] <- "Central"
  } else {
    if (fdgrps2$BATTALION_[i] == 4 || fdgrps2$BATTALION_[i] == 5 || fdgrps2$BATTALION_[i] == 9){
      fdgrps2$DIVISION_N[i] <- "West"
    } else {
      if (fdgrps2$BATTALION_[i] == 10 || fdgrps2$BATTALION_[i] == 12 || fdgrps2$BATTALION_[i] == 14 ||fdgrps2$BATTALION_[i] == 15 ||fdgrps2$BATTALION_ == 17){
        fdgrps2$DIVISION_N[i] <- "Valley"
      } else {
        fdgrps2$DIVISION_N[i] <- "South"
      }
    }
  }
}

lafd3 <- left_join(lafd3, fdgrps2)
lafd3$First.in.District <- factor(lafd3$First.in.District)
fdgrps2$First.in.District <- factor(fdgrps2$First.in.District)



names(lafd3)[17] <- "FD_Bureau"

lafd3 <- lafd3[,-16]



require(ggplot2)
ggplot(data = lafd3, mapping = aes(x = FD_Bureau, y = transformed)) +
         geom_boxplot()  # based on this they don't appear to be that different

ggplot(data = lafd3, mapping = aes(x = unit_groups, y = transformed)) +
  geom_boxplot()


## 1 refers to the Central Bureau, 2 refers to West Bureau, 3 refers to Valley Bureau
## 4 refers to South Bureau



# Grouping Dispatch Status by Frequency -----------------------------------

nrow(lafd3)/5
sort(table(lafd3$Dispatch.Status))
sum(sort(table(lafd3$Dispatch.Status))[1:6])
plot(lafd$Dispatch.Status, lafd3$transformed)
dispatch <- function(x){
  if (x == "TSP"){
    val <- "Other"
  } else {
    if (x == "OVD"){
      val <- "Other"
    } else {
      if (x == "WRS"){
        val <- "Other"
      } else {
        if (x == "CAV"){
          val <- "Other"
        } else {
          if (x == "ENR"){
            val <- "Other"
          } else {
            val <- x
          }
        }
      }
    }
  }
  return(val)
}  
# I added the bottom 6 groups into one bucket except for PGR which seemed to be 
# significantly different from the boxplot

lafd3$disp_stat_groups <- sapply(lafd3$Dispatch.Status, dispatch)
lafd3$disp_stat_groups[lafd3$disp_stat_groups == 1] <- "AVI"
lafd3$disp_stat_groups[lafd3$disp_stat_groups == 10] <- "RAD"
lafd3$disp_stat_groups[lafd3$disp_stat_groups == 4] <- "HSP"
lafd3$disp_stat_groups[lafd3$disp_stat_groups == 5] <- "NAV"
lafd3$disp_stat_groups[lafd3$disp_stat_groups == 6] <- "ONS"
lafd3$disp_stat_groups[lafd3$disp_stat_groups == 8] <- "PGR"
lafd3$disp_stat_groups[lafd3$disp_stat_groups == 9] <- "QTR"

table(lafd3$disp_stat_groups)


# Making Dispatch Status Binary -------------------------------------------

AVI <-function(x){
  if (is.na(x)){
    val <- 0
  } else {
    if(x == "AVI"){
      val <-1
    } else {
      val <- 0
    }
  }
  return(val)
}

RAD <-function(x){
  if (is.na(x)){
    val <- 0
  } else {
    if(x == "RAD"){
      val <-1
    } else {
      val <- 0
    }
  }
  return(val)
}

HSP <-function(x){
  if (is.na(x)){
    val <- 0
  } else {
    if(x == "HSP"){
      val <-1
    } else {
      val <- 0
    }
  }
  return(val)
}

NAV <-function(x){
  if (is.na(x)){
    val <- 0
  } else {
    if(x == "NAV"){
      val <-1
    } else {
      val <- 0
    }
  }
  return(val)
}

# Grouping Hour by Times of Day -------------------------------------------
hist(lafd3$hour)
### most common over night and late at night and evening rush hour
### not common during working hours -- makes sense


require(dplyr)
times <- function(x){
  if (x %in% c(24,1,2,3,4)){
    val <- "Overnight"
  } else {
    if (x %in% 5:8){
      val <- "Early.AM"
    } else {
      if (x %in% 9:12){
        val <- "Mid.AM"
      } else {
        if (x %in% 13:16){
          val <- "Afternoon"
        } else{
          if (x %in% 17:20){
            val <- "Evening"
          } else {
            val <- "Night"
          }
        }
      }
    }
  }
}

lafd3$times <- sapply(lafd3$hour, times)

table(lafd3$times)

afternoon <- function(x){
  if (is.na(x)){
    val <- 0
  } else {
    if (x == "Afternoon") {
      val <- 1
    } else {
      val <- 0 
    }
  }
  return(val)
}

early.am <- function(x){
  if (is.na(x)){
    val <- 0
  } else {
    if (x == "Early.AM") {
      val <- 1
    } else {
      val <- 0 
    }
  }
  return(val)
}

evening <- function(x){
  if (is.na(x)){
    val <- 0
  } else {
    if (x == "Evening") {
      val <- 1
    } else {
      val <- 0 
    }
  }
  return(val)
}

mid.am <- function(x){
  if (is.na(x)){
    val <- 0
  } else {
    if (x == "Mid.AM") {
      val <- 1
    } else {
      val <- 0 
    }
  }
  return(val)
}

night <- function(x){
  if (is.na(x)){
    val <- 0
  } else {
    if (x == "Night") {
      val <- 1
    } else {
      val <- 0 
    }
  }
  return(val)
}

overnight <- function(x){
  if (is.na(x)){
    val <- 0
  } else {
    if (x == "Overnight") {
      val <- 1
    } else {
      val <- 0 
    }
  }
  return(val)
}


lafd4$Is_After <- sapply(lafd4$times, afternoon)
lafd4$Is_Early <- sapply(lafd4$times, early.am)
lafd4$Is_Evening <- sapply(lafd4$times, evening)
lafd4$Is_Mid <- sapply(lafd4$times, mid.am)
lafd4$Is_Night <- sapply(lafd4$times, night)
lafd4$Is_Over <- sapply(lafd4$times, overnight)

table(lafd3$times)
table(lafd4$Is_After)
table(lafd4$Is_Early)
table(lafd4$Is_Night)
table(lafd4$Is_Mid)
table(lafd4$Is_Over)
table(lafd4$Is_Evening)

### tables to make sure it separated them correctly which it did go you
# New Dataset Frequency -------------------------------------------------------------

lafd4 <- lafd3[, c(1:3, 5, 6, 9, 10, 15, 16, 18 ,11, 13 )]

## this dataset has the untransformed variables as well as the transformed variables with 
## respect to frequency and intuition

## still have to convert things to binary

########## FD_Bureau Binary ----------------------------------------------------------

table(lafd4$FD_Bureau)



central <- function(x){
  if(is.na(x)){
    val <- 0
  } else {
    if (x == "Central"){
      val <-1
    } else {
      val <-0
    }
  }
  return(val)
}

lafd4$Is_Central<-sapply(lafd4$FD_Bureau, central)

south <- function(x){
  if(is.na(x)){
    val <- 0
  } else {
    if (x == "South"){
      val <-1
    } else {
      val <-0
    }
  }
  return(val)
}

west <- function(x){
  if(is.na(x)){
    val <- 0
  } else {
    if (x == "West"){
      val <-1
    } else {
      val <-0
    }
  }
  return(val)
}
valley <- function(x){
  if(is.na(x)){
    val <- 0
  } else {
    if (x == "Valley"){
      val <-1
    } else {
      val <-0
    }
  }
  return(val)
}

lafd4$Is_South <- sapply(lafd4$FD_Bureau, south)
lafd4$Is_West <- sapply(lafd4$FD_Bureau, west)
lafd4$Is_Valley <- sapply(lafd4$FD_Bureau, valley)

table(lafd4$FD_Bureau)
table(lafd4$Is_South)
table(lafd4$Is_Central)
table(lafd4$Is_West)
table(lafd4$Is_Valley)


# Make Unit Type Groups Binary --------------------------------------------

table(lafd4$unit_groups)

ALS <- function(x){
  if(is.na(x)){
    val <- 0
  } else {
    if (x == "ALS_Rescue_Ambulance"){
      val <-1
    } else {
      val <-0
    }
  }
  return(val)
}

BLS <- function(x){
  if(is.na(x)){
    val <- 0
  } else {
    if (x == "BLS_Rescue_Ambulance"){
      val <-1
    } else {
      val <-0
    }
  }
  return(val)
}

Battalion <- function(x){
  if(is.na(x)){
    val <- 0
  } else {
    if (x == "Battalion_Chief"){
      val <-1
    } else {
      val <-0
    }
  }
  return(val)
}

lafd4$Is_ALS <- sapply(lafd4$unit_groups, ALS)
lafd4$Is_BLS <- sapply(lafd4$unit_groups, BLS)
lafd4$Is_Battalion <- sapply(lafd4$unit_groups, Battalion)



  


