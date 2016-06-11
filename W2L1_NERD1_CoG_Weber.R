# NERD Problems in R

# NERD1_CoG_Weber


cityData <- data.frame(id = c('BO','PR','SP'),
                   cityName = c('Boston', 'Providence','Springfield'),
                   wgt = c(425,320,220),
                   X = c(100,86,20),
                   Y = c(80,40,60))

cityData$id <- as.character(cityData$id)
cityData$cityName <- as.character(cityData$cityName)


# Going to use the orloca package

# Uncomment out and install if not yet installed
#install.packages("orloca")

# load the orloca package
if(require('orloca')){
  print('orloca package loaded OK')
}else{
  stop('Please install the orloca package')
}


# Create a loca.p object with the X-Y cordinates and the weights

weightedLocations <- loca.p(x = cityData$X,
                            y = cityData$Y,
                            w = cityData$wgt)
                            
# Use zummin in orloca package to calculate the Weber Coordinates

X_Y_Weber <- zsummin(weightedLocations, max.iter = 500,
        verbose = FALSE)

# Calculate the Center of Gravity X - Y Coordinates

# Get the total weight
totalWeight <- sum(weightedLocations@w)

# Center Of Gravity X coordinate  
COG_x <- c(crossprod(weightedLocations@w,weightedLocations@x))/totalWeight

# Center Of Gravity Y coordinate  
COG_y <- c(crossprod(weightedLocations@w,weightedLocations@y))/totalWeight

# Calculate each City Weber Distance
WeberDists <- sqrt((weightedLocations@x-X_Y_Weber[1])^2+
                          (weightedLocations@y-X_Y_Weber[2])^2)

# Calculate each City Weber Weight Distance
WeberWgtDists <- WeberDists*weightedLocations@w

# Calculate each City COG Distance
COGDists <- sqrt((weightedLocations@x-COG_x)^2+
                         (weightedLocations@y-COG_y)^2)

# Calculate each City COG Weight Distance
COGWgtDists <- COGDists*weightedLocations@w

# Add the Center of Gravity and Weber Coordinates to the CityData
cityData <- rbind(cityData, data.frame(
  id = 'CG', cityName ='Center of Gravity', wgt = 0,
  X = round(COG_x,0) , Y =round(COG_y,0)))

cityData <- rbind(cityData, data.frame(
  id = 'WC', cityName ='Weber Calculated Center', wgt = 0 ,
  X =round(X_Y_Weber[1],0),Y = round(X_Y_Weber[2],0)))

totalWeberWeightDist <- sum(WeberWgtDists)
avgWeberDist <- round(totalWeberWeightDist / totalWeight,2)

totalCOGWeightDist <- sum(COGWgtDists)
avgCOGDist <- round(totalCOGWeightDist / totalWeight,2)

##################
# At this point you can stop and review the variable.
#  All the calculations have been done.
#     Would just need to review the data

cityData

distWgtDist <- data.frame(cbind(cityData$cityName[1:3],
                                round(WeberDists,2),round(WeberWgtDists,0),
                     round(COGDists,2),round(COGWgtDists,0)),stringsAsFactors = F)

colnames(distWgtDist) <- c("cityName", "WeberDist", "WeberWgtDist", "CoGDist","CoGWgtDist")
distWgtDist

avgCOGDist
avgWeberDist


## So that would do it to calculate and review the results
#   What follows is a ggplot plot of the cities, Weber point and COG 
#   on an x-y plot

library(ggplot2)
library(ggrepel)
        

p1 <- ggplot(cityData, aes(x = X, y = Y))
p1 <- p1 + geom_point() + 
  geom_text_repel(aes(label=id), size=3, nudge_x = -4) +
  geom_segment(x = cityData$X[1], y = cityData$Y[1],
               xend = cityData$X[2], yend = cityData$Y[2]) +
  geom_segment(x = cityData$X[1], y = cityData$Y[1],
               xend = cityData$X[3], yend = cityData$Y[3]) +
  geom_segment(x = cityData$X[3], y = cityData$Y[3],
               xend = cityData$X[2], yend = cityData$Y[2])
 
p1

# Remove COG and Weber data
cityData <- cityData[-c(4,5),]

# Add Calculated distance and weights for Weber and CoG
allData <- merge(cityData, distWgtDist, by = "cityName")
allData$WeberWgtDist <- as.numeric(allData$WeberWgtDist)
allData$CoGWgtDist <- as.numeric(allData$CoGWgtDist)

# Creta dummy df for total weight line
totalWeightDF <- data.frame(weights=c("Total Package Weight",
                                    "Weber Total Weight Distance",
                                    "CoG Total Weight Distance"),
                          weightValues = c(totalWeight,
                                           round(totalWeberWeightDist,0),
                                           round(totalCOGWeightDist,0)))

summaryData <- data.frame(type = c("Weber", "CoG"),
                          X = c(X_Y_Weber[1],COG_x),
                          Y = c(X_Y_Weber[2],COG_y),
                          TotalWwightDist = c(totalWeberWeightDist,totalCOGWeightDist),
                          averageDist = c(avgWeberDist, avgCOGDist))

summaryData$X <- round(as.numeric(summaryData$X),0)  
summaryData$Y <- round(as.numeric(summaryData$Y),0)
summaryData$TotalWwightDist <- round(as.numeric(summaryData$TotalWwightDist),0)
summaryData$averageDist <- round(as.numeric(summaryData$averageDist),2)


library(stargazer)
stargazer(allData, summary=F, type = "text", rownames = F)
stargazer(totalWeightDF, summary = F, type = "text", rownames = F)
stargazer(summaryData, summary = F, type = "text", rownames = F)
