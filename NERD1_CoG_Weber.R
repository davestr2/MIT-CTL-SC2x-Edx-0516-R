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
totalWeght <- sum(weightedLocations@w)

# Center Of Gravity X coordinate  
COG_x <- c(crossprod(weightedLocations@w,weightedLocations@x))/totalWeght

# Center Of Gravity Y coordinate  
COG_y <- c(crossprod(weightedLocations@w,weightedLocations@y))/totalWeght

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

library(ggplot2)
library(ggrepel)
        
cityData <- rbind(cityData, data.frame(
  id = 'CG', cityName ='Center of Gravity', wgt = 0,
  X = COG_x , Y =COG_y))

cityData <- rbind(cityData, data.frame(
  id = 'WC', cityName ='Weber calculated Center', wgt = 0 ,
  X = X_Y_Weber[1],Y = X_Y_Weber[2]))
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

cityData$X <- round(cityData$X,0)
cityData$Y <- round(cityData$Y,0)

cityData <- cityData[-c(4,5),]

cityData <- cbind(cityData,WeberDists,WeberWgtDists,COGDists,COGWgtDists)
cityData$WeberDists <- round(cityData$WeberDists,2)
cityData$WeberWgtDists <- round(cityData$WeberWgtDists,0)
cityData$COGDists <- round(cityData$COGDists,2)
cityData$COGWgtDists <- round(cityData$COGWgtDists,0)

cityData

print(paste("Total weight-",totalWeght,
            " Total Weber Wgt Dist-",sum(cityData$WeberWgtDists),
            " Total COG Wgt Dist-",sum(cityData$COGWgtDists)))

print(paste("Weber Coordinates", round(X_Y_Weber[1],0)," ",round(X_Y_Weber[2],0)))
print(paste("CoG   Coordinates",round(COG_x,0), round(COG_y,0)))
print(paste("Average Diantace Weber = ", round(sum(cityData$WeberWgtDists)/totalWeght,2)))
print(paste("Average Diantace COG   = ", round(sum(cityData$COGWgtDists)/totalWeght,2)))
