# NERD Problems in R

# NERD1_CoG_Weber

cities <- c('Boston', 'Providence','Springfield')

# Going to use the orloca package

# Uncomment out and install if not yet installed
#install.packages("orloca")

# load the orloca package
library(orloca)

weightedLocations <- loca.p(x = c(100,86,20),
                            y = c(80,40,60),
                            w = c(425,320,220))

# Calculate the Weber distance answer 

(X_Y_Weber <- zsummin(weightedLocations, max.iter = 500,
        verbose = FALSE))

# Calculate the Center of Gravity

totalWeght <- sum(weightedLocations@w)

C_of_G_x <- c(crossprod(weightedLocations@w,weightedLocations@x))/totalWeght
C_of_G_y <- c(crossprod(weightedLocations@w,weightedLocations@y))/totalWeght

#print(paste0("The Weber co-ordinates are ",round(X_Y_Weber[1],2)," ",
#      round(X_Y_Weber[2],2)))
#print(paste0("The  COG  co-ordinates are ",round(C_of_G_x,2)," ",
#             round(C_of_G_y,2)))

WeberDist <- sqrt((weightedLocations@x-X_Y_Weber[1])^2+
                          (weightedLocations@y-X_Y_Weber[2])^2)
WeberWgtDist <- round(WeberDist*weightedLocations@w,0)

COGDist <- sqrt((weightedLocations@x-C_of_G_x)^2+
                          (weightedLocations@y-C_of_G_y)^2)
COGWgtDist <- round(COGDist*weightedLocations@w,0)

weightRow <- c(" ",totalWeght," "," ",
               "Total Weight Dist",
               sum(WeberWgtDist)," ",sum(COGWgtDist))

outMatrix <- cbind(cities,weightedLocations@w,
             weightedLocations@x,weightedLocations@y,
             round(WeberDist,2),WeberWgtDist,
             round(COGDist,2),COGWgtDist
                   )
outMatrix <- rbind(outMatrix,weightRow)
outMatrix
colnames(outMatrix) <- c("Cities", "Wgt","X","Y",
                         "Dist","Wgt Dist",
                         "Dist","Wgt Dist")
library(tables)
tabular((Species +1) ~ (n=1) + Format(digits=2)*
                (Sepal.Length + Sepal.Width)*(mean + sd), data=iris)
