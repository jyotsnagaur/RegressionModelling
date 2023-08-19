value<- (300-278.6033)/53.8656
value

round(value, 4)

#-------------------------------------------------------------------------------
data("cars")
head(cars)

dist <- cars$dist
dist
mean(dist)

#-------------------------------------------------------------------------------
#create scatterplot of dist(y) vs speed(x)

plot(cars$speed, cars$dist,
     main="sctrplt of dist vs speed",
     xlab="speed",
     ylab="distance",
     pch=16, col="blue") #pch is plotting character, in this case solid circle

#-------------------------------------------------------------------------------
#a linear model for dist vs speed and report its intercept estimate, 
#slope estimates, and R square value

model <- lm(dist~speed, data=cars)
summary(model)
#intercept=-17.5791
#slope=3.9324

#r square value from the summary
r <- summary(model)$r.squared

round(r,4) #round off to 4 places



