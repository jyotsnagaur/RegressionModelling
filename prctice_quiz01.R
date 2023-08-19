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
# r square value ranges [0,1]
# r square means how well model explains variation in dependent variable(dist) based on 
#independent variable(speed)
#higher value indicates better fit of model to the data
r <- summary(model)$r.squared

round(r,4) #round off to 4 places

#-------------------------------------------------------------------------------
#further probability questions from week2
m <- 3
s <- 10

#P(y<34.1)
pnorm(34.1,m,s)

#P(y>34.1)
1-pnorm(34.1,m,s)
#-------------------------------------------------------------------------------
 #error in regression model if x=1.4, y obsv =5.3
# Y pred = 1.8 + 2.5X 
Y_pred <- 1.8 + 2.5*1.4
Y_pred
Y_obsv <- 5.7
E <- Y_obsv-Y_pred
E