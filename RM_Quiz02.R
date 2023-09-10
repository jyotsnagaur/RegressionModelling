movies <- read.csv("movies.csv")
attach(movies)

head(movies, 3) # not shown here
movies6 <- movies[ ,-1]

### Step 2 - Is multivariate linear regression appropriate? – get ideas

# scatter plot matrix
pairs(movies6)


# correlation matrix
cor(movies6) # this is the r value, not R^2 ## 

cor(Box, Rate)
cor(Box, User)

### Step 3 - Fit the regression model
################
## First, create the all predictors model (using the 6 variables) - This is
## called the Complete Model (sometimes the full model)

my.result6 <- lm(Box ~ . , data=movies6) # Complete model

### Step 4 - Look at the results
summary(my.result6) # complete model


anova(my.result6)

deviance(my.result6) # Complete Model


### Now we are making a Reduced Model #####
## as there are variables that may not be contributing
## to the models results
#### Question (b):
# We need to make a model with a reduced set of variables
# that are contributing to the model results
# 3 predictor model, we will call a reduced model
movies3 <- movies[,2:5] # Make reduced model
head(movies3)

my.result3 <- lm(Box ~ ., data=movies3) # Reduced model
summary(my.result3)

anova(my.result3)


confint(my.result3, level=0.95) # are there any betas that include 0?

deviance(my.result3)


## Model Selection ##
# Question (c--d):
# F-test for H0: b4=b5=b6=0 (if H0 = 0 = no slope)
# If H0 is true assumed, my.result3 is correct
# If H0 is NOT true, my.result6 is correct

anova(my.result3, my.result6) # reduced model, complete model


# Question (c)
# Alternative using F formula
dftop <- 6-3 # or just 3, the number of b's in NH
dfbot <- 25-1-6 # or df for residuls in anova for complete model
Ftop <- (32823-32435)/dftop # (MSred for reduced minus MSred for complete model)/3
Fbot <- 32435/dfbot #it's actually the same as MSRes in anova for complete model
F <- Ftop/Fbot # F formula
F






# use RHS tail only with alpha=0.05 to find F_critical value
# choose lower.tail = F to use the upper tail i.e. RHS tail

qf(0.05,3,18,lower.tail = F)

#or better use RHS tail to the right of F value to find the probability value
#the probability value will then be compared with say alpha=0.05
#choose lower.tail = F to use the upper tail i.e. RHS tail
p.value <- pf(F,3,18,lower.tail = F) # RHS tail
p.value

1- pf(F,3,18)

# to get F and p value to compare the 2 fitted models
# if p value is NOT smaller than say alpha=0.05 then do not reject NH
# the reduced model is Better
# if p value is smaller than say alpha=0.05
# the complete model is Better


#### Question (d)
anova(my.result6, my.result3) # complete and reduced models

#### Question (e)
summary(my.result6) # complete model

summary(my.result3)

? deviance
deviance(my.result6) # complete model
## [1] 32435.31
deviance(my.result3) # reduced model
## [1] 32822.96 # can’t just compare deviance as the df is different

## Select the best model:- reduced model, smaller residual standard errors


#practice quiz Q4
data(trees)
attach(trees)

model <- lm(Volume~Height)

summary(model)




#Q4
modelc <- lm(Volume~Girth+Height)
s <- summary(modelc)

mse <- s$sigma^2

round(mse, 3)

rsquared <- s$r.squared
round(rsquared,3)

#DO RESIDUALS FOLLOW NORMAL DISTRIBUTION
# Extract residuals
residuals <- residuals(modelc)

# Create a histogram
hist(residuals, main="Histogram of Residuals")

# Create a Q-Q plot
qqnorm(residuals)
qqline(residuals)


#DO RESIDUALS HAVE OUTLIERS
# Create a residual plot against predicted values
plot(predict(model), residuals, main="Residual Plot")
abline(h = 0, col = "red")  # Add a horizontal line at y = 0

# Create a boxplot of residuals
boxplot(residuals, main="Residual Boxplot")


#DO RESIDUALS HAVE OUTLIERS





