value<- (300-278.6033)/53.8656
value

round(value, 4)

#-------------------------------------------------------------------------------
data("cars")
head(cars)

dist <- cars$dist
dist
mean(dist)
median(dist)
sd(dist)
var(dist)
min(dist)
max(dist)

#-------------------------------------------------------------------------------
#create histogram
hist(cars$speed)

boxplot(cars$speed)

#-------------------------------------------------------------------------------
qqnorm(cars$dist) # also QQ plot
qqline(cars$dist)
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

#-------------------------------------------------------------------------------
#IQR contains 50 percent of the data

# median = center line = middle range of data (quartile 2) = 50th percentile
# Quartile 1 = 25th percentile
#index_1 <- 0.25*(n+1) #n=no of data points
#index_2 <- 0.75*(n+1)

# Quartile 3 = 75th percentile
# IQR = contains 50% of the data
# Minimum = Q1 - 1.5 * IQR (lower fence)
# Maximum = Q3 + 1.5 * IQR (upper fence)
# Any points outside is a outlier

# Sample dataset
my_data <- c(12, 18, 21, 23, 27, 30, 35, 38, 40, 45)

# Calculate Q1 (25th percentile)
q1 <- quantile(my_data, probs = 0.25)

# Calculate Q3 (75th percentile)
q3 <- quantile(my_data, probs = 0.75)

# Print Q1 and Q3
print(q1)
print(q3)
summary(my_data)

# Calculate the IQR
iqr_value <- IQR(my_data)

# Print the IQR
print(iqr_value)
#-------------------------------------------------------------------------------
#week2lab q2
# Given data
mean_individual = 0.03  # Mean of individual fund's return
sd_individual = 0.10    # Standard deviation of individual fund's return
n_funds = 50            # Number of mutual funds

# (a) Pr(Y > 34.1%)
prob_a = 1 - pnorm(0.341, mean = mean_individual, sd = sd_individual)
print(prob_a)

# (b) Pr(Y > 15.7%)
prob_b = 1 - pnorm(0.157, mean = mean_individual, sd = sd_individual)
print(prob_b)

# (c) Pr(Y < -13.3%)
prob_c = pnorm(-0.133, mean = mean_individual, sd = sd_individual)
print(prob_c)

# (d) Pr(My > 7.4%)
mean_sample = mean_individual
sd_sample = sd_individual / sqrt(n_funds)  # Standard deviation of sample mean
prob_d = 1 - pnorm(0.074, mean = mean_sample, sd = sd_sample)
print(prob_d)

# (e) Pr(My > 4.8%)
prob_e = 1 - pnorm(0.048, mean = mean_sample, sd = sd_sample)
print(prob_e)

# (f) Pr(My < 0.7%)
prob_f = pnorm(0.007, mean = mean_sample, sd = sd_sample)
print(prob_f)
#-------------------------------------------------------------------------------

nbasalary<- read.csv("nbasalary.csv")



#-------------------------------------------------------------------------------
hep <- read.csv("hep.csv")




#-------------------------------------------------------------------------------
countries <- read.csv("countries.csv")
# Load necessary libraries
library(dplyr)
library(stats)

# Assuming you have loaded the Gapminder data as 'countries'

# (a) Calculate the sample mean and sample standard deviation of Pop
sample_mean_pop <- mean(countries$Pop)
sample_sd_pop <- sd(countries$Pop)



# (b) Explanation for why calculating a confidence interval for the population mean Pop is not useful
#"Calculating a confidence interval for the population mean of Pop would not be useful for understanding mean population counts for all countries in the world because the sample only includes the 55 most populous countries, which is not representative of the entire world population. The sample is not randomly selected from all countries, so the results wouldn't generalize to the entire world population."



# (c) Calculate a 95% confidence interval for the population mean of Life
confidence_level <- 0.95
n <- nrow(countries)

sample_mean_life <- mean(countries$Life)

sample_sd_life <- sd(countries$Life)

standard_error_life <- sample_sd_life / sqrt(n)

#qt() function is used to calculate the t-score for the confidence interval calculation.
margin_of_error <- qt((1 + confidence_level) / 2, df = n - 1) * standard_error_life
confidence_interval <- c(sample_mean_life - margin_of_error, sample_mean_life + margin_of_error)

print(paste("95% Confidence Interval for the population mean of Life:", confidence_interval))

