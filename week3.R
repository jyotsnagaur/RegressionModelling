mydata <- trees
attach(mydata)
library(ggplot2)
ggplot(trees, aes(x=Height, y=Volume))+
  geom_point()+
  geom_smooth(method=lm)
myresult <- lm(Volume ~ Height)
anova(myresult)
summary(myresult)
abline(myresult)
 

