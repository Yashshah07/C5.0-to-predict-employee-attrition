
rm(list=ls())   # Clears the environment , removes all the objects 

library(rpart)

## load the file
rm(list=ls())
Emp1 <-read.csv("IBM_Employee_Attrition_V2.csv")
View(Emp1)

?na.omit() ## remove all missing records ##

## Applying C5.0 to predict employee attrition ##
install.packages('C50')
library('C50')
View(Emp1)

index <- seq(1,nrow(Emp1),by=5)
test <- Emp1[index, ]
train <- Emp1[-index, ]

?c5.0
mytree <- C5.0(factor(Attrition)~. , data = Emp1)
summary(mytree)
dev.off()
plot(mytree)

## Applying prediction methods
prediction <- predict( mytree ,test , type="class" )
table(actual=test[,2],prediction)
wrong<- (test[,2]==prediction)
C5.0_rate <- sum(wrong)/length(wrong)*100
View(C5.0_rate)
