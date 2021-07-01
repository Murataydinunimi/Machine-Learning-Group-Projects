setwd("C:/Users/barla/OneDrive/Desktop/Academia/Data Science and Economics/Statistical Learning/Project")
library(caret)
library(gridExtra)
library(ggplot2)
library(dplyr)
library(gplots)
library(plyr)
library("devtools")
library("patchwork")
library(MASS)
library(pROC)
library(ROCR)



data<-read.csv("clients.csv")


data<-data[-c(1,2,3)]
data<-data[data$EstimatedSalary>8000,]
nrow(data)

View(data)

summary(data)
#we have some categorical variables. Factor them.

sum(is.na(data)) # no missing values.

data[,c(2,3,5,7,8,9,11)]<-lapply(data[,c(2,3,5,7,8,9,11)], as.factor)

#Churn Distribution
ggplot(data, aes(x = Exited)) +
  geom_bar(aes(fill = Exited)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

boxplot(Age~Exited, data = data)

ggplot(data, aes(x=Gender)) + ggtitle("Gender") + xlab("Gender") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage")  + theme_minimal()




par(mfrow=c(2,2))



c1=ggplot(data, aes(x=CreditScore)) + 
  geom_histogram(color="red", fill="blue")

c2=ggplot(data, aes(x=Age)) + 
  geom_histogram(color="red", fill="blue")

c3=ggplot(data, aes(x=Balance)) + 
  geom_histogram(color="red", fill="blue")

c4=ggplot(data, aes(x=EstimatedSalary)) + 
  geom_histogram(color="red", fill="blue")


c1+c2+c3+c4
#Correlations
colnames(data)
mydata<- data[,c(1,4,6,10)]
res<-cor(mydata)
round(res,2)
View(data)



library("Hmisc")

#Correlations with P values

rcorr(as.matrix(mydata))

library(car)

scatterplotMatrix(mydata,regLine=TRUE) # it plots the first correlation matrix we had, the res variable. 

col<-colorRampPalette(c("blue","white","red"))(20) #blue being minimum and red being maximum.
heatmap(x=res, col=col,symm=TRUE,Colv=NA,Rowv=NA)


#No correlation between continuous variables so we do not have to worry about multicollinearity.

#AGE HIST AND BOX

#Let's look at them in greater detail.
par(mfrow=c(1,2))

Agebox=ggplot(data, aes(x=Age,y=Exited,fill=Exited)) + 
  geom_boxplot()+coord_flip() +geom_boxplot(outlier.colour="red", outlier.shape=8,
                                            outlier.size=4)
Age_hist=ggplot(data, aes(x = Age)) +
  geom_histogram(aes(color = Exited), fill = "white",
                 position = "identity", bins = 30) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +  scale_fill_manual(values = c("#00AFBB", "#E7B800"))

Agebox+Age_hist
#Interestingly, there is a clear difference between age groups since older customers are more likely to churn. 
#This could potentially indicate that preferences change with age, and the bank hasn't adapted its strategy to meet the requirements of older customers.

#Testing the independence between age and churn

chisq.test(table(data$Age, data$Exited)) # p value is lower than 0.05 so we reject the hypothesis and conclude that they are dependent.


#CREDIT HIST AND BOX

#This does not give us any useful information
creditbox<-ggplot(data, aes(x=CreditScore,y=Exited,fill=Exited)) + 
  geom_boxplot()+coord_flip() +geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4)

credit_hist<-ggplot(data, aes(x = CreditScore)) +
  geom_histogram(aes(color = Exited), fill = "white",
                 position = "identity", bins = 30) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +  scale_fill_manual(values = c("#00AFBB", "#E7B800"))

creditbox+credit_hist

#There is no significant difference between retained and churned customers in terms of their credit score.






#Again, the two distributions are quite similar. There is a big percentage of non-churned customers with a low account balance.

#BALANCE HIST AND BOX

balancebox<-ggplot(data, aes(x=Balance,y=Exited,fill=Exited)) + 
  geom_boxplot()+coord_flip() +geom_boxplot(outlier.colour="red", outlier.shape=8,
                                            outlier.size=4)


balance_hist<-ggplot(data, aes(x = Balance)) +
  geom_histogram(aes(color = Exited), fill = "white",
                 position = "identity", bins = 30) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +  scale_fill_manual(values = c("#00AFBB", "#E7B800")) 



balancebox+balance_hist

balance_hist



#ESTIMATED SALARY HIST AND BOX

salarybox<-ggplot(data, aes(x=EstimatedSalary, y=Exited, fill=Exited)) +
  geom_boxplot()+coord_flip()



salary_hist<-ggplot(data, aes(x = EstimatedSalary)) +
  geom_histogram(aes(color = Exited), fill = "white",
                 position = "identity", bins = 30) +
  scale_color_manual(values = c("red", "blue")) +  scale_fill_manual(values = c("red", "blue"))



salarybox+salary_hist

#Both churned and retained customers display a similar uniform distribution for their salary.
#Consequently, we can conclude that salary doesn't have a significant effect on the likelihood to churn.


#CATEGORICAL VARIABLES

#Geography

p1 <- ggplot(data, aes(x = Geography)) +
  geom_bar(aes(fill = Exited)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

p1+p2
#Gender
p2 <- ggplot(data, aes(x = Gender)) +
  geom_bar(aes(fill = Exited)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

p2

#Tenure
p3 <- ggplot(data, aes(x = Tenure)) +
  geom_bar(aes(fill = Exited)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)
p3

#Number of products
p4 <- ggplot(data, aes(x = NumOfProducts)) +
  geom_bar(aes(fill = Exited)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)



p4

#Has-card

p5 <- ggplot(data, aes(x = HasCrCard)) +
  geom_bar(aes(fill = Exited)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)+ scale_x_discrete(breaks=c("0", "1"),
                                        labels=c("Has-No-Card", "Has-Card"))

p5



#do a box plot if you can 



#Is-Active-Member

p6 <- ggplot(data, aes(x = IsActiveMember)) +
  geom_bar(aes(fill = Exited)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

p6


p2

#Important points:
  
#The bank has customers in three countries (France, Spain, and Germany). Most customers are in France.
#There are more male customers than females,
#Only a small percentage leaves within the first year. The count of customers in tenure years between 1 and 9 is almost the same,
#Most of the customers have purchased 1 or 2 products, while a small portion has purchased 3 and 4,
#A significant majority of customers has a credit card, and
#Almost 50% of customers are not active.




#Data Preprocessing.

Chi_results=c()
col.order = c("Cat_vars","statistic","p.value")
chi_col = c(2,3,5,7,8,9)
Cat_vars = c("Geography","Gender","Tenure","NumOfProducts","HasCrCard","IsActiveMember")
for(i in chi_col){
  Chi_results =rbind(Chi_results,(chisq.test(table(data[,i],data$Exited)))[c(1,3)])
}


res = cbind(Chi_results,Cat_vars)
res =res[,col.order]
as.data.frame(res)
#Tenure and HasCrCard have a small chi-square  and P-value higher than 0.05 so we cannot reject the independence. I think we can drop them.


#Standardization

scaled=data
scaled[,c(1,4,6,10)]<-lapply(data[,c(1,4,6,10)], function(x){(x-min(x))/(max(x)-min(x))})


View(scaled)


set.seed(123)
train=sample(c(TRUE,FALSE),nrow(scaled),rep=TRUE)
test =(!train)


#Compare the full and step-wise models

#full model

full.model <- glm(Exited ~., data = scaled[train,], family = binomial(link="logit"))
summary(full.model)



probabilities <- predict(full.model,scaled[test,],type="response")
predicted.classes <- ifelse(probabilities > 0.18, "1", "0")
# Prediction accuracy
observed.classes <- scaled[test,]$Exited
mean(predicted.classes == observed.classes)

pred_roc.full <- predict(full.model,scaled[test,],type="response")
test_roc.full = roc(scaled[test,]$Exited~pred_roc.full, plot=TRUE,print.auc=TRUE)

confusionMatrix(as.factor(predicted.classes), as.factor(scaled[test,]$Exited))

length(coef(full.model))


# Step-wise model
step.model<-stepAIC(trace=FALSE, glm(Exited~.,data=scaled[train,],family=binomial))
probs<- predict(step.model, scaled[test,],type="response")
predicted.classes <- ifelse(probs > 0.18, "1", "0")



table(predicted.classes,data[test,]$Exited)

mean(predicted.classes==data[test,]$Exited) #.83 percent


summary(step.model)
confusionMatrix(as.factor(predicted.classes), as.factor(data[test,]$Exited))


pred_roc <- predict(step.model,scaled[test,],type="response")
test_roc = roc(scaled[test,]$Exited~pred_roc, plot=TRUE,print.auc=TRUE)

length(coef(step.model))

#Specificity


a=table(predicted.classes,data[test,]$Exited)[1,1]
b=table(predicted.classes,data[test,]$Exited)[2,1]

a/(a+b)

#Specificity

c=table(predicted.classes,data[test,]$Exited)[1,2]
d=table(predicted.classes,data[test,]$Exited)[2,2]

(d)/(c+d)
(x[2,2])/(x[1,2]+x[2,2])

sens_grid = seq(0.18,0.3,by=0.01)
cv.sens = rep(NA,length(sens_grid))
cv.spec = rep(NA,length(sens_grid))


for(i in 1:13){
a=sens_grid[i]  
res1<- ifelse(probs > a, "1", "0")
res=table(res1,data[test,]$Exited)
cv.spec[i] =(res[2,2])/(res[1,2]+res[2,2])
cv.sens[i] =(res[1,1])/(res[1,1]+res[2,1])  
}



plot(cv.spec,cv.sens,type="l",xlab="Specificity",ylab="Sensitivity")




cv.spec
cv.sens
which.max(cv.spec)#1
sens_grid[1]

#ERROR Rate

prb<-predict(step.model,scaled[test,], type="response")
pred_fit<- prediction(prb, scaled[test,]$Exited)
perf_fit<-performance(pred_fit, "tpr", "fpr")
plot(perf_fit, colorize=T, print.cutoffs.at=seq(0,1,by=0.1))









#ASSOCIATIONS

mytab<-xtabs(~Exited+Geography, data = data)
mytab

prop.table(mytab,1) # row percentages, the expected probability of churn is highest in Germany and lowest in spain.

prop.table(mytab,2) # col percentages


plot(mytab,col=c("green","blue","red")) # we can see more clearly that when the origin is Germany, the churn rate is higher.
#the area is larger.

Test<-chisq.test(mytab,correct=FALSE)
Test






##FULLL DATA

# Step-wise model
step.model_full<-stepAIC(trace=FALSE, glm(Exited~.,data=scaled[train,],family=binomial))
probs_scaled_full<- predict(step.model_full, scaled,type="response")
predicted.classes_full <- ifelse(probs_scaled_full > 0.18, "1", "0")



table(predicted.classes_full,scaled$Exited)


mean(predicted.classes_full==scaled$Exited)

confusionMatrix(as.factor(predicted.classes_full), as.factor(scaled$Exited))

summary(step.model_full)


prb_full<-predict(step.model_full,scaled, type="response")
pred_fit_full<- prediction(prb_full, scaled$Exited)
perf_fit_full<-performance(pred_fit_full, "tpr", "fpr")
plot(perf_fit_full, colorize=T, print.cutoffs.at=seq(0,1,by=0.1))

