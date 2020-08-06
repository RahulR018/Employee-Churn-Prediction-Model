library(caret)
library(tidyverse)
library(corrplot)
library(fastDummies)
library(lattice)
library(e1071)
library(Hmisc)
ch <- read.csv('E:/R/capstone/Attrition1.csv', header = T, stringsAsFactors = F)
str(ch)
ch <- ch[,-c(9,22,27)]
ch <- as.data.frame(ch)
str(ch)
ch$Attrition <- as.factor(ch$Attrition)
str(ch)
hist(ch$DailyRate)
boxplot(ch$DailyRate)
quantile(ch$DailyRate)
table(ch$Attrition)
prop.table(table(ch$Attrition))
boxplot(ch$Age)
quantile(ch$Age)
hist(ch$Age)
hist(ch$MonthlyIncome)
monthlyincome_outliers <- boxplot(ch$MonthlyIncome)$out
ch[which(ch$MonthlyIncome %in% monthlyincome_outliers),]
grep("EmployeeNumber",colnames(ch))#looking for column name position using column name
ch <- ch[,-9]
ch1 <- ch
ch <- ch[-which(ch$MonthlyIncome %in% monthlyincome_outliers),]
boxplot(ch$MonthlyIncome)
hist(ch$MonthlyIncome)
ch$BusinessTravel <- as.factor(ch$BusinessTravel)
barchart(ch$BusinessTravel, horizontal= F, border="white")
prop.table(table(ch$BusinessTravel))
plot(Department~BusinessTravel, data=ch, col=colors()[100:102])
boxplot(ch$DistanceFromHome)
plot(ch$MonthlyIncome~ch$Department)
table(ch$Department)
prop.table(table(ch$Department))
chisq.test(ch$Attrition,ch$Department)
chiSquare(ch$Attrition~ch$Department)
ch$Education <- as.factor(ch$Education)
ch$EducationField <- as.factor(ch$EducationField)
ch$EnvironmentSatisfaction <- as.factor(ch$EnvironmentSatisfaction)
ch$Gender <- as.factor(ch$Gender)
ch$JobInvolvement <- as.factor(ch$JobInvolvement)
ch$JobLevel <- as.factor(ch$JobLevel)
ch$JobRole <- as.factor(ch$JobRole)
ch$JobSatisfaction <- as.factor(ch$JobSatisfaction)
ch$MaritalStatus <- as.factor(ch$MaritalStatus)
ch$NumCompaniesWorked <- as.factor(ch$NumCompaniesWorked)
ch$OverTime <- as.factor(ch$OverTime)
ch$PercentSalaryHike <- as.factor(ch$PercentSalaryHike)
ch$PerformanceRating <- as.factor(ch$PerformanceRating)
ch$RelationshipSatisfaction <- as.factor(ch$RelationshipSatisfaction)
ch$StockOptionLevel <- as.factor(ch$StockOptionLevel)
ch$TrainingTimesLastYear <- as.factor(ch$TrainingTimesLastYear)
ch$WorkLifeBalance <- as.factor(ch$WorkLifeBalance)
ch$YearsAtCompany <- as.factor(ch$YearsAtCompany)
ch$YearsInCurrentRole <- as.factor(ch$YearsInCurrentRole)
ch$YearsSinceLastPromotion <- as.factor(ch$YearsSinceLastPromotion)
ch$YearsWithCurrManager <- as.factor(ch$YearsWithCurrManager)
#univariate and bivariate exploratory analysis for rest of the categories
plot(ch$EducationField)
plot(ch$Attrition,ch$EducationField)
plot(ch$EnvironmentSatisfaction)
table(ch$Attrition,ch$EnvironmentSatisfaction)
prop.table(table(ch$Attrition,ch$EnvironmentSatisfaction))
plot(ch$Attrition,ch$EnvironmentSatisfaction)
temp <- naiveBayes(ch$Attrition,ch$EnvironmentSatisfaction)
plot(as.matrix(temp$tables$x),color=c('blue','black'))
chisq.test(ch$Attrition,ch$EnvironmentSatisfaction)# significant variable
plot(ch$Gender,ch$Attrition)
prop.table(table(ch$Gender,ch$Attrition))
naiveBayes(ch$Attrition,ch$Gender)
chisq.test(ch$Attrition,ch$Gender)#insignificant variable
plot(ch$JobInvolvement)
plot(ch$Attrition~ch$JobInvolvement)
chisq.test(ch$Attrition,ch$JobInvolvement)
plot(ch$MaritalStatus,ch$Attrition)
naiveBayes(ch$Attrition,ch$MaritalStatus)
chisq.test(ch$Attrition,ch$MaritalStatus)
plot(ch$NumCompaniesWorked)
naiveBayes(ch$Attrition,ch$NumCompaniesWorked)
plot(as.matrix(naiveBayes(ch$Attrition,ch$NumCompaniesWorked)$table$x), 
     color=c("blue","Lightblue"), 
     main="Attrition VS Number of Companies Worked")
chisq.test(ch$Attrition,ch$NumCompaniesWorked)
plot(ch$OverTime)
naiveBayes(ch$Attrition,ch$OverTime)
plot(as.matrix(naiveBayes(ch$Attrition,ch$OverTime)$table$x), 
     main="Attrition VS Overtime",
     xlab = "Overtime", ylab = "Attrited?",
     col=c('blue','darkblue'))
chisq.test(ch$Attrition,ch$OverTime)
plot(ch$PercentSalaryHike)
plot(as.matrix(naiveBayes(ch$Attrition,ch$PercentSalaryHike)$table$x), 
     col=c('darkblue', "blue"))
chisq.test(ch$Attrition,ch$PercentSalaryHike)
fisher.test(ch$Attrition,ch$PercentSalaryHike, simulate.p.value = T)
plot(ch$PerformanceRating)
table(ch$PerformanceRating)
table(ch$Attrition,ch$PerformanceRating)
plot(as.matrix(naiveBayes(ch$Attrition,ch$PerformanceRating)$table$x), col=c("blue","skyblue"))
chisq.test(ch$Attrition,ch$PerformanceRating)
plot(ch$RelationshipSatisfaction)
table(ch$RelationshipSatisfaction)
plot(as.matrix(naiveBayes(ch$Attrition,ch$RelationshipSatisfaction)$table$x),
     col=c('blue','skyblue'), main='Relationship Satisfaction',
     xlab = "Degrees", ylab = "Attrited?")
chisq.test(ch$Attrition,ch$RelationshipSatisfaction)
plot(as.matrix(naiveBayes(ch$Attrition, ch$StockOptionLevel)$table$x))
chisq.test(ch$Attrition,ch$StockOptionLevel)
plot(ch$TrainingTimesLastYear)
plot(as.matrix(naiveBayes(ch$Attrition,ch$TrainingTimesLastYear)$table$x))
chisq.test(ch$Attrition,ch$TrainingTimesLastYear)
plot(ch$WorkLifeBalance)
plot(as.matrix(naiveBayes(ch$Attrition,ch$WorkLifeBalance)$table$x))
chisq.test(ch$Attrition,ch$WorkLifeBalance)
plot(ch$YearsAtCompany)
plot(as.matrix(naiveBayes(ch$Attrition,ch$YearsAtCompany)$table$x))
fisher.test(ch$Attrition,ch$YearsAtCompany, simulate.p.value = T)
plot(ch$YearsInCurrentRole)
plot(as.matrix(naiveBayes(ch$Attrition,ch$YearsInCurrentRole)$table$x))
fisher.test(ch$Attrition,ch$YearsInCurrentRole, simulate.p.value = T)
plot(ch$YearsSinceLastPromotion)
boxplot(ch$YearsSinceLastPromotion)
plot(as.matrix(naiveBayes(ch$Attrition,ch$YearsSinceLastPromotion)$table$x))
fisher.test(ch$Attrition,ch$YearsSinceLastPromotion, simulate.p.value = T)
plot(ch$YearsWithCurrManager)
plot(as.matrix(naiveBayes(ch$Attrition,ch$YearsWithCurrManager)$table$x))
fisher.test(ch$Attrition,ch$YearsWithCurrManager, simulate.p.value = T)

#dataframe of numeric variables
dimnames(ch)
ch_numer <- ch[,c(1,6,11,17,18)]
dim(ch_numer)
ch_numer <- as.matrix(ch_numer)
dim(ch_numer)
rcorr(as.matrix(ch_numer))
corrplot(rcorr(as.matrix(ch_numer))$r,type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
hist(log10(ch$MonthlyIncome)) #log transformation for normality in monthlyincome
#splitting train and test data
set.seed(12345)
inTrain <- createDataPartition(ch$Attrition, p=0.8, list = F)
Train <- ch[inTrain,]
Test <- ch[-inTrain,]
prop.table(table(Train$Attrition))
#Model creation
model <- glm(Attrition ~ Age+BusinessTravel+Department+DailyRate+DistanceFromHome+
               Education+EducationField+EnvironmentSatisfaction+Gender+HourlyRate+JobInvolvement+
               JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+MonthlyRate+NumCompaniesWorked+
               OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+StockOptionLevel+
               TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+
               YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+
               YearsWithCurrManager,family=binomial(link='logit'),data=Train)
summary(model)
model2 <- step(model)
summary(model2)
#Train accuracy
p=predict(model2,data=Train)
pred <- ifelse(p>0.5,1,0)
confusionMatrix(as.factor(pred),as.factor(Train$Attrition))
#Test Accuracy
p1=predict(model2,Test)
pred1 <- ifelse(p1>0.5,1,0)
Test_matrix <- table(pred1,Test$Attrition)
Test_Accuracy <- ((Test_matrix[1,1]+Test_matrix[2,2])/sum(Test_matrix))*100
Test_Accuracy

















