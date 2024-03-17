library(psych)
library(dplyr)
library(readxl)
library(factoextra)
library(FactoMineR)
library(GPArotation)

rm(list=ls())
#DATA PREPROCESSING 
  

data <- read_excel("C:/Users/Owner/Desktop/archive (1)/HBAT100.xlsx")



df<-data[7:19]
head(df)
colnames(df) = c('Product Quality', 'E-Commerce Activities/Website', 'Technical Support', 'Complaint Resolution', 
                 'Advertising', 'Product Line', 'Salesforce Image', 'Competitive Pricing', 'Warrantly and Claims', 
                 'New Products', 'Ordering and Billing', 'Price Flexibility', 'Delivery Speed')

head(df)
#View(data)
summary(df)


#principal component analysis 
pca<-princomp(data,cor = T)
summary(pca)
screeplot(pca)
plot(pca,type='l')

#factor analysis no rotation 

#adequacy check
kmo<-KMO(df)
bartlett.test(df)

#removing new products because it has less relevance from the KMO test
df = subset(df, select = -c(`New Products`))
head(df)
KMO(df)

#removing price flexibility because it has less relevance from the KMO test
df = subset(df, select = -c(`Price Flexibility`))
KMO(df)

#all the variables seem adequate and relevant to the requirements from the KMO test 

#checking bartletts test 
bartlett.test(df)
#correlation matrix


mat<-cor(df)
eig=eigen(mat)
eig$values
e<-as.vector(eig)
plot(eig$values,type='l',main = "Eigen Values")
#DETERMINIG THE NUMBER OF FACTORS TO BE DRAWN
nfactors=sum((eig$values>=1))
nfactors
#thus well draw 4 factors

#perfroming pca on the final data frame of the selected variables 
pca<-princomp(df,cor = T)
screeplot(pca)
plot(pca,type='l',main = "Eigen Values")

#seems like well have to make 4-5 factors 4 variables have a eigen value>1 but elbow at comp 5 


par(mfrow=c(1,2))

#no rotation
fan<-psych::fa(df,nfactors = 4,rotate = 'none')
load=fan$loadings
fa.diagram(load,main = "No Rotation")

#varimax rotation 
fa1<-psych::fa(df,nfactors=4,rotate='varimax')
fa1$loadings
fa.diagram(fa1$loadings,main = "Varimax Rotation")

#equamax rotation
fa2<-psych::fa(df,nfactors=4,rotate='equamax')
fa2$loadings
fa.diagram(fa2$loadings,main = "Equamax Rotation Variable classification")
summary(fa2)
#promax rotation 
fa3<-psych::fa(df,nfactors=4,rotate='promax')
fa3$loadings
fa.diagram(fa3$loadings,main = 'Promax Rotation ')

#obilim rotation 
fa4<-psych::fa(df,nfactors=4,rotate='oblimin')
fa4$loadings
fa.diagram(fa4$loadings,main = "Oblimin Rotation")

#quartimax rotation
fa5<-psych::fa(df,nfactors=4,rotate='quartimax')
fa5$loadings
fa.diagram(fa5$loading,main = "Quartimax Rotation")

#now we have to find suitable rotation on the data out of the above 4 

#from the diagrams we can see that all the rotation methods are giving us distinct groups of ariables

#the factors can be classified as 1)sales service 2)Marketing 3)After sales Services 4)Products portfolio Perspective


#creating a transformed dataset
#all methods see suitable we'll use equamax here

#obtaining the dataframe for factor scores
tr.mat<-fa2$scores
tr.df<-as.data.frame(tr.mat)
colnames(tr.df)<-c('Sales Service','Marketing','After Sales','Product Portfolio')
head(tr.df)
head(data)
res.data<-data[20:24]
head(res.data)
#res.data<-scale(res.data)
colnames(res.data)<-c("satisfaction","Recommendation","Likelihood of Purchase","Current Purchase","Possible Partnership")
head(res.data)
View(res.data)
tr.df<-data.frame(tr.df,res.data)
head(tr.df)

#regression analysis

#testing noramlities of dependent variables

for (i in 1:4){
  print(paste("Testing variable:", variable))
  boxplot(res.data[i])
}

#loop for Shapiro-wilk normality test 
shapiro_stat <- numeric()  # Vector for test statistics
shapiro_p <- numeric()     # Vector for p-values

for (variable in colnames(res.data))
  {
 
 # Perform Shapiro-Wilk test for normality
  shapiro_test <- shapiro.test(tr.df[[variable]])
 
 # Store test statistics and p-values
  shapiro_stat <- c(shapiro_stat, shapiro_test$statistic)
  shapiro_p <- c(shapiro_p, shapiro_test$p.value)
  }

results <- data.frame(Variable = colnames(res.data), Shapiro_Statistic = shapiro_stat, P_Value = shapiro_p)
print(results)  
#all null hypothesis is rejected thus data is all dependent variables are normal

#satisfaction
model1<-lm(data = tr.df,satisfaction~Sales.Service+Marketing+After.Sales+Product.Portfolio)
summary(model1)
par(mfrow=c(2,2))
plot(model1)
#improvised satisfaction model by dropping After.Sales because its insignificant
model1.2<-lm(data = tr.df,satisfaction~Sales.Service+Marketing+Product.Portfolio)
summary(model1.2)
par(mfrow=c(2,2))
plot(model1.2)


# Recommendation
model2<-lm(data = tr.df,Recommendation~Sales.Service+Marketing+After.Sales+Product.Portfolio)
summary(model2)
par(mfrow=c(2,2))
plot(model2)

#Likelihood Of Purchase
model3<-lm(data = tr.df,Likelihood.of.Purchase~Sales.Service+Marketing+After.Sales+Product.Portfolio)
summary(model3)
par(mfrow=c(2,2))
plot(model3)

#Prospective of a Partnership
model4<-glm(data = tr.df,Possible.Partnership~Sales.Service+Marketing+After.Sales+Product.Portfolio,family = binomial)
summary(model4)

model4.2<-glm(data = tr.df,Possible.Partnership~Sales.Service+Marketing+Product.Portfolio,family = binomial)
summary(model4.2)
plot(model4.2)

# for logistic regression 
library(caret)
head(res.data)
head(tr.df)
set.seed(123)
train_indices <- createDataPartition(tr.df$Possible.Partnership, p = 0.6, list = FALSE)
train_df<-tr.df[train_indices,]
test_df<-tr.df[-train_indices,]

#setting the actual values 
actual<-test_df$Possible.Partnership
#making the final test dataframe without the response variable
test_df_wo_y<-test_df[-9]
nrow(test_df_wo_y)
head(test_df)

# Fitting model on training data
fin_log_model<-glm(data=train_df,Possible.Partnership~Sales.Service+Marketing+Product.Portfolio,family=binomial)
summary(fin_log_model)
n<-nrow(test_df)
predicted_prob<-as.vector(predict(fin_log_model,test_df_wo_y,type = 'response'))

#Converting the probability into output
predicted<-ifelse(predicted_prob<0.5,0,1)


which(actual==predicted)  # accurately classified obs.
which(actual!=predicted)   #Misclassified obs.

#Making a confusion matrix of the model out puts
CM<-table(actual,predicted)

#Devising inference from confusion matrix
Accuracy_rate<-sum(diag(CM))/sum(CM)

#Accuracy_rate<-sum(actual==predicted)/n
Error_rate<-1-Accuracy_rate
#Error_rate<-sum(actual!=predicted)/n
Sensitivity<-CM[1,1]/sum(CM[1,])
recall<-Sensitivity
Specificity<-CM[2,2]/sum(CM[2,])
Precision<-CM[1,1]/sum(CM[,1])

# presentation related
combined_results <- as.matrix(
  Factor = rownames(fa2$loadings),
  Loadings = fa2$loadings,
  Variance = fa2$uniquenesses,
  Communality = fa2$communalities
)
library(writexl)
combined_results
write_xlsx(combined_results, "combined_result.xlsx")
