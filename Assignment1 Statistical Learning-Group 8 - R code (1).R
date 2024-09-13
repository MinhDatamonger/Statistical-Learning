#install.packages('leaps')
#install.packages("glmnet") 
#install.packages("pls") 

library(glmnet)
library(leaps)
bestpara <- read.csv("C:/Users/tholo/Downloads/bestpara.csv", header=TRUE)
row.names(bestpara)=bestpara[,1]
bestpara=bestpara[,-1]

dataset1 <- read.csv("C:/Users/tholo/Downloads/dataset1.csv", header = TRUE)
dataset2 <- read.csv("C:/Users/tholo/Downloads/dataset2.csv", header=TRUE)
dataset3 <- read.csv("C:/Users/tholo/Downloads/dataset3.csv", header=TRUE)
dataset4 <- read.csv("C:/Users/tholo/Downloads/dataset4.csv", header=TRUE)
dataset5 <- read.csv("C:/Users/tholo/Downloads/dataset5.csv", header=TRUE)

Answer_Sheet <- read.csv("C:/Users/tholo/Downloads/Answer_Sheet.csv", header=TRUE)


### Dataset 1 
#(Ridge, since all variables are important and low correlation)
grid<-10^seq(10, -2, length = 100)
D1=model.matrix(target~.,data = dataset1)[,-1]
reg1=glmnet(D1, dataset1$target, alpha=0, lambda=grid)
yhat1 <-as.numeric(predict(reg1,s=bestpara['Ridge', 'dataset1.csv'],newx=D1) )
yhat1


### Dataset 2 
# Lasso for high dimensional variable selection of a sparse model
D2=model.matrix(target~.,data = dataset2)[,-1]
r2=glmnet(D2, dataset2$target,alpha=1,lambda=grid)
yhat2 <-as.numeric(predict(r2,s=bestpara['Lasso', 'dataset2.csv'],newx=D2) )
yhat2



### Dataset 3 
# Ridge since all predictors are useful and their impacts are comparable
D3=model.matrix(target~.,data = dataset3)[,-1]
r3=glmnet(D3,dataset3$target ,alpha=0,lambda=grid)
yhat3 <- as.numeric(predict(r3,s=bestpara['Ridge', 'dataset3.csv'],newx=D3))
yhat3


### Dataset 4
# Forward selection for performing variable selection for n>>p
D4=model.matrix(target~.,data = dataset4)
r4=regsubsets(target~.,data = dataset4, method="forward", nvmax=bestpara["Forward","dataset4.csv"])
forward_coef=coef(r4, bestpara["Forward","dataset4.csv"])
yhat4=as.numeric(D4[,names(forward_coef)]%*%forward_coef)
yhat4


### Dataset 5 
#PCR due to high correlation
library(pls)
D5 <- model.matrix(target~., dataset5)[,-1]
r5=pcr(target~.,data=dataset5, scale=T, ncomp=bestpara['PCR', 'dataset5.csv'])
yhat5<-as.numeric(predict(r5, D5, ncomp=bestpara['PCR', 'dataset5.csv']))
yhat5



Answer_Sheet[,c("Data.Set.1", "Data.Set.2", 
"Data.Set.3","Data.Set.4", "Data.Set.5")]<-cbind(yhat1, yhat2, yhat3, yhat4, yhat5)

write.csv(Answer_Sheet, file = "Assignment1_group08.csv")
