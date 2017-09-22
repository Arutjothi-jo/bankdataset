#### ******* German Credit Data ******* ####
#### ******* data on 1000 loans ******* ####
## read data and create some `interesting' variables
credit <- read.csv("D:/Ph.D.,/germancredit.csv")
credit
credit$Default <- factor(credit$Default)
## re-level the credit history and a few other variables
credit$history = factor(credit$history,
                        levels=c("A30","A31","A32","A33","A34"))
levels(credit$history) = c("good","good","poor","poor","terrible")
credit$foreign <- factor(credit$foreign, levels=c("A201","A202"),
                         labels=c("foreign","german"))
credit$rent <- factor(credit$housing=="A151")
credit$purpose <- factor(credit$purpose,
                         levels=c("A40","A41","A42","A43","A44","A45","A46","A47","A48","A49","A410"))
levels(credit$purpose) <-
  c("newcar","usedcar",rep("goods/repair",4),"edu",NA,"edu","biz","biz")
## for demonstration, cut the dataset to these variables
credit <- credit[,c("Default","duration","amount","installment","age",
                    "history", "purpose","foreign","rent")]
credit
summary(credit) # check out the data
## create a design matrix
## factor variables are turned into indicator variables
## the first column of ones is omitted
Xcred <- model.matrix(Default~.,data=credit)[,-1]
Xcred[1:3,]
## creating training and prediction datasets
## select 900 rows for estimation and 100 for testing
set.seed(1)
train <- sample(1:1000,900)
xtrain <- Xcred[train,]
xnew <- Xcred[-train,]
ytrain <- credit$Default[train]
ynew <- credit$Default[-train]
credglm=glm(Default~.,family=binomial,data=data.frame(Default=ytrain,xtrain))
summary(credglm)
## Now to prediction: what are the underlying default probabilities
## for cases in the test set
ptest <- predict(credglm, newdata=data.frame(xnew),type="response")
data.frame(ynew,ptest)
## What are our misclassification rates on that training set?
31
## We use probability cutoff 1/6
## coding as 1 (predicting default) if probability 1/6 or larger
cut=1/6
gg1=floor(ptest+(1-cut))
ttt=table(ynew,gg1)
ttt
truepos <- ynew==1 & ptest>=cut
trueneg <- ynew==0 & ptest<cut
# Sensitivity (predict default when it does happen)
sum(truepos)/sum(ynew==1)
# Specificity (predict no default when it does not happen)
sum(trueneg)/sum(ynew==0)
## Next, we use probability cutoff 1/2
## coding as 1 if probability 1/2 or larger
cut=1/2
gg1=floor(ptest+(1-cut))
ttt=table(ynew,gg1)
ttt
truepos <- ynew==1 & ptest>=cut
trueneg <- ynew==0 & ptest<cut
# Sensitivity (predict default when it does happen)
sum(truepos)/sum(ynew==1)
# Specificity (predict no default when it does not happen)
sum(trueneg)/sum(ynew==0)
## R macro for plotting the ROC curve
## plot the ROC curve for classification of y with p
roc <- function(p,y){
  y <- factor(y)
  n <- length(p)
  p <- as.vector(p)
  Q <- p > matrix(rep(seq(0,1,length=500),n),ncol=500,byrow=TRUE)
  fp <- colSums((y==levels(y)[1])*Q)/sum(y==levels(y)[1])
  tp <- colSums((y==levels(y)[2])*Q)/sum(y==levels(y)[2])
  plot(fp, tp, xlab="1-Specificity", ylab="Sensitivity")
  abline(a=0,b=1,lty=2,col=8)
}
## ROC for hold-out period
roc(p=ptest,y=ynew)
## ROC for all cases (in-sample)
credglmall <- glm(credit$Default ~ Xcred,family=binomial)
roc(p=credglmall$fitted, y=credglmall$y)
## using the ROCR package to graph the ROC curves
library(ROCR)
## input is a data frame consisting of two columns
## predictions in first column and actual outcomes in the second
## ROC for hold-out period
predictions=ptest
labels=ynew
data=data.frame(predictions,labels)
data
32
## pred: function to create prediction objects
pred <- prediction(data$predictions,data$labels)
pred
## perf: creates the input to be plotted
## sensitivity and one minus specificity (the false positive rate)
perf <- performance(pred, "sens", "fpr")
perf
plot(perf)
## ROC for all cases (in-sample)
credglmall <- glm(credit$Default ~ Xcred,family=binomial)
predictions=credglmall$fitted
labels=credglmall$y
data=data.frame(predictions,labels)
pred <- prediction(data$predictions,data$labels)
perf <- performance(pred, "sens", "fpr")
plot(perf)