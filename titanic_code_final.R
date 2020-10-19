####### Kaggle ML Competition
# The goal is to predict the probability of survival of Titanic Passenger, based on prior information
# This code is written in R language and includes a phase of Variable Engineering and four forecasting methods and 
# Further improvement may be added in future

##### 1 - Data Loading

rm(list=ls())
setwd("C:\\Users\\Matteo Zucca\\Desktop\\Project Kaggle\\titanic_rawdata")
raw_train_set=read.csv("train.csv", header=T, sep=",")
raw_test_set=read.csv("test.csv", header=T, sep=",")
library(kknn); library(randomForest); library(pROC); library(party)
temp_train_set=raw_train_set[,-c(1,9,11)] # remove useless variables 
temp_test_set=raw_test_set[,-c(1,8,10)]

colnames(temp_train_set)=c("Surv","Class","Name","Female","Age","NrBro","NrPar","Price","Emb")
temp_train_set$Female=factor(ifelse(temp_train_set$Female=="female",1,0))
temp_train_set$Class=as.factor(temp_train_set$Class)
#temp_train_set$Class=factor(temp_train_set$Class,levels=c("3","2","1"),ordered = F)
temp_train_set[which(temp_train_set$Emb==""),"Emb"]=sample(c("C","Q","S"),2) # quite corret, will be removed later
temp_train_set$Emb=droplevels(temp_train_set$Emb)


colnames(temp_test_set)=c("Class","Name","Female","Age","NrBro","NrPar","Price","Emb")
temp_test_set[,"Class"]=as.factor(temp_test_set$Class)
#temp_test_set[,"Class"]=factor(temp_test_set$Class,levels=c("3","2","1"),ordered = F)
temp_test_set$Female=factor(ifelse(temp_test_set$Female=="female",1,0))
#summary(temp_test_set); str(temp_test_set)

#for (i in ncol(temp_train_set)){ # setting egual levels
#  if (class(temp_train_set[1,i])=="factor") {
#    levels(temp_train_set[,i])=levels(temp_test_set[,i-1])
#  }
#}


#### 2 - Pre-precessing dati

total_set= rbind(temp_train_set[,-1],temp_test_set)
summary(total_set); str(total_set); table(is.na(total_set$Age)) #263 NA's in Age column -> model estimate

to_substitute=median(na.omit(total_set$Price)) #only 1 na value -> substitute by median
total_set[which(is.na(total_set$Price)),"Price"]=to_substitute

cor(na.omit(total_set[,-which(sapply(total_set,class)=="factor")]))
boxplot(total_set$Age~total_set$Class,xlab="Class",ylab="Age") #coulb be used as regressor?

mod_age_reg=lm(Age~.-Name,data=na.omit(total_set))
summary(mod_age_reg) # not very good, try knn

k_empiric_total=round(sqrt(nrow(total_set)),0)
mod_age_knn=kknn(Age~.-Name,train=total_set,test=total_set,k=k_empiric_total)
predicted_age_knn=mod_age_knn$fitted.values
total_set[which(is.na(total_set$Age)),"Age"]=predicted_age_knn[which(is.na(total_set$Age))]
summary(total_set$Age)


#### 3 - Feature Engineering (using Name variable)

total_set$Name = as.character(total_set$Name); total_set$Name[1] #can't work on factor -> transform on string
strsplit(total_set$Name[1], split='[,.]')[[1]][2] #double index: first is row, second is the part of splitted string

# Engineered variable: Title
total_set$Title = sapply(total_set$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
total_set$Title = sub(' ', '', total_set$Title) #deletes blank space
table(total_set$Title)
total_set$Title[total_set$Title %in% c('Mme', 'Mlle')] = 'Mlle' # combine small title groups
total_set$Title[total_set$Title %in% c('Capt', 'Don', 'Major', 'Sir')] = 'Sir'
total_set$Title[total_set$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] = 'Lady'
total_set$Title = factor(total_set$Title)
table(total_set$Title)


# Engineered variable: Family size and FamilyID

total_set$FamilySize = total_set$NrBro + total_set$NrPar + 1
total_set$Surname = sapply(total_set$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
total_set$FamilyID = paste(as.character(total_set$FamilySize), total_set$Surname, sep="") #id is formed by size and surname combined
total_set$FamilyID[total_set$FamilySize <= 2] = 'Small' #small family in the same level
table(total_set$FamilyID) #inspect
family_id_temp = data.frame(table(total_set$FamilyID)) #single persone can have same surname -> exclude them
family_id_temp = family_id_temp[family_id_temp$Freq <= 3,] #to reduce the number of levels
total_set$FamilyID[total_set$FamilyID %in% family_id_temp$Var1] = 'Small'
total_set$FamilyID = factor(total_set$FamilyID) #reconvert from string to factor

train_set=total_set[1:nrow(temp_train_set),]
test_set=total_set[setdiff(1:nrow(total_set),1:nrow(train_set)),]
train_set=cbind(Surv=as.integer(temp_train_set[,1]), train_set)

#train_set=train_set[,c("Surv","Class","Female","Age","NrBro","NrPar","Price","Emb","Title","FamilySize","FamilyID")]
#test_set=test_set[,c("Surv","Class","Female","Age","NrBro","NrPar","Price","Emb","Title","FamilySize","FamilyID")]

#### 3 - Forecasting Methods

simple_formula=formula( as.factor(Surv) ~ Class + Female + Age + NrBro + NrPar + Price + Emb)
engineered_formula=formula( as.factor(Surv) ~ Class + Female + Age + NrBro + NrPar + Price + Emb + Title + FamilySize + FamilyID)

# 1 - Logistic Model

cor(train_set[,which(sapply(train_set,class) %in% c("integer" , "numeric"))]) #look for collinearity
boxplot(train_set$Surv~train_set$Class) #useful only for median for 1 and 2 class level

real_surv=train_set[,1]
mod_surv_log = glm(engineered_formula,family="binomial",data=train_set,control=glm.control(maxit=100))
summary(mod_surv_log)
# plot(mod_surv_log$fitted.values,resid(mod_surv_log, type="pearson"), xlab="Fitted",ylab="Pearson Resid")
mod_null=glm(Surv~1, family="binomial",data=train_set)
pseudo_rsquare=as.numeric(1-logLik(mod_surv_log)/logLik(mod_null)); pseudo_rsquare #r^2 is too low

mod_surv_lin=lm(Surv~ Class + Female + Age + NrBro + NrPar + Price + Emb,data=train_set) #linear probability model: treats response like integer (and not factor)
summary(mod_surv_lin) #linear model with engineered formula works better
summary(lm(Surv~ Class + Female + Age + NrBro + NrPar + Price + Emb + Title + FamilySize + FamilyID,data=train_set)) #
anova(mod_surv_lin) #no predictors have p-value greater than 0.4, however r^2 is too low

mod_logistic_roc=roc(real_surv,mod_surv_log$fitted.values,transpose=T)
plot(mod_logistic_roc)
coords(mod_logistic_roc, x="best", ret=c("thresh","acc","sens","spec"), transpose=T) #accuracy is too low in internal validation
tresh_log=coords(mod_logistic_roc, x="best", ret="thresh",transpose=T)

coords(roc(real_surv,mod_surv_lin$fitted.values,transpose=T), x="best", ret=c("thresh","acc"), transpose=T) #worse than logistic (as expected)


# 2 - kNN

k_empiric=round(sqrt(nrow(train_set)))
mod_surv_knn=kknn(engineered_formula,train=train_set,test=train_set,k=k_empiric)

mod_knn_roc=roc(real_surv,mod_surv_knn$prob[,2])
plot(mod_knn_roc)
coords(mod_knn_roc, x="best", ret=c("thresh","acc","sens","spec"), transpose=T) #better than logistic
coords(mod_knn_roc, x=0.5, ret=c("thresh","acc","sens","spec"), transpose=T)
tresh_knn=coords(mod_knn_roc, x="best", ret="thresh", transpose=T)


# 3 - Random Forest

mod_surv_forest=randomForest(engineered_formula,data=train_set,ntree=1000)
surv_pred_forest=predict(mod_surv_forest,train_set,type="prob")[,2]
mod_forest_roc=roc(real_surv,surv_pred_forest)
plot(mod_forest_roc) #got improved!
coords(mod_forest_roc, x="best", ret=c("thresh","acc","sens","spec"), transpose=T)
tresh_forest=coords(mod_forest_roc, x="best", ret="thresh", transpose=T)

# Conditional Forest has a problem with the dimension of prediction.
#mod_surv_forest_expanded=cforest(my_formula, data = train_set, controls=cforest_unbiased(ntree=2000, mtry=3))
#forest_expanded_pred = predict(mod_surv_forest_expanded, test_set, OOB=TRUE, type = "response")
#length(forest_expanded_pred)
#output_forest_upgrade = data.frame(PassengerId = setdiff(1:nrow(total_set),1:nrow(train_set)), Survived = forest_expanded_pred)
#write.csv(output_forest_upgrade, file = "forest_expanded", row.names = F)

#4 - Basic Gender Model (with submission too)

table(train_set$Surv,train_set$Female)
summary(train_set$Surv); summary(train_set$Female)
gender_prediction=rep(0,nrow(test_set))
for (i in 1:nrow(test_set)){
  if(test_set$Female[i]==1) gender_prediction[i]=1
}
gender_matrix=data.frame(PassengerID=setdiff(1:nrow(total_set),1:nrow(train_set)),
                         Survived=gender_prediction)
write.table(gender_matrix,file="my_gender_simple_model.csv",row.names=FALSE, col.names=TRUE ,sep=",")

#### 4 - Final Output

pred_log=matrix(nrow=nrow(test_set),ncol=2)
colnames(pred_log)=c("PassengerID","Survived")
pred_log[,1]=setdiff(1:nrow(total_set),1:nrow(train_set))
pred_knn=pred_forest=pred_log

fitted_logistic=predict(mod_surv_log,test_set,type="response")
fitted_knn=kknn(engineered_formula,train=train_set,test=test_set,k=k_empiric)$prob[,2]
fitted_forest=predict(mod_surv_forest,test_set,type="prob")[,2]

for(i in 1:nrow(pred_log)){
  if(fitted_logistic[i]>tresh_log) pred_log[i,2]=1 else pred_log[i,2]=0
  if(fitted_knn[i]>tresh_knn) pred_knn[i,2]=1 else pred_knn[i,2]=0
  if(fitted_forest[i]>tresh_forest) pred_forest[i,2]=1 else pred_forest[i,2]=0
}

write.table(as.data.frame(pred_log), file="logistic_prediction.csv", row.names=FALSE, col.names=TRUE ,sep=",")
write.table(as.data.frame(pred_knn), file="knn_prediction.csv", row.names=FALSE, col.names=TRUE ,sep=",")
write.table(as.data.frame(pred_forest), file="random_forest_prediction.csv", row.names=FALSE, col.names=TRUE ,sep=",")

###### End! (External Submission to get score)
