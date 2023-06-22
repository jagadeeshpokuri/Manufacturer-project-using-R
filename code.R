library(stringr)
library(data.table)
library(MASS)
library(randomForest)
library(purrr)
library(caret)
library(visdat)
library(tidyr)
library(dplyr)



setwd("D:\\Edvancer\\Projects\\Manufacturing")
train=read.csv("product_train.csv")
test=read.csv("product_test.csv")
summary(train)
table(train)
glimpse(train)

test$went_on_backorder=NA
train$data='train'
test$data='test'
main=rbind(train,test)
library(visdat)

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

cat_cols=c("potential_issue","deck_risk","oe_constraint","ppap_risk","stop_auto_buy","rev_stop")

for(cat in cat_cols){
  main=CreateDummies(main,cat,50)
}

glimpse(main)
vis_dat(main, warn_large_data = F)
library(dplyr)
train=main %>% filter(data=='train') %>% select(-data)
test=main %>% filter(data=='test') %>% select (-data,-went_on_backorder)

set.seed(2)
s=sample(1:nrow(train),0.8*nrow(train))
train1=train[s,]
train2=train[-s,]

library(car)
library(h2o)

h2o.init(nthreads = 3)
train <- as.h2o(train)
test <- as.h2o(test)
glimpse(train)
y <- "went_on_backorder"
x <- setdiff(names(train),y)

train[,y] <- as.factor(train[,y])


#####yet to execute

aml <- h2o.automl(x=x, y=y,
                  training_frame = train,
                  max_models = 10,
                  seed=1)
lb <- aml@leaderboard
print(lb,n = nrow(lb))

#bm <- h2o.get_best_model(aml)
best <- h2o.getModel("GBM_1_AutoML_1_20230420_190433")
best
x=as.data.frame(h2o.predict(best,test))

x$sku=test$sku

colnames(x)[1]="went_on_backorder"

x=x[,c("sku", "went_on_backorder")]
View(x)
write.csv(x,"jagad.csv", row.names=F)
write.csv(x,"D:\\Edvancer\\Projects\\Manufacturing\\submission-3.csv",row.names = F)



############### values taken from the model execution taken from result


TP=50
FN=16
TN=9891
FP=18

P=TP+FN
N=TN+FP

Sn=TP/P
Sp=TN/N
precision=TP/(TP+FP)
recall=Sn

KS=(TP/P)-(FP/N)
F5=(26*precision*recall)/((25*precision)+recall)
F.1=(1.01*precision*recall)/((.01*precision)+recall)

M=(4*FP+FN)/(5*(P+N))

Sn
Sp
KS
F5
F.1
M



##########Quiz questions


View(train)

unique(train$pieces_past_due)
table(train$pieces_past_due)


library(stringr)
library(data.table)
library(MASS)
library(randomForest)
library(purrr)
library(caret)
library(visdat)
library(tidyr)
library(dplyr)



setwd("D:\\Edvancer\\Projects\\Manufacturing")
train=read.csv("product_train.csv")
test=read.csv("product_test.csv")
summary(train)
table(train)
glimpse(train)

cat_cols=c("potential_issue","deck_risk","oe_constraint","ppap_risk","stop_auto_buy","rev_stop")

for(cat in cat_cols){
  train=CreateDummies(train,cat,50)
}

vis_dat(train, warn_large_data = F)
glimpse(train)


test$went_on_backorder=NA
train$data='train'
test$data='test'
main=rbind(train,test)
library(visdat)

rm(list=ls())

round(cor(train$forecast_9_month,train$sales_9_month),3)

x=train$min_bank[train$went_on_backorder == "yes"]
x
x=mean(train$min_bank[train$went_on_backorder])
x

x=train$deck_risk[train$went_on_backorder == "yes"]
x
y=train$deck_risk[train$went_on_backorder == "no"]
y

s=43.77213622
n=51.30743813
mu=51.25877526

t.test(s, n,mu=51.25877526)
