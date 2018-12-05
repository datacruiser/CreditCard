set.seed(4321)  
d=sort(sample(nrow(data1), nrow(data1)*.7))

train<-data1[d,]
test<-data1[-d,]

table(train$default)
table(test$default)

# 加载标签的训练数据
labels = train$default
df_train = train[-grep('default', colnames(train))]
# combine train and test data


library(xgboost)
library(stringr)
library(caret)
library(car)


xgb <- xgboost(data = data.matrix(df_train), 
               label = labels, 
               eta = 0.2,
               booster= "gbtree",
               max_depth = 3, 
               nround=60, 
               scale_pos_weight = 1,
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 1,
               eval_metric = "logloss",
               objective = "reg:logistic",
               nthread = 3)

model<-xgb.dump(xgb,with.stats = T)
model[1:10] #打印模型的前10个节点

print(xgb, verbose=TRUE)

＃获取功能实名
names <- dimnames(data.matrix(df_train))[[2]]
＃计算特征重要性矩阵
importance_matrix <- xgb.importance(names, model = xgb)
write.csv(importance_matrix,"importance_matrix.csv")
＃画图
xgb.plot.importance(importance_matrix[1:20,])

#测试结果是否有意义
test <- chisq.test(train$Age, output_vector)
print(test)



# 检验模型
#做出概率预测
train$p<-predict(xgb,data.matrix(train[,-1]))
test$p<-predict(xgb,data.matrix(test[,-1]))

#绘制ROC曲线
library(ROCR)
library(pROC)
pred_train<- prediction(train$p, train$default)
perf_train <- performance(pred_train,"tpr","fpr")

modelroc_train <- roc(train$default,train$p) #画的实际与（向后）预测的ROC
modelroc_test  <- roc(test$default,test$p)

plot(modelroc_train , print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)

plot(modelroc_test, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)


#模型评估,混肴矩阵
#对分类数据预测需要加上default参数 

summary(test$p)
test$out<-1
test[test$p<0.30,]$out<-0
table(test$default,test$out)


#7.3.1计算准确率
sum(test$out==test$default)/length(test$default)

#不用模型原先准确率
sum(data1$default==0)/length(data1$default)

#7.3.2绘制ROC曲线

r1<-plot.roc(default~p,train,col="1")
rocobjtr<- roc(train$default, train$p)
auc(rocobjtr)
r2<-lines.roc(default~p,test,col='2')
rocobjte <- roc(test$default, test$p)
auc(rocobjte)
roc.test(r1,r2)
#7.3.3绘制洛伦兹曲线
pred_Tr <- prediction(train$p, train$default)
tpr <- performance(pred_Tr,measure='tpr')@y.values[[1]]
depth <- performance(pred_Tr,measure='rpp')@y.values[[1]]
plot(depth,tpr,type='l',main='Lorenz图',ylab='查全率(tpr)',xlab='深度(depth)')


#绘制累积提升度曲线

pred_Tr <- prediction(train$p, train$default)
lift <- performance(pred_Tr,measure='lift')@y.values[[1]]
depth <- performance(pred_Tr,measure='rpp')@y.values[[1]]
plot(depth,lift,type='l',main='lift图',ylab='lift',xlab='depth')

#绘制K-S曲线
pred_Tr <- prediction(test$p, test$default)
tpr <- performance(pred_Tr,measure='tpr')@y.values[[1]]
fpr <- performance(pred_Tr,measure='fpr')@y.values[[1]]
ks<-(tpr-fpr)
depth <- performance(pred_Tr,measure='rpp')@y.values[[1]]
plot(depth,ks,type='l',main='K-S曲线',ylab='KS值',xlab='深度(depth)')
kslable<-paste("KS值:",max(ks),sep="")
legend(0.3,0.2,c(kslable),2:8)
library(gmodels)
t<-CrossTable(test$default, test$out, prop.chisq = FALSE, prop.c = F, 
              prop.r = F, dnn = c("actual", "predicted "))
t$prop.col[2,2]#准确率
t$prop.row[2,2]#召回率