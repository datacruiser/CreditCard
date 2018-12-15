####评分卡设定####
#最终模型入选变量
fitcols <- c("default",
             "RevolvingUtilizationOfUnsecuredLines_woe",
             "NumberOfTime30_59DaysPastDueNotWorse_woe",
             "age_woe",
             "MonthlyIncome_woe",
             "NumberOfOpenCreditLinesAndLoans_woe",
             "DebtRatio_woe",
             "NumberRealEstateLoansOrLines_woe",
             "NumberOfDependents_woe"
            
             
)
###############################建模########################################
#获取没有woe后缀的变量名
fitcols_variable <- lapply(fitcols, function(x) sub(x = x,pattern = "_woe", replacement = ""))
fitcols_variable <- as.character(fitcols_variable)


#皮尔逊相关性分析
library(corrplot)
cor1<-cor(train[,fitcols_variable])
corrplot(cor1,method = "number")

fit <- glm(default~.,family=binomial(link='logit'),data=step2_3[,fitcols])

summary(fit)
vif(fit)
#################################计算训练集分数#############################
#将变量参数赋给coe
coe = fit$coefficients

##1.计算基础分

A = 500
B = 30/log(2)
base_score = A-B*coe[1]

train_fitcols <- step2_3[fitcols]

for(i in fitcols[2:length(fitcols)]){
  train_fitcols[paste(i, "_score", sep = "")] <- (-1)*B*train_fitcols[i]*coe[i]
}
train_fitcols$score <- apply(train_fitcols[,(length(fitcols) + 1):length(train_fitcols)], MARGIN = 1, function(x) sum(x))
train_fitcols$score <- train_fitcols$score + base_score
train_fitcols$default <- step2_3$default

train_score_data.filename <- paste("train_score_result",format(Sys.time(), "%m%d_%H%M%S"), ".csv", sep = "")
train_score.csv <- paste(output_path, train_score_data.filename,sep = "/")

write_csv(train_fitcols, train_score.csv)


##################################计算训练集模型指标#####################
#绘制评分分布图
hist(train_fitcols$score,col = "green",freq = F)
lines(density(train_fitcols$score),col="blue",lwd=3)
abline(v=median(train_fitcols$score),col="magenta",lwd=3)

#AUC
library(pROC)
roc_result <- roc(train_fitcols$default, 1000 - train_fitcols$score, plot = TRUE, auc = TRUE)
train_auc = roc_result$auc[1]
##K-S曲线
pred_Tr <- prediction(1000 - train_fitcols$score, train_fitcols$default)
tpr <- performance(pred_Tr,measure='tpr')@y.values[[1]]
fpr <- performance(pred_Tr,measure='fpr')@y.values[[1]]
ks <- (tpr-fpr)
depth <- performance(pred_Tr,measure='rpp')@y.values[[1]]
plot(depth,ks,type='l',main='train k-s',ylab='ks',xlab='depth',ylim=c(0,1))
lines(depth,tpr,col='2')
lines(depth,fpr,col='3')
kslable <- c(paste("KS:",round(max(ks),3),sep=""),paste("tpr:",round(tpr[which(ks==max(ks))],3),sep=""),paste("fpr:",round(fpr[which(ks==max(ks))],3),sep=""))
legend(0.0,1.0,c(kslable),bty="n",ncol = 1)
legend(0.4,1.0,legend=c("TPR", "FPR","TPR-FPR"), col=c("2", "3","1"),cex=0.6,lwd=2)

train_ks <- round(max(ks),3)