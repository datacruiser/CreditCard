#模型评估
#对分类数据预测需要加上default参数 
train_KS_test <- train_fitcols
train_KS_test$lg_p <- predict(fit, train_KS_test) 
train_KS_test$p <- (1/(1+exp(-1*train_KS_test$lg_p)))


##K-S曲线
pred_Tr <- prediction(train_KS_test$p, train_KS_test$default)
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