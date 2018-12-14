####评分卡设定####
#最终模型入选变量
fitcols <- c("default",
             "loan_7day_amount_woe",
             "normal_overdue_woe",
             #"score_woe",
             "each_other_count_2m_rate_woe",
             "dateDelta_first_apply_to_first_loan_woe",
             "call_time_15srate_woe",
             "inittime_woe",
             "call_3time_15s_woe",
             "relation_model_num_woe",
             "deposit_base_woe"
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

######################################基于训练集进行评分卡制作#################################
#针对单个变量的分箱情况计算该变量各区间评分

# 获取变量的评分区间
get_test_score <- function(df, coe, variableName){
  #df <- step2_3
  iv_info <- as.data.frame(iv.mult(df,"default",vars=c(variableName)))
  score_class <- as.character(iv_info$class)
  score <- iv_info$woe*(-1)*B*coe[paste(variableName, "_woe",sep = "")]
  score_info <- cbind(score_class, score)
  score_info <- as.data.frame(score_info)
  score_info$score <- sapply(as.numeric(as.character(score_info$score)), function(x) round(x))
  score_info$label_0 <- iv_info$outcome_0
  score_info$label_1 <- iv_info$outcome_1
  score_info$total <- score_info$label_0 + score_info$label_1
  low <- c()
  high <- c()
  for (i in score_class) {
    s_interval = strsplit(i,";")
    #print(parse_number(s_interval[[1]][1]))
    low <- append(low, parse_number(s_interval[[1]][1]))
    high <- append(high, parse_number(s_interval[[1]][2]))
  }
  score_info$low <- low
  score_info$high <- high
  score_info[1,"low"] <- -Inf
  score_info[dim(score_info)[1],"high"] <- Inf
  #print(c1)
  return(score_info)
}

# 获取变量的 WOE 区间
get_test_WOE <- function(df, coe, variableName){
  #df <- step2_3
  iv_info <- as.data.frame(iv.mult(df,"default",vars=c(variableName)))
  WOE_class <- as.character(iv_info$class)
  WOE_value <- iv_info$woe
  WOE_info <- cbind(WOE_class, WOE_value)
  WOE_info <- as.data.frame(WOE_info)
  WOE_info$WOE_value <- sapply(as.numeric(as.character(WOE_info$WOE_value)), function(x) round(x))
  WOE_info$label_0 <- iv_info$outcome_0
  WOE_info$label_1 <- iv_info$outcome_1
  WOE_info$total <- WOE_info$label_0 + WOE_info$label_1
  low <- c()
  high <- c()
  for (i in WOE_class) {
    s_interval = strsplit(i,";")
    #print(parse_number(s_interval[[1]][1]))
    low <- append(low, parse_number(s_interval[[1]][1]))
    high <- append(high, parse_number(s_interval[[1]][2]))
  }
  WOE_info$low <- low
  WOE_info$high <- high
  WOE_info[1,"low"] <- -Inf
  WOE_info[dim(WOE_info)[1],"high"] <- Inf
  #print(c1)
  return(WOE_info)
}


for (i in score_class) {
  s_interval = strsplit(i,";")
  #print(parse_number(s_interval[[1]][1]))
  low <- append(low, parse_number(s_interval[[1]][1]))
  high <- append(high, parse_number(s_interval[[1]][2]))
}
score_info$low <- low
score_info$high <- high
score_info[1,"low"] <- -Inf
score_info[dim(score_info)[1],"high"] <- Inf

#获取测试样本中某个变量所所对应的评分
get_feature_score <- function(x, feature){
  b <- x
  feature_score_card <- score_table[[feature]]
  #print(c1)
  for(i in 1:dim(feature_score_card)[1]){
    if(b>=feature_score_card[i,"low"] & b<feature_score_card[i, "high"]){
      feature_score <- feature_score_card[i, "score"]
      break}}
  return(as.numeric(as.character(feature_score)))
}



#将所有变量的评分表合一
score_table <- list()
for(v in fitcols_variable[2:length(fitcols_variable)]){
  s_table <- get_test_score(step2_3, coe, v)
  print(s_table)
  score_table[v] <- list(s_table)
}


#将所有变量的WOE表合一
woe_table <- list()
for(v in fitcols_variable[2:length(fitcols_variable)]){
  w_table <- get_test_WOE(step2_3, coe, v)
  print(w_table)
  woe_table[v] <- list(w_table)
}

test_fitcols <- test[fitcols_variable]

for(i in fitcols_variable[2:length(fitcols_variable)]){
  print(i)
  test_fitcols[paste(i, "_score", sep = "")] <- apply(test[i], MARGIN = 1,  get_feature_score, feature=i)
}
test_fitcols$score <- apply(test_fitcols[,(length(fitcols) + 1):length(test_fitcols)], MARGIN = 1, function(x) sum(x))
test_fitcols$score <- test_fitcols$score + base_score



summary(test_fitcols$score)


#############################基于训练集制作分数分布表##########################
#分数分组,默认分14组，对于营销模型可以多分几组，特别是增加低分用户的粒度
score_group <- 52
card_table <- data.frame()
score_step <- (max(train_fitcols$score) -min(train_fitcols$score))/score_group
score_min <- round(min(train_fitcols$score),0)
score_step <- round(score_step, 0)
for(i in 1:score_group){
  score_head <- score_min + score_step*(i - 1)
  score_tail <- score_min + score_step*i
  score <- paste(score_head, score_tail, sep = "-")
  card_table[i, "score"] <- score
  score_section <- train_fitcols[(train_fitcols$score >=score_head) 
                       & (train_fitcols$score <score_tail),"default"]
  card_table[i, "good"] <- length(score_section[score_section == 0])
  card_table[i, "bad"] <- length(score_section[score_section == 1])
  card_table[i, "total"] <- length(score_section)
}
write_csv(card_table, "card_table.csv")
if(dim(card_table)[1]<17) {
  card_table[(dim(card_table)[1] +1):17,] <- 0
}



# for (i in fitcols_variable[2:length(fitcols_variable)]){
#   print(score_table[i])
#   write_csv(as.data.frame(score_table[i]), paste(i, ".csv",sep = ""))
# }

#计算进入模型的变量IV，最大变量个数限制为15，不足15个的变量值用0补充
## the max feature number modified to 20
feature_IV<-iv.mult(step2_3[fitcols_variable],"default",TRUE)
if(dim(feature_IV)[1]<20) {
  feature_IV[(dim(feature_IV)[1] +1):20,] <- 0
}

#计算各个变量的加分率
for(i in fitcols_variable[2:length(fitcols_variable)]){
  print(i)
  temp <- score_table[i][[1]]
  feature_IV[feature_IV$Variable ==i, "awarded_rate"] <- sum(temp[temp$score>0,"total"]) / dim(train)[1]
}

##########################################测试集模型指标绘制#############################
#绘图
hist(test_fitcols$score,col = "green",freq = F)
lines(density(test_fitcols$score),col="blue",lwd=3)
abline(v=median(test_fitcols$score),col="magenta",lwd=3)
#write.csv(test,"test_score1203.csv")

library(pROC)
roc(test_fitcols$default, 1000-test_fitcols$score, plot = TRUE, auc = TRUE)
test_auc = roc_result$auc[1]

##K-S曲线
pred_Tr <- prediction(1000-test_fitcols$score, test_fitcols$default)
tpr <- performance(pred_Tr,measure='tpr')@y.values[[1]]
fpr <- performance(pred_Tr,measure='fpr')@y.values[[1]]
ks <- (tpr-fpr)
depth <- performance(pred_Tr,measure='rpp')@y.values[[1]]
plot(depth,ks,type='l',main='test k-s',ylab='ks',xlab='depth',ylim=c(0,1))
lines(depth,tpr,col='2')
lines(depth,fpr,col='3')
kslable <- c(paste("KS:",round(max(ks),3),sep=""),paste("tpr:",round(tpr[which(ks==max(ks))],3),sep=""),paste("fpr:",round(fpr[which(ks==max(ks))],3),sep=""))
legend(0.0,1.0,c(kslable),bty="n",ncol = 1)
legend(0.4,1.0,legend=c("TPR", "FPR","TPR-FPR"), col=c("2", "3","1"),cex=0.6,lwd=2)
test_ks <- round(max(ks),3)

###############################绘制psi曲线############################
#提取train$score的十分位数
score_q = quantile(train_fitcols$score,probs = seq(0, 1, 0.1),names=FALSE)
#用无穷小和无穷大分别替换最小值和最大值
score_q[1] = -Inf
score_q[length(score_q)] = Inf
#生成分组组号
train_fitcols$score_rank=cut(train_fitcols$score,score_q,labels = F)
test_fitcols$score_rank=cut(test_fitcols$score,score_q,labels = F)
#计算PSI
a=b=c=0
for(i in 1:10){
  a=sum(train_fitcols$score_rank==i)/length(train_fitcols$score);
  b=sum(test_fitcols$score_rank==i)/length(test_fitcols$score);
  c=c+(b-a)*log(b/a);
}
#画出分布图
plot(density(train_fitcols$score),main='PSI',col=2,lwd=5)
lines(density(test_fitcols$score),col=1,lwd=2)
PSIlable<-paste("PSI:",round(c,5),sep="")
PSIlable
legend(500,0.013,c(PSIlable),bty="n",ncol = 1)
legend(450,0.017, legend=c("score", "test"), col=c("2", "1"), cex=0.75,lwd=2)

model_psi <- round(c,5)




#汇总模型性能指标
model_performance<- c(train_ks, train_auc, test_ks, test_auc, model_psi, base_score)
names(model_performance) <- c("Train_KS","Train_AUC", "Test_KS", "Test_AUC", "PSI","基础分")


#将评分卡结果直接写入xlsx
library(openxlsx)
# wb = loadWorkbook("score_card_template_test1.xlsx")
# addWorksheet(wb,"feature_score")
# addWorksheet(wb,"card_table")

#读取评分卡模板
wb = loadWorkbook("score_card_template_with16Features.xlsx")
template_sheet = "score_card"

for (woe in names(woe_table)){
  #print(score)
  woe_index = which(names(woe_table) == WOE_value)
  temp = woe_table[WOE_value][[1]][1:5]
  title = c(WOE)
  row_index = (as.integer((woe_index+1)/2)) *7 -6
  col_index = (woe_index+1) %% 2 *8+1
  
  writeData(wb,template_sheet,temp,startRow=row_index + 0,startCol=col_index,colName=TRUE)
  writeData(wb,template_sheet,title,startRow=row_index,startCol=col_index,colName=FALSE)
  print(row_index)
  print(col_index)
}

writeData(wb,template_sheet,card_table,startRow=2,startCol=18,colName=FALSE)
writeData(wb,template_sheet,feature_IV,startRow=60,startCol=1,colName=TRUE)
# 模型指标输出位置与变量统计还下移一行
writeData(wb,template_sheet,names(model_performance),startRow=55,startCol=18,colName=TRUE)
# Format the output of model performance
writeData(wb,template_sheet,formatC(model_performance, digits = 4),startRow=55,startCol=19,colName=TRUE)

score_card_name <- str_c("score_card_group_", score_group , "_KS_" , train_ks, "+", format(Sys.time(), "%m%d_%H%M%S"), ".xlsx")

saveWorkbook(wb,score_card_name,overwrite=TRUE)

##############################modling end##########################################

导入相关包