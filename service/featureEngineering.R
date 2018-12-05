#当前项目运行根路径
projectPath <- getwd()
package_manage <- paste(projectPath, "/util/packageManage.R",sep = "")
source(package_manage)

#output文件路径
output_path <- str_c(projectPath,
                     "output",
                     sep = "/")


#util路径
encodingPath <- str_c(projectPath,
                     "util",
                     "udf.R",
                     sep="/")

source(encodingPath)


####################################导入数据，并计算iv###############################
df <- read_csv(paste(output_path, "application_data_1205_170726.csv", sep = "/"))

step2_1 <- df

summary(step2_1)

str(step2_1)

step2_3 <- step2_1

#remove the feature with Suspicious IV

# step2_3 <- step2_3[, -which(colnames(step2_3) %in% c("due_days"))]

#library(woe)
#将step2_3处理为数据框 tibble 不能用来计算IV
step2_3 <- as.data.frame(step2_3)
row.names(step2_3) <- 1:nrow(step2_3)

filteredIV<-iv.mult(step2_3,"default",TRUE)
filteredIV.filename <- paste("filteredIV_",format(Sys.time(), "%m%d_%H%M%S"), ".csv", sep = "")
write_csv(filteredIV,paste(output_path, filteredIV.filename, sep = "/"))
filteredIVplot <- iv.plot.summary(filteredIV)

#保存分析结果
filteredIVplot.filename <- paste("filteredIVplot_",format(Sys.time(), "%m%d_%H%M%S"), ".jpg", sep = "")
filteredIVoutputpath <- str_c(output_path, filteredIVplot.filename, sep="/")

ggsave(filename = filteredIVoutputpath, plot = filteredIVplot)



##根据IV值选取进入模型变量
summary(step2_3)
###################################训练集和验证集划分##########################
#进行变量WOE转换
row.names(step2_3) = seq(1,nrow(step2_3))
step2_3 <- iv.replace.woe(step2_3,iv=iv.mult(step2_3,"default"))

##将数据分为训练和验证集
set.seed(2018)  
#set.seed(4312) 
d = sort(sample(nrow(step2_3), nrow(step2_3)*.7))
train <- step2_3[d,]
test <- step2_3[-d,]


#逻辑回归初步(使用woe转换之后的变量) 
# length函数采用WOE变量前的数据框
lg.full <- glm(default ~.,family = binomial(link='logit'), data = train[,c(1,(length(step2_1) +1):length(train))])
summary(lg.full)


# #逻辑回归仅保留显著变量
# lg.reduced <- glm(default ~., family = binomial(link='logit'), data = train[,c(1,34:39,41,45:47,49:50,54,57,58,60:65)])
# summary(lg.reduced)
# 
# #卡方检验
# #anova(lg.full, lg.reduced, test = "Chisq")
# 
# #去除cook值4倍以上的异常值
# 
# # 通过 cook 距离来查看异常点
# cooksd <- cooks.distance(lg.reduced)
# # 画图
# plot(cooksd, pch=".", cex=1, main="不同cook距离的影响")  # 绘制cook距离
# abline(h = 4*mean(cooksd, na.rm=T), col="red")  # 添加截止分界线
# text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>20*mean(cooksd, na.rm=T),names(cooksd),""), col="blue")  # 添加图签
# ##
# #去除cook4倍以上的异常值
# influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])
# #上一条中得到的influential有NA值需要去除
# influential <- influential[!is.na(influential)]
# #去除训练数据中的异常值
# train <- train[ -influential, ]
# 
# #去除异常值后建模
# lg.reduceOutlier <- glm(default ~., family = binomial(link ='logit'), data = train[,c(1,34:39,41,45:47,49:50,54,57,58,60:65)])
# #结果优于前面的结果
# summary(lg.reduceOutlier)


lg_both <- step(lg.full, direction = "both")
lg_forward <- step(lg.full, direction = "forward")
summary(lg_both)
summary(lg_forward)
# #library(car)
# #
# # #library(pROC)
# #方差膨胀因子检验
# ##经验判断方法表明：当0<VIF<10，不存在多重共线性；当10≤VIF<100，存在较强的多重共线性；当VIF≥100，存在严重多重共线性##
#
vif_result <- vif(lg_both)
vif.filename <- paste("vif_",format(Sys.time(), "%m%d_%H%M%S"), ".csv", sep = "")
write.csv(vif(lg_both), paste(output_path, vif.filename,sep = "/"))

# 自动筛选出逐步回归以后的变量变量名进行WOE单调性判断及相应的绘图，省去手动导出添加WOE后缀再粘贴回来的手续

vif_result <- as.data.frame(vif_result)
vif_result_T <- t(vif_result)

# 不进行逐步回归

reducecols <- c("default", colnames(vif_result_T))
reducecols

# lg.reduce <- glm(default ~.,family = binomial(link='logit'), data = train[,reducecols])
# summary(lg.reduce)
# 
# 
# #再次向前向后
# lg_both <- step(lg.reduce, direction = "both")
# lg_forward <- step(lg.reduce, direction = "forward")
# summary(lg_both)
# summary(lg_forward)
# #library(car)
# 
# #library(pROC)
# #方差膨胀因子检验
# ##经验判断方法表明：当0<VIF<10，不存在多重共线性；当10≤VIF<100，存在较强的多重共线性；当VIF≥100，存在严重多重共线性##
# 
# vif(lg_both)
# vif.filename <- paste("vif2_",format(Sys.time(), "%m%d_%H%M%S"), ".csv", sep = "")
# write.csv(vif(lg_both), paste(output_path, vif.filename, sep = "/"))

#######################################输出变量分布图和woe分箱结果#########################################
#去除"_WOE"获取原始变量名称
reducecols_variable <- lapply(reducecols, function(x) sub(x = x,pattern = "_woe", replacement = ""))
reducecols_variable <- as.character(reducecols_variable)

#获取剩余变量的IV值并保存
filteredIV2<-iv.mult(step2_3[,reducecols_variable],"default",TRUE)
write_csv(filteredIV2,paste(output_path, "filteredIV2018.csv", sep = "/"))

filteredIV2<-iv.mult(step2_3[,reducecols_variable],"default",TRUE)
#获取变量分箱后的单调性
for (i in filteredIV2$Variable) {
  iv_info <- iv.mult(as.data.frame(df),"default",vars = i, summary=FALSE)
  woe_info <- iv_info[[1]]$woe
  woe_info <- as.data.frame(woe_info,nm = "woe_org")
  woe_info$woe_shift <- shift(woe_info$woe_org,1)
  woe_info$diff <- woe_info$woe_org - woe_info$woe_shift
  woe_info <- woe_info[-is.na(woe_info$diff),]
  woe_info$monotone <- woe_info$diff > 0
  if(all(woe_info$diff>=0) ||all(woe_info$diff<0)){
    filteredIV2[which(i == filteredIV2$Variable),"monotone"] <- T
  }
  else{
    filteredIV2[which(i == filteredIV2$Variable),"monotone"] <- F
  }
}

#批量绘制WOE分箱图 根据分箱结果是否单调分别存放在两个不同的文件夹中
monotone_path <- paste(output_path, "woe_monotone", sep = "/")
nomonotone_path <- paste(output_path, "woe_nomonotone", sep = "/")
dir.create(monotone_path)
dir.create(nomonotone_path)
dir.create(paste(monotone_path, "woe", sep = "/"))
dir.create(paste(monotone_path, "IVplot", sep = "/"))
dir.create(paste(nomonotone_path, "woe", sep = "/"))
dir.create(paste(nomonotone_path, "IVplot", sep = "/"))
for (i in filteredIV2$Variable) {
  if(filteredIV2[which(i == filteredIV2$Variable),"monotone"]==TRUE)
  {
    MyivPlot(as.data.frame(df), variableName = i, plot_path = monotone_path)
  }
  else
  {
    MyivPlot(as.data.frame(df), variableName = i, plot_path = nomonotone_path)
  }
}
#批量绘制单变量分布图
histogram_path <- paste(output_path, "histogram", sep = "/")
dir.create(histogram_path)
for (i in filteredIV2$Variable){
  print(as.name(i))
  plot_density <- ggplot(train, aes_string(x = i)) + geom_histogram(position = "dodge",aes(fill=factor(default),y=..density..)) 
  plotname <- paste(i,".jpg",sep = "")
  ggsave(filename = paste(histogram_path, plotname, sep="/"), plot = plot_density)
}


