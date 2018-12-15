###################运行环境配置###################
# 清空变量空间

rm(list = ls())

# 当前项目运行根路径
project_path <- getwd()

# 安装载入各种包
package_manage <- paste(project_path, "/util/packageManage.R",sep = "")
source(package_manage)

# 自定义函数路径
encodingPath <- str_c(project_path,
                      "util",
                      "udf.R",
                      sep="/")

source(encodingPath)

# 输出文件路径
output_path <- str_c(project_path,
                     "output",
                     sep = "/")


####################################导入数据 并合并##############################
# rawData文件路径
datapath <- str_c(project_path,
                  "rawData",
                  "cs-training.csv",
                  sep = "/")



# 原始数据导入
labelledSample_df <- read_csv(datapath, na = c("", "NA", "NULL", "未知"))


# 数据查看
dim(labelledSample_df)
colnames(labelledSample_df)

# 删除序列项
labelledSample_df <- labelledSample_df[,-1]

dim(labelledSample_df)
colnames(labelledSample_df)

# 修改是否通过变量为default

colnames(labelledSample_df)[1] <- 'default'

#保留合并后的原始数据 develop
develop <- labelledSample_df
sample <- develop
dim(sample)
table(sample$default)
# ###################################数据预处理######################################


# ###################################特征衍生######################################


# ###################################异常值处理######################################

boxplot(sample$age)
unique(sample$age)
#删除年龄为0的异常值
sample <- sample[-which(sample$age == 0),]

boxplot(sample$DebtRatio)
unique(sample$DebtRatio)
#删除DebtRatio大于1的异常值
sample=sample[-which(sample$DebtRatio>1),]

#删除逾期次数异常的数据
boxplot(sample$`NumberOfTime30-59DaysPastDueNotWorse`,sample$`NumberOfTime60-89DaysPastDueNotWorse`,sample$NumberOfTimes90DaysLate, names = c('x3','x7','x9'))
unique(sample$`NumberOfTime60-89DaysPastDueNotWorse`)
unique(sample$`NumberOfTime30-59DaysPastDueNotWorse`)
unique(sample$NumberOfTimes90DaysLate)
sample=sample[-which(sample$`NumberOfTime60-89DaysPastDueNotWorse`>95|sample$`NumberOfTime60-89DaysPastDueNotWorse`>95|sample$NumberOfTimes90DaysLate>95),]

dim(sample)
table(sample$default)

####################################缺失值删除并插补#####################################
step1_1 <- sample
#统计所有变量的缺失值
temp <- sapply(step1_1, function(x) sum(is.na(x)))

#按照缺失率排序
miss <- sort(temp, decreasing = T)

#查看含有缺失值的变量概括，并人工删除高缺失值的变量
miss[miss>0]
summary(step1_1[,names(miss)[miss>0]])
miss[miss>0]/dim(step1_1)[1]

colnames(step1_1)

# 用mice包插补缺失值，采用分类回归树算法 cart
imp <- mice(step1_1, met="cart",m=1)
sample_imp <- complete(imp)


#获取没有缺失值的样本 删除有缺失的样本
step1_3 <- na.omit(sample_imp)
dim(step1_3)
table(step1_3$default)


####################################iv值计算并进行初步变量筛选#####################################

colnames(step1_3)
step1_3 <- as.data.frame(step1_3)


#计算各变量IV值并排序
row.names(step1_3) <- 1:nrow(step1_3) 
IV <- iv.mult(step1_3, "default", TRUE)


#将IV保存
iv.filename <- paste("iv_",format(Sys.time(), "%m%d_%H%M%S"), ".csv", sep = "")
iv.csv <- paste(output_path, iv.filename, sep = "/")
write_csv(IV, iv.csv)


#IVcols <- filter(IV, IV$InformationValue > 0.02 & IV$InformationValue < 2)[1]
IVcols <- IV$Variable[1:8]
IVcols <- c("default", IVcols)
#将待训练样本降维
step1_4 <- step1_3[,IVcols]
dim(step1_4)

#保存结果
savehistory()
save.image()

#########################################随机森林筛选变量#################################
set.seed(4321)
crf <- cforest(default~.,control = cforest_unbiased(mtry = 8, ntree = 20), data = step1_4)
varimpt <- data.frame(varimp(crf))
c <- as.data.frame(varimpt)
save.image() #保存中间结果
crf.filename <- paste("crf_importance_",format(Sys.time(), "%m%d_%H%M%S"), ".csv", sep = "")
crf_varimpt.csv <- paste(output_path, crf.filename,sep = "/")
write.csv(c, file = crf_varimpt.csv)

#选择随机森林重要性0.00000以上的变量
#设定随机森林重要性参数阈值
CRF_BASE <- 0.0000

#筛选随机森林后结果变量
crfcols <- NULL

for(i in 1:length(c[,1])){
  if(c[i,1]>CRF_BASE){
    crfcols <- c(crfcols, rownames(c)[i])
  }
}

crfcols <- c("default",crfcols)

#如果随机森林结果不含特定想加入的变量又在阈值设定的排除在外可在以下代码中加入

# additiveCols <- c("test","test2")
# colBoolean <- additiveCols %in% crfcols
# 
# for (i in 1:length(colBoolean)) {
#   if (!colBoolean[i]) {
#     print(colBoolean[i])
#     crfcols <- c(crfcols, additiveCols[i])
#   }
# }

step1_5 <- step1_3[, crfcols]

colnames(step1_5)

# 保存最终清洗后数据
application_data.filename <- paste("application_data_",format(Sys.time(), "%m%d_%H%M%S"), ".csv", sep = "")
application_data.csv <- paste(output_path, application_data.filename,sep = "/")
write_csv(step1_5, application_data.csv)
save.image()


