#清空变量空间

rm(list = ls())

#当前项目运行根路径
project_path <- getwd()

#安装载入各种包
package_manage <- paste(project_path, "/util/packageManage.R",sep = "")
source(package_manage)

#util路径
encodingPath <- str_c(project_path,
                      "util",
                      "udf.R",
                      sep="/")

source(encodingPath)

output_path <- str_c(project_path,
                     "output",
                     sep = "/")

#######################新逻辑数据的读取与合并


####################################导入数据 并合并##############################
#rawData文件路径
datapath <- str_c(project_path,
                  "rawData",
                  "product_cid_94.csv",
                  sep = "/")



# 原始数据导入
labelledSample_df <- read_csv(datapath, na = c("", "NA", "NULL", "未知"))


# mergedDatadf <- labelledSample_df %>% 
#   inner_join(riskDB_df, by = "user_id") %>% 
#   inner_join(behavior_df, by = "user_id")




# 筛选出有借款表现的用户，去除申请被拒绝的用户
# labelledSample_df_isLoan <- filter(labelledSample_df,labelledSample_df$is_loan==1)

dim(labelledSample_df)
colnames(labelledSample_df)

# delete the step number column
labelledSample_df <- labelledSample_df[,-1]

dim(labelledSample_df)
colnames(labelledSample_df)

# 修改是否通过变量为default

colnames(labelledSample_df)[119] <- 'default'

# 将不通过的指标标记为1，通过的指标标记为0，模型预测不通过的概率
labelledSample_df$default[labelledSample_df$default %in% 1] <- 2
labelledSample_df$default[labelledSample_df$default %in% 0] <- 1
labelledSample_df$default[labelledSample_df$default %in% 2] <- 0


#保留合并后的原始数据 develop
develop <- labelledSample_df
sample <- develop
dim(sample)
table(sample$default)
# ###################################数据预处理######################################
# #处理real_valid因子变量
# sample$real_valid <- as.factor(sample$real_valid)
# levels(sample$real_valid)[2] <- 1
# levels(sample$real_valid)[1] <- 0
# table(sample$real_valid)
# 
# #将extend_phone_age为0的赋值为缺失值再进行插补
# sample$extend_phone_age[sample$extend_phone_age==0]<- NA

# ###################################特征衍生######################################

step1_1 <- sample

## 省份数据处理，参考国美
ggplot(step1_1, aes(province)) + geom_histogram(position = "dodge", stat = "count",  aes(fill=factor(default)))

province_df  <- group_by(step1_1, province)
province_df <- province_df[,c('province', 'default')]
province_df <- ddply(province_df, ~province, summarise ,mean = mean(default))
province_df <- province_df[order(province_df$mean),]

ggplot(province_df, aes(x=province, y=mean,group=1)) + geom_line() + geom_point()
row.names(province_df) <- 1:nrow(province_df)
province_df

province_data.filename <- paste("province_data_",format(Sys.time(), "%m%d_%H%M%S"), ".csv", sep = "")
province_data.csv <- paste(output_path, province_data.filename,sep = "/")
write_csv(province_df, province_data.csv)

step1_1$province[step1_1$province %in% c("西藏",
                                         "广西",
                                         "陕西",
                                         "北京",
                                         "河北",
                                         "江西",
                                         "江苏",
                                         "浙江",
                                         "海南")] <- 4

step1_1$province[step1_1$province %in% c("安徽",
                                         "湖南",
                                         "辽宁",
                                         "河南",
                                         "贵州",
                                         "山西",
                                         "山东",
                                         "黑龙江")] <- 3

step1_1$province[step1_1$province %in% c("甘肃",
                                         "四川",
                                         "重庆",
                                         "湖北",
                                         "吉林",
                                         "广东")] <- 2

step1_1$province[step1_1$province %in% c("云南",
                                         "上海",
                                         "NA",
                                         "宁夏")] <- 1


# 检查省份数据是否替换完成
unique(step1_1$province)
table(step1_1$province)

## 城市数据处理参考我来贷，无缺失值
ggplot(step1_1, aes(city)) + geom_histogram(position = "dodge", stat = "count",  aes(fill=factor(default)))

city_df  <- group_by(step1_1, province)
city_df <- city_df[,c('city', 'default')]
city_df <- ddply(city_df, ~city, summarise ,mean = mean(default))
city_df <- city_df[order(city_df$mean),]

ggplot(city_df, aes(x=city, y=mean,group=1)) + geom_line() + geom_point()

row.names(city_df) <- 1:nrow(city_df)
city_df

city_data.filename <- paste("city_data_",format(Sys.time(), "%m%d_%H%M%S"), ".csv", sep = "")
city_data.csv <- paste(output_path, city_data.filename,sep = "/")
write_csv(city_df, city_data.csv)


step1_1$city[step1_1$city %in% c("北京", "上海", "广州", "深圳")] <- 4

step1_1$city[step1_1$city %in% c("成都", "杭州", "重庆", "武汉", "苏州", "西安", "天津", "南京", "郑州",
                                 "长沙", "沈阳", "青岛", "宁波", "东莞", "无锡",
                                 "昆明", "大连", "厦门")] <- 3

step1_1$city[step1_1$city %in% c("合肥", "佛山", "福州", "哈尔滨", "济南", "温州", "长春", "石家庄", "常州",  
                                 "泉州", "南宁", "贵阳", "南昌", "南通", "金华", "徐州", "太原", "嘉兴", "烟台", "惠州", "保定", "台州",
                                 "中山", "绍兴", "乌鲁木齐", "潍坊", "兰州")] <- 2

step1_1$city[step1_1$city %!in% c("4", "3", "2")] <- 1


# 检查城市数据是否替换完成
unique(step1_1$city)

## 处理手机系统


# 显示手机系统所含字段
unique(step1_1$ov)
step1_1$ov[grepl("^iOS|^iPhone", step1_1$ov)] <- 3

step1_1$ov[grepl("^and|^Android|^2", step1_1$ov)] <- 1

step1_1$ov[step1_1$ov %in% NA] <- -999

# 显示手机型号所含字段
unique(step1_1$ov)

# iphone手机版本9以上
step1_1$phone_model[grepl("^iPhone[9,1]", step1_1$phone_model)] <- 6

step1_1$phone_model[grepl("^iPhone[2-8]|^iPad", step1_1$phone_model)] <- 5

# step1_1$phone_model[grepl("iP", step1_1$phone_model)] <- 5

step1_1$phone_model[grepl("^HONOR|^HUAWEI", str_to_upper(step1_1$phone_model))] <- 4

step1_1$phone_model[grepl("^XIAOMI|^MI", str_to_upper(step1_1$phone_model))] <- 3

step1_1$phone_model[grepl("^SAMSUNG|^SM-|^NOKIA|^LENOVO|^ZTE", str_to_upper(step1_1$phone_model))] <- 2

step1_1$phone_model[grepl("^VIVO|^OPPO|^MEIZU|^COOLPAD", str_to_upper(step1_1$phone_model))] <- 1

step1_1$phone_model[step1_1$phone_model %in% NA] <- -999

step1_1$phone_model[step1_1$phone_model %!in%  c("-999", "1", "2", "3", "4", "5","6")] <- 0

# 检查手机型号数据是否替换完成
unique(step1_1$phone_model)


## 性别处理

sex_df  <- group_by(step1_1, sex)
sex_df <- sex_df[,c('sex', 'default')]
sex_df <- ddply(sex_df, ~sex, summarise ,mean = mean(default))
sex_df <- sex_df[order(sex_df$mean),]
row.names(step2_3) <- 1:nrow(step2_3)


ggplot(sex_df, aes(x=sex, y=mean,group=1)) + geom_line() + geom_point()


step1_1$sex[step1_1$sex %in% c("女")] <- 3

step1_1$sex[step1_1$sex %in% c("男")] <- 1

step1_1$sex[step1_1$sex %in% NA] <- 2

# 检查性别数据是否替换完成
unique(step1_1$sex)


## 公积金状态处理 参考冰鉴，处理成哑变量，且NA 填充为正常
unique(step1_1$status)


step1_1$status[step1_1$status %in% c("正常", 
                                     "正常汇缴", 
                                     "缴存", 
                                     "正常缴存", 
                                     "合并", 
                                     "进行中",
                                     "交存",
                                     "正常缴交",
                                     NA)] <- "normal"

step1_1$status[step1_1$status %in% c("停缓缴",
                                     "封存")] <- "seal_up"

step1_1$status[step1_1$status %in% c("转出",
                                     "已转出",
                                     "转出（跨）")] <- "transfer"

step1_1$status[step1_1$status %in% c("销户",
                                     "已销户",
                                     "支取销户",
                                     "外部转出销户",
                                     "合并销户",
                                     "提取销户")] <- "close"

# 公积金状态数据替换检查
unique(step1_1$status)
# 哑变量转换
status.dummy <- model.matrix(~status-1, data = step1_1)

step1_1 <- cbind(step1_1, status.dummy)



#因行为数据出错，先去除行为数据
# step1_1 <- sample[,-c(220:264)]
step1_2 <- step1_1

colnames(step1_2)

colnames(step1_2)[33] <- "first_apply_time"

## 将平台业务行为数据框中NA数据批量填充为0值填充
for(i in 42:118){
  step1_2[, i][is.na(step1_2[, i])] <- 0
}

# 原国美衍生变量及补充
step1_2 <-  mutate(step1_2,

                   # 第一次申请到第一次放款时间之差
                   dateDelta_first_apply_to_first_loan = as.numeric(difftime(ymd(as.Date(step1_2$first_apply_time, "%d/%m/%Y")), 
                                                                             ymd(as.Date(step1_2$first_loan_time, "%d/%m/%Y")), units = "days")),
                   # 最后一次放款到第一次放款时间之差
                   dateDelta_last_loan_to_first_loan =  as.numeric(difftime(ymd(as.Date(step1_2$last_loan_time, "%d/%m/%Y")), 
                                                                            ymd(as.Date(step1_2$first_loan_time, "%d/%m/%Y")), units = "days")),
                   # 公积金刷新截止当前时间
                   dateDelta_refresh_time_to_apply_time =  as.numeric(difftime(ymd(as.Date(step1_2$confirm_time_day, "%Y-%m-%d")), 
                                                                               ymd(as.Date(step1_2$refresh_time_to_current_days, "%d/%m/%Y")), units = "days")),
                   # 最近1月通讯录被叫次数
                   each_other_called_type_count = step1_2$each_other_call_count - step1_2$each_other_call_type_count,
                   # 最近3月平均通讯录被叫次数
                   each_other_called_type_3count = step1_2$each_other_call_3count - step1_2$each_other_call_type_3count,
                   # 最近6月平均通讯录 被叫次数
                   each_other_called_type_6count = step1_2$each_other_call_6count - step1_2$each_other_call_type_6count,
                   # 最近1月通讯录通话次数占比
                   each_other_count_1m_rate = step1_2$each_other_call_count / step1_2$call_count,
                   # 最近3月通讯录通话次数占比
                   each_other_count_2m_rate = step1_2$each_other_call_3count / step1_2$call_3count,
                   # 最近6月通讯录里通话次数占比
                   each_other_count_1m_rate = step1_2$each_other_call_6count / step1_2$call_6count,
                   # 逾期金额率
                   overdue_amount_rate = step1_2$amount_overdue / step1_2$loan_amount,
                   # 逾期申请率
                   overdue_apply_rate = step1_2$overdue_apply / step1_2$confirm_apply,
                   # 逾期产品率
                   overdue_product_rate = step1_2$overdue_apply / step1_2$confirm_apply,
                   # 申请通过率
                   approve_apply_rate = step1_2$approve_apply / step1_2$confirm_apply,
                   # 产品通过率
                   approve_product_rate = step1_2$approve_product / step1_2$confirm_product,
                   # 贷款额是缴存基数的倍数
                   loan_multiple = loan_amount / deposit_base
                   
                   )


#将因子类型数据数字化 将字符串变量改成数字
index_factor <- sapply(step1_2, is.factor)
step1_2[index_factor] <- lapply(step1_2[index_factor], function(x) as.numeric(as.character(x)))


index_character <- sapply(step1_2, is.character)
step1_2[index_character] <- lapply(step1_2[index_character], function(x) as.numeric((x)))



####################################缺失值删除并插补#####################################
#统计所有变量的缺失值
temp <- sapply(step1_2, function(x) sum(is.na(x)))

#按照缺失率排序
miss <- sort(temp, decreasing = T)

#查看含有缺失值的变量概括，并人工删除高缺失值的变量
miss[miss>0]
summary(step1_2[,names(miss)[miss>0]])
miss[miss>0]/dim(step1_2)[1]

colnames(step1_2)

# 删除完全缺失以及已经衍生过的特征
step1_2 <- step1_2[, -which(colnames(step1_2) %in% c("birthday",
                                                     "refresh_time_to_current_days",
                                                     "location",
                                                     "status",
                                                     "first_apply_time",
                                                     "first_loan_time",
                                                     "last_apply_time",
                                                     "last_loan_time",
                                                     "is_loan",
                                                     "confirm_time_day"))]



####################################缺失值删除并插补#####################################


#部分比例数据用0来填充

step1_2$dateDelta_last_loan_to_first_loan[step1_2$dateDelta_last_loan_to_first_loan %in% NA] <- 0

step1_2$overdue_amount_rate[step1_2$overdue_amount_rate %in% c("NaN", "NA")] <- 0

step1_2$dateDelta_first_apply_to_first_loan[step1_2$dateDelta_first_apply_to_first_loan %in% NA] <- 0


#统计所有变量的缺失值
temp <- sapply(step1_2, function(x) sum(is.na(x)))

#按照缺失率排序
miss <- sort(temp, decreasing = T)

#查看含有缺失值的变量概括，并人工删除高缺失值的变量
miss[miss>0]
summary(step1_2[,names(miss)[miss>0]])
miss[miss>0]/dim(step1_2)[1]

# 删除完全缺失以及已经衍生过的特征
step1_2 <- step1_2[, -which(colnames(step1_2) %in% c("top10_call_time",
                                                     "top10_call_count",
                                                     "top10_call_3time",
                                                     "top10_call_3count",
                                                     "top10_call_6time",
                                                     "top10_call_6count"))]
#统计所有变量的缺失值
temp <- sapply(step1_2, function(x) sum(is.na(x)))

#按照缺失率排序
miss <- sort(temp, decreasing = T)

#查看含有缺失值的变量概括，并人工删除高缺失值的变量
miss[miss>0]
summary(step1_2[,names(miss)[miss>0]])
miss[miss>0]/dim(step1_2)[1]


step1_2$approve_product[step1_2$approve_product %in% c("NaN", "NA")] <- 0
step1_2$approve_product_rate[step1_2$approve_product_rate %in% c("NaN", "NA")] <- 0
step1_2$approve_apply_rate[step1_2$approve_apply_rate %in% c("NaN", "NA")] <- 0
step1_2$overdue_apply_rate[step1_2$overdue_apply_rate %in% c("NaN", "NA")] <- 0
step1_2$overdue_product_rate[step1_2$overdue_product_rate %in% c("NaN", "NA")] <- 0
step1_2$loan_multiple[step1_2$loan_multiple %in% c("NaN", "NA")] <- 0


#保存结果
savehistory()
save.image()

colnames(step1_2)



# 用mice包插补缺失值，采用分类回归树算法 cart
imp <- mice(step1_2,met="cart",m=1)
sample_imp <- complete(imp)

#确认插补后的样本已无缺失值
# temp <- sapply(step1_2, function(x) sum(is.na(x))==0)
# sample_imp_no_miss <- sample_imp[temp]

#获取没有缺失值的样本 删除有缺失的样本
step1_3 <- na.omit(sample_imp)
dim(step1_3)
table(step1_3$default)
## 数据类型处理

#将因子类型数据数字化 将字符串变量改成数字
# index_factor <- sapply(step1_3, is.factor)
# step1_3[index_factor] <- lapply(step1_3[index_factor], function(x) as.numeric(as.character(x)))

index_date <- sapply(step1_3, is.Date)
step1_3[index_date] <- lapply(step1_3[index_date], function(x) as.numeric((x)))

index_character <- sapply(step1_3, is.character)
step1_3[index_character] <- lapply(step1_3[index_character], function(x) as.numeric((x)))

####################################iv值计算并进行初步变量筛选#####################################
#step1_3 <- sample_imp
table(step1_3$default)
colnames(step1_3)
step1_3 <- as.data.frame(step1_3)


#计算各变量IV值并排序
row.names(step1_3) <- 1:nrow(step1_3) 
IV <- iv.mult(step1_3, "default", TRUE)

# 输出CSV文件用于python决策树模型开发
dfForPython.filename <- paste(output_path, "dfForPython_include_operation.csv", sep = "/")

write_csv(step1_3, dfForPython.filename)

#将IV保存
iv.filename <- paste("iv_",format(Sys.time(), "%m%d_%H%M%S"), ".csv", sep = "")
iv.csv <- paste(output_path, iv.filename, sep = "/")
write_csv(IV, iv.csv)


IVcols <- filter(IV, IV$InformationValue > 0.02 & IV$InformationValue < 1)[1]
IVcols <- c("default", IVcols$Variable)
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
#设定随机森林重要性参数
CRF_BASE <- 0.0000

#筛选随机森林后结果变量
crfcols <- NULL

for(i in 1:length(c[,1])){
  if(c[i,1]>CRF_BASE){
    crfcols <- c(crfcols, rownames(c)[i])
  }
}

crfcols <- c("default",crfcols)

#如果随机森林结果不含system则加入这三个变量

additiveCols <- c("ov","phone_model")
colBoolean <- additiveCols %in% crfcols

for (i in 1:length(colBoolean)) {
  if (!colBoolean[i]) {
    print(colBoolean[i])
    crfcols <- c(crfcols, additiveCols[i])
  }
}

step1_5 <- step1_3[, crfcols]

colnames(step1_5)

# 删除完全缺失以及已经衍生过的特征
step1_5 <- step1_5[, -which(colnames(step1_5) %in% c("gjj_sid",
                                                     "user_sid",
                                                     "apply_sid"))]
colnames(step1_5)

# step1_5 <- step1_5[,-c(2)] # 将逾期天数变量去除

# #去除borrow_risk_days为负的样本
# step1_6 <- step1_5[step1_5$borrow_risk_days>=0,]
# s
# # #去除与borrow_risk_days变量相关的变量
# # step3_1 <- step3_1[,-which(colnames(step3_1) %in% c("borrow_risk_days","average_borrow_internal_days","borrow_latest_1month_count","borrow_latest_2month_count"))]
# # 
# # #去除重复变量，后续在SQL语句中更新
# # step3_1 <- step3_1[,-which(colnames(step3_1) %in% c("renewal_count.1"))]
# 
# #保存清洗后数据
application_data.filename <- paste("application_data_",format(Sys.time(), "%m%d_%H%M%S"), ".csv", sep = "")
application_data.csv <- paste(output_path, application_data.filename,sep = "/")
write_csv(step1_5, application_data.csv)
save.image()


