
# -*- coding:utf-8 -*-
# Data Manipulation 
import pandas as pd
import time
import datetime
import numpy as np
import csv
import pickle
import numbers
import statsmodels.api as sm
from itertools import combinations
from sklearn.preprocessing import StandardScaler
from sklearn import preprocessing
from sklearn.metrics import roc_curve
from sklearn.metrics import *
from sklearn.metrics import roc_auc_score
from sklearn.externals import joblib
import math
# Visualization
import matplotlib.pyplot as plt
import seaborn as sns
from pandas.tools.plotting import scatter_matrix
from mpl_toolkits.mplot3d import Axes3D

# Machine learning
import sklearn.ensemble as ske
from sklearn import datasets, model_selection, tree, preprocessing, metrics, linear_model
from sklearn.svm import LinearSVC
from sklearn.ensemble import VotingClassifier, RandomForestClassifier, GradientBoostingClassifier
from sklearn.neighbors import KNeighborsClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.linear_model import LinearRegression, LogisticRegression, Ridge, Lasso, SGDClassifier, LogisticRegressionCV
from sklearn.tree import DecisionTreeClassifier

# Grid and Random Search
import scipy.stats as st
from scipy.stats import randint as sp_randint
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import train_test_split
from sklearn.model_selection import RandomizedSearchCV
import scorecard_functions_V3

df_train_1 = pd.read_csv('E:\\jq_data.csv', header=0,encoding='gbk')
# df_oot_1 = pd.read_csv('df_oot.csv', header=0, encoding = 'gbk')

# 缺失值填充
# 最后再统一用-1填充
trainData = df_train_1.copy()
for col in trainData.columns.tolist():
    if trainData[col].dtype == 'object':
        trainData[col].fillna('nulls', inplace=True)
        trainData[col].fillna('nulls', inplace=True)
    else:
        trainData[col].fillna(-1, inplace=True)
        trainData[col].fillna(-1, inplace=True)



# # IV重要性:分箱

# 查看

allFeatures = list(trainData.columns)
allFeatures.remove('label')
print(trainData.shape)
trainData.head(30)



# 将特征区分为数值型和类别型
numerical_var = []
for var in allFeatures:
    # pd.unique()去除列表当中的重复值
    uniq_vals = trainData[var].unique()
    # uniq_vals = list(set(trainData[var]))
    if np.nan in uniq_vals:
        # 将去重后的列表当中的naN值删除
        uniq_vals.remove(np.nan)
    # 如果值的个数在10个以上且列表中的第一个值为数值型，则加入为数值型
    if len(uniq_vals) >= 10 and isinstance(uniq_vals[0], numbers.Real):
        numerical_var.append(var)

# 数值型以外的特征均为类别型特征
categorical_var = [i for i in allFeatures if i not in numerical_var]

print(numerical_var)
print(categorical_var)



'''
对于类别型变量，按照以下方式处理
1，如果变量的取值个数超过5，计算bad rate进行编码
2，除此之外，其他任何类别型变量如果有某个取值中，对应的样本全部是坏样本或者是好样本，进行合并。
'''
deleted_features = []  # 将处理过的变量删除，防止对后面建模的干扰
encoded_features = {}  # 将bad rate编码方式保存下来，在以后的测试和生产环境中需要使用
merged_features = {}  # 将类别型变量合并方案保留下来
var_IV = {}  # save the IV values for binned features       #将IV值保留和WOE值
var_WOE = {}
for col in categorical_var:
    print('we are processing {}'.format(col))
    if len(set(trainData[col])) > 5:
        print('{} is encoded with bad rate'.format(col))
        col0 = str(col) + '_encoding'

        # (1), 计算坏样本率并进行编码
        encoding_result = scorecard_functions_V3.BadRateEncoding(trainData, col, 'label')
        trainData[col0], br_encoding = encoding_result['encoding'], encoding_result['bad_rate']

        # (2), 将（1）中的编码后的变量也加入数值型变量列表中，为后面的卡方分箱做准备
        numerical_var.append(col0)

        # (3), 保存编码结果
        encoded_features[col] = [col0, br_encoding]

        # (4), 删除原始值

        deleted_features.append(col)
    else:
        bad_bin = trainData.groupby([col])['label'].sum()
        # 对于类别数少于5个，但是出现0坏样本的特征需要做处理
        if min(bad_bin) == 0:
            print('{} has 0 bad sample!'.format(col))
            col1 = str(col) + '_mergeByBadRate'
            # (1), 找出最优合并方式，使得每一箱同时包含好坏样本
            mergeBin = scorecard_functions_V3.MergeBad0(trainData, col, 'label')
            # (2), 依照（1）的结果对值进行合并
            trainData[col1] = trainData[col].map(mergeBin)
            maxPcnt = scorecard_functions_V3.MaximumBinPcnt(trainData, col1)
            # 如果合并后导致有箱占比超过90%，就删除。
            if maxPcnt > 0.9:
                print('{} is deleted because of large percentage of single bin'.format(col))
                deleted_features.append(col)
                categorical_var.remove(col)
                # 此处可以不删除，因为已经存入deleted_features中了
                del trainData[col]
                continue
            # (3) 如果合并后的新的变量满足要求，就保留下来
            merged_features[col] = [col1, mergeBin]
            WOE_IV = scorecard_functions_V3.CalcWOE(trainData, col1, 'label')
            var_WOE[col1] = WOE_IV['WOE']
            var_IV[col1] = WOE_IV['IV']
            # 此处可以删除类别型原始变量，以后用encoding编码替代
            # del trainData[col]
            deleted_features.append(col)
        else:
            WOE_IV = scorecard_functions_V3.CalcWOE(trainData, col, 'label')
            var_WOE[col] = WOE_IV['WOE']
            var_IV[col] = WOE_IV['IV']

print(encoded_features)

'''
对于连续型变量，处理方式如下：
1，利用卡方分箱法将变量分成5个箱
2，检查坏样本率的单调性，如果发现单调性不满足，就进行合并，直到满足单调性
'''
var_cutoff = {}
for col in numerical_var:
    print("{} is in processing".format(col))
    col1 = str(col) + '_Bin'

    # (1),用卡方分箱法进行分箱，并且保存每一个分割的端点。例如端点=[10,20,30]表示将变量分为x<10,10<x<20,20<x<30和x>30.
    # 特别地，缺失值-1不参与分箱
    if -1 in set(trainData[col]):
        special_attribute = [-1]
    else:
        special_attribute = []
    cutOffPoints = scorecard_functions_V3.ChiMerge(trainData, col, 'label', special_attribute=special_attribute)
    var_cutoff[col] = cutOffPoints
    trainData[col1] = trainData[col].map(lambda x: scorecard_functions_V3.AssignBin(x, cutOffPoints, special_attribute=special_attribute))

    # (2), check whether the bad rate is monotone
    BRM = scorecard_functions_V3.BadRateMonotone(trainData, col1, 'label', special_attribute=special_attribute)
    if not BRM:
        if special_attribute == []:
            bin_merged = scorecard_functions_V3.Monotone_Merge(trainData, 'label', col1)
            removed_index = []
            for bin in bin_merged:
                if len(bin) > 1:
                    indices = [int(b.replace('Bin ', '')) for b in bin]
                    removed_index = removed_index + indices[0:-1]
            removed_point = [cutOffPoints[k] for k in removed_index]
            for p in removed_point:
                cutOffPoints.remove(p)
            var_cutoff[col] = cutOffPoints
            trainData[col1] = trainData[col].map(
                lambda x: scorecard_functions_V3.AssignBin(x, cutOffPoints, special_attribute=special_attribute))
        else:
            cutOffPoints2 = [i for i in cutOffPoints if i not in special_attribute]
            temp = trainData.loc[~trainData[col].isin(special_attribute)]
            bin_merged = scorecard_functions_V3.Monotone_Merge(temp, 'label', col1)
            removed_index = []
            for bin in bin_merged:
                if len(bin) > 1:
                    indices = [int(b.replace('Bin ', '')) for b in bin]
                    removed_index = removed_index + indices[0:-1]
            removed_point = [cutOffPoints2[k] for k in removed_index]
            for p in removed_point:
                cutOffPoints2.remove(p)
            cutOffPoints2 = cutOffPoints2 + special_attribute
            var_cutoff[col] = cutOffPoints2
            trainData[col1] = trainData[col].map(
                lambda x: scorecard_functions_V3.AssignBin(x, cutOffPoints2, special_attribute=special_attribute))

    # (3), 分箱后再次检查是否有单一的值占比超过90%。如果有，删除该变量
    maxPcnt = scorecard_functions_V3.MaximumBinPcnt(trainData, col1)
    if maxPcnt > 0.9:
        del trainData[col1]
        deleted_features.append(col)
        numerical_var.remove(col)
        print('we delete {} because the maximum bin occupies more than 90%'.format(col))
        continue

    WOE_IV = scorecard_functions_V3.CalcWOE(trainData, col1, 'label')
    
    
    var_IV[col] = WOE_IV['IV']
    var_WOE[col] = WOE_IV['WOE']
    # del trainData[col]


print(var_IV)


print(var_WOE)


print(deleted_features)


print(encoded_features)


print(merged_features)


print(var_cutoff)


trainData.to_csv('trainData.csv', header=True, index=False, encoding='gbk')
print (len(numerical_var), len(categorical_var))

# # 序列化存储


with open('var_WOE.pkl', "wb") as f:
    f.write(pickle.dumps(var_WOE))

with open('var_IV.pkl', "wb") as f:
    f.write(pickle.dumps(var_IV))

with open('var_cutoff.pkl', "wb") as f:
    f.write(pickle.dumps(var_cutoff))

with open('merged_features.pkl', "wb") as f:
    f.write(pickle.dumps(merged_features))

# # WOE编码后的单变量分析与多变量分析


# 读取数据和序列化
trainData = pd.read_csv('trainData.csv', header=0, encoding='gbk')


# 查看变量WOE：可以单独打开运行
with open('var_WOE.pkl', "rb") as f:
    var_WOE = pickle.load(f)

# 查看变量IV
with open('var_IV.pkl', "rb") as f:
    var_IV = pickle.load(f)

# 查看分割数据箱
with open('var_cutoff.pkl', "rb") as f:
    var_cutoff = pickle.load(f)

with open('merged_features.pkl', "rb") as f:
    merged_features = pickle.load(f)


# 综合数据，使得数据变为
for col in var_WOE.keys():
    print(col)
    col
    col2 = str(col) + "_WOE"
    if col in var_cutoff.keys():
        cutOffPoints = var_cutoff[col]
        special_attribute = []
        if -1 in cutOffPoints:
            special_attribute = [-1]
        binValue = trainData[col].map(lambda x: scorecard_functions_V3.AssignBin(x, cutOffPoints, special_attribute=special_attribute))
        trainData[col2] = binValue.map(lambda x: var_WOE[col][x])
    #     elif trainData[col].dtype=='object':
    #         trainData[col2] = trainData[col].map(lambda x: var_WOE[col][str(x)])
    else:
        trainData[col2] = trainData[col].map(lambda x: var_WOE[col][x])


all_IV = list(var_IV.values())
all_IV = sorted(all_IV, reverse=True)
print(var_IV)
plt.xlabel('var')
plt.ylabel('IV')
plt.title('IV sort')
plt.bar(range(len(all_IV)), all_IV)
iv_threshould = 0.1
varByIV = [k for k, v in var_IV.items() if v > iv_threshould ]


print(varByIV)



# 检查WOE编码后的变量的两两线性相关性
# 检查相关性并剔除,单变量分析：比较两两线性相关性。如果相关系数的绝对值高于阈值，剔除IV较低的一个
var_IV_selected = {k: var_IV[k] for k in varByIV}
var_IV_sorted = sorted(var_IV_selected.items(), key=lambda d: d[1], reverse=True)
var_IV_sorted = [i[0] for i in var_IV_sorted]
print(var_IV_sorted)
trainData_1 = trainData.copy()


removed_var = []
roh_thresould = 0.6
for i in range(len(var_IV_sorted) - 1):                       
    if var_IV_sorted[i] not in removed_var:
        x1 = var_IV_sorted[i] + "_WOE"
        for j in range(i + 1, len(var_IV_sorted)):
            if var_IV_sorted[j] not in removed_var:
                x2 = var_IV_sorted[j] + "_WOE"
                roh = np.corrcoef([trainData_1[x1], trainData_1[x2]])[0, 1]
                if abs(roh) >= roh_thresould:
                    print('the correlation coeffient between {0} and {1} is {2}'.format(x1, x2, str(roh)))
                    if var_IV[var_IV_sorted[i]] > var_IV[var_IV_sorted[j]]:
                        removed_var.append(var_IV_sorted[j])
                    else:
                        removed_var.append(var_IV_sorted[i])
removed_var.append("gjj_time_to_current_days")
#removed_var.append("is_last_apply_overdue")
#removed_var.append("top10_call_3count")
#removed_var.append("call_6time")
var_IV_sortet_2 = [i for i in var_IV_sorted if i not in removed_var]
print(var_IV_sortet_2)



# 画热力图 有mask只保留一半即可
plt.style.use('seaborn-whitegrid')
fig = plt.figure(figsize=(25, 10))
plt.subplot(1, 2, 1)
plt.title('Heatmap')
mask = np.zeros_like(trainData_1[var_IV_sorted].corr(), dtype=np.bool)
mask[np.triu_indices_from(mask)] = True
sns.heatmap(trainData_1[var_IV_sorted].corr(), vmin=-0.1, vmax=0.1, mask=mask, square=True)


'''

### (iii）检查是否有变量与其他所有变量的VIF > 10,但是此步骤并未删除和发现多重共线性
'''
#这一步没有发现有多重共线性


for i in range(len(var_IV_sortet_2)):
    x0 = trainData_1[var_IV_sortet_2[i] + '_WOE']
    
    var_IV_sortet_2[i]
    x0 = np.array(x0)
    X_Col = [k + '_WOE' for k in var_IV_sortet_2 if k != var_IV_sortet_2[i]]
    X = trainData_1[X_Col]
    X = np.matrix(X)
   
    regr = LinearRegression()
    clr = regr.fit(X, x0)
    
    x_pred = clr.predict(X)
    R2 = 1 - ((x_pred - x0) ** 2).sum() / ((x0 - x0.mean()) ** 2).sum()
    vif = 1 / (1 - R2)
    if vif > 10:
        print("Warning: the vif for {0} is {1}".format(var_IV_sortet_2[i], vif))





# 应用逻辑回归模型
multi_analysis = [i + '_WOE' for i in var_IV_sortet_2]
y = trainData_1['label']
X = trainData_1[multi_analysis].copy()
X['intercept'] = [1] * X.shape[0]
#print(trainData_1[multi_analysis].head(2))

#print(X.head(1))

#np.seterr(divide='ignore', invalid='ignore')
LR = sm.Logit(y, X).fit()
summary = LR.summary2()
pvals = LR.pvalues.to_dict()
params = LR.params.to_dict()
print(summary)
print(pvals,params)








# 置信区间(confidence interval)可以看出模型系数的健壮性:
# 较窄的置信区间比较宽的置信区间能提供更多的有关总体参数的信息
LR.conf_int()


print(multi_analysis)

# 逐步回归法，依次单独检验大于显著性为0.1的
# p_value_list[var] = LR.pvalues[var] 说明单独检验是正常的，就存在共线性

varLargeP = {k: v for k, v in pvals.items() if v >= 0.1}
varLargeP = sorted(varLargeP.items(), key=lambda d: d[1], reverse=True)
varLargeP = [i[0] for i in varLargeP]
p_value_list = {}
for var in varLargeP:
    X_temp = trainData_1[var].copy().to_frame()
    X_temp['intercept'] = [1] * X_temp.shape[0]
    LR = sm.Logit(y, X_temp).fit()
    p_value_list[var] = LR.pvalues[var]
 #   print( p_value_list)
for k, v in p_value_list.items():
    print("{0} has p-value of {1} in univariate regression".format(k, v))
    multi_analysis.remove(k)
#    if k in multi_analysis:
#        multi_analysis.remove(k)

#print(multi_analysis)

var_IV_sortet_2 = [i for i in var_IV_sorted if i not in removed_var]


# 系数为正单独检验，检验后为负说明之前的正是受多重共线性影响的
# coef_list[var] = LR.params[var]：检测后为负，说明单独正确，但整体存在在存在共线性
varPositive = [k for k, v in params.items() if v >= 0]
varPositive.remove('intercept')
coef_list = {}
for var in varPositive:
    X_temp = trainData_1[var].copy().to_frame()
    X_temp['intercept'] = [1] * X_temp.shape[0]
    LR = sm.Logit(y, X_temp).fit()
    coef_list[var] = LR.params[var]
#    print(coef_list)
for k, v in coef_list.items():
    print("{0} has coefficient of {1} in univariate regression".format(k, v))
    if k in  multi_analysis:
        multi_analysis.remove(k)
#    if k in multi_analysis:
#        multi_analysis.remove(k)

#print(multi_analysis)


# 最后筛选变量跑模型，循环跑研究数据不断迭代模型
# 此次循环不断迭代最后迭代所有变量
# 向前筛选法，要求每一步选进来的变量需要使得所有变量的系数的符号和p值同时符合要求

selected_var = [multi_analysis[0]]
for var in multi_analysis[1:]:
    try_vars = selected_var + [var]
    X_temp = trainData_1[try_vars].copy()
    X_temp['intercept'] = [1] * X_temp.shape[0]
    LR = sm.Logit(y, X_temp).fit()
    summary = LR.summary2()
    pvals, params = LR.pvalues, LR.params
    del params['intercept']
    if max(pvals) < 0.1 and max(params) < 0:
        selected_var.append(var)
#print(selected_var)



y = trainData_1['label']
X = trainData_1[selected_var].copy()
X['intercept'] = [1] * X.shape[0]
LR = sm.Logit(y, X_temp).fit()
summary = LR.summary2()
#print(summary)

# 预测模型
y_pred = LR.predict(X_temp)
roc_auc_score(trainData_1['label'], y_pred)


fpr, tpr, threshold = roc_curve(trainData_1['label'], y_pred)
rocauc = auc(fpr, tpr)#计算AUC
print(rocauc)
plt.plot(fpr, tpr, 'b', label='AUC = %0.2f' % rocauc)#生成ROC曲线
plt.legend(loc='lower right')
plt.plot([0, 1], [0, 1], 'r--')
plt.xlim([0, 1])
plt.ylim([0, 1])
plt.ylabel('真正率')
plt.xlabel('假正率')
plt.show()






joblib.dump(LR, 'filename.pkl')




# 最终结果展现
params = LR.params
pvalues = LR.pvalues
fit_result = pd.concat([params, pvalues], axis=1)
fit_result.columns = ['coef', 'p-value']
#print(fit_result)



pd.set_option('display.width', 1000)  # 设置字符显示宽度
pd.set_option('display.max_rows', None)  # 设置显示最大行
scores = scorecard_functions_V3.Prob2Score(y_pred, 600, 20)
plt.hist(scores, bins=100)
scorecard = pd.DataFrame({'y_pred': y_pred, 'y_real': list(trainData_1['label']), 'score': scores})
ks=scorecard_functions_V3.KS_Train(scorecard, 'score', 'y_real')
#print(ks)

# 也可用sklearn带的函数
roc_auc_score(trainData_1['label'], y_pred)

ROC_AUC(scorecard, 'score','y_real')

f=open("E:/python/result1.csv","w")
#print(scorecard,file=f)
f.close()




#print(var_cutoff)

###计算变量分箱得分
scores={}
for col in var_WOE.keys():
    col2 = str(col) + "_WOE" 
    if col2 not in selected_var:
        continue
    temp={}
    for k,v in var_WOE[col].items():
        col3 = str(col) + "_WOE"
        coe=fit_result.loc[col3,"coef"]
        s=round((coe*v*(-1))*(20/math.log(2)),0)
        temp[k]=s
    scores[col]=temp
print(scores)



for k,v in scores.items():
    print (k,v)





###打印模型变量分箱段
for col in selected_var:
    print(col[0:(len(col)-4)],var_cutoff[(col[0:(len(col)-4)])])
    



#scores_1 = scorecard_functions_V3.Prob2Score(y_pred, 600, 20)




#KS(scorecard, 'score', 'y_real')




# 树模型筛选变量




allData_tree = pd.read_csv('df_train.csv', header=0, encoding='gbk')
allFeatures_tree = list(allData_tree.columns)



'''
# 缺失值填充
# 最后再统一用-1填充
for col in allData_tree.columns.tolist():
    if allData_tree[col].dtype == 'object':
        allData_tree[col].fillna('nulls', inplace=True)
        allData_tree[col].fillna('nulls', inplace=True)
    else:
        allData_tree[col].fillna(-1, inplace=True)
        allData_tree[col].fillna(-1, inplace=True)



# 基于树模型的传参,本节单独调用
# 数值与类别型变量区分
numerical_var_tree = []
for var in allFeatures_tree:
    uniq_vals = allData_tree[var].unique()
    # uniq_vals = list(set(allData_tree[var]))
    if np.nan in uniq_vals:
        uniq_vals.remove(np.nan)
    if len(uniq_vals) >= 10 and isinstance(uniq_vals[0], numbers.Real):
        numerical_var_tree.append(var)

categorical_var_tree = [i for i in allFeatures_tree if i not in numerical_var_tree]



# 因为决策树只能处理数值型和标称型变量，所以转换一下类别变量
'''
因为本文只运用随机森林来判断变量的重要性，而树模型不需要One-Hot编码，
所以只对类型型变量LabelEncoding就可以
'''
for col in categorical_var_tree:
    allData_tree[col] = preprocessing.LabelEncoder().fit_transform(allData_tree[col])



# 随机森林重要性画图
y = allData_tree['label']
x = allData_tree.drop('label', axis=1)
clf = RandomForestClassifier()
clf.fit(x, y)

plt.style.use('seaborn-whitegrid')
importance = clf.feature_importances_
importance = pd.DataFrame(importance, index=x.columns, columns=["Importance"])
importance.sort_values(by='Importance', ascending=True).plot(kind='barh', figsize=(20, len(importance) / 2))




importance_tree = importance.sort_values(by='Importance', ascending=False)
importance_tree.to_csv('importance_tree_sort.csv')

# # 其他模型




# Splitting the Training and Test data sets
X_train = X_temp.loc[0:4300, :]
y_train = y.loc[0:4300]
X_test = X_temp.loc[4300:, :]
y_test = y.loc[4300:]





# 在不同阈值上计算fpr
def plot_roc_curve(y_test, preds):
    fpr, tpr, threshold = metrics.roc_curve(y_test, preds)
    roc_auc = metrics.auc(fpr, tpr)
    plt.title('Receiver Operating Characteristic')
    plt.plot(fpr, tpr, 'b', label='AUC = %0.2f' % roc_auc)
    plt.legend(loc='lower right')
    plt.plot([0, 1], [0, 1], 'r--')
    plt.xlim([-0.01, 1.01])
    plt.ylim([-0.01, 1.01])
    plt.ylabel('True Positive Rate')
    plt.xlabel('False Positive Rate')
    plt.show()





# 返回结果
def fit_ml_algo(algo, X_train, y_train, X_test, cv, model_name):
    # One Pass
    model = algo.fit(X_train, y_train)
    test_pred = model.predict(X_test)
    if (isinstance(algo, (LogisticRegression,
                          KNeighborsClassifier,
                          GaussianNB,
                          DecisionTreeClassifier,
                          RandomForestClassifier,
                          GradientBoostingClassifier))):
        joblib.dump(model, model_name + '.pkl')
        probs = model.predict_proba(X_test)[:, 1]
    else:
        probs = "Not Available"
    acc = round(model.score(X_test, y_test) * 100, 2)
    # CV
    train_pred = model_selection.cross_val_predict(algo,
                                                   X_train,
                                                   y_train,
                                                   cv=cv,
                                                   n_jobs=-1)
    acc_cv = round(metrics.accuracy_score(y_train, train_pred) * 100, 2)
    return train_pred, test_pred, acc, acc_cv, probs





# Logistic Regression - Random Search for Hyperparameters

# Utility function to report best scores
import time


def report(results, n_top=5):
    for i in range(1, n_top + 1):
        candidates = np.flatnonzero(results['rank_test_score'] == i)
        for candidate in candidates:
            print("Model with rank: {0}".format(i))
            print("Mean validation score: {0:.3f} (std: {1:.3f})".format(
                results['mean_test_score'][candidate],
                results['std_test_score'][candidate]))
            print("Parameters: {0}".format(results['params'][candidate]))
            print("")


# Specify parameters and distributions to sample from
param_dist = {'penalty': ['l2', 'l1'],
              'class_weight': [None, 'balanced'],
              'C': np.logspace(-20, 20, 10000),
              'intercept_scaling': np.logspace(-20, 20, 10000)}

# Run Randomized Search
n_iter_search = 10
lrc = LogisticRegression()
random_search = RandomizedSearchCV(lrc,
                                   n_jobs=-1,
                                   param_distributions=param_dist,
                                   n_iter=n_iter_search)

start = time.time()
random_search.fit(X_train, y_train)
print("RandomizedSearchCV took %.2f seconds for %d candidates"
      " parameter settings." % ((time.time() - start), n_iter_search))
report(random_search.cv_results_)



# Logistic Regression
import datetime

start_time = time.time()
train_pred_log, test_pred_log, acc_log, acc_cv_log, probs_log = fit_ml_algo(LogisticRegression(n_jobs=-1),
                                                                            X_train,
                                                                            y_train,
                                                                            X_test,
                                                                            10,
                                                                            'LR_1')
log_time = (time.time() - start_time)
print("Accuracy: %s" % acc_log)
print("Accuracy CV 10-Fold: %s" % acc_cv_log)
print("Running Time: %s" % datetime.timedelta(seconds=log_time))




print(metrics.classification_report(y_train, train_pred_log))
print(metrics.classification_report(y_test, test_pred_log))
plot_roc_curve(y_test, probs_log)




KNeighborsClassifier(n_neighbors=3,
                     n_jobs=-1),




# k-Nearest Neighbors
start_time = time.time()
train_pred_knn, test_pred_knn, acc_knn, acc_cv_knn, probs_knn = fit_ml_algo(KNeighborsClassifier(n_neighbors=3,
                                                                                                 n_jobs=-1),
                                                                            X_train,
                                                                            y_train,
                                                                            X_test,
                                                                            10,
                                                                            'Knn')
knn_time = (time.time() - start_time)
print("Accuracy: %s" % acc_knn)
print("Accuracy CV 10-Fold: %s" % acc_cv_knn)
print("Running Time: %s" % datetime.timedelta(seconds=knn_time))




print(metrics.classification_report(y_train, train_pred_knn))
print(metrics.classification_report(y_test, test_pred_knn))
plot_roc_curve(y_test, probs_knn)




# Gaussian Naive Bayes
start_time = time.time()
train_pred_gaussian, test_pred_gaussian, acc_gaussian, acc_cv_gaussian, probs_gau = fit_ml_algo(GaussianNB(),
                                                                                                X_train,
                                                                                                y_train,
                                                                                                X_test,
                                                                                                10,
                                                                                                'Bayes')
gaussian_time = (time.time() - start_time)
print("Accuracy: %s" % acc_gaussian)
print("Accuracy CV 10-Fold: %s" % acc_cv_gaussian)
print("Running Time: %s" % datetime.timedelta(seconds=gaussian_time))



print(metrics.classification_report(y_train, train_pred_gaussian))
print(metrics.classification_report(y_test, test_pred_gaussian))
plot_roc_curve(y_test, probs_gau)




# Linear SVC
start_time = time.time()
train_pred_svc, test_pred_svc, acc_linear_svc, acc_cv_linear_svc, _ = fit_ml_algo(LinearSVC(),
                                                                                  X_train,
                                                                                  y_train,
                                                                                  X_test,
                                                                                  10,
                                                                                  'SVC')
linear_svc_time = (time.time() - start_time)
print("Accuracy: %s" % acc_linear_svc)
print("Accuracy CV 10-Fold: %s" % acc_cv_linear_svc)
print("Running Time: %s" % datetime.timedelta(seconds=linear_svc_time))




print(metrics.classification_report(y_train, train_pred_svc))
print(metrics.classification_report(y_test, test_pred_svc))



# Stochastic Gradient Descent
start_time = time.time()
train_pred_sgd, test_pred_sgd, acc_sgd, acc_cv_sgd, _ = fit_ml_algo(SGDClassifier(n_jobs=-1),
                                                                    X_train,
                                                                    y_train,
                                                                    X_test,
                                                                    10,
                                                                    'SGD')
sgd_time = (time.time() - start_time)
print("Accuracy: %s" % acc_sgd)
print("Accuracy CV 10-Fold: %s" % acc_cv_sgd)
print("Running Time: %s" % datetime.timedelta(seconds=sgd_time))



print(metrics.classification_report(y_train, train_pred_sgd))
print(metrics.classification_report(y_test, test_pred_sgd))



# Decision Tree Classifier
start_time = time.time()
train_pred_dt, test_pred_dt, acc_dt, acc_cv_dt, probs_dt = fit_ml_algo(DecisionTreeClassifier(),
                                                                       X_train,
                                                                       y_train,
                                                                       X_test,
                                                                       10,
                                                                       'Tree_c')
dt_time = (time.time() - start_time)
print("Accuracy: %s" % acc_dt)
print("Accuracy CV 10-Fold: %s" % acc_cv_dt)
print("Running Time: %s" % datetime.timedelta(seconds=dt_time))



print(metrics.classification_report(y_train, train_pred_dt))
print(metrics.classification_report(y_test, test_pred_dt))
plot_roc_curve(y_test, probs_dt)




# Random Forest Classifier
start_time = time.time()
rfc = RandomForestClassifier(n_estimators=10,
                             min_samples_leaf=2,
                             min_samples_split=17,
                             criterion='gini',
                             max_features=8)
train_pred_rf, test_pred_rf, acc_rf, acc_cv_rf, probs_rf = fit_ml_algo(rfc,
                                                                       X_train,
                                                                       y_train,
                                                                       X_test,
                                                                       10,
                                                                       'RF')
rf_time = (time.time() - start_time)
print("Accuracy: %s" % acc_rf)
print("Accuracy CV 10-Fold: %s" % acc_cv_rf)
print("Running Time: %s" % datetime.timedelta(seconds=rf_time))




print(metrics.classification_report(y_train, train_pred_rf))
print(metrics.classification_report(y_test, test_pred_rf))
plot_roc_curve(y_test, probs_rf)




# Gradient Boosting Trees
start_time = time.time()
train_pred_gbt, test_pred_gbt, acc_gbt, acc_cv_gbt, probs_gbt = fit_ml_algo(GradientBoostingClassifier(),
                                                                            X_train,
                                                                            y_train,
                                                                            X_test,
                                                                            10,
                                                                            'GBDT')
gbt_time = (time.time() - start_time)
print("Accuracy: %s" % acc_gbt)
print("Accuracy CV 10-Fold: %s" % acc_cv_gbt)
print("Running Time: %s" % datetime.timedelta(seconds=gbt_time))




# 综合结果
models = pd.DataFrame({
    'Model': ['KNN', 'Logistic Regression',
              'Random Forest', 'Naive Bayes',
              'Stochastic Gradient Decent', 'Linear SVC',
              'Decision Tree', 'Gradient Boosting Trees'],
    'Score': [
        acc_knn,
        acc_log,
        acc_rf,
        acc_gaussian,
        acc_sgd,
        acc_linear_svc,
        acc_dt,
        acc_gbt
    ]})
models.sort_values(by='Score', ascending=False)



plt.style.use('seaborn-whitegrid')
fig = plt.figure(figsize=(10, 10))

models = [
    'KNN',
    'Logistic Regression',
    'Random Forest',
    'Naive Bayes',
    'Decision Tree',
    'Gradient Boosting Trees'
]
probs = [
    probs_knn,
    probs_log,
    probs_rf,
    probs_gau,
    probs_dt,
    probs_gbt
]
colors = [
    'blue',
    'green',
    'red',
    'cyan',
    'magenta',
    'yellow',
]

plt.title('Receiver Operating Characteristic')
plt.plot([0, 1], [0, 1], 'r--')
plt.xlim([-0.01, 1.01])
plt.ylim([-0.01, 1.01])
plt.ylabel('True Positive Rate')
plt.xlabel('False Positive Rate')


def plot_roc_curves(y_test, prob, model):
    fpr, tpr, threshold = metrics.roc_curve(y_test, prob)
    roc_auc = metrics.auc(fpr, tpr)
    plt.plot(fpr, tpr, 'b', label=model + ' AUC = %0.2f' % roc_auc, color=colors[i])
    plt.legend(loc='lower right')


for i, model in list(enumerate(models)):
    plot_roc_curves(y_test, probs[i], models[i])

plt.show()

'''