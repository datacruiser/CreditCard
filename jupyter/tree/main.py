# -*- coding:utf-8 -*- 
__author__ = "lld"
import c45tree
import scorecard_functions
import treePlotter
import pandas as pd
from sklearn.model_selection import train_test_split

inputfile = 'D:/jinque_data.xlsx'    #这里输入你个人的文件路径
data = pd.read_excel(inputfile, 'Sheet1')
col_name = pd.read_excel(inputfile, 'Sheet3')["name"].tolist()
col_name2 = ["gender","is_first_tier_city","is_loan","is_amount_overdue","is_last_apply_overdue",
"is_first_apply","is_approve_self","is_refuse_self","gjj_state_is_normol","system"]
dataSet = {}
for i in range(len(col_name)):
    coff = scorecard_functions.ChiMerge(data, col_name[i], "label", max_interval=3, minBinPcnt=0)
    dataSet[col_name[i]] = coff

for name in col_name2:
    dataSet[name] = data[name]
dataSet["label"] = data["label"]
col_name.extend(col_name2)
col_name.append("label")
newdata = pd.DataFrame(dataSet,columns=col_name)
trainSet, testSet = train_test_split(newdata.as_matrix(), random_state=0, test_size=0.2)
testSet = [list(r[0 : 105]) for r in list(testSet)]
labels = newdata.columns.values.tolist()
labels_tmp = labels[:]  # 拷贝，createTree会改变labels
desicionTree = c45tree.createTree(trainSet.tolist(), labels_tmp)
c45tree.storeTree(desicionTree, 'classifierStorage.txt')
#desicionTree = c45tree.grabTree('classifierStorage.txt')
print('desicionTree:\n', desicionTree)
treePlotter.createPlot(desicionTree)
print('classifyResult:\n', c45tree.classifyAll(desicionTree, labels, testSet))
