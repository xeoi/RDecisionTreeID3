#if your folder has structure
#-DecisionTree
#--demo.R
#--data.txt
#--DecisionTreeDunction.R

setwd("DecisionTree/")
data = data.matrix(read.table("data.txt"))
label=data[,1]
x=data[,-1]
tree = build_tree(x,label)
use_tree(tree,x[39,])
