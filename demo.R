setwd("/Users/lixukun/Desktop/LearnDecisionTree/")
data = data.matrix(read.table("data.txt"))
label=data[,1]
x=data[,-1]
tree = build_tree(x,label)
use_tree(tree,x[39,])
