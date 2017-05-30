# This file include four functions to build and use decision tree. 
# This is implement of ID3 algorithm.

# Created by Xukun Li at 09/14/16 based on matlab code on this website 
# https://alliance.seas.upenn.edu/~cis520/wiki/#index.php?n=Lectures.DecisionTrees


# greater or equal, go to left



build_tree <- function(x,label){
	# Builds a decision tree to construct the tree based on training data
	
	# x is sample, label is the class for sample
	
	# return value is the root of tree save the value of best_feature, 
	# best_splitvalue, left child and right child

	freq=table(label)
	if(length(freq)==1||dim(unique(data.frame(x)))[1]==1){
		target=names(freq[freq==max(freq)])
		return( list(targets=target,left=NULL,right=NULL))
	}
	
	H=ent(label)
	
	best_ig = -Inf
	best_feature = 0
	best_val = 0
	for (i in 1:ncol(x)){
		feat=x[,i]
		vals = unique(feat)
		if(length(vals)==1){
		  next
		}
		splits = 0.5*(vals[1:length(vals)-1]+vals[2:length(vals)])
		
		bin_mat = 1*(matrix(rep(feat,length(splits)),ncol=length(splits))<matrix(rep(splits,
												each = length(feat)),ncol=length(splits)))
		if(is.vector(bin_mat)){
			bin_mat=matrix(bin_mat,ncol=1)
		}
		H_cond = rep(0,ncol(bin_mat))
		for(j in 1:ncol(bin_mat)){
			H_cond[j]=cond_ent(label,bin_mat[,j])
		}
		IG = H - H_cond
		
		val=max(IG)
		ind=which(IG==max(IG))
		if( val>best_ig){
			best_ig=max(IG)
			best_feature=i
			best_val=splits[ind]
		}
	}
	feat=x[,best_feature]
	yes=feat>=best_val
	no=feat<best_val
	left=build_tree(x[yes,],label[yes])
	right=build_tree(x[no,],label[no])
	
	return(list(best_ig=best_ig,best_feature=best_feature,best_val=best_val,left=left,right=right))
}


use_tree<-function(tree, newx){
	# this function is to predict given a new sample
	
	# tree is the Decision Tree constructed by build_tree, 
	# newx is the new sample, sample size should be one
	
	# return value is class label for this single new sample
  if(is.null(tree$left)){
    return(tree$targets)
  }
  
  newx=matrix(newx,nrow=1)
  if(newx[,tree$best_feature]<tree$best_val){
    result = return(use_tree(tree$right,newx))
  }else if(newx[,tree$best_feature]>=tree$best_val){
    result = return(use_tree(tree$left,newx))
  }
  return(result)
  
}

ent<-function(y){
	# Calculate entropy 
	# y is the class label for samples
	
	freq=table(y)
	Pnode=NULL
	for(i in 1:length(freq)){
		Pnode[i]=freq[i]/length(y)
	}
	Inode = -sum(Pnode%*%log(Pnode,base=2))
	return (Inode)
}

cond_ent<-function(y,x){
	# calculate condition entropy
	# y is all the class label, x is binary data decide 
	# which one go left, which one go right.
	
	result=0
	
	freq=table(x)
	
	for(i in 1:2){
		H = ent(y[x==names(freq[i])])
		prob=freq[i]/length(y)
		result=result+prob*H
	}
	return(result)
	
}