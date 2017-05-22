library(fdrtool)

#****************************************************************************
# read training and test set
setwd('/exeh/exe3/zhaok/GE_result/')
trainset <- read.csv('Cmap_differential_expression_antipsycho_train_part1.csv', header=F)
testset <- read.csv('Cmap_differential_expression_antipsycho_test_part1.csv', header=F)

# compute lfdr for vec, and transfer it to positive value for the computed lfdr
generate_prob <- function(vec){
    fdr <- fdrtool(vec, statistic = 'normal', plot = FALSE, verbose = FALSE)
    return(1 - fdr$lfdr)
}

# convert to dataframe, otherwise R cannot recognize the expr levels as numeric variables
cmap <- t(data.frame(degList$tstat))

# generate_prob to each element and combine the result for each element
ldfr_trainset <- do.call(rbind, apply(as.matrix(trainset[,c(-1,-2)]), 1, generate_prob)) 
ldfr_testset <- do.call(rbind, apply(as.matrix(testset[,c(-1,-2)]), 1, generate_prob))


# *******************************************************
# begin training with ldfr_trainset
# *******************************************************

# compute prior probability of positive observations
positive_idx <- (trainset[[2]] == 1)
observation_num <- length(positive_idx)
prior_pos <- sum(positive_idx) / observation_num

# compute posterior probability for positive and negative observations
posterior_pos <- apply(ldfr_trainset[positive_idx,], 2, mean)
posterior_neg <- apply(ldfr_trainset[-positive_idx,], 2, mean)

# *******************************************************
# testing on ldfr_testset
# *******************************************************

#compute positive rate for single observation
compute_pos_rate <- function(obs){
    obs[obs < 0.1] <- 0  # we consider features with positive-fdr greater than 0.1
    valid_feature <- (obs < 0.1)
    
    weighted_posterior_pos <- posterior_pos * obs 
    weighted_posterior_neg <- posterior_neg * obs
    
    cumpod_pos <- cumpod(weighted_posterior_pos[valid_feature])
    cumpod_neg <- cumpod(weighted_posterior_neg[valid_feature])
    
    prob <- (cumpod_pos*prior_pos)/(cumpod_neg*(1-prior_pos)+cumpod_pos*prior_pos)
    return(prob)
}