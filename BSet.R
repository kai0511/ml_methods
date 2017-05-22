# @algorithm: Bayesian Sets (Ghahramani and Heller, 2005)
# @param mat: binary spare matrix (feature * entries) 
# @c: Dirichlet concentration parameter (usually, c = 2)
# @query: a vector of index of entries
# return: top ranked entries based on query
# @author: ZHAO, Kai

Bset <- function(mat, c, query){
    size <- dim(mat)
    m <- apply(mat, 1, sum) / size[2]
    alpha = c * m
    beta <- c * (1 - m)
    
    len <- length(query)
    cMat <- apply(mat[, query], 1, sum)
    w <- log(1 + cMat / alpha) - log(1 + (len - cMat) / beta)
    score <- t(w) %*% mat
    return(score)
}