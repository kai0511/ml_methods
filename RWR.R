# function: Random Walk with Restart
# @param mat: normalized adjacency matrix
# @param c: degree of preference,and 1 - c is the restart possibility
# @param prefVector: n * 1 starting vector for the i-th node, the i-th element 1 and other 0
# return: relevant score vector (n * 1)to particular node
# @author: ZHAO, Kai

rwr <- function(mat, prefVector, c = 0.5, MaxIterNum = 10000, threshold = 1e-09){
    size <- dim(mat)
    r <- prefVector
    
    if (size[1] != size[2]){
        stop("Inconsistent adjacent matrix.")
    }
    
    for(i in c(1:MaxIterNum)){
        previousPrefVector = r
        
        r <- c * mat %*% r - (1 - c) * prefVector
        
        if (sum(abs(previousPrefVector - r)) <= threshold){
            break
        }
    }    
    return(r)    
}

