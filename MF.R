library(NMF)
library(data.table)

# function: read data from a file separated by specific separator
# the formate of file: columnNum. rowNum. value
fileReader <- function(path, names, colums, separator="\t") {    
    rating <- fread(path, sep = separator)[, colums, with=FALSE]
    setnames(rating, names)
    return(rating)
}

# covert dataframe to matrix
# the first column spcifys the row number
# the first column spcifys the column number
df2Mat <- function(df, na.d.value = 0){
    tab <- xtabs(df[[3]] ~ df[[1]] + df[[2]] , data = df)
    if (na.d.value != 0) 
        tab[tab == 0] <- na.d.value   ## this is a trick used in the package NMF
    m <- as.matrix(tab)
    return(as(m, "matrix"))
}

# create weighting matrix with 0 for all NAs in m 
generateWeightMat <- function(m, na.d.value = 0){
    l <- length(m)
    na.m <- which(m == na.d.value)
    w <- matrix(1, nrow(m), ncol(m))
    w[na.m] <- 0
    return(w)
}

# initial variables 
setwd("E:/Biostatistics Research/R Code")
trainingData  <- "u1.base" 
testData  <- "u1.test" 
names <- c("UserId", "MovieId", "Rating")  # names for each column
colums <- c(1, 2, 3)  # the colums will be loaded

# read training and test file
trndf <- fileReader(trainingData, names, colums)
tstdf <- fileReader(testData, names, colums)

trnm <- df2Mat(trndf)
wght <- generateWeightMat(trnm) # get weight matrix

tstm <- df2Mat(tstdf)
sampleNum <- nrow(tstdf)   # the number of test sample

# start matrix decomposition
reslt <- nmf(trnm, 3, 'ls-nmf', weight = wght)
estimated.m <- fitted(reslt) 

# get corresponding predicted matrix
# pred <- V.hat[as(rownames(tstm), "numeric"), as(colnames(tstm), "numeric")]
pred <- estimated.m[as(rownames(tstm), "numeric"),as(colnames(tstm), "numeric")]
pred[which(tstm == 0)] <- 0   # set NA value equal 0

tstm <- as(tstm, 'matrix')
mse <- (tstm - pred)*(tstm - pred)
MRSE = sum(apply(mse, 1, sum))/sampleNum

userBiase <- basis(res) # user feature matrix matrix
itemBiase <- coef(res) # movie feature matrix
