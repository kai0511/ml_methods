library(NMF)
library(data.table)
library(reshape2)

source("matSampling.R")

DEFAULT.NA = 0  # The default value for NA
RATIO = 0.005  # The precent of date used as test data
THRESHOLD = 0.5  # The threshold for labaling prediction as 1 or 0

path <- '../data/MEDI_01212013_HPS_0.csv'
data <- read.csv(path, sep = ',', header = TRUE)
DDA <- data[c(1,3)]
DDA$wght <- rep(1, nrow(DDA))

tbl <- acast(DDA, RXCUI_IN~ICD9, value.var="wght", fill = NA)
mat <- as(tbl, "matrix")
splited.data <- matrixSampling(mat, RATIO)
training.set <- splited.data[[1]]
test.set <- splited.data[[2]]

# Set default value for test and train matrix
if (!is.na(DEFAULT.NA)) {
    training.set[which(is.na(training.set))] <- DEFAULT.NA
    training.set[which(is.null(training.set))] <- DEFAULT.NA
}

# Generate weight matrix
w <- matrix(1, nrow(mat), ncol(mat))
w[which(is.na(test.set))] <- 0

reslt <- nmf(training.set, 3, 'ls-nmf', weight = w)
# reslt <- nmf(mat, 4, "lee")
estimated.mat <- fitted(reslt)

estimated.mat[which(is.na(test.set))] <- 0
pred <- estimated.mat
pred[which(pred >= THRESHOLD)] = 1
pred[which(pred < THRESHOLD)] = 0
tstm <- as(estimated.mat, 'matrix')
sampleSize <- length(which(!is.na(test.set)))
test.set[which(is.na(test.set))] <- 0
mse <- abs(tstm - pred)
MRSE = sum(apply(mse, 1, sum))/sampleSize
