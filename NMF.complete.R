library(NMF)
library(data.table)
library(reshape2)

DEFAULT.NA = 0  # The default value for NA
THRESHOLD = 0.5  # The threshold for labaling prediction as 1 or 0

path <- '../data/MEDI_01212013_HPS_0.csv'
data <- read.csv(path, sep = ',', header = TRUE)
DDA <- data[c(1,3)]
DDA$wght <- rep(1, nrow(DDA))

tbl <- acast(DDA, RXCUI_IN~ICD9, value.var="wght", fill = 0)
mat <- as(tbl, "matrix")

# Set default value for test and train matrix
if (!is.na(DEFAULT.NA)) {
    mat[which(is.na(training.set))] <- DEFAULT.NA
}

reslt <- nmf(mat, 4, "lee")
estimated.mat <- fitted(reslt)
pred <- estimated.mat
pred[which(pred >= 0.5)] <- 1
pred[which(pred < 0.5)] <- 0
diff <- pred - mat
diseases <- colnames(diff)
drugs <- rownames(diff)

dis <- c()
drg <- c()
for (i in diseases){
    for (j in drugs){
        if (diff[j,i] == 1){
           dis <- c(dis, i)
           drg <- c(drg, j)
        }        
    }
}




