library(Rcpp)
library(data.table)


sourceCpp("dtcreate.cpp")



make_data_table  <- function(nbrows, nbcols) {
    data.table::setalloccol(partial_make_data_table(nbrows, nbcols))
}


## example of usage:

x  <- make_data_table(1e8, 10)


## and it seems correct:

print(x)
str(x)

x[1,1] <- 100
setkey(x, col_0)

y <- x[col_0 %% 10000L == 0]
