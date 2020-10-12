#2.10.01 Computations in R
2 + 3

sqrt(36)

log10(32)

log2(32)

x <- 2 + 3+ 4

abs(5 - 145)

x <- sqrt(625) / 5

x <- x / 10000


#2.10.2 Data structures in R
vec <- c(1, 2, 3, 5, 10)

length(vec)

vec <- 2:15

rep(4, 10)

c(TRUE, FALSE, TRUE, FALSE)

c("PAX6", "ZIC2", "OCT4", "SOX2")

vec[c(5, 6)]

myvec=1:5
# the length of the logical vector 
# should be equal to length(myvec) 
myvec[c(TRUE,TRUE,FALSE,FALSE,FALSE)] 
myvec[c(TRUE,FALSE,FALSE,FALSE,TRUE)]

myvec > 3
myvec == 4
myvec <= 2
myvec != 4

myvec > 2

matrix(1:5, 5, 3)

mat <- matrix(1:5, 5, 3, byrow = TRUE)

mat[1:3, 1:3]

mat[4:5, ]

res <- mat[, c(1, 2)]
class(res)

res <- mat[, 1]
class(res)

dat <- data.frame(col1 = 1:5, col2 = c("a", "b", "c", "d", "e"), col3 = 6:10)

dat[1:2, 1:2]

dat[4:5, ]

dat[, c("col2", "col3")]

dat$col2

dat$col1 > 3

dat$col1 >= 3

as.matrix(dat)

li <- list(a = 3, b = 5, c = 6, d = 7)

li$a

li$d

li[1]

li[4]

fac <- factor(c("a", "b", "c", "d", "e"))

fa <- c("a", "b", "c", "d", "e")
as.factor(fa)

as.character(fac)


#2.10.3 Reading in and writing data out in R