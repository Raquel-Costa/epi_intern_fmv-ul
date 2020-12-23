# Livro <https://compgenomr.github.io/book/> e github <https://github.com/compgenomr/compGenomRData>

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
devtools::install_github("compgenomr/compGenomRData")
cpgFilePath=system.file("extdata",
                        "CpGi.table.hg18.txt",
                        package="compGenomRData")
library(data.table)
cpgi <- fread(cpgFilePath)

head(cpgi)

cpgtFilePath=system.file("extdata",
                         "CpGi.table.hg18.txt",
                         package="compGenomRData")
cpgtFilePath
cpgiSepComma=read.table(cpgtFilePath,header=TRUE,sep=",")
head(cpgiSepComma)

cpgiHF=read.table("intro2R_data/data/CpGi.table.hg18.txt",
                  header=FALSE,sep="\t",
                  stringsAsFactors=FALSE)
cpgi[1:10,]

cpgFilePath=system.file("extdata",
                        "CpGi.table.hg18.txt", 
                        package="compGenomRData")
cpg <- fread(cpgFilePath, header = FALSE)
head(cpg)

my.cpgi.file.txt <- cpgi
file="~/my.cpgi.file.txt"

cpgi <- fread(cpgFilePath, quote=FALSE, sep="\t")
my.cpgi.file2.txt <- (cpgi)

cpgi[1:10,]

cpgi[, 1:3]

cpgi[chrom == "chr1"]

df1Path = system.file("extdata", "rn4.refseq.bed", package="compGenomRData") #nao existe esta base de dados no package
df1 <- fread(df1Path)


#2.10.4 Plotting in R
set.seed(1001)
x1=1:100+rnorm(100,mean=0,sd=15)
y1=1:100

plot(x1, y1)

plot(x1, y1, main = "title")

plot(x1, y1, 
     main = "title",
     xlab = "xlab",
     ylab = "ylab")

mtext(side=3,text="hi there")

mtext(side=2,text="hi there")

mtext(side=3,text=paste("..."))

cor(x1, y1)

mtext(side=3, text=paste(cor(x1, y1)))

plot(x1, y1, 
     main = "title",
     xlab = "xlab",
     ylab = "ylab",
     col = "red",
     pch=19)

plot(x1, y1, 
     main = "title",
     xlab = "xlab",
     ylab = "ylab",
     col = "red",
     pch=18)

hist(x1,
     main = "title", 
     xlab = "xlab",
     ylab = "ylab",
     col = "blue")

boxplot(y1)

boxplot(x1, y1)

boxplot(x1, y1,
        horizontal = TRUE)

par(mfrow=c(2,1))
boxplot(y1)
hist(x1)

par(mfrow=c(1,2))
boxplot(y1)
hist(x1)

x2=1:1000+rnorm(1000,mean=0,sd=200)
y2=1:1000
plot(x2,y2,pch=19,col="blue")
smoothScatter(x2,y2,
              colramp=colorRampPalette(c("white","blue",
                                         "green","yellow","red")))
smoothScatter(x2,y2, colramp=heat.colors)


#2.10.5 Functions and control structures (for, if/else, etc.)

cpgtFilePath=system.file("extdata",
                         "CpGi.table.hg18.txt",
                         package="compGenomRData")
cpgi=read.table(cpgtFilePath,header=TRUE,sep="\t")
head(cpgi)

hist(cpgi$perGc)

boxplot(cpgi$perGc)

if (cpgi$perGc < 60) {
        result="low"
        cat("low")
} else if (cpgi$perGc > 75) {
        
        result="high"
        cat("high")
} else {
        result="medium"
}
result

GCclass<-function(my.gc){
        
        if (my.gc < 60) {
                result="low"
                cat("low")
        } else if (my.gc > 75) {
                
                result="high"
                cat("high")
        } else {
                result="medium"
                cat("medium")
        }
        
        return(result)
}
GCclass(10) # should return "low"
GCclass(90) # should return "high"
GCclass(65) # should return "medium"

gcValues=c(10,50,70,65,90)
for( i in gcValues){
        GCclass(i)
}

vec=c(1,2,4,5)
power2=function(x){ return(x^2)  }
lapply(vec, power2)

sapply(vec, power2)

library(dplyr)
cpgi <- cpgi %>% 
        mutate(class = "")
cpgi$class[cpgi$perGc < 60] = "low"
cpgi$class[cpgi$perGc > 75] = "high"
cpgi$class[cpgi$perGc > 60 & cpgi$perGc < 75] = "medium"               
