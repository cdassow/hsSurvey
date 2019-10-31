#Example
x <- "dataset"
typeof(x)
attributes(x)
y <- 1:10
y
typeof(y)
length(y)
z <- as.numeric(y)
z
typeof(z)
vector()# an empty 'logical' default vector
vector("character",length = 5)
#a vector of mode 'charac" with 5 elements
character(5)#same but using constructor
numeric(5)#numeric vector with 5 elements
logical(5)#logical vec with 5 elements

x <- c(1, 2, 3)
x1 <- c(1L, 2L, 3L)
y <- c(TRUE, TRUE, FALSE, FALSE)
z <- c("Sarah", "Tracy", "Jon")

typeof(z)
length(z)
class(z)
str(z)

z <- c(z, "Annette")
z
z<- c("Greg",z)
z


#Vectors from a seqence of numbers
series <- 1:10
seq(10)
seq(from = 1, to = 10, by = 0.1)

#Missing data
x<- c(0.5, NA, 0.7)
x <- c(TRUE, FALSE, NA)
x <- c("a", NA, "c", "d", "e")
x <- c(1+5i, 2-3i, NA)

x <- c("a", NA, "c", "d", NA)
y <- c("a", "b", "c", "d", "e")
is.na(x)
is.na(y)
anyNA(x)
anyNA(y)
1/0
0/0

xx <- c(1.7, "a")
xx <- c(TRUE, 2)
xx <- c("a", TRUE)

as.numeric("1")
as.character(1:2)

length(1:10)

nchar("Software Carpentry")

m <- matrix(nrow = 2, ncol = 2)
m
dim(m)

m <- matrix(c(1:3))
class(m)

typeof(m)

FOURS <- matrix(
  c(4, 4, 4, 4),
  nrow = 2,
  ncol = 2)

x <- list(1, "a", TRUE, 1+4i)
x

x <- vector("list", length = 5) # empty list
length(x)

x[[1]]

x <- 1:10
x <- as.list(x)
length(x)

xlist <- list(a = "Karthik Ram", b = 1:10, data = head(iris))
xlist

dat <- data.frame(id = letters[1:10], x = 1:10, y = 11:20)
dat

is.list(dat)

class(dat)

dat[1, 3]

dat[["y"]]

dat$y

str(iris)
