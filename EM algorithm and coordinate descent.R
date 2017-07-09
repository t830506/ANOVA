library(bootstrap)
library(boot)
library("fields")

#ex1--------------------------------------------------------------------------------------------------------------------------
a1 <- read.table("/Users/shawn/Desktop/hw3_problem1.txt", header=FALSE, sep=",")
x1 <- unlist(read.table("/Users/shawn/Desktop/hw3_problem1.txt", header=FALSE, sep=","))

myfunction <- function(para, input){
  -sum(log((exp(-para[1])*para[1]^input/factorial(input))*para[2]+(1-para[2])*(input == 0)))
}
opt <- optim(c(3.2, 0.5), fn=myfunction, lower = c(0,0), upper = c(Inf, 1), input = a1, hessian = T)
opt$par # (3.8680677, 0.8272886)

#---------------------------------------------------------
efun <- function(para, input){
  la <- para[1]
  p <- para[2]
  p*(exp(-la)*la^input/(factorial(input)))/(p*(exp(-la)*la^input/factorial(input)) + (1-p)*(input == 0))
}
input <- a1
mfun <- function(para, r){
  la <- para[1]
  p <- para[2]
  -sum(log((exp(-la)*la^input/(factorial(input)))^r * (input == 0)^(1-r) * p^r * (1-p)^(1-r)))
}
initial = c(mean(a1$V1), 0.5)
gamma = efun(initial, a1)
mle_em = optim(initial, fn = mfun, lower=c(0,10^(-9)), upper=c(Inf, 1-10^(-9)), r=gamma, hessian = F)
error= abs(mle_em$par - initial)
n=1
while(error[1] >= 10^-5 | error[2] >= 10^-5){
  mle_em1 = mle_em$par
  gamma = efun(mle_em1, a1)
  mle_em = optim(initial, fn = mfun, lower=c(0,10^(-9)), upper=c(Inf, 1-10^(-9)), r=gamma, hessian = F)
  error = abs(mle_em$par - mle_em1) 
  n = n+1
}
mle_em$par # (3.8679227 0.8272895)

#---------------------------------------------------------
hist(a1$V1,probability = T)
x2 <- sort(x1)
pmf = function(x1,mle_em){
  la = mle_em[1]
  p      = mle_em[2]
  p*dpois(x1,la)+(1-p)*(x1==0)
}
lines(x2, pmf(x2, mle_em$par), col = 2, lwd = 2)

#ex2------------------------------------------------------------------------------------------------------------------
mydata <- read.table("/Users/shawn/Desktop/hw3_problem2.txt", header=FALSE, sep=",")

trim <- 0 # 0.1, 0.05
x <- mydata$V1
u <- mean(x, trim = trim)
s <- sqrt(var(x))
s.hat <- sqrt(var(x)/length(x))

f1 = function(x){
  c(mean(x, trim = trim), median(x), var(x))
}

B = 10000
x.B = matrix(0,B,3)

for (i in 1:B){
  x1 = sample(x,size=200,prob=rep(1,200),replace=T)
  x.B[i,] = f1(x1)
}

#---------------------------------------------------------
### j=1: mean
#(1-alpha) bootstrap CI
tmp = x.B[,1]
s.BT = sd(tmp)
alpha=0.05
dd = quantile(tmp-u,probs=c(alpha/2,1-alpha/2))
u- rev(dd)
# 0 (-0.4954436, -0.0310249)
# 0.05 (-0.29216827, 0.05893311)
# 0.1 (-0.3030417, 0.0479672)

#---------------------------------------------------------
bias <- mean(tmp - u) # -0.004219598
variance <- var(tmp) # 0.008023818


#ex3------------------------------------------------------------------------------------------------------------------
df <- read.table("/Users/shawn/Desktop/hw3_problem3.csv", header=TRUE, sep=",")

b = matrix(0,200,200)
for (i in 1:200){
  for (j in 1:200){
    b[i,j] = df[(i-1)*200+j,3]
  } }
image.plot(b,col = grey(seq(0, 1, length = 256)))

cost_fun <- function(t, ti, la, i, n, y) {
  ti[i] <- t
  ti_matrix <- matrix(ti, nrow = n, byrow = T)
  p1 <- sum((ti_matrix[, -1]-ti_matrix[, -n])^2)
  p2 <- sum((ti_matrix[-1, ]-ti_matrix[-n, ])^2)
  sum((y-ti)^2) + la*(p1+p2)
}
f <- function(n, lambda) {
  ti_hat <- rep(0.1, n^2)
  df1 <- df[which(df$i <= n & df$j <= n), ]
  y <- df1$Y
  
  error <- 1
  c <- 0
  while(error >= 10^(-4)) {
    ti_hat1 <- ti_hat
    for(i in 1:n^2) {
      ti_hat[i] <- optim(0.1, cost_fun, lower = 0, upper = 1, ti = ti_hat, la = lambda, i = i, n = n, y = y, hessian = F)$par
    }
    error <- norm(ti_hat - ti_hat1, "2") 
    c <- c+1
  }
  list(c, ti_hat)
}
a1 <- f(200, 1)
b2 <- t(matrix(a1[[2]], 200, 200))
image.plot(b2, col = grey(seq(0, 1, length = 256)))

