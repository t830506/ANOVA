###ex1
df <- read.table("C:/Users/stat_pc/Desktop/midterm.txt", header = TRUE,  stringsAsFactors = F)

y1 <- df$LATITUDE
y2 <- df$LONGITUDE
A <- df$CONTROL

Y <- cbind(y1,y2)
fit <- manova(Y ~ factor(A))
summary(fit, test = "Pillai")

###ex2
df_pub <- df[df$CONTROL == 1, ]
df_non <- df[df$CONTROL == 2, ]

f1 <- function(df){
  x <- df[,7:8]
  mu <- cbind(mean(x$LATITUDE), mean(x$LONGITUDE))
  sd <- cov(x)
  stat <- rbind(mu, sd)
  return(stat)
}
stat_pub <- f1(df_pub)
stat_non <- f1(df_non)
mu1 <- stat_pub[1,]
mu2 <- stat_non[1,]
s1 <- stat_pub[2:3,]
s2 <- stat_non[2:3,]
n1 <- 68
n2 <- 11
sp <- ((n1-1)*s1 + (n2-1)*s2)/(n1+n2-2)
Y <- (mu1-mu2)/sqrt(1/n1 + 1/n2)
T_2 <- t(Y) %*% solve(sp) %*% Y
p <- 2
F <- T_2/(n1+n2-2)*((n1+n2-2)-p+1)/p
F
qf(0.95,p,(n1+n2-2)-p+1)

library("psych")
invS = solve(1/n1*s1+1/n2*s2)
v = (p+p^2) / (1/n1*(tr((1/n1*s1%*%invS)%*%(1/n1*s1%*%invS)) + (tr(1/n1*s1%*%invS))^2)
           + 1/n2*(tr((1/n2*s2%*%invS)%*%(1/n2*s2%*%invS)) + (tr(1/n2*s2%*%invS))^2))

T_2_none = t(mu1 - mu2) %*% solve(1/n1*s1+1/n2*s2) %*% (mu1 - mu2)
F = T_2_none / v * (v-p+1) / p
F
qf(0.95,p,v-p+1)

### ex3
c <- sqrt((n1+n2-2)*p/(n1+n2-p-1)*qf(0.95,p,(n1+n2-2)-p+1))
lambda <- svd(sp)$d
e1 <- svd(sp)$u[,1]
e2 <- svd(sp)$u[,2]
r1 <- sqrt(lambda[1])*sqrt((1/n1 + 1/n2) * c^2)
r2 <- sqrt(lambda[2])*sqrt((1/n1 + 1/n2) * c^2)
o <- mu1 - mu2


sp_1 <- s1/n1+s2/n2
c_1 <- sqrt(v*p/(v-p+1)*qf(0.95,p,v-p+1))
lambda_1 <- svd(sp_1)$d
e_1 <- svd(sp_1)$u[,1]
e_2 <- svd(sp_1)$u[,2]
r_1 <- sqrt(lambda_1[1])*sqrt((1/n1 + 1/n2) * c_1^2)
r_2 <- sqrt(lambda_1[2])*sqrt((1/n1 + 1/n2) * c_1^2)
o <- mu1 - mu2

library("ellipse")
plot(ellipse((1/n1+1/n2)*sp, centre = o,level = 0.05, t = c),type = "l")
plot(ellipse(sp_1, centre = o, level = 0.05, t = c_1),type = "l")

### ex4
CI_11 <- c(mu1[1]-mu2[1]-c*sqrt((1/n1+1/n2)*sp[1,1]), mu1[1]-mu2[1]+c*sqrt((1/n1+1/n2)*sp[1,1]))
CI_12 <- c(mu1[2]-mu2[2]-c*sqrt((1/n1+1/n2)*sp[2,2]), mu1[2]-mu2[2]+c*sqrt((1/n1+1/n2)*sp[2,2]))

CI_21 <- c(mu1[1]-mu2[1]-c_1*sqrt(sp_1[1,1]), mu1[1]-mu2[1]+c_1*sqrt(sp_1[1,1]))
CI_22 <- c(mu1[2]-mu2[2]-c_1*sqrt(sp_1[2,2]), mu1[2]-mu2[2]+c_1*sqrt(sp_1[2,2]))
