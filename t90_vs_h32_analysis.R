file_path <- 'C:/Users/Dell/My Files/Rfiles/project/batse_data.csv'
data <- read.table(file_path,header=TRUE,sep=",")
head(data)
attach(data)
H32 = F3/F2
x <- cbind(log(t90),log(H32))
x
plot(x=log(t90),y=log(H32))
x[is.infinite(x)] <- NA
y <- na.omit(x)
y
dim(y)
library(mixsmsn)
library("sn")
library("stats")
AIC<- numeric()
BIC <- numeric()
t<- y

for (n in 2:8)
{
  
  Skewnorm.analysis <- smsn.mmix(t,nu=3,g=n,get.init=TRUE,group=TRUE,calc.im=FALSE,family="Normal")
  aic <- Skewnorm.analysis$aic
  bic <- Skewnorm.analysis$bic
  
  AIC <- append(AIC,aic)
  BIC<- append(BIC,bic)
}
#skew-normal
for (n in 2:5)
{ Skewnorm.analysis <- smsn.mmix(t,nu=3,g=n,get.init=TRUE,group=TRUE,calc.im=FALSE,family="Skew.normal")
aic <- Skewnorm.analysis$aic
bic <- Skewnorm.analysis$bic
AIC <- append(AIC,aic)
BIC<- append(BIC,bic)
}
#t
for (n in 2:5)
{ Skewnorm.analysis <- smsn.mmix(t,nu=6,g=n,get.init=TRUE,group=TRUE,calc.im=FALSE,family="t")
aic <- Skewnorm.analysis$aic
bic <- Skewnorm.analysis$bic
AIC <- append(AIC,aic)
BIC<- append(BIC,bic)
}
#skewed-t
for (n in 2:5)
{ Skewnorm.analysis <- smsn.mmix(t,nu=6,g=n,get.init=TRUE,group=TRUE,calc.im=FALSE,family="Skew.t")
aic <- Skewnorm.analysis$aic
bic <- Skewnorm.analysis$bic
AIC <- append(AIC,aic)
BIC<- append(BIC,bic)
}
X<- c("2G","3G","4G","5G","6G","7G","8G","2SN","3SN","4SN","5SN","2T","3T","4T","5T","2ST","3ST","4ST","5ST")
AIC_min <- min(AIC)
BIC_min <- min(BIC)
diff_AIC<- AIC-AIC_min
diff_BIC <- BIC- BIC_min
# Load the ggplot2 package
library(ggplot2)
df <- data.frame(cbind(X,AIC_min,BIC_min))

order_levels <- c("2G", "3G", "4G", "5G", "6G", "7G", "8G", "2SN", "3SN", "4SN", "5SN", "2T", "3T", "4T", "5T", "2ST", "3ST", "4ST", "5ST")

df$X <- factor(df$X, levels = order_levels)

p1 <- ggplot(df, aes(x = X, y = diff_AIC, group = 1)) +
  geom_line() +
  labs(title = "Line Plot of AIC vs distributions",
       x = "Distributions",
       y = "AIC")
p2 <- ggplot(df, aes(x = X, y = diff_BIC, group = 1)) +
  geom_line() +
  labs(title = "Line Plot of BIC vs distributions",
       x = "Distributions",
       y = "BIC")

p1
p2
