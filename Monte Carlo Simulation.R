
# INSTALL PACKAGES #

library("readxl")
library("foreign")
library("plm")
library("haven")
install.packages(c("fastDummies", "recipes"))
library("dplyr")
library("tidyverse")
library("lifecycle")
library("stargazer")
library("lmtest")
install.packages("lfe")
install.packages("broom")
install.packages("devtools")
install.packages("bookdown")
install.packages("effects")
install.packages("xlsx")
install.packages("writexl")
install.packages("Rlab")
install.packages("sm")
install.packages("ggplot2")
install.packages("ggthemes")
library("grid")

mydata <- read_dta("D:\\Research_module\\MonthlyPanel.dta")

# TABLE 3 The number of car thefts by block #

apr_july_n <- mean(subset(mydata,institu1==0 & (mes>=4 & mes <=7))$totrob)
apr_july_s <- mean(subset(mydata,institu1==1 & (mes>=4 & mes <=7))$totrob)
apr_july_d <- apr_july_n - apr_july_s


aug_dec_n <- mean(subset(mydata,institu1==0 & (mes>=8 & mes<=12))$totrob)
aug_dec_s <- mean(subset(mydata,institu1==1 & (mes>=8 & mes<=12))$totrob)
aug_dec_d <- aug_dec_n - aug_dec_s
n_n <- aug_dec_n - apr_july_n
s_s <- aug_dec_s - apr_july_s
d <- s_s - n_n

table1 <- data.frame(rbind(c(apr_july_s, apr_july_n, apr_july_d),
                           c(aug_dec_s, aug_dec_n, aug_dec_d),
                           c(s_s, n_n, d)))
colnames(table1) <- c("Same Block","Not on Same Block","Difference")
rownames(table1) <- c("April-July","August-December","Difference")


# Creating Time dummy variables

mydata$inst3_1 <- mydata$institu3 - mydata$institu1
mydata$month5<- ifelse(mydata$mes==5,1,0)
mydata$month6<- ifelse(mydata$mes==6,1,0)
mydata$month7<- ifelse(mydata$mes==7,1,0)
mydata$month8<- ifelse(mydata$mes==8,1,0)
mydata$month9<- ifelse(mydata$mes==9,1,0)
mydata$month10<- ifelse(mydata$mes==10,1,0)
mydata$month11<- ifelse(mydata$mes==11,1,0)
mydata$month12<- ifelse(mydata$mes==12,1,0)
mydata$post<- ifelse(mydata$mes>7,1,0)
mydata$inst1p<-mydata$institu1*mydata$post
mydata$inst3_1p=mydata$inst3_1*mydata$post


# Creating Distance dummy variables

mydata$cuad0 <- ifelse(mydata$distanci==0,1,0) 
mydata$cuad0p <- mydata$cuad0*mydata$post
mydata$cuad1 <- ifelse(mydata$distanci==1,1,0) 
mydata$cuad1p <- mydata$cuad1*mydata$post
mydata$cuad2 <- ifelse(mydata$distanci==2,1,0) 
mydata$cuad2p <- mydata$cuad2*mydata$post
mydata$cuad3 <- ifelse(mydata$distanci==3,1,0) 
mydata$cuad3p <- mydata$cuad3*mydata$post
mydata$cuad4 <- ifelse(mydata$distanci==4,1,0) 
mydata$cuad4p <- mydata$cuad4*mydata$post
mydata$cuad5 <- ifelse(mydata$distanci==5,1,0) 
mydata$cuad5p <- mydata$cuad5*mydata$post
mydata$cuad6 <- ifelse(mydata$distanci==6,1,0) 
mydata$cuad6p <- mydata$cuad6*mydata$post
mydata$cuad7 <- ifelse(mydata$distanci==7,1,0) 
mydata$cuad7p <- mydata$cuad7*mydata$post


# CROSS-SECTIONAL DIFFERENCES OF MEANS

mydata$codigo <- ifelse(mydata$institu1==1,1, ifelse(mydata$inst3_1==1,2, ifelse(mydata$cuad2==1,3,4))) 
mydata$otromes1<-ifelse(mydata$mes==72,7.2, ifelse(mydata$mes==73,7.3,mydata$mes)) 
newdata <- mydata[order(mydata$observ, mydata$mes),]
newdata$totrob2 <- ifelse(newdata$mes==72 | newdata$mes==73, sum(newdata$totrob), ifelse(newdata$mes!=72 & newdata$mes!=73, newdata$totrob,0))


# TABLE A.1. Monthly Evolution of car theft
 
newdata<-subset(newdata, newdata$mes!=72)
table(newdata$otromes1, newdata$codigo)
mes_groups <- group_by(newdata, otromes1, codigo)

mean_for <- summarise(mes_groups, totrob2_count_mean=mean(totrob2), n=n())
sd_for <- summarise(mes_groups, totrob2_count_sd=sd(totrob2), n=n())


lastdata <- newdata[order(newdata$mes,newdata$observ),]
t.test(totrob2~codigo, data=subset(lastdata, (lastdata$codigo==1 |lastdata$codigo==4) & lastdata$mes!=4), paired = FALSE)

# TABLE 4 The Effect of Police Presence on Car Theft


lastdata<-subset(lastdata, lastdata$mes!=73) #Omit car theft observation for 18-31 July.

didreg_3A = lm(totrob ~ inst1p  + factor(observ) + factor(mes), data=lastdata)
summary(didreg_3A)
stargazer(didreg_3A, type='text')

didreg_3B = lm(totrob ~ inst1p + inst3_1p + factor(observ) + factor(mes), data=lastdata)
summary(didreg_3B)
stargazer(didreg_3B, type='text')

didreg_3C = lm(totrob ~ inst1p + inst3_1p + cuad2p + factor(observ) + factor(mes),  data=lastdata)
summary(didreg_3C)
stargazer(didreg_3C, type='text')

didreg_3D = lm(totrob ~ institu1 + inst3_1 + cuad2+ factor(mes), data=subset(lastdata, post==1))
summary(didreg_3D)
stargazer(didreg_3D, type='text')


### RESULTS FOR TIME SERIES ###

lastdata$totrobc <- ifelse(lastdata$mes==7, lastdata$totrob*(30/17), 
                           ifelse(lastdata$mes==5, lastdata$totrob*(30/31),
                                  ifelse(lastdata$mes==8, lastdata$totrob*(30/31),
                                         ifelse(lastdata$mes==10, lastdata$totrob*(30/31),
                                                ifelse(lastdata$mes==12, lastdata$totrob*(30/31),lastdata$totrob)))))

didreg_3E = lm(totrobc ~ inst1p + inst3_1p + cuad2p + factor(observ), data=subset(lastdata,lastdata$institu1==1 | lastdata$inst3_1==1 | lastdata$cuad2==1))
summary(didreg_3E)
stargazer(didreg_3E, type='text') 






# MONTE CARLO SIMULATION PART #

set.seed(123)     

R=1000 # Number of simulations 

sig=0.05 # 5-percent significance level 

N=876  # Total number of blocks
N.in=37 # Number of treated blocks
T=9  # Total number of months
T.pre=4 # Number of pre-attack periods
theta = -0.07752 # Estimator from the assumed true model


D_it=c(rep(0,N*T.pre),rep(c(rep(1,N.in),rep(0,N-N.in)),T-T.pre)) # Dummy variable 
v_t=sort(c(0.016,-0.011,-0.032, -0.070, 0.001, -0.012,-0.001,-0.010,-0.005)) # It is calculated from the didreg_3A
u_i=rep(rnorm(N,0.05+0.1*D_it[((T-1)*N+1):(N*T)],0.1054),T) # Block fixed effects and Month fixed effects are not ortogonal anymore to our inst
# There is a correlation
t=rep(1,N)
for (k in 2:T){
  t=c(t,rep(k,N)) 
}

# Code to store the results

theta.hat=NaN*1:R
theta.hat_b=NaN*1:R
theta.hat_i=NaN*1:R
theta.hat_t=NaN*1:R
theta.hat_Dit=NaN*1:R


for (r in  1:R){     #Start of Monte Carlo Simulation loop
  e_it=rnorm(N*T,0,0.2166) # Huber-White standard error term
  y=theta*D_it + v_t+ u_i +e_it # assumed true model with parallel trend
  y_b=theta*D_it + (1+D_it)*v_t+ u_i +e_it # assumed true model without parallel trend
  data_MC=data.frame(y=y,D_it=D_it,i=rep(1:N,T),t=t)
  data_MC_b=data.frame(y=y_b,D_it=D_it,i=rep(1:N,T),t=t)
  theta.hat[r]=lm(y~D_it+factor(i)+factor(t)-1,data=data_MC)$coefficients[1]
  theta.hat_b[r]=lm(y~D_it+factor(i)+factor(t)-1,data=data_MC_b)$coefficients[1]
  theta.hat_i[r]=lm(y~D_it+factor(i)-1,data=data_MC)$coefficients[1]
  theta.hat_t[r]=lm(y~D_it+factor(t)-1,data=data_MC)$coefficients[1]
  theta.hat_Dit[r]=lm(y~D_it-1,data=data_MC)$coefficients[1]
}

# Code for the table of control and treatment blocks

table(data_MC$t,data_MC$D_it)

# Code for calculation the estimated bias,MSE, mean and standard deviation of theta.hat

Bias=mean(theta.hat)-theta
MSE=mean((theta.hat-theta)**2)
Mean=mean(theta.hat)
Sd=sd(theta.hat)

Bias_b=mean(theta.hat_b)-theta
MSE_b=mean((theta.hat_b-theta)**2)
Mean_b=mean(theta.hat_b)
Sd_b=sd(theta.hat_b)

Bias_i=mean(theta.hat_i)-theta
MSE_i=mean((theta.hat_i-theta)**2)
Mean_i=mean(theta.hat_i)
Sd_i=sd(theta.hat_i)

Bias_t=mean(theta.hat_t)-theta
MSE_t=mean((theta.hat_t-theta)**2)
Mean_t=mean(theta.hat_t)
Sd_t=sd(theta.hat_t)

Bias_Dit=mean(theta.hat_Dit)-theta
MSE_Dit=mean((theta.hat_Dit-theta)**2)
Mean_Dit=mean(theta.hat_Dit)
Sd_Dit=sd(theta.hat_Dit)

# Code for calculation the confidence intervals at 5-percent significance level

CI.lower=sort(theta.hat)[max(1,floor(sig/2*R))]
CI.upper=sort(theta.hat)[ceiling((1-sig/2)*R)]

CI.lower=sort(theta.hat_b)[max(1,floor(sig/2*R))]
CI.upper=sort(theta.hat_b)[ceiling((1-sig/2)*R)]

CI.lower_i=sort(theta.hat_i)[max(1,floor(sig/2*R))]
CI.upper_i=sort(theta.hat_i)[ceiling((1-sig/2)*R)]

CI.lower_t=sort(theta.hat_t)[max(1,floor(sig/2*R))]
CI.upper_t=sort(theta.hat_t)[ceiling((1-sig/2)*R)]

CI.lower_Dit=sort(theta.hat_Dit)[max(1,floor(sig/2*R))]
CI.upper_Dit=sort(theta.hat_Dit)[ceiling((1-sig/2)*R)]

CI.lower_b=sort(theta.hat_b)[max(1,floor(sig/2*R))]
CI.upper_b=sort(theta.hat_b)[ceiling((1-sig/2)*R)]


# Figure A1 "Density of theta.hat under 4 different specifications"

plot(density(theta.hat),
     xlim=c(-0.2, 0.2),
     ylim=c(0,25))
lines(density(theta.hat_i),
      col=2)
lines(density(theta.hat_t),
      col=3)
lines(density(theta.hat_Dit),
      col=4)
legend("topright",
       c(expression(hat(theta), hat(theta[i]), hat(theta[t]), hat(theta[Dit]))),
       col = 1:4,
       lty = 2)
abline(v=-0.07752, col="red",lwd=3, lty=2)

# Figure 1 "Density of theta.hat with parallel and non-parallel trend"

plot(density(theta.hat),
     xlim=c(-0.17, 0.00),
     ylim=c(0,25))
lines(density(theta.hat_b),
      col=2)

legend("topright",
       c("parallel trend", "non-parallel trend"),
       col = 1:2,
       lty = 1, cex=1.0)
abline(v=-0.07752, col="red",lwd=3, lty=2)


