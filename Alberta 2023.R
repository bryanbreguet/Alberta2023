#Alberta 2023#

results<-read.csv(file.choose(), header=FALSE) #pick csv file called results
adjustments<-read.csv(file.choose(), header=FALSE) #pick adjustment file. They are in percentages.

n=400 # sample size, corresponding to MoE of 4.8% for a party at 40%. Consistent with historical polling accuracy
N=10000 #number of simulations. My computer has no issue with 10k and is old, but YMMV.
nr=87 # number of ridings
pr=c(46.9, 44.6, 4.3, 1.1, 0.6) #current voting intentions. UCP, NDP, AB, Lib, WR. No need for 'others'. This is what you want to change.
sumpr=sum(pr)
pr2=c(pr,100-sumpr) # so that it adds to 100% with others

# Randomize at the provincial level
r=100*t(rmultinom(N, n, prob = pr2))/n

s=c(54.9, 32.7, 9.1, 1, 0.5) # past election results at the aggregate level.


swing1=(r[,1]-s[1])
swing2=(r[,2]-s[2])
swing3=(r[,3]-s[3])
swing4=(r[,4]-s[4])
swing5=(r[,5]-s[5])

swing=cbind(swing1, swing2, swing3, swing4, swing5)

proj1=matrix(0, nrow=nr, ncol=N)
proj2=matrix(0, nrow=nr, ncol=N)
proj3=matrix(0, nrow=nr, ncol=N)
proj4=matrix(0, nrow=nr, ncol=N)
proj5=matrix(0, nrow=nr, ncol=N)
proj6=matrix(0, nrow=nr, ncol=N) #independents



for (i in 1:N) {
  proj1[,i]=results[,1]+swing1[i]+adjustments[,1]
}
proj1[proj1<0]=0
proj1[proj1>100]=100


for (i in 1:N) {
  proj2[,i]=results[,2]+swing2[i]+adjustments[,2]
}
proj2[proj2<0]=0
proj2[proj2>100]=100

for (i in 1:N) {
  proj3[,i]=results[,3]+swing3[i]+adjustments[,3]
}
proj3[proj3<0]=0
proj3[proj3>100]=100

for (i in 1:N) {
  proj4[,i]=results[,4]+swing4[i]+adjustments[,4]
}
proj4[proj4<0]=0
proj4[proj4>100]=100

for (i in 1:N) {
  proj5[,i]=results[,5]+swing5[i]+adjustments[,5]
}
proj5[proj5<0]=0
proj5[proj5>100]=100

for (i in 1:N) {
  proj6[,i]=adjustments[,6]
}


#Adjustments to the simple linear swing in case a party is experiencing a big swing

above1<-matrix(as.numeric(proj1>=r[,1]),nr,N)
above2<-matrix(as.numeric(proj2>=r[,2]),nr,N)
above3<-matrix(as.numeric(proj3>=r[,3]),nr,N)
above4<-matrix(as.numeric(proj4>=r[,4]),nr,N)
above5<-matrix(as.numeric(proj5>=r[,5]),nr,N)
above6<-matrix(as.numeric(proj6>=r[,6]),nr,N)

aboveprov1<-matrix(as.numeric(r[,1]>=59),nr,N)
belowprov1<-matrix(as.numeric(r[,1]<=49),nr,N)

aboveprov2<-matrix(as.numeric(r[,2]>=38),nr,N)
belowprov2<-matrix(as.numeric(r[,2]<=28),nr,N)

aboveprov3<-matrix(as.numeric(r[,3]>=14),nr,N)
belowprov3<-matrix(as.numeric(r[,3]<=4),nr,N)

aboveprov4<-matrix(as.numeric(r[,4]>=5),nr,N)
belowprov4<-matrix(as.numeric(r[,4]<=0.5),nr,N)

aboveprov5<-matrix(as.numeric(r[,5]>=5),nr,N)
belowprov5<-matrix(as.numeric(r[,5]<=0.25),nr,N)


proj1=(1-above1*belowprov1-(1-above1)*aboveprov1)*proj1+ above1*belowprov1*(r[,1]+r[,1]/49*(proj1-r[,1]))+
  (1-above1)*aboveprov1*(proj1+(r[,1]-59)/(100-59)*(r[,1]-proj1))

proj2=(1-above2*belowprov2-(1-above2)*aboveprov2)*proj2+ above2*belowprov2*(r[,2]+r[,2]/28*(proj2-r[,2]))+
  (1-above2)*aboveprov2*(proj2+(r[,2]-38)/(100-38)*(r[,2]-proj2))

proj3=(1-above3*belowprov3-(1-above3)*aboveprov3)*proj3+ above3*belowprov3*(r[,3]+r[,3]/4*(proj3-r[,3]))+
  (1-above3)*aboveprov3*(proj3+(r[,3]-14)/(100-14)*(r[,3]-proj3))

proj4=(1-above4*belowprov4-(1-above4)*aboveprov4)*proj4+ above4*belowprov4*(r[,4]+r[,4]/0.5*(proj4-r[,4]))+
  (1-above4)*aboveprov4*(proj4+(r[,4]-5)/(100-5)*(r[,4]-proj4))

proj5=(1-above5*belowprov5-(1-above5)*aboveprov5)*proj5+ above5*belowprov5*(r[,5]+r[,5]/0.25*(proj5-r[,5]))+
  (1-above5)*aboveprov5*(proj5+(r[,5]-5)/(100-5)*(r[,5]-proj5))



# 2nd randomization, at the riding level (because having correct province-wide % isn't everything)

proj1_2=matrix(0, nrow=nr, ncol=N)
proj2_2=matrix(0, nrow=nr, ncol=N)
proj3_2=matrix(0, nrow=nr, ncol=N)
proj4_2=matrix(0, nrow=nr, ncol=N)
proj5_2=matrix(0, nrow=nr, ncol=N)
proj6_2=matrix(0, nrow=nr, ncol=N)

n2=200 # sample size for the randomization at the riding level. Yes it's low because a lot of uncertainty exists empirically


for (i in 1:N) {
  for (j in 1:nr) {
    sumproj=proj1[j,i]+proj2[j,i]+proj3[j,i]+proj4[j,i]+proj5[j,i]+proj6[j,i]
    pr3=c(proj1[j,i]/sumproj, proj2[j,i]/sumproj, proj3[j,i]/sumproj, proj4[j,i]/sumproj, proj5[j,i]/sumproj, proj6[j,i]/sumproj)
    r2=100*t(rmultinom(1, n2, prob = pr3))/n2
    proj1_2[j,i]=r2[1]+0.00001*runif(1) #small random number added to avoid ties
    proj2_2[j,i]=r2[2]+0.00001*runif(1)
    proj3_2[j,i]=r2[3]+0.00001*runif(1)
    proj4_2[j,i]=r2[4]+0.00001*runif(1)
    proj5_2[j,i]=r2[5]+0.00001*runif(1)
    proj6_2[j,i]=r2[6]+0.00001*runif(1)
  }
}



#wins

projmax=pmax(proj1_2, proj2_2, proj3_2, proj4_2, proj5_2, proj6_2)

win1<-matrix(as.numeric(proj1_2==projmax),nr,N)
win2<-matrix(as.numeric(proj2_2==projmax),nr,N)
win3<-matrix(as.numeric(proj3_2==projmax),nr,N)
win4<-matrix(as.numeric(proj4_2==projmax),nr,N)
win5<-matrix(as.numeric(proj5_2==projmax),nr,N)
win6<-matrix(as.numeric(proj6_2==projmax),nr,N)



#Sum of wins

sumwin1=rowSums(t(win1))
sumwin2=rowSums(t(win2))
sumwin3=rowSums(t(win3))
sumwin4=rowSums(t(win4))
sumwin5=rowSums(t(win5))
sumwin6=rowSums(t(win6))


#Outcomes

outcome=matrix(0,5,1)
outcome[1]=sum(sumwin1>=44) # UCP majority
outcome[2]=sum(sumwin1<44 & sumwin1>sumwin2) # UCP plurality
outcome[3]=sum(sumwin2>=44) # NDP majority
outcome[4]=sum(sumwin2<44 & sumwin2>sumwin1) # NDP plurality
outcome[5]=sum(sumwin1==sumwin2) #tie


outcome



#Probabilities to win each riding

sumwin1riding=rowSums(win1)
sumwin2riding=rowSums(win2)
sumwin3riding=rowSums(win3)
sumwin4riding=rowSums(win4)
sumwin5riding=rowSums(win5)
sumwin6riding=rowSums(win6)

probwinriding=cbind(sumwin1riding, sumwin2riding, sumwin3riding, sumwin4riding, sumwin5riding, sumwin6riding)*1/N # chances of winning each riding

sumwinall=cbind(sumwin1, sumwin2, sumwin3, sumwin4, sumwin5, sumwin6)

quantile(sumwinall[,1],c(0.05, 0.95)) #95% CI
quantile(sumwinall[,2],c(0.05, 0.95))
quantile(sumwinall[,3],c(0.05, 0.95))
quantile(sumwinall[,4],c(0.05, 0.95))
quantile(sumwinall[,5],c(0.05, 0.95))
quantile(sumwinall[,6],c(0.05, 0.95))

