k1=3
k2=4
k3=6

# p=k=No. of periods and b=n=No. of subjects=v*NB
v=16
y= 0
#NB=3 #No. of sets of shifts
NB1=2   #No. of set of shifts for p1=k1 
NB2=1
NB3=1
#No. of set of shifts for p2=k2

#Set of shifts  
T=c(10,14) #First set of shifts
T1=c(3,12) # Second and so on . . . .
T2=c(6,8,13)
T3=c(4,7,9,11,15)
#T4=c(47,48,59,64,68,85,89)
#T5=c(30,38,40,42,47,82,83)
#T6=c(4,5,16,18,55,91,92)
#T7=c(23,24,37,41,49,51,60,78)
#T8=c(28,32,43,56,66,67,77,90)
#T9=c(12,21,35,45,50,69,72,76,84)
#T10=c(17,29,33,39,52,58,74,79,81)
#T11=c(9,11,27,41,55,67,82,86)
#T12=c(10,28,39,47,51,52,63,87)
#T13=c(28,29,67,73,83,97)
#T14=c(10,60,68,79,80,95)
#T15=c(6,43,52,62,64,78,90)
#T16=c(46,64,76,79)
#T17=c(11,16,37,40,85)
#T18=c(27,28,30,55,59,91)
#T19=c(20,20,20)
#T20=c(21,21,21)
#T21=c(22,22,22)
#T22=c(23,23,23)
#Increase 

#Initial blocks from set of shifts
IB=c(y,cumsum(T)%%v)
IB1=c(y,cumsum(T1)%%v)
IB2=c(y,cumsum(T2)%%v)
IB3=c(y,cumsum(T3)%%v)
#IB4=c(y,cumsum(T4)%%v)
#IB5=c(y,cumsum(T5)%%v)
#IB6=c(y,cumsum(T6)%%v)
#IB7=c(y,cumsum(T7)%%v)
#IB8=c(y,cumsum(T8)%%v)
#IB9=c(y,cumsum(T9)%%v)
#IB10=c(y,cumsum(T10)%%v)
#IB11=c(y,cumsum(T11)%%v)
#IB12=c(y,cumsum(T12)%%v)
#IB13=c(y,cumsum(T13)%%v)
#IB14=c(y,cumsum(T14)%%v)
#IB15=c(y,cumsum(T15)%%v)
#IB16=c(y,cumsum(T16)%%v)
#IB17=c(y,cumsum(T17)%%v)
#IB18=c(y,cumsum(T18)%%v)
#IB19=c(y,cumsum(T19)%%v)
#IB20=c(y,cumsum(T20)%%v)
#Increase

#Initial blocks without set of shifts
#IB=c(0,1,3,2,4)   
#IB1=c(0,1,4)
#IB2=c(0,7,15,25)
#IB3=c(0,36,2,16)
#IB4=c(0,32,3,28)
#IB5=c(0,21,40,16)
#IB6=c(0,30,3,19)
p=seq(from=0, to=v-1, by=1)
w=rep(0,v)

#print(ww3)
l=NULL;l1=NULL;l2=NULL;l3=NULL;l4=NULL;l5=NULL;l6=NULL;l7=NULL;l8=NULL;l9=NULL;l10=NULL;l11=NULL;l12=NULL;l13=NULL;l14=NULL;l15=NULL;l16=NULL;l17=NULL;l18=NULL;l19=NULL;l20=NULL;l21=NULL;l22=NULL

# For Block size K1
for(i in 1:k1){
  for(j in 1:v){
    l=c(l,rep((IB[i]+p[j]+v)%% v))
    l1=c(l1,rep((IB1[i]+p[j]+v)%% v))
    #l2=c(l2,rep((IB2[i]+p[j]+v)%% v))
    #l3=c(l3,rep((IB3[i]+p[j]+v)%% v))
    #l4=c(l4,rep((IB4[i]+p[j]+v)%% v))
    #l5=c(l5,rep((IB5[i]+p[j]+v)%% v))
    #l6=c(l6,rep((IB6[i]+p[j]+v)%% v))
    #l7=c(l7,rep((IB7[i]+p[j]+v)%% v))
    #l8=c(l8,rep((IB8[i]+p[j]+v)%% v))
    #l9=c(l9,rep((IB9[i]+p[j]+v)%% v))
    #l10=c(l10,rep((IB10[i]+p[j]+v)%% v))
    #l11=c(l11,rep((IB11[i]+p[j]+v)%% v))
    #l12=c(l12,rep((IB12[i]+p[j]+v)%% v))
    #l13=c(l13,rep((IB13[i]+p[j]+v)%% v))
    #l14=c(l14,rep((IB14[i]+p[j]+v)%% v))
    #l15=c(l15,rep((IB15[i]+p[j]+v)%% v))
    #l16=c(l16,rep((IB16[i]+p[j]+v)%% v))#Increase
  }
}

# For Block size K2
for(i in 1:k2){
  for(j in 1:v){
    #l=c(l,rep((IB[i]+p[j]+v)%% v))
    #l1=c(l1,rep((IB1[i]+p[j]+v)%% v))
    l2=c(l2,rep((IB2[i]+p[j]+v)%% v))
    #l3=c(l3,rep((IB3[i]+p[j]+v)%% v))
    #l4=c(l4,rep((IB4[i]+p[j]+v)%% v))
    #l5=c(l5,rep((IB5[i]+p[j]+v)%% v))
    #l6=c(l6,rep((IB6[i]+p[j]+v)%% v))
    #l7=c(l7,rep((IB7[i]+p[j]+v)%% v))
    #l8=c(l8,rep((IB8[i]+p[j]+v)%% v))
    #l9=c(l9,rep((IB9[i]+p[j]+v)%% v))
    #l10=c(l10,rep((IB10[i]+p[j]+v)%% v))
    #l11=c(l11,rep((IB11[i]+p[j]+v)%% v))
    #l12=c(l12,rep((IB12[i]+p[j]+v)%% v))
    #l13=c(l13,rep((IB13[i]+p[j]+v)%% v))
    #l14=c(l14,rep((IB14[i]+p[j]+v)%% v))
    #l15=c(l15,rep((IB15[i]+p[j]+v)%% v))
    #l16=c(l16,rep((IB16[i]+p[j]+v)%% v))
    #l17=c(l17,rep((IB17[i]+p[j]+v)%% v))
    #l18=c(l18,rep((IB18[i]+p[j]+v)%% v))
    #l19=c(l19,rep((IB19[i]+p[j]+v)%% v))#Increase
  }
}


# For Block size K3
for(i in 1:k3){
  for(j in 1:v){
    #l=c(l,rep((IB[i]+p[j]+v)%% v))
    #l1=c(l1,rep((IB1[i]+p[j]+v)%% v))
    #l2=c(l2,rep((IB2[i]+p[j]+v)%% v))
    l3=c(l3,rep((IB3[i]+p[j]+v)%% v))
    #l4=c(l4,rep((IB4[i]+p[j]+v)%% v))
    #l5=c(l5,rep((IB5[i]+p[j]+v)%% v))
    #l6=c(l6,rep((IB6[i]+p[j]+v)%% v))
    #l7=c(l7,rep((IB7[i]+p[j]+v)%% v))
    #l8=c(l8,rep((IB8[i]+p[j]+v)%% v))
    #l9=c(l9,rep((IB9[i]+p[j]+v)%% v))
    #l10=c(l10,rep((IB10[i]+p[j]+v)%% v))
    #l11=c(l11,rep((IB11[i]+p[j]+v)%% v))
    #l12=c(l12,rep((IB12[i]+p[j]+v)%% v))
    #l13=c(l13,rep((IB13[i]+p[j]+v)%% v))
    #l14=c(l14,rep((IB14[i]+p[j]+v)%% v))
    #l15=c(l15,rep((IB15[i]+p[j]+v)%% v))
    #l16=c(l16,rep((IB16[i]+p[j]+v)%% v))
    #l17=c(l17,rep((IB17[i]+p[j]+v)%% v))
    #l18=c(l18,rep((IB18[i]+p[j]+v)%% v))
    #l19=c(l19,rep((IB19[i]+p[j]+v)%% v))
    #l20=c(l20,rep((IB20[i]+p[j]+v)%% v))#Increase
  }
}

g = matrix(c(l),nrow=k1,ncol=v,byrow =  TRUE)
g1= matrix(c(l1),nrow=k1,ncol=v,byrow =  TRUE)
g2= matrix(c(l2),nrow=k2,ncol=v,byrow =  TRUE)
g3= matrix(c(l3),nrow=k3,ncol=v,byrow =  TRUE)
#g4= matrix(c(l4),nrow=k1,ncol=v,byrow =  TRUE)
#g5= matrix(c(l5),nrow=k1,ncol=v,byrow =  TRUE)
#g6= matrix(c(l6),nrow=k1,ncol=v,byrow =  TRUE)
#g7= matrix(c(l7),nrow=k2,ncol=v,byrow =  TRUE)
#g8= matrix(c(l8),nrow=k2,ncol=v,byrow =  TRUE)
#g9= matrix(c(l9),nrow=k3,ncol=v,byrow =  TRUE)
#g10= matrix(c(l10),nrow=k3,ncol=v,byrow =  TRUE)
#g11= matrix(c(l11),nrow=k3,ncol=v,byrow =  TRUE)
#g12= matrix(c(l12),nrow=k3,ncol=v,byrow =  TRUE)
#g13= matrix(c(l13),nrow=k2,ncol=v,byrow =  TRUE)
#g14= matrix(c(l14),nrow=k2,ncol=v,byrow =  TRUE)
#g15= matrix(c(l15),nrow=k3,ncol=v,byrow =  TRUE)
#g16= matrix(c(l16),nrow=k1,ncol=v,byrow =  TRUE)
#g17= matrix(c(l17),nrow=k2,ncol=v,byrow =  TRUE)
#g18= matrix(c(l18),nrow=k3,ncol=v,byrow =  TRUE)
#g19= matrix(c(l19),nrow=k3,ncol=v,byrow =  TRUE)
#g20= matrix(c(l20),nrow=k3,ncol=v,byrow =  TRUE)
#Increase 

#G=cbind(g,g1,g2,g3,g4)   #Increase as ,,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,g21,g22g1, g2,,g20,g21 . . .   
#print(G)
#G1 Bind blocks of size k1
G1=cbind(g,g1)   #Increase as g1, g2, . . .   
print(G1)
#G2 Bind blocks of size k2
G2=cbind(g2)   #Increase as g1, g2, . . . 
print(G2)
G3=cbind(g3)   #Increase as g1, g2, . . . 
print(G3)
#G4=cbind(g4)   #Increase as g1, g2, . . . 
#print(G4)

#Model for repeated measurement designs  y=Pa+Ub+Tt+Ff
#P=Incidence matrix for period effect, U=incidence matrix for subject effect, T=for treatment effect 
#and F=for residual effect

P1=kronecker(matrix(1,nrow=v*NB1),diag(nrow = k1))
print(P1)
P2=kronecker(matrix(1,nrow=v*NB2),diag(nrow = k2))
print(P2)
P3=kronecker(matrix(1,nrow=v*NB3),diag(nrow = k3))
print(P3)
U1=kronecker(diag(nrow = v*NB1),matrix(1,nrow=k1))
print(U1)
U2=kronecker(diag(nrow = v*NB2),matrix(1,nrow=k2))
print(U2)
U3=kronecker(diag(nrow = v*NB3),matrix(1,nrow=k3))
print(U3)

wrapind1 <- function(i,n)
  ifelse((r <- i %% n) == 0, n, r)
n <- nrow(G1)
incmat1 <- matrix(0,ncol=v,nrow=prod(dim(G1)),
                  dimnames=list(NULL,0:(v-1)))
m <- 1
for (i in seq(ncol(G1)))
  for (j in seq(nrow(G1))) {
    if(j==1){
      tt <- table(as.character(G1[wrapind1(j-1,n),i]))
    }
    # if(j>1&j<k){
    #  tt <- table(as.character(G[wrapind(c(j-1,j+1),n),i]))
    #}
    else{
      tt <- table(as.character(G1[wrapind1(j-1,n),i]))
    }
    
    incmat1[m,names(tt)] <- tt
    m <- m+1
  }
wrapind2 <- function(i,n)
  ifelse((r <- i %% n) == 0, n, r)
n <- nrow(G2)
incmat2 <- matrix(0,ncol=v,nrow=prod(dim(G2)),
                  dimnames=list(NULL,0:(v-1)))
m <- 1
for (i in seq(ncol(G2)))
  for (j in seq(nrow(G2))){ 
    if(j==1){
      tt <- table(as.character(G2[wrapind2(j-1,n),i]))
    }
    # if(j>1&j<k){
    #  tt <- table(as.character(G[wrapind(c(j-1,j+1),n),i]))
    #}
    else{
      tt <- table(as.character(G2[wrapind2(j-1,n),i]))
    }
    
    incmat2[m,names(tt)] <- tt
    m <- m+1
  }
wrapind3 <- function(i,n)
  ifelse((r <- i %% n) == 0, n, r)
n <- nrow(G3)
incmat3 <- matrix(0,ncol=v,nrow=prod(dim(G3)),
                  dimnames=list(NULL,0:(v-1)))
m <- 1
for (i in seq(ncol(G3)))
  for (j in seq(nrow(G3))) {
    if(j==1){
      tt <- table(as.character(G3[wrapind3(j-1,n),i]))
    }
    # if(j>1&j<k){
    #  tt <- table(as.character(G[wrapind(c(j-1,j+1),n),i]))
    #}
    else{
      tt <- table(as.character(G3[wrapind3(j-1,n),i]))
    }
    
    incmat3[m,names(tt)] <- tt
    m <- m+1 
  }
incmat=rbind(incmat1,incmat2,incmat3)
print(incmat)
#for (i in seq(ncol(G)))
# for (j in seq(nrow(G))) {
#  tt <- table(as.character(G[wrapind(c(j-1),n),i]))
# incmat[m,names(tt)] <- tt
#  m <- m+1
#}
wrap <- function(i,n)
  ifelse((R <- i %% n) == 0, n, R)
n <- nrow(G1)
incma1 <- matrix(0,ncol=v,nrow=prod(dim(G1)),
                 dimnames=list(NULL,0:(v-1)))
u <- 1
for (i in seq(ncol(G1)))
  for (j in seq(nrow(G1))) {
    ttt<- table(as.character(G1[wrap((j),n),i]))
    incma1[u,names(ttt)] <- ttt
    u <- u+1
  }
wrap1 <- function(i,n)
  ifelse((R <- i %% n) == 0, n, R)
n <- nrow(G2)
incma2 <- matrix(0,ncol=v,nrow=prod(dim(G2)),
                 dimnames=list(NULL,0:(v-1)))
u <- 1
for (i in seq(ncol(G2)))
  for (j in seq(nrow(G2))) {
    ttt<- table(as.character(G2[wrap((j),n),i]))
    incma2[u,names(ttt)] <- ttt
    u <- u+1
  }
wrap2 <- function(i,n)
  ifelse((R <- i %% n) == 0, n, R)
n <- nrow(G3)
incma3 <- matrix(0,ncol=v,nrow=prod(dim(G3)),
                 dimnames=list(NULL,0:(v-1)))
u <- 1
for (i in seq(ncol(G3)))
  for (j in seq(nrow(G3))) {
    ttt<- table(as.character(G3[wrap((j),n),i]))
    incma3[u,names(ttt)] <- ttt
    u <- u+1
  }
incma=rbind(incma1,incma2,incma3)
s=t(incmat)%*%incma

E= as.data.frame(table(s[1,]))


a1=apply(G1, 2, function(z){
  ret <- numeric(v)
  for( ii in seq_along(z) )
  {
    ret[z[ii]+1] <- ret[z[ii]+1]+ 1}
  ret}) 
a2=apply(G2, 2, function(z){
  ret <- numeric(v)
  for( ii in seq_along(z) )
  {
    ret[z[ii]+1] <- ret[z[ii]+1]+ 1}
  ret}) 
A1=a1%*%t(a1)
A2=a2%*%t(a2)
a=cbind(a1,a2)
#print(a)
A=a%*%t(a)
print(A)

F= as.data.frame(table(A[1,]))
#print (E)
#print (F)
# for efficiency 
B11 = diag(A1)
B12 = diag(A2)
B13 = diag(B11)
B14 = diag(B12)
B1 = B13+B14
print("R matrix")
print(B1)
B2 = B1-(((1/k1)*A1)+((1/k2)*A2))
print(B2)
B3 = eigen(B2)
BB4 = B3$values
B4= round(BB4,digits = 4)
B5 = B4[!B4%in%B4[v]]
B6 =round( sum(1/B5),digits = 4)
B7= round((B6/(v-1)),digits = 4)
#print (IB)
#print (IB1)
#print (IB2)
#print (IB3)
#print (IB4)
print ("Efficiency Factor 'E'")
Efficiency= round((1/B7)*(1/A[1,1]),digits = 4)
print(Efficiency)
#print (B9)
#print("********************************************")
U_bound=round((((k1-1)*v)/((v-1)*k1)),digits = 4)
#print("Upper Bound =")
#print (M10)
m1=t(incmat1)%*%U1
m2=t(incmat2)%*%U2
m3=t(incmat3)%*%U3
M1=m1%*%t(m1)
M2=m2%*%t(m2)
M3=m3%*%t(m3)
Mmatrix=cbind(m1,m2,m3)
#print(a)
M=Mmatrix%*%t(Mmatrix)
print(M)
M11 = diag(M1)
M12 = diag(M2)
M13 = diag(M11)
M14 = diag(M12)
M111 = M13+M14
M22 = M111-(((1/k1)*M1)+((1/k2)*M2))
print(M22)
M3 = eigen(M22)
MM4 = M3$values
M4= round(MM4,digits = 4)
M5 = M4[!M4%in%M4[v]]
M6 =round(sum(1/M5),digits = 4)
M7= round((M6/(v-1)),digits = 4)
EfficiencyM=round((1/M7)*(1/M[1,1]),digits = 4)
print(EfficiencyM)


n1=t(incma1)%*%U1
print(n1)
n2=t(incma2)%*%U2
print(n2)
n3=t(incma3)%*%U3
CM=n1%*%t(n1)+(n2%*%t(n2))+(n3%*%t(n3))
MM=m1%*%t(m1)+(m2%*%t(m2))+(m3%*%t(m3))
n=(1/k1*(n1%*%t(n1)))+(1/k2*(n2%*%t(n2))+(1/k3*(n3%*%t(n3))))
L1=t(incma1)%*%incmat1
L2=t(incma2)%*%incmat2
L3=t(incma3)%*%incmat3
L=t(incma)%*%incmat
NO=matrix(c(1),nrow=v,ncol=1)
CN=cbind(NO,L)
CHS=chisq.test(CN)
print(CHS)
SUME= sum(CN)
value=CHS$statistic
VC=sqrt(((value)/SUME)/(min(v-1,v)))
ES=(1-VC)
print(ES)


