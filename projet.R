install.packages("expm")
library("expm")

#generate distance matrix
library(Matrix)

x<-Matrix(rexp(400),20)

M <- forceSymmetric(x)
for (i in 1:20){
  M[i,i]=0
}

#delete 5 pairs of cities
a<-c(3,5,7,8,1)
b<-c(4,11,18,13,10)
for (i in 1:5){
  M[a[i],b[i]]=0
  M[b[i],a[i]]=0
}

#***********************************************************

#generate new series moves.
h<-c(1:20)
c<-sample(h)
c[21]<-c[1]
#********************************
#validation function 

main<-function(k,a,b){
  e<-sample(d,2)
  u<-replace(k,c(e[1],e[2]),k[c(e[2],e[1])])
  while(b2!=0 && b1!=0){
    b1=0 
    b2=0 #reset the game
    for (i in 1:20) {
      g=u[i]
      j=u[i+1]
      
      q1<-which(a %in% g)
      q2<-which(b %in% j)
      
      p1<-which(b %in% g)
      p2<-which(a %in% j)
      
      #checking right side,
      if(length(q1)==0 || length(q2)==0){
        a1=0 
      }
      else if (q1==q2){
        
        b1=1   # b1 is an indicator to check for invalid sets of cities
      } 
      
      #checking left side
      if(length(p1)==0 || length(p2)==0){
        
        a1=0
      }
      else if(p1==p2){
        
        b2=1
      } 
      if(b2==1){
        
        e<-sample(d,2)
        u<-replace(c,c(e[1],e[2]),c[c(e[2],e[1])])
      }
      #if u is not valid, we replace this iteration with a new city set
      if(b1==1){
        e<-sample(d,2)
        u<-replace(c,c(e[1],e[2]),c[c(e[2],e[1])])
      }
    }}
  return(u)
}
#***************************************************
#sum distance of function 
sumdistance<-function(c){
  for (i in 1:20){
    i=c[i]
    j=c[i+1]
    f=f+M[i,j]
  }
  return(f)
}
#***********************************************
#generate pandoa function
pandoa<-function(Nsim,Msim){
  g=0
  g1=0
  #run first n steps
  # Nsim=150
  for (n in 1:Nsim){
    c<-main(c,a,b)#input a new valid series
    #sum a valid series cities
    f<-sumdistance(c)
    g[n]=f
    f=0
  }
  z1<-mean(g)
  
  #let the series run another m steps
  # Msim=100
  for (m in 1:Msim){
    #random pick two number then swap them to create new chain. new chain, the variable is u
    c<-main(c,a,b)
    f<-sumdistance(c)
    g1[m]=f
    f=0
  }
  #output the result from m steps
  z2<-mean(g1)
  #output of the simulation   
  print(z1)
  print(z2)
  
}