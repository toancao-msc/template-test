#project math 4430
install.packages("expm")
library("expm")
#Generate time-cost matrix
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
#generate distance matrix


#*******************************

#generate new series moves.
h<-c(1:20)
c<-sample(h)
c[21]<-c[1]
d<-c(2:20)
e<-sample(d,2)


#generate pandoa function
pandoa<-function(Nsim,Msim){
  g=0
  g1=0
  #run first n steps
  # Nsim=150
  #simulation start from here
  for (n in 1:Nsim){ 
    #random pick two number then swap them to create new chain. new chain, the variable is u
    e<-sample(d,2)
    u<-replace(c,c(e[1],e[2]),c[c(e[2],e[1])])
    
    #generate a valid series cities (it will repeat ,until it get a valid series)
    while(b1==1 || b2==1){
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
        if(length(q1)==0 || length(q2)==0)
        {a1=0} else if
        (q1==q2){
          b1=1
        } 
        
        if(b1==1){e<-sample(d,2)
        u<-replace(c,c(e[1],e[2]),c[c(e[2],e[1])])}
        #checking left side
        if(length(p1)==0 || length(p2)==0)
        {a1=0} else if
        (p1==p2){
          b2=1} 
        if(b2==1)
        {
          e<-sample(d,2)
          u<-replace(c,c(e[1],e[2]),c[c(e[2],e[1])])}
      }
    }
    
    c<-u #input a new valid series
    #sum a valid series cities
    for (i in 1:20){
      i=c[i]
      j=c[i+1]
      f=f+M[i,j]
    }
    g[n]=f
    f=0
  }
  z1<-mean(g)
  
  #let the series run anothe m steps
  # Msim=100
  for (m in 1:Msim){
    #random pick two number then swap them to create new chain. new chain, the variable is u
    e<-sample(d,2)
    u<-replace(c,c(e[1],e[2]),c[c(e[2],e[1])])
    #generate a valid series cities (it will repeat ,until it get a valid series)
    while(b1==1 || b2==1){
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
        if(length(q1)==0 || length(q2)==0)
        {a1=0} else if
        (q1==q2){
          b1=1
        } 
        
        if(b1==1){e<-sample(d,2)
        u<-replace(c,c(e[1],e[2]),c[c(e[2],e[1])])}
        #checking left side
        if(length(p1)==0 || length(p2)==0)
        {a1=0} else if
        (p1==p2){
          b2=1} 
        if(b2==1)
        {
          e<-sample(d,2)
          u<-replace(c,c(e[1],e[2]),c[c(e[2],e[1])])}
      }
    }
    
    c<-u #save a new valid series
    #sum a valid series cities
    for (i in 1:20){
      i=c[i]
      j=c[i+1]
      f=f+M[i,j]
    }
    g1[m]=f
    f=0
  }
  #output the result from m steps
  z2<-mean(g1)
  
  print(z1)
  print(z2)
  
}

