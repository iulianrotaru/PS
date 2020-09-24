#X si Y v.a
#XY repartitia comuna a X si Y
frepcomgen <- function(m,n) {
  #set.seed(1)
  X <- sample(n:100,m,replace = TRUE) #generez X
  sum <- 0
  for(i in 1:m)
    sum=sum+X[i]
  for(i in 1:m)
    X[i]=X[i]/sum
  
  Y <- sample(m:100,n,replace = TRUE) #generez Y
  sum <- 0
  for(j in 1:n)
    sum=sum+Y[j]
  for(j in 1:n)
    Y[j]=Y[j]/sum
  
  #generez XY
  XY <- matrix(nrow = m,ncol = n)
  for(i in 1:m)
    for(j in 1:n)
      XY[i,j] <- X[i]*Y[j]
  ###########
  #elimin o linie intreaga random I
  I <- sample(1:m,1)
  for(j in 1:n)
    XY[I,j] <- 0
  #elimin pe celelalte m-1 linii cate o celula random
  for(i in 1:m)
    if(i!=I)
    {
      J <- sample(1:n,1)
      XY[i,J] <- 0
    }
  #50% sanse ca X si Y sa fie independente
  #0 sa fie 1 sa nu fie
  ceva <- sample(0:1,1)
  if(ceva==1) # in cazul in care nu sunt independente
  {
    #modific valorile a K valori din XY
    celule <- (n*m) -n -m+1
    K <- sample(1:celule,1)
    for(i in 1:K) {
      ok <- 0
      #cat timp caut o celula diferita de 0
      while(ok<1) {
        #generez ii si jj
        ii <- sample(1:m,1)
        jj <- sample(1:n,1)
        if(XY[ii,jj]!=0) {
          #sterg valoarea lui ii,jj
          maxim <- XY[ii,jj]
          XY[ii,jj]=0
          #ii atribui o valoare random mai mica decat cea anterioara
          while(ok<1) {
            
            cel <- runif(1,0,maxim)
            if(cel != 0 && cel != maxim) {
              if(maxim - cel < 0.001) {
                XY[ii,jj] = cel
                ok = 1
              }
            }
          }
        }
      }
    }
  }
  I <- sample(1:m,1)
  X[I] <- 0
  J <- sample(1:n,1)
  Y[J] <- 0
  ans <- matrix(nrow = m+1 ,ncol = n+1)
  for(i in 1:m)
    for(j in 1:n)
      ans[i,j] <- XY[i,j]
  for(i in 1:m)
    ans[i,n+1] <-X[i]
  for(j in 1:n)
    ans[m+1,j] <-Y[j]
  ans[m+1,n+1] <- 1
  write.table(ans,file="test.txt",sep=" ",eol="\n",dec=".",row.names = FALSE,col.names = FALSE)
  CC <- read.table(file="test.txt",sep=" ")
  print(CC)
}

fcomplrepcom <-function() {
  matrice <- read.table(file="test.txt",sep=" ")
  m <- length(matrice[,1])
  n <- length(matrice[1,])
  m_ <- m-1
  n_ <- n-1
  #completez daca gasesc pe Y valoare lipsa
  nr0 <- 0
  newval <- 1
  for(i in 1:n_) {
    if(matrice[m,i]==0) 
      nr0 <- nr0 +1
    newval <- newval - matrice[m,i]
  }
  if(nr0>1) {
    print("NU se poate completa")
    return
  }
  for(i in 1:n_)
    if(matrice[m,i]==0)
      matrice[m,i]=newval
  #completez daca gasesc pe X valoare lipsa
  nr0 <- 0
  newval <- 1
  for(i in 1:m_) {
    if(matrice[i,n]==0) nr0 <- nr0 +1
    newval <- newval - matrice[i,n]
  }
  if(nr0>1) {
    print("NU se poate completa")
    return
  }
  for(i in 1:m_)
    if(matrice[i,n]==0)
      matrice[i,n]=newval
  #completez in interior
  ok<-1
  while(ok==1) {
    ok=0
    # completez pe linii
    for(i in 1:m_) {
      newval<- matrice[i,n]
      nr0 <- 0
      for(j in 1:n_) {
        if(matrice[i,j]==0)
          nr0 <- nr0 +1
        newval <- newval - matrice[i,j]
      }
      if(nr0==1) {
        ok=1
        for(j in 1:n_)
          if(matrice[i,j]==0)
            matrice[i,j]=newval
      }
    }
    #completez pe coloane
    for(j in 1:n_) {
      newval<- matrice[m,j]
      nr0 <- 0
      for(i in 1:m_) {
        if(matrice[i,j]==0) 
          nr0 <- nr0 +1
        newval <- newval - matrice[i,j]
      }
      if(nr0==1) {
        ok=1
        for(i in 1:m_)
          if(matrice[i,j]==0)
            matrice[i,j]=newval
      }
    }
    
  }
  nr0 <- 0
  for(i in 1:m)
    for(j in 1:n)
      if(matrice[i,j]==0)
        nr0 = nr0 +1
  if(nr0>1) {
    print("NU se poate completa")
    return
  }
  write.table(matrice,file="rezolvare.txt",sep=" ",eol="\n",dec=".",row.names = FALSE,col.names = FALSE)
  print(matrice)
}

fverind <- function() {
  matrice <- read.table(file="rezolvare.txt",sep=" ")
  m <- length(matrice[,1])
  n <- length(matrice[1,])
  m_ <- m-1
  n_ <- n-1
  ok <- 1
  for(i in 1:m_)
    for(j in 1:n_) {
      maxim <- matrice[i,j]
      minim <- matrice[i,n]*matrice[m,j]
      if(maxim<minim) {
        aux <- maxim
        maxim <- minim
        minim <- aux
      }
      if(maxim-minim>0.000001)
        ok=0
    }
  if(ok==0)
    print("X si Y sunt dependente")
  else print("X si Y sunt independente")
}

fvernecor <- function() {
  matrice <- read.table(file="rezolvare.txt",sep=" ")
  m <- length(matrice[,1])
  n <- length(matrice[1,])
  m_ <- m-1
  n_ <- n-1
  Ex <- 0
  for(i in 1:m_)
    Ex = Ex + i*matrice[i,n]
  Ey <- 0
  for(j in 1:n_)
    Ey = Ey + j*matrice[m,j]
  n_m_ <- n_*m_
  XY <- 1:n_m_
  for(i in 1:n_m_)
    XY[i] <- 0
  for(i in 1:m_)
    for(j in 1:n_)
      XY[i*j] = XY[i*j] + matrice[i,j]
  Exy <- 0
  for(i in 1:n_m_)
    Exy = Exy + i*XY[i]
  covarianta = Exy - Ex*Ey
  if(covarianta < 0.000001 && covarianta > -0.000001)
    print("X si Y sunt necorelate")
  else print("X si Y sunt corelate")
}
cerintaC <- function() {
  matrice <- read.table(file="rezolvare.txt",sep=" ")
  m <- length(matrice[,1])
  n <- length(matrice[1,])
  m_ <- m-1
  n_ <- n-1
  #####         cov(5x,-3y)
  #####         = -15 cov(x,y)
  Ex <- 0
  for(i in 1:m_)
    Ex = Ex + i*matrice[i,n]
  Ey <- 0
  for(j in 1:n_)
    Ey = Ey + j*matrice[m,j]
  n_m_ <- n_*m_
  XY <- 1:n_m_
  for(i in 1:n_m_)
    XY[i] <- 0
  for(i in 1:m_)
    for(j in 1:n_)
      XY[i*j] = XY[i*j] + matrice[i,j]
  Exy <- 0
  for(i in 1:n_m_)
    Exy = Exy + i*XY[i]
  covariantaxy = Exy - Ex*Ey
  covarianta5x_3y = -15 *covariantaxy
  print("cov(5x,-3y) = ")
  print(covarianta5x_3y) # cov(5x,-3y)
  
  #P(0<X<3/Y>2)
  p1 <- 0
  for(i in 1:m_)
    for(j in 1:n_)
      if(i<3||j>2)
        p1 = p1 + matrice[i,j]
  print("P(0<X<3/Y>2) = ")
  print(p1)
  
  #P(X>6,Y<7)
  p2 <- 0
  for(i in 1:m_)
    for(j in 1:n_)
      if(i>6&&j<7)
        p2 = p2 + matrice[i,j]
  print("P(X>6,Y<7) = ")
  print(p2)
}
#Apelurile functiilor pentru a rezolva cerintele
frepcomgen(4,3) # aici imi setez m si n cat de mari sa fie         ####rezolv cerinta a)
fcomplrepcom()  # aici rezolv modelul de la cerinta a)             ####revolv cerinta b)
cerintaC()      # aici rezolv cerintele pentru rezolvarea de la b) ####rezolv cerinta c)
fverind()       # aici rezolv cerintele pentru rezolvarea de la b) ####rezolv cerinta d) prima functie
fvernecor()     # aici rezolv cerintele pentru rezolvarea de la b) ####rezolv cerinta d) a doua functie