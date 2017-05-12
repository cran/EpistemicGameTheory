#' Eliminating strictly dominated choices
#'
#' This function eliminates strictly dominated choices.
#'
#' @param n an integer representing the number of choices of player 1
#' @param m an integer representing the number of choices of player 2
#' @param A an nxm matrix representing the payoff matrix of player 1
#' @param choices.A a vector of length n representing the names of player 1's choices
#' @param B an nxm matrix representing the payoff matrix of player 2
#' @param choices.B a vector of length m representing the names of player 2's choices
#' @param iteration an integer representing the iteration number of algorithm
#' @return The reduced matrices of players' that are obtained after eliminating strictly dominated choices
#' @author Bilge Baser
#' @details This function works for the games with two players.
#' @export "esdc"
#' @importFrom "stats" runif
#' @importFrom "utils" combn
#' @examples
#' a=4
#' b=4
#' pay.A=matrix(c(0,3,2,1,4,0,2,1,4,3,0,1,4,3,2,0),4,4)
#' ch.A=c("Blue","Green","Red","Yellow")
#' pay.B=matrix(c(5,4,4,4,3,5,3,3,2,2,5,2,1,1,1,5),4,4)
#' ch.B=c("Blue","Green","Red","Yellow")
#' iter=5
#' esdc(a,b,pay.A,ch.A,pay.B,ch.B,iter)

esdc<-function(n,m,A,choices.A,B,choices.B,iteration){

  rownames(A)<-choices.A
  rownames(B)<-rownames(A)
  colnames(B)<-choices.B
  colnames(A)<-colnames(B)

  br=dim(B)[1]
  ac=dim(A)[2]
  t=1
  elimination=0

  Dim_A=dim(A)
  Dim_B=dim(B)
  print("The utility matrix of Player 1:")
  print(A)
  print("The utility matrix of Player 2:")
  print(B)

  repeat{
    if(n<2){
      br=1
    }
    else{
      br=dim(B)[1]

      while((n-t)>=1){ #Satir oyuncusunun strateji sayisi 2'den buyukse
        row=nrow(A)
        C<-t(combn(n,n-t))  #Kombinasyon matrisleri olusturmak icin
        N<-t(matrix(rep(1:n, ncol(combn(n,n-t))),n,ncol(combn(n,n-t))))
        if((ncol(N)-ncol(C))<2){ #D matrisinin sutun sayisi 1 ise
          D<-matrix(nrow=nrow(C),ncol=ncol(N)-ncol(C))
          for(dif in 1:nrow(C)){
            D[dif]=setdiff(N[dif,],C[dif,])# Nde olup Cde olmayan
            D<-as.matrix(D)
          }
        }else{ # D matrisinin sutun sayisi 1'den buyuk ise
          D<-matrix(nrow=nrow(C),ncol=ncol(N)-ncol(C))
          for(dif in 1:nrow(C)){
            D[dif, ]=setdiff(1:n,C[dif, ])
          }
        }
        Prob<-vector(mode="numeric",length=(n-t))
        R<-vector(mode="numeric",length=(n-t))
        P<-vector(mode="numeric",length=(n-t))
        interval=1
        csa=1
        while(csa<=nrow(C)){
          if(ncol(C)<2){
            if(ncol(D)<2){
              if(m<2){
                if(n<2){
                  A<-t(as.matrix(A))
                  break
                }
                else if (n==2){# if n=2
                  if(A[1]<A[2]){
                    A=A[-1]
                    elimination=elimination+1
                    n=n-1
                    t=1
                    if(n==1){
                      A<-t(as.matrix(A))
                      print(paste("Elimination:",elimination))
                      cat("For Player 1:",choices.A[1],"is strictly dominated by the choice",choices.A[2],"with probability",1,'\n')
                      print("The reduced utility matrix of Player 1:")
                      print(A)
                      print("The reduced utility matrix of Player 2:")
                      print(B)
                      choices.A<-rownames(A)
                      break
                    }
                    else{
                      print(paste("Elimination:",elimination))
                      cat("For Player 1:",choices.A[1],"is strictly dominated by the choice",choices.A[2],"with probability",1,'\n')
                      print("The reduced utility matrix of Player 1:")
                      print(A)
                      print("The reduced utility matrix of Player 2:")
                      print(B)
                      choices.A<-rownames(A)
                    }
                  }
                  else if(A[1]>A[2]) {
                    A=A[-2]
                    elimination=elimination+1
                    n=n-1
                    t=1
                    if(n==1){
                      A<-t(as.matrix(A))
                      print(paste("Elimination:",elimination))
                      cat("For Player 1:",choices.A[2],"is strictly dominated by the choice",choices.A[1],"with probability",1,'\n')
                      print("The reduced utility matrix of Player 1:")
                      print(A)
                      print("The reduced utility matrix of Player 2:")
                      print(B)
                      choices.A<-rownames(A)
                      break
                    }
                    else{
                      print(paste("Elimination:",elimination))
                      cat("For Player 1:",choices.A[2],"is strictly dominated by the choice",choices.A[1],"with probability",1,'\n')
                      print("The reduced utility matrix of Player 1:")
                      print(A)
                      print("The reduced utility matrix of Player 2:")
                      print(B)
                      choices.A<-rownames(A)
                    }
                  }
                  else{

                  }
                }
                else{
                  max=which(A==max(A),arr.ind = T)
                  max<-as.vector(max)
                  A=A[max[1],]
                  elimination=elimination+1
                  print(paste("Elimination For Player 1:",elimination))
                  print("The reduced utility matrix of Player 1:")
                  print(A)
                  print("The reduced utility matrix of Player 2:")
                  print(B)
                  choices.A<-rownames(A)
                  n=1
                  break
                }
              }#if m=1
              else{#if m>=2
                if(n<2){
                  A<-as.matrix(A)
                  print(A)
                  print(B)
                  break
                }
                else if(n==2){
                  if(all(A[1,]<A[2,])){
                    A=A[-1,]
                    B=B[-1,]
                    elimination=elimination+1
                    n=n-1
                    t=1
                    if(n==1){
                      A<-t(as.matrix(A))
                      print(paste("Elimination:",elimination))
                      cat("For Player 1:",choices.A[1],"is strictly dominated by the choice",choices.A[2],"with probability",1,'\n')
                      print("The reduced utility matrix of Player 1:")
                      print(A)
                      print("The reduced utility matrix of Player 2:")
                      print(B)
                      choices.A<-rownames(A)
                      break
                    }
                    else{
                      print(paste("Elimination:",elimination))
                      cat("For Player 1:",choices.A[1],"is strictly dominated by the choice",choices.A[2],"with probability",1,'\n')
                      print("The reduced utility matrix of Player 1:")
                      print(A)
                      print("The reduced utility matrix of Player 2:")
                      print(B)
                      choices.A<-rownames(A)
                    }

                  }
                  else if(all(A[2,]<A[1,])){
                    A=A[-2,]
                    B=B[-2,]
                    elimination=elimination+1
                    n=n-1
                    t=1
                    if(n==1){
                      A<-t(as.matrix(A))
                      print(paste("Elimination:",elimination))
                      cat("For Player 1:",choices.A[2],"is strictly dominated by the choice",choices.A[1],"with probability",1,'\n')
                      print("The reduced utility matrix of Player 1:")
                      print(A)
                      print("The reduced utility matrix of Player 2:")
                      print(B)
                      choices.A<-rownames(A)
                      break
                    }
                    else{
                      print(paste("Elimination:",elimination))
                      cat("For Player 1:",choices.A[2],"is strictly dominated by the choice",choices.A[1],"with probability",1,'\n')
                      print("The reduced utility matrix of Player 1:")
                      print(A)
                      print("The reduced utility matrix of Player 2:")
                      print(B)
                      choices.A<-rownames(A)
                    }
                  }
                  else{

                  }
                }
                else{

                }
                C<-t(combn(n,n-t))
                N<-t(matrix(rep(1:n,ncol(combn(n,n-t))),n,ncol(combn(n,n-t))))
                if((ncol(N)-ncol(C))<2){ #D matrisinin sutun sayisi 1 ise
                  D<-matrix(nrow=nrow(C),ncol=ncol(N)-ncol(C))
                  for(dif in 1:nrow(C)){
                    D[dif]=setdiff(N[dif,],C[dif,])# Nde olup Cde olmayan
                    D<-as.matrix(D)
                  }
                }else{ # D matrisinin sutun sayisi 1'den buyuk ise
                  D<-matrix(nrow=nrow(C),ncol=ncol(N)-ncol(C))
                  for(dif in 1:nrow(C)){
                    D[dif, ]=setdiff(1:n,C[dif, ])
                  }
                }
              }#if m>=2
            }#D<2
            else{#if D>=2
              C<-t(combn(n,n-t))
              N<-t(matrix(rep(1:n,ncol(combn(n,n-t))),n,ncol(combn(n,n-t))))
              if((ncol(N)-ncol(C))<2){ #D matrisinin s?tun say?s? 1 ise
                D<-matrix(nrow=nrow(C),ncol=ncol(N)-ncol(C))
                for(dif in 1:nrow(C)){
                  D[dif]=setdiff(N[dif,],C[dif,])# N?de olup C?de olmayan
                  D<-as.matrix(D)
                }
              }else{ # D matrisinin s?tun say?s? 1'den b?y?k ise
                D<-matrix(nrow=nrow(C),ncol=ncol(N)-ncol(C))
                for(dif in 1:nrow(C)){
                  D[dif, ]=setdiff(1:n,C[dif, ])
                }
              }
              for(z in 1:nrow(D)){
                k=1
                while(k<ncol(D)){
                  if(all(A[D[z,k],]<A[D[z,k+1],])){
                    A=A[-D[z,k],]
                    B=B[-D[z,k],]
                    n=n-1
                    t=1
                    elimination=elimination+1
                    if(n==1){
                      A<-t(as.matrix(A))
                      print(paste("Elimination:",elimination))
                      cat("For Player 1:",choices.A[D[z,k]],"is strictly dominated by the choice",choices.A[D[z,k+1]],"with the probability",1,'\n')
                      print("The reduced utility matrix of Player 1:")
                      print(A)
                      print("The reduced utility matrix of Player 2:")
                      print(B)
                      choices.A<-rownames(A)
                      break
                    }
                    else{

                      print(paste("Elimination:",elimination))
                      cat("For Player 1:",choices.A[D[z,k]],"is strictly dominated by the choice",choices.A[D[z,k+1]],"with the probability",1,'\n')
                      print("The reduced utility matrix of Player 1:")
                      print(A)
                      print("The reduced utility matrix of Player 2:")
                      print(B)
                      choices.A<-rownames(A)
                    }
                    C<-t(combn(n,n-t))
                    N<-t(matrix(rep(1:n,ncol(combn(n,n-t))),n,ncol(combn(n,n-t))))
                    if((ncol(N)-ncol(C))<2){ #D matrisinin s?tun say?s? 1 ise
                      D<-matrix(nrow=nrow(C),ncol=ncol(N)-ncol(C))
                      for(dif in 1:nrow(C)){
                        D[dif]=setdiff(N[dif,],C[dif,])# N?de olup C?de olmayan
                        D<-as.matrix(D)
                      }
                    }else{ # D matrisinin s?tun say?s? 1'den b?y?k ise
                      D<-matrix(nrow=nrow(C),ncol=ncol(N)-ncol(C))
                      for(dif in 1:nrow(C)){
                        D[dif, ]=setdiff(1:n,C[dif, ])
                      }
                    }
                  }#if
                  else if(all(A[D[z,k],]>A[D[z,k+1],])){
                    A=A[-D[z,k+1],]
                    B=B[-D[z,k+1],]
                    n=n-1
                    t=1
                    elimination=elimination+1
                    if(n==1){
                      A<-t(as.matrix(A))
                      print(paste("Elimination:",elimination))
                      cat("For Player 1:",choices.A[D[z,k+1]],"is strictly dominated by the choice",choices.A[D[z,k]],"with the probability",1,'\n')
                      print("The reduced utility matrix of Player 1:")
                      print(A)
                      print("The reduced utility matrix of Player 2:")
                      print(B)
                      choices.A<-rownames(A)
                      break
                    }
                    else{
                      print(paste("Elimination:",elimination))
                      cat("For Player 1:",choices.A[D[z,k+1]],"is strictly dominated by the choice",choices.A[D[z,k]],"with the probability",1,'\n')
                      print("The reduced utility matrix of Player 1:")
                      print(A)
                      print("The reduced utility matrix of Player 2:")
                      print(B)
                      choices.A<-rownames(A)
                    }
                    C<-t(combn(n,n-t))
                    N<-t(matrix(rep(1:n,ncol(combn(n,n-t))),n,ncol(combn(n,n-t))))
                    if((ncol(N)-ncol(C))<2){ #D matrisinin sutun sayisi 1 ise
                      D<-matrix(nrow=nrow(C),ncol=ncol(N)-ncol(C))
                      for(dif in 1:nrow(C)){
                        D[dif]=setdiff(N[dif,],C[dif,])# Nde olup Cde olmayan
                        D<-as.matrix(D)
                      }
                    }else{ # D matrisinin sutun sayisi 1'den buyuk ise
                      D<-matrix(nrow=nrow(C),ncol=ncol(N)-ncol(C))
                      for(dif in 1:nrow(C)){
                        D[dif, ]=setdiff(1:n,C[dif, ])
                      }
                    }
                  }#else if
                  else{
                  }
                  k=k+1
                }#k<ncol(D)
              }#z
            }#else D>2
          }#C<2
          else{
            for(s in 1:(iteration)){ # Kombinasyon matrisinin her bir satiri icin iteration kez olasilik vektoru uretmek icin
              Prob<-0
              P<-0
              R<-0
              k=1
              sum=0
              Ex<-vector(mode="numeric",length=m)
              Mul<-vector(mode="numeric",length=(n-t))
              counter=1
              for(j in 1:(n-t)){
                R[j]<-C[csa,j]
                P[R[j]]<-runif(1,min=0,max=interval)
                Prob[k]<-P[R[j]]
                sum<-sum+P[R[j]]
                interval<-(1-sum)
                counter=counter+1
                if(k==(n-(t+1))){
                  R[counter]<-C[csa,counter]
                  P[R[counter]]<-interval
                  Prob[(k+1)]<-P[R[counter]]
                  break
                }
                else {
                  k=k+1
                }
              }#for_j
              if(ncol(D)<2){
                Ex<-vector(mode="numeric",length=m)
                val<-vector(mode="numeric",length=m)
                for(e in 1:m){
                  c=1
                  while(c<=ncol(C)){ #Belirlenen olas?l?klarla beklenen de?erin hesaplanmas?
                    Mul[c]=Prob[c]*A[C[csa,c],e]
                    Ex[e]=Ex[e]+Mul[c]
                    c<-c+1
                  }#while_c
                  val[e]=A[D[csa],e]
                } #e

                if(all(Ex>val)){
                  elimination=elimination+1
                  A=A[-D[csa], ]
                  B=B[-D[csa], ]
                  n=n-1
                  if(n==1){
                    A<-t(as.matrix(A))
                    print(paste("Elimination:",elimination))
                    cat("For Player 1:",choices.A[D[csa]],"is strictly dominated by the randomization of the choices",choices.A[C[csa, ]],"with the probabilities",Prob,'\n')
                    print("The reduced utility matrix of Player 1:")
                    print(A)
                    print("The reduced utility matrix of Player 2:")
                    print(B)
                    choices.A<-rownames(A)
                    break
                  }else{
                    print(paste("Elimination:",elimination))
                    cat("For Player 1:",choices.A[D[csa]],"is strictly dominated by the randomization of the choices",choices.A[C[csa, ]],"with the probabilities",Prob,'\n')
                    print("The reduced utility matrix of Player 1:")
                    print(A)
                    print("The reduced utility matrix of Player 2:")
                    print(B)
                    choices.A<-rownames(A)
                  }
                  C<-t(combn(n,n-t))
                  N<-t(matrix(rep(1:n, ncol(combn(n,n-t))),n,ncol(combn(n,n-t))))
                  D<-matrix(nrow=nrow(C),ncol=ncol(N)-ncol(C))
                  for(dif in 1:nrow(C)){
                    D[dif, ]=setdiff(1:n,C[dif, ])
                  }
                  csa=0
                  break     # break den sonra s'nin d???na ??k?yor. E?er SD ise indirge ve yeni C olu?tur. SD de?ilse iterasyonlara devam et.
                }
                else{
                }
              } #if ncol(D)
              else{#if ncol(D)>=2
                for(ds in 1:ncol(D)){
                  val<-vector(mode="numeric",length=m)
                  for(e in 1:m){
                    c=1
                    while(c<=ncol(C)){ #Belirlenen olas?l?klarla beklenen de?erin hesaplanmas?
                      Mul[c]=Prob[c]*A[C[csa,c],e]
                      Ex[e]=Ex[e]+Mul[c]
                      c<-c+1
                    }#while_c

                    val[e]=A[D[csa,ds],e]
                  }#for e
                  if(all(Ex>val)){
                    elimination=elimination+1
                    A=A[-D[csa,ds], ]
                    B=B[-D[csa,ds], ]
                    n=n-1
                    if(n==1){
                      A<-t(as.matrix(A))
                      print(paste("Elimination:",elimination))
                      cat("For Player 1:",choices.A[D[csa,ds]],"is strictly dominated by the randomization of choices",choices.A[C[csa, ]],"with the probabilities",Prob,'\n')
                      print("The reduced utility matrix of Player 1:")
                      print(A)
                      print("The reduced utility matrix of Player 2:")
                      print(B)
                      choices.A<-rownames(A)
                      break
                    }else{
                      print(paste("Elimination:",elimination))
                      cat("For Player 1:",choices.A[D[csa,ds]],"is strictly dominated by the randomization of choices",choices.A[C[csa, ]],"with the probabilities",Prob,'\n')
                      print("The reduced utility matrix of Player 1:")
                      print(A)
                      print("The reduced utility matrix of Player 2:")
                      print(B)
                      choices.A<-rownames(A)
                    }
                    C<-t(combn(n,n-t))
                    N<-t(matrix(rep(1:n, ncol(combn(n,n-t))),n,ncol(combn(n,n-t))))
                    D<-matrix(nrow=nrow(C),ncol=ncol(N)-ncol(C))
                    for(dif in 1:nrow(C)){
                      D[dif, ]=setdiff(1:n,C[dif, ])
                    }
                    csa=0
                    break     # break den sonra s'nin d???na ??k?yor. E?er SD ise indirge ve yeni C olu?tur. SD de?ilse iterasyonlara devam et.

                  }#if all
                  else{

                  }
                } #for ds
                break
              }#else
            } #s
          }#C>=2
          csa=csa+1

        }#csa
        if(n==1){

          break
        }
        else{

        }

        if(row==nrow(A)){
          t=t+1
        }else{
          t=1
        }

      }#while(n-t)
    }
    A<-as.matrix(A)
    B<-as.matrix(B)

    if(m<2){
      ac=1
    }
    else{
      ac=dim(A)[2]


      t=1
      while((m-t)>=1){ #S?tun oyuncusunun strateji say?s? 2'den b?y?kse
        col=ncol(B)
        CC<-t(combn(m,m-t)) #Kombinasyon matrisleri olu?turmak i?in
        NN<-t(matrix(rep(1:m, ncol(combn(m,m-t))),m,ncol(combn(m,m-t))))
        if((ncol(NN)-ncol(CC))<2){ #DD matrisinin s?tun say?s? 1 ise
          DD<-matrix(nrow=nrow(CC),ncol=ncol(NN)-ncol(CC))
          for(dif in 1:nrow(CC)){
            DD[dif]=setdiff(NN[dif,],CC[dif, ])# N?de olup C?de olmayan
            DD<-as.matrix(DD)
          }
        }else{ # DD matrisinin s?tun say?s? 1'den b?y?k ise
          DD<-matrix(nrow=nrow(CC),ncol=ncol(NN)-ncol(CC))
          for(dif in 1:nrow(CC)){
            DD[dif, ]=setdiff(1:m,CC[dif, ])
          }
        }
        Prob<-vector(mode="numeric",length=(m-t))
        R<-vector(mode="numeric",length=(m-t))
        P<-vector(mode="numeric",length=(m-t))
        interval=1
        csu=1
        while(csu<=nrow(CC)){
          if(ncol(CC)<2){
            if(ncol(DD)<2){
              if(n<2){
                if(m<2){
                  B<-as.matrix(B)
                  break
                }
                else if(m==2){
                  if(B[1]<B[2]){
                    B=B[-1]
                    elimination=elimination+1
                    m=m-1
                    t=1
                    if(m==1){
                    B<-as.matrix(B)
                    print(paste("Elimination For Player 2:",elimination))
                    print("The reduced utility matrix of Player 1:")
                    print(A)
                    print("The reduced utility matrix of Player 2:")
                    print(B)
                      break
                    }
                    else{
                      print(paste("Elimination For Player 2:",elimination))
                      print("The reduced utility matrix of Player 1:")
                      print(A)
                      print("The reduced utility matrix of Player 2:")
                      print(B)
                    }
                  }
                  else if(B[2]<B[1]){
                    B=B[-2]
                    elimination=elimination+1
                    m=m-1
                    t=1
                    if(m==1){
                      B<-as.matrix(B)
                      print(paste("Elimination For Player 2:",elimination))
                      print("The reduced utility matrix of Player 1:")
                      print(A)
                      print("The reduced utility matrix of Player 2:")
                      print(B)
                      break
                    }
                    else{
                      print(paste("Elimination For Player 2:",elimination))
                      print("The reduced utility matrix of Player 1:")
                      print(A)
                      print("The reduced utility matrix of Player 2:")
                      print(B)
                    }
                  }
                  else{

                  }
                }#if m=2
                else{
                  maxx=(which(maxx==max(B),arr.ind = T))
                  maxx<-as.vector(maxx)
                  B=B[,max[2]]
                  elimination=elimination+1
                  print(paste("Elimination For Player 2:",elimination))
                  print("The reduced utility matrix of Player 1:")
                  print(A)
                  print("The reduced utility matrix of Player 2:")
                  print(B)
                  m=1
                  break
                }
              }#if n<2
              else{# if n>=2
                if(m==1){
                  B<-as.matrix(B)
                  print(B)
                  break
                }
                else if(m==2){
                  if(all(B[,1]<B[,2])){
                    B=B[,-1]
                    A=A[,-1]
                    elimination=elimination+1
                    m=m-1
                    t=1
                    if(m==1){
                      B<-as.matrix(B)
                      print(paste("Elimination:",elimination))
                      cat("For Player 2:",choices.B[1],"is strictly dominated by the choice",choices.B[2],"with probability",1,'\n')
                      print("The reduced utility matrix of Player 1:")
                      print(A)
                      print("The reduced utility matrix of Player 2:")
                      print(B)
                      break
                    }
                    else{
                      print(paste("Elimination:",elimination))
                      cat("For Player 2:",choices.B[1],"is strictly dominated by the choice",choices.B[2],"with probability",1,'\n')
                      print("The reduced utility matrix of Player 1:")
                      print(A)
                      print("The reduced utility matrix of Player 2:")
                      print(B)
                    }
                  }
                  else if(all(B[,2]<B[,1])){
                    A=A[,-2]
                    B=B[,-2]
                    elimination=elimination+1
                    m=m-1
                    t=1
                    if(m==1){
                      B<-as.matrix(B)
                      choices.B<-colnames(B)
                      print(paste("Elimination:",elimination))
                      cat("For Player 2:",choices.B[2],"is strictly dominated by the choice",choices.B[1],"with probability",1,'\n')
                      print("The reduced utility matrix of Player 1:")
                      print(A)
                      print("The reduced utility matrix of Player 2:")
                      print(B)
                      break
                    }
                    else{
                      choices.B<-colnames(B)
                      print(paste("Elimination:",elimination))
                      cat("For Player 2:",choices.B[2],"is strictly dominated by the choice",choices.B[1],"with probability",1,'\n')
                      print("The reduced utility matrix of Player 1:")
                      print(A)
                      print("The reduced utility matrix of Player 2:")
                      print(B)
                    }
                  }
                  else{

                  }
                }
                else{

                }
                CC<-t(combn(m,m-t)) #Kombinasyon matrisleri olu?turmak i?in
                NN<-t(matrix(rep(1:m, ncol(combn(m,m-t))),m,ncol(combn(m,m-t))))
                if((ncol(NN)-ncol(CC))<2){ #DD matrisinin s?tun say?s? 1 ise
                  DD<-matrix(nrow=nrow(CC),ncol=ncol(NN)-ncol(CC))
                  for(dif in 1:nrow(CC)){
                    DD[dif]=setdiff(NN[dif,],CC[dif, ])# N?de olup C?de olmayan
                    DD<-as.matrix(DD)
                  }
                }else{ # DD matrisinin s?tun say?s? 1'den b?y?k ise
                  DD<-matrix(nrow=nrow(CC),ncol=ncol(NN)-ncol(CC))
                  for(dif in 1:nrow(CC)){
                    DD[dif, ]=setdiff(1:m,CC[dif, ])
                  }
                }

              }#if n>=2
            }#DD<2
            else{#if DD>=2
              CC<-t(combn(m,m-t)) #Kombinasyon matrisleri olu?turmak i?in
              NN<-t(matrix(rep(1:m, ncol(combn(m,m-t))),m,ncol(combn(m,m-t))))
              if((ncol(NN)-ncol(CC))<2){ #DD matrisinin s?tun say?s? 1 ise
                DD<-matrix(nrow=nrow(CC),ncol=ncol(NN)-ncol(CC))
                for(dif in 1:nrow(CC)){
                  DD[dif]=setdiff(NN[dif,],CC[dif, ])# N?de olup C?de olmayan
                  DD<-as.matrix(DD)
                }
              }else{ # DD matrisinin s?tun say?s? 1'den b?y?k ise
                DD<-matrix(nrow=nrow(CC),ncol=ncol(NN)-ncol(CC))
                for(dif in 1:nrow(CC)){
                  DD[dif, ]=setdiff(1:m,CC[dif, ])
                }
              }
              for(z in 1:nrow(DD)){
                k=1
                while(k<ncol(DD)){
                  if(all(B[,DD[z,k]]<B[,DD[z,k+1]])){
                    B=B[,-DD[z,k]]
                    A=A[,-DD[z,k]]
                    elimination=elimination+1
                    m=m-1
                    t=1
                    if(m==1){
                      B<-as.matrix(B)
                      print(paste("Elimination:",elimination))
                      cat("For Player 2:",choices.B[DD[z,k]],"is strictly dominated by the choice",choices.B[DD[z,k+1]],"with probability",1,'\n')
                      print(A)
                      print(B)
                      choices.B<-colnames(B)
                      break
                    }
                    else{
                    }
                    CC<-t(combn(m,m-t)) #Kombinasyon matrisleri olu?turmak i?in
                    NN<-t(matrix(rep(1:m, ncol(combn(m,m-t))),m,ncol(combn(m,m-t))))
                    if((ncol(NN)-ncol(CC))<2){ #DD matrisinin s?tun say?s? 1 ise
                      DD<-matrix(nrow=nrow(CC),ncol=ncol(NN)-ncol(CC))
                      for(dif in 1:nrow(CC)){
                        DD[dif]=setdiff(NN[dif,],CC[dif, ])# N?de olup C?de olmayan
                        DD<-as.matrix(DD)
                      }
                    }else{ # DD matrisinin s?tun say?s? 1'den b?y?k ise
                      DD<-matrix(nrow=nrow(CC),ncol=ncol(NN)-ncol(CC))
                      for(dif in 1:nrow(CC)){
                        DD[dif, ]=setdiff(1:m,CC[dif, ])
                      }
                    }
                  }#if
                  else if(all(B[,DD[z,k]]>B[,DD[z,k+1]])){
                    A=A[,-DD[z,k+1]]
                    B=B[,-DD[z,k+1]]
                    elimination=elimination+1
                    m=m-1
                    t=1
                    if(m==1){
                      B<-as.matrix(B)
                      print(paste("Elimination:",elimination))
                      cat("For Player 2:",choices.B[DD[z,k+1]],"is strictly dominated by the choice",choices.B[DD[z,k]],"with probability",1,'\n')
                      print(A)
                      print(B)
                      choices.B<-rownames(B)
                      break
                    }
                    else{
                    }
                    CC<-t(combn(m,m-t)) #Kombinasyon matrisleri olu?turmak i?in
                    NN<-t(matrix(rep(1:m, ncol(combn(m,m-t))),m,ncol(combn(m,m-t))))
                    if((ncol(NN)-ncol(CC))<2){ #DD matrisinin s?tun say?s? 1 ise
                      DD<-matrix(nrow=nrow(CC),ncol=ncol(NN)-ncol(CC))
                      for(dif in 1:nrow(CC)){
                        DD[dif]=setdiff(NN[dif,],CC[dif, ])# N?de olup C?de olmayan
                        DD<-as.matrix(DD)
                      }
                    }else{ # DD matrisinin s?tun say?s? 1'den b?y?k ise
                      DD<-matrix(nrow=nrow(CC),ncol=ncol(NN)-ncol(CC))
                      for(dif in 1:nrow(CC)){
                        DD[dif, ]=setdiff(1:m,CC[dif, ])
                      }
                    }
                  }#else if
                  else{
                  }
                  k=k+1
                }#k<ncol(DD)
              }#z
            }#else DD>2
          }#CC<2

          else{#CC>=2
            for(s in 1:(iteration)){ # Kombinasyon matrisinin her bir sat?r? i?in iteration kez olas?l?k vekt?r? ?retmek i?in
              Prob<-0
              P<-0
              R<-0
              k=1
              sum=0
              Ex<-vector(mode="numeric",length=n)
              Mul<-vector(mode="numeric",length=(m-t))
              counter=1
              for(j in 1:(m-t)){
                R[j]<-CC[csu,j]
                P[R[j]]<-runif(1,min=0,max=interval)
                Prob[k]<-P[R[j]]
                sum<-sum+P[R[j]]
                interval<-(1-sum)
                counter=counter+1
                if(k==(m-(t+1))){
                  R[counter]<-CC[csu,counter]
                  P[R[counter]]<-interval
                  Prob[(k+1)]<-P[R[counter]]
                  break
                }
                else {
                  k=k+1
                }
              }#for_j

              if(ncol(DD)<2){
                Ex<-vector(mode="numeric",length=n)
                val<-vector(mode="numeric",length=n)
                for(e in 1:n){
                  c=1
                  while(c<=ncol(CC)){ #Belirlenen olas?l?klarla beklenen de?erin hesaplanmas?
                    Mul[c]=Prob[c]*B[e,CC[csu,c]]
                    Ex[e]=Ex[e]+Mul[c]
                    c<-c+1
                  }#while_c
                  val[e]=B[e,DD[csu]]
                } #e
                if(all(Ex>val)){
                  elimination=elimination+1
                  print(paste("Elimination:",elimination))
                  cat("For Player 2:",choices.B[DD[csu]],"is strictly dominated by the randomization of the choices",choices.B[CC[csu, ]],"with the probabilities",Prob,'\n')
                  B=B[,-DD[csu]]
                  A=A[,-DD[csu]]
                  print("The reduced utility matrix of Player 1:")
                  print(A)
                  print("The reduced utility matrix of Player 2:")
                  print(B)
                  choices.B<-colnames(B)
                  m=m-1
                  if(m==1){
                    B<-as.matrix(B)
                    break
                  }else{
                  }
                  CC<-t(combn(m,m-t))
                  NN<-t(matrix(rep(1:m, ncol(combn(m,m-t))),m,ncol(combn(m,m-t))))
                  DD<-matrix(nrow=nrow(CC),ncol=ncol(NN)-ncol(CC))
                  for(dif in 1:nrow(CC)){
                    DD[dif, ]=setdiff(1:m,CC[dif, ])
                  }
                  csu=0
                  break     # break den sonra s'nin d???na ??k?yor. E?er SD ise indirge ve yeni C olu?tur. SD de?ilse iterasyonlara devam et.

                }#all
                else{

                }
              } #if ncol(DD)<2
              else{#if ncol(DD)>=2
                for(ds in 1:ncol(DD)){
                  val<-vector(mode="numeric",length=n)
                  for(e in 1:n){
                    c=1
                    while(c<=ncol(CC)){ #Belirlenen olas?l?klarla beklenen de?erin hesaplanmas?
                      Mul[c]=Prob[c]*B[e,CC[csu,c]]
                      Ex[e]=Ex[e]+Mul[c]
                      c<-c+1
                    }#while_c
                    val[e]=B[e,DD[csu,ds]]
                  }#for e

                  if(all(Ex>val)){
                    elimination=elimination+1
                    print(paste("Elimination:",elimination))
                    cat("For Player 2:",choices.B[DD[csu,ds]],"is strictly dominated by the randomization of the choices",choices.B[CC[csu, ]],"with the probabilities",Prob,'\n')
                    B=B[,-DD[csu,ds]]
                    A=A[,-DD[csu,ds]]
                    print("The reduced utility matrix of Player 1:")
                    print(A)
                    print("The reduced utility matrix of Player 2:")
                    print(B)
                    choices.B<-colnames(B)
                    m=m-1
                    if(m==1){
                      B<-as.matrix(B)
                      break
                    }else{
                    }
                    CC<-t(combn(m,m-t))
                    NN<-t(matrix(rep(1:m, ncol(combn(m,m-t))),m,ncol(combn(m,m-t))))
                    DD<-matrix(nrow=nrow(CC),ncol=ncol(NN)-ncol(CC))
                    for(dif in 1:nrow(CC)){
                      DD[dif, ]=setdiff(1:m,CC[dif, ])
                    }
                    csu=0
                    break     # break den sonra s'nin d???na ??k?yor. E?er SD ise indirge ve yeni C olu?tur. SD de?ilse iterasyonlara devam et.

                  }#if all
                  else{

                  }
                } #for ds
                break
              }#else ncol(DD)>2
            } #s
          }#CC>=2
          csu=csu+1
        }#csu
        if(m==1){

          break
        }
        else{

        }
        if(col==ncol(B)){
          t=t+1
        }else{
          t=1
        }
      } #while(m-t)
    }
    ac_new=m
    br_new=n
    if(ac_new==ac&&br_new==br){
      break
    }
    else{

    }
  }
  print("ELIMINATION IS OVER.")
  print("The Last Reduced Matrix For Player 1:")
  if(n==1){
    print(A)
  }
  else{
  print(A)
  }
  print("The Last Reduced Matrix For Player 2:")
  print(B)
}

#'Finding types that express common belief in rationality for optimal choices
#'
#'This function takes the reduced payoff matrices and finds out the probabilities for the types that expresses common belief in rationality for optimal choices.
#'
#'
#' @param A an nxm matrix representing the reduced payoff matrix of player 1
#' @param B an nxm matrix representing the reduced payoff matrix of player 2
#' @param choices.A a vector of length n representing the names of player 1's choices
#' @param choices.B a vector of length m representing the names of player 2's choices
#' @return Probabilities of the types that expresses common belief in rationality for optimal choices
#' @author Bilge Baser
#' @details This function works for the games with two players. It returns infeasible solution for the irrational choices.
#' @export "type"
#' @importFrom "lpSolve" lp
#' @importFrom "utils" install.packages
#' @seealso \code{lp}
#' @examples
#' Ar=matrix(c(0,3,2,4,0,2,4,3,0),3,3)
#' choices.Ar=c("Blue","Green","Red")
#' Br=matrix(c(5,4,4,3,5,3,2,2,5),3,3)
#' choices.Br=c("Blue","Green","Red")
#' type(Ar,Br,choices.Ar,choices.Br)


type<-function(A,B,choices.A,choices.B){
  rownames(A)<-choices.A
  rownames(B)<-rownames(A)
  colnames(B)<-choices.B
  colnames(A)<-colnames(B)
  S<-vector(mode="numeric",length=ncol(A))
  Fa<-vector(mode="numeric",length=ncol(A)-1)
  SS<-vector(mode = "numeric",length = nrow(B))
  Fb<-vector(mode="numeric",length=nrow(B)-1)
  S<-c(1:ncol(A))
  SS<-c(1:nrow(B))
  print("The utility matrix of Player 1:")
  print(A)
  for (i in 1:nrow(A)) {
    Fark<-matrix(nrow=nrow(A)-1,ncol=ncol(A))
    for (j in 1:nrow(A)-1) {
      Fa<-setdiff(S,i)
      Fark[j,]<-A[i,]-A[Fa[j],]
    }
    if(nrow(A)>1){
    print(paste("The difference between the coefficients of the utility functions for the strategy",rownames(A)[i],":"))
    print(Fark)
    }
    else{

    }
    Kat<-rbind(Fark,rep(1,ncol(A)))
    f.obj<-vector(mode="numeric",length=ncol(A))
    f.obj<-c(A[i,])
    f.obj<-as.data.frame(f.obj)
    f.con<-matrix(Kat,nrow=nrow(A),byrow=TRUE)
    f.dir<-c(rep(">=",nrow(A)-1),"=")
    f.rhs<-c(rep(0,nrow(A)-1),1)
    Sols<-lp("max",f.obj,f.con,f.dir,f.rhs,transpose.constraints = FALSE)$solution
    cat("Player 1's type for the strategy",rownames(A)[i],":",Sols,"\n")
    Sol<-lp("max",f.obj,f.con,f.dir,f.rhs,transpose.constraints = FALSE)
    print(Sol)
  }
  print("The utility matrix of Player 2:")
  print(B)
  for (i in 1:ncol(B)) {
    Farkb<-matrix(nrow=nrow(B),ncol=ncol(B)-1)
    for (j in 1:ncol(B)-1) {
      Fb<-setdiff(SS,i)
      Farkb[,j]<-B[,i]-B[,Fb[j]]
    }
    if(ncol(B)>1){
    print(paste("The difference between the coefficients of the utility functions for the strategy",colnames(B)[i],":"))
    print(Farkb)
    }
    else{

    }
    Katb<-cbind(Farkb,rep(1,nrow(B)))
    fb.obj<-vector(mode="numeric",length=nrow(B))
    fb.obj<-c(B[,i])
    fb.obj<-as.data.frame(fb.obj)
    fb.con<-matrix(Katb,ncol=ncol(B))
    fb.dir<-c(rep(">=",ncol(B)-1),"=")
    fb.rhs<-c(rep(0,ncol(B)-1),1)
    Solsb<-lp("max",fb.obj,fb.con,fb.dir,fb.rhs,transpose.constraints = FALSE)$solution
    cat("Player 2's Type For The Strategy",colnames(B)[i],":",Solsb,"\n")
    Solb<-lp("max",fb.obj,fb.con,fb.dir,fb.rhs,transpose.constraints = FALSE)
    print(Solb)
  }

}

