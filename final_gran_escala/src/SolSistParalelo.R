SolSistParalelo=function(n,m,H1,H2,Amod,rc,rb,iter,cluster){
  
  p=detectCores()
  if(iter==0){
  
  cluster=makeCluster(p)
  clusterEvalQ(cluster,
{library(Matrix)
 NULL})
clusterExport(cluster,list('Amod'),envir=environment())
  }
  np=floor(seq(1,n+m,length.out=(p+1)))
  
  fun1=function(Hs,A){
    A%*%solve(Hs,t(A))
  }
  
  clusterExport(cluster,list('H2','fun1','np'),envir=environment())
  
  
  ejecutar.tarea1=function(t){
    H=rBind(H1,H2)
    
    if(t==1){
      return(fun1(.sparseDiagonal((np[t+1]-np[t]+1),H@x[np[t]:np[t+1]]),
                  Amod[,np[t]:np[t+1]]))
      
    }
    else{
      return(fun1(.sparseDiagonal((np[t+1]-np[t]),H@x[(np[t]+1):np[t+1]]),
                  Amod[,(np[t]+1):np[t+1]]))
      
    } 
  }
  
  C=Cholesky(-1*Reduce('+',clusterApply(cluster,1:p,ejecutar.tarea1)))
  fun2=function(Hs,A,r){
    A%*%solve(Hs,r)
  }
    
  clusterExport(cluster,list('fun2','rc'),envir=environment())
  
  
  ejecutar.tarea2=function(t){
    H=rBind(H1,H2)
    
    if(t==1){
      return(fun2(.sparseDiagonal((np[t+1]-np[t]+1),H@x[np[t]:np[t+1]]),
                  Amod[,np[t]:np[t+1]],rc[np[t]:np[t+1]]))
    }
    else{
      return(fun2(.sparseDiagonal((np[t+1]-np[t]),H@x[(np[t]+1):np[t+1]]),
                  Amod[,(np[t]+1):np[t+1]],rc[(np[t]+1):np[t+1]]))
    }
  }
  suma1=-1*Reduce('+',clusterApply(cluster,1:p,ejecutar.tarea2))
  dlambda=solve(C,rb+suma1,'LDLt')
  
  
  fun3=function(Hs,A,r,dl){
    solve(Hs,r)-solve(t(Hs),t(A)%*%dl)
  }
  
  clusterExport(cluster,list('dlambda','fun3'),envir=environment())
  
  
  ejecutar.tarea3=function(t){
    H=rBind(H1,H2)
    
    if(t==1){
      return(fun3(.sparseDiagonal((np[t+1]-np[t]+1),H@x[np[t]:np[t+1]]),
                  Amod[,np[t]:np[t+1]],rc[np[t]:np[t+1]],
             dlambda))
    }
    else{
      return(fun3(.sparseDiagonal((np[t+1]-np[t]),H@x[(np[t]+1):np[t+1]]),
                  Amod[,(np[t]+1):np[t+1]],rc[(np[t]+1):np[t+1]],
                  dlambda))

    }
  }
  dzeta=do.call(rBind,clusterApply(cluster,1:p,ejecutar.tarea3))
  
  list(dzeta[1:n],dzeta[(n+1):(n+m)],dlambda,cluster)
  

}


