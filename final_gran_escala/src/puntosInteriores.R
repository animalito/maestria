puntosinteriores=function(m,n,Amod,Ax,Ay,e,b,C,tol,sigma,x,y,s,v,
                                  lambda,mu,iter,paralelo){
  
  source('SolSistParalelo.R')
  tau=0.9995
  itot=1e5

  #Asignación de variables en método de PI
  X=.sparseDiagonal(length(x),x)
  UmenosX=.sparseDiagonal(m,C*e)-X
  S=.sparseDiagonal(length(s),s)
  V=.sparseDiagonal(length(v),v)
  rcy=Matrix(y-t(Ay)%*%lambda,sparse=T)
  rcx1=Matrix(v-t(Ax)%*%lambda-e-s,sparse=T)
  Xinv_mu_sigma_e=solve(X,mu*sigma*e)
  UmenosXinv_mu_sigma_e=solve(UmenosX,mu*sigma*e)
  rcx=Matrix(-t(Ax)%*%lambda-e-Xinv_mu_sigma_e+UmenosXinv_mu_sigma_e,sparse=T)
  rb=Matrix(-(Ay%*%y+Ax%*%x),sparse=T)
  F3=Matrix(X%*%s-mu*sigma*e,sparse=T)
  F4=Matrix(UmenosX%*%v-mu*sigma*e,sparse=T)
  Fmod=Matrix(rBind(rcy,rcx,rb),sparse=T)
  gradF=Matrix(rBind(rcy,rcx1,F3,F4,rb),sparse=T)
  #La siguiente línea para detener el método
  nF=sqrt(sum(gradF*gradF))
  while(nF>tol && iter<itot){
    #Las siguientes líneas para monitoreo del método
    obj=as.numeric(1/2*(t(y)%*%y)-t(e)%*%x-mu*(sum(log(x))+sum(log(C*e-x))));
    nrcy=sqrt(sum(rcy*rcy));
    nrcx1=sqrt(sum(rcx1*rcx1))
    nrb=sqrt(sum(rb*rb));
    nF3=sqrt(sum(F3*F3));
    nF4=sqrt(sum(F4*F4));

    #Variables del método de PI
    Xinv_S=solve(X,S)
    UmenosXinv_V=solve(UmenosX,V)
    Tetainv=Xinv_S+UmenosXinv_V
    J=Matrix(rBind(cBind(.sparseDiagonal(n,1),Matrix(0,n,m),-t(Ay)),
                   cBind(Matrix(0,m,n),Tetainv,-t(Ax)),
                   cBind(-Ay,-Ax,Matrix(0,n+1,(n+1)))),sparse=T)

    #Monitoreo del número de condición de la Jacobiana:
    cJ=condest(J)$est 

    #Será el método en paralelo o secuencial?
    if(paralelo==FALSE){      
      d=Matrix(solve(J,-Fmod,sparse=T),sparse=T);
      dy=d[1:n]
      dx=d[(n+1):(n+m)]
      dl=d[(n+m+1):(n+m+n+1)]
      
    }
      
    if(paralelo==TRUE){

      #Esta sección es la que se debe de adaptar al clúster MPI:

      H2=cBind(Matrix(0,m,n),Tetainv)
      if(iter==0){
        H1=cBind(.sparseDiagonal(n,1),Matrix(0,n,m))
        respParalelo=SolSistParalelo(n,m,H1,H2,
                                     -Amod,-rBind(rcy,rcx),-rb,iter,NULL)
        
      }
      else{
        respParalelo=SolSistParalelo(n,m,H1,H2,
                                     -Amod,-rBind(rcy,rcx),-rb,iter,respParalelo[[4]]) 
      }
      dy=respParalelo[[1]]
      dx=respParalelo[[2]]
      dl=respParalelo[[3]]      
    }

 
    #Variables del método de PI
    ds=solve(X,-F3-S%*%dx)  
    dv=solve(UmenosX,-F4+V%*%dx)

    #Para cortar el paso de Newton las siguientes líneas se ejecutan:
    indx=dx<0;
    ax=min(c(1,-x[indx]/dx[indx]));
    indx2=dx>0
    ax2=min(c(1,(C*e-x)[indx2]/dx[indx2]))
    inds=as(ds<0,'lgCMatrix')
    as=min(c(1,-s[inds@i+1]/ds[inds@i+1]))
    indv=as(dv<0,'lgCMatrix');
    av=min(c(1,-v[indv@i+1]/dv[indv@i+1]))
    alfa=tau*min(c(ax,as,av,ax2));

    #Impresión de monitoreo:
    cat(sprintf('%3i  %2.4e   %2.4e   %2.4e  %2.4e  %2.4e  %2.4e  %2.4e  %2.4e  %2.4e\n',
                iter,nF,nrcx1,nrb,nF3,nF4,obj,mu,alfa,cJ));                 

    #Tomar el paso (ya cortado)
    x=x+alfa*dx;
    y=y+alfa*dy;
    lambda=lambda+alfa*dl;
    s=s+alfa*ds;
    v=v+alfa*dv;

    #Actualización de variables de PI:

    X=.sparseDiagonal(length(x),x)
    UmenosX=.sparseDiagonal(m,C*e)-X
    S=.sparseDiagonal(length(s),as.numeric(s))
    V=.sparseDiagonal(length(v),as.numeric(v))
    rcy=Matrix(y-t(Ay)%*%lambda,sparse=T)
    rcx1=Matrix(v-t(Ax)%*%lambda-e-s,sparse=T)
    Xinv_mu_sigma_e=solve(X,mu*sigma*e)
    UmenosXinv_mu_sigma_e=solve(UmenosX,mu*sigma*e)
    rcx=Matrix(-t(Ax)%*%lambda-e-Xinv_mu_sigma_e+UmenosXinv_mu_sigma_e,sparse=T)
    rb=Matrix(-(Ay%*%y+Ax%*%x),sparse=T)
    F3=Matrix(X%*%s-mu*sigma*e,sparse=T)
    F4=Matrix(UmenosX%*%v-mu*sigma*e,sparse=T)
    Fmod=Matrix(rBind(rcy,rcx,rb),sparse=T)
    gradF=Matrix(rBind(rcy,rcx1,F3,F4,rb),sparse=T)
    nF=sqrt(sum(gradF*gradF))
    mu=as.numeric((t(x)%*%s+t(C*e-x)%*%v)/(2*m));
    iter=iter+1;
    
  }
  if(paralelo==TRUE){
    
    stopCluster(respParalelo[[4]])
    
  }
  
  list(nF,iter,x,y,lambda,mu)
  
}