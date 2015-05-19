source('puntosInteriores.R')
library(Matrix)
library(parallel)
options(digits=16)

#lectura de datos de entrenamiento

Train=read.csv('Train.csv',stringsAsFactors=F,header=F)

#Asignación de variables de acuerdo a la nomenclatura de Puntos Interiores (2a versión)

dim_Train=dim(Train)
m=dim_Train[1]
n=dim_Train[2]
X_est=scale(Train[,2:n],center=T,scale=T);
X_est=t(X_est);
ind=Train[,1]==0;
b=Train[,1];
b[ind]=-1;
n=n-1;
Y=.sparseDiagonal(length(b),b)    
A=Matrix(X_est%*%Y,sparse=T);
e=(vector('numeric',m)+1)
Ay=rBind(.sparseDiagonal(n,1),0)
Ax=rBind(-A,-t(b))

#Puntos iniciales para PI
C=1e3;
x=(C/2)*e;
y=A%*%x;
v=x;
s=x;
lambda=rBind(y,1);
Amod=Matrix(cBind(Ay,Ax),sparse=T)
mu=as.numeric((t(x)%*%s+t(C*e-x)%*%v)/(2*m));

cat(sprintf('   i    |F|       |rcx1|        |rb|          |f3|        |f4|          obj          mu        alfa       cond\n'))

#Puntos interiores:
#Si se desea secuencial escribir FALSE, si se desea paralelo escribir TRUE
resPI=puntosinteriores(m,n,Amod,Ax,Ay,e,b,C,1e-6,.1,x,y,s,v,lambda,mu,0,TRUE)
nF=resPI[[1]]
iter=resPI[[2]]
x=resPI[[3]]
y=resPI[[4]]
lambda=resPI[[5]]
mu=resPI[[6]]


#Estimación de w0: 
ind1=x>0
ind2=x>0 & x<C
X_est_aux1=X_est[,ind1]
X_est_aux2=X_est[,ind2]
baux1=b[ind1]
baux2=b[ind2]
xaux=x[ind1]
aux=matrix(0,length(baux2))
for(j in 1:length(baux2)){
 suma=sum(t(xaux)*t(baux1)*(t(X_est_aux2[,j])%*%X_est_aux1))
  aux[j]=baux2[j]-suma;
}

print('intercepto')
w0=mean(aux)
print(w0)

#Clasificaci?n en el conjunto de entrenamiento
  y_ent=Matrix(0,m,1);
  for(j in 1:m){
    y_ent[j]=sign(sum(xaux*baux1*(t(X_est_aux1)%*%X_est[,j]))+w0);
  }
   
#Tasa de clasificaci?n incorrecta:
print('En entrenamiento')
print(mean(b!=y_ent))

#Clasificaci?n en el conjunto de prueba

Test=read.csv('Test.csv',stringsAsFactors=F,header=F)
dim_prueba=dim(Test)
mprueba=dim_prueba[1]
nprueba=dim_prueba[2]
b_prueba=Test[,1]
ind_prueba=b_prueba==0
b_prueba[ind_prueba]=-1
X_prueba=t(scale(Test[,2:nprueba],center=T,scale=T))
nprueba=nprueba-1

y_prueba=Matrix(0,mprueba,1)
 for(j in 1:mprueba){
   y_prueba[j]=sign(sum(xaux*baux1*(t(X_est_aux1)%*%X_prueba[,j]))+w0);
}

#Tasa de clasificaci?n incorrecta:
print('En prueba')
print(mean(b_prueba!=y_prueba))
   