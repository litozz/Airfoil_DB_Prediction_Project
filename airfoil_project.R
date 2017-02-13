#LECTURA
datos <- read.delim("./airfoil_self_noise.dat", header=FALSE)
colnames(datos)<-c("freq","aattack","clength","fsvel","ssdisp","sound")

#BARAJAR DATOS
set.seed(199)
orden<-runif(nrow(datos))
datos<-datos[order(orden),]


##############REGRESION LINEAL CON WEIGHT DECAY LASSO##################
library(glmnet)

set.seed(199)
x <- as.matrix(datos[,-ncol(datos)])
y <- as.matrix(datos[,ncol(datos)])

#Elegimos el mejor lambda por validación cruzada de 10 particiones
cv<-cv.glmnet(x,y,alpha=1,nfolds=10)
plot(cv$glmnet.fit, "norm",ylim=c(-1,0.6),col=c(1:length(datos)-1))
best_lambda<-cv$lambda.min
abline(h=best_lambda,col="orange",lty=4,lwd=3)
print(paste("El mejor lambda es:",best_lambda))


nparticiones<-5
parts <- cut(seq(1,nrow(datos)),breaks=nparticiones,labels=FALSE)
errors_cv_rl<-numeric(nparticiones)

for (i in 1:nparticiones){
  testIndexes <- which(parts==i,arr.ind=TRUE)
  trainIndexes<- setdiff(as.numeric(rownames(datos)),testIndexes)
  
  conj_test_cv<-datos[testIndexes,]
  conj_training_cv<-datos[trainIndexes,]
  
  x <- as.matrix(conj_training_cv[,-ncol(conj_training_cv)])
  y <- as.matrix(conj_training_cv[,ncol(conj_training_cv)])
  
  mdl_lasso <- glmnet(x,y,lambda=best_lambda , family="gaussian",alpha=1)
  pred_lasso<-predict(mdl_lasso,as.matrix(conj_test_cv[,-ncol(conj_test_cv)]),s=best_lambda)
  error_lasso<-mean( (pred_lasso - conj_test_cv[,ncol(conj_test_cv)])*(pred_lasso - conj_test_cv[,ncol(conj_test_cv)]) )
  
  errors_cv_rl[i]<-error_lasso
}
print(paste("Error en test validacion cruzada 5cv:",mean(errors_cv_rl)))
#######################################################################


##############REGRESION LINEAL CON WEIGHT DECAY RIDGE##################
library(glmnet)

set.seed(199)
x <- as.matrix(datos[,-ncol(datos)])
y <- as.matrix(datos[,ncol(datos)])

#Elegimos el mejor lambda por validación cruzada de 10 particiones
cv<-cv.glmnet(x,y,alpha=0,nfolds=10)
plot(cv$glmnet.fit, "norm",ylim=c(-1,0.6),col=c(1:length(datos)-1))
best_lambda<-cv$lambda.min
abline(h=best_lambda,col="orange",lty=4,lwd=3)
print(paste("El mejor lambda es:",best_lambda))


nparticiones<-5
parts <- cut(seq(1,nrow(datos)),breaks=nparticiones,labels=FALSE)
errors_cv_rl<-numeric(nparticiones)

for (i in 1:nparticiones){
  testIndexes <- which(parts==i,arr.ind=TRUE)
  trainIndexes<- setdiff(as.numeric(rownames(datos)),testIndexes)
  
  conj_test_cv<-datos[testIndexes,]
  conj_training_cv<-datos[trainIndexes,]
  
  x <- as.matrix(conj_training_cv[,-ncol(conj_training_cv)])
  y <- as.matrix(conj_training_cv[,ncol(conj_training_cv)])
  
  mdl_lasso <- glmnet(x,y,lambda=best_lambda , family="gaussian",alpha=0)
  pred_lasso<-predict(mdl_lasso,as.matrix(conj_test_cv[,-ncol(conj_test_cv)]),s=best_lambda)
  error_lasso<-mean( (pred_lasso - conj_test_cv[,ncol(conj_test_cv)])*(pred_lasso - conj_test_cv[,ncol(conj_test_cv)]) )
  
  errors_cv_rl[i]<-error_lasso
}
print(paste("Error en test validacion cruzada 5cv:",mean(errors_cv_rl)))
#######################################################################

































##############TUNING REGRESION LINEAL PARA VER EL MEJOR ALPHA##################
library(glmnet)
set.seed(199)

error_lasso_vector<-numeric(11)
for(j in 0:10){
  
  nparticiones<-5
  parts <- cut(seq(1,nrow(datos)),breaks=nparticiones,labels=FALSE)
  errors_cv_rl<-numeric(nparticiones)
  
  for (i in 1:nparticiones){
    testIndexes <- which(parts==i,arr.ind=TRUE)
    trainIndexes<- setdiff(as.numeric(rownames(datos)),testIndexes)
    
    conj_test_cv<-datos[testIndexes,]
    conj_training_cv<-datos[trainIndexes,]
    
    x <- as.matrix(conj_training_cv[,-ncol(conj_training_cv)])
    y <- as.matrix(conj_training_cv[,ncol(conj_training_cv)])
    
    cv<-cv.glmnet(x,y,alpha=j/10,nfolds=10)
    best_lambda<-cv$lambda.min
    
    mdl_lasso <- glmnet(x,y,lambda=best_lambda , family="gaussian",alpha=j/10)
    pred_lasso<-predict(mdl_lasso,as.matrix(conj_test_cv[,-ncol(conj_test_cv)]),s=best_lambda)
    error_lasso<-mean( (pred_lasso - conj_test_cv[,ncol(conj_test_cv)])*(pred_lasso - conj_test_cv[,ncol(conj_test_cv)]) )
    
    errors_cv_rl[i]<-error_lasso
  }
  error_lasso_vector[j+1]<-mean(errors_cv_rl)
}
best_alpha_abs<-(which(error_lasso_vector==min(error_lasso_vector))-1)/10
plot(error_lasso_vector, type="o")
#######################################################################


















##############REGRESION LINEAL CON WEIGHT DECAY BEST_ALPHA (VALIDACION CRUZADA)##################
set.seed(199)
nparticiones<-5
parts <- cut(seq(1,nrow(datos)),breaks=nparticiones,labels=FALSE)
errors_cv_rl<-numeric(nparticiones)

for (i in 1:nparticiones){
  testIndexes <- which(parts==i,arr.ind=TRUE)
  trainIndexes<- setdiff(as.numeric(rownames(datos)),testIndexes)
  
  conj_test_cv<-datos[testIndexes,]
  conj_training_cv<-datos[trainIndexes,]
  
  x <- as.matrix(conj_training_cv[,-ncol(conj_training_cv)])
  y <- as.matrix(conj_training_cv[,ncol(conj_training_cv)])
  
  
  cv<-cv.glmnet(x,y,alpha=best_alpha_abs,nfolds=10)
  best_lambda<-cv$lambda.min
  mdl_lasso <- glmnet(x,y,lambda=best_lambda , family="gaussian",alpha=best_alpha_abs)
  pred_lasso<-predict(mdl_lasso,as.matrix(conj_test_cv[,-ncol(conj_test_cv)]),s=best_lambda)
  error_lasso<-mean( (pred_lasso - conj_test_cv[,ncol(conj_test_cv)])*(pred_lasso - conj_test_cv[,ncol(conj_test_cv)]) )
  
  errors_cv_rl[i]<-error_lasso
}
print(paste("Error en test validacion cruzada 5cv:",mean(errors_cv_rl)))
#######################################################################


##############REGRESION LINEAL CON WEIGHT DECAY RIDGE TRANSF. POLINOMICA CUADRATICA (VALIDACION CRUZADA)##################
set.seed(199)
library(glmnet)
nparticiones<-5
parts <- cut(seq(1,nrow(datos)),breaks=nparticiones,labels=FALSE)

errors_cv_rl<-numeric(nparticiones)

for (i in 1:nparticiones){
  testIndexes <- which(parts==i,arr.ind=TRUE)
  trainIndexes<- setdiff(as.numeric(rownames(datos)),testIndexes)
  
  conj_test_cv<-datos[testIndexes,]
  conj_training_cv<-datos[trainIndexes,]
  
  x <- cbind(
    as.matrix(conj_training_cv[,-ncol(conj_training_cv)]),
    as.matrix(conj_training_cv[,c(1,3,5)])**2
  )
  y <- as.matrix(conj_training_cv[,ncol(conj_training_cv)])
  
  
  cv<-cv.glmnet(x,y,alpha=best_alpha_abs,nfolds=10)
  best_lambda<-cv$lambda.min
  
  mdl_lasso <- glmnet(x,y,lambda=best_lambda , family="gaussian",alpha=best_alpha_abs)
  pred_lasso<-predict(mdl_lasso,
                      cbind(
                        as.matrix(conj_test_cv[,-ncol(conj_test_cv)]),
                        as.matrix(conj_test_cv[,c(1,3,5)])**2
                      )
                      ,s=best_lambda)
  error_lasso<-mean( (pred_lasso - conj_test_cv[,ncol(conj_test_cv)])*(pred_lasso - conj_test_cv[,ncol(conj_test_cv)]) )
  
  errors_cv_rl[i]<-error_lasso
}
print(paste("Error en test validacion cruzada 5cv:",mean(errors_cv_rl)))
#######################################################################


##############REGRESION LINEAL CON WEIGHT DECAY RIDGE TRANSF. POLINOMICA CUBICA (VALIDACION CRUZADA)##################
set.seed(199)
library(glmnet)
nparticiones<-5
parts <- cut(seq(1,nrow(datos)),breaks=nparticiones,labels=FALSE)

errors_cv_rl<-numeric(nparticiones)

for (i in 1:nparticiones){
  testIndexes <- which(parts==i,arr.ind=TRUE)
  trainIndexes<- setdiff(as.numeric(rownames(datos)),testIndexes)
  
  conj_test_cv<-datos[testIndexes,]
  conj_training_cv<-datos[trainIndexes,]
  
  x <- cbind(
    as.matrix(conj_training_cv[,-ncol(conj_training_cv)]),
    as.matrix(conj_training_cv[,c(1,2,3,4,5)])**2,
    as.matrix(conj_training_cv[,c(1,2,3,4,5)])**3
  )
  y <- as.matrix(conj_training_cv[,ncol(conj_training_cv)])
  
  
  cv<-cv.glmnet(x,y,alpha=best_alpha_abs,nfolds=10)
  best_lambda<-cv$lambda.min
  
  mdl_lasso <- glmnet(x,y,lambda=best_lambda , family="gaussian",alpha=best_alpha_abs)
  pred_lasso<-predict(mdl_lasso,
                      cbind(
                        as.matrix(conj_test_cv[,-ncol(conj_test_cv)]),
                        as.matrix(conj_test_cv[,c(1,2,3,4,5)])**2,
                        as.matrix(conj_test_cv[,c(1,2,3,4,5)])**3
                      )
                      ,s=best_lambda)
  error_lasso<-mean( (pred_lasso - conj_test_cv[,ncol(conj_test_cv)])*(pred_lasso - conj_test_cv[,ncol(conj_test_cv)]) )
  
  errors_cv_rl[i]<-error_lasso
}
print(paste("Error en test validacion cruzada 5cv:",mean(errors_cv_rl)))
#######################################################################




##############REGRESION LINEAL CON WEIGHT DECAY RIDGE TRANSF. POLINOMICA CUARTA (VALIDACION CRUZADA)##################
set.seed(199)
library(glmnet)
nparticiones<-5
parts <- cut(seq(1,nrow(datos)),breaks=nparticiones,labels=FALSE)

errors_cv_rl<-numeric(nparticiones)

for (i in 1:nparticiones){
  testIndexes <- which(parts==i,arr.ind=TRUE)
  trainIndexes<- setdiff(as.numeric(rownames(datos)),testIndexes)
  
  conj_test_cv<-datos[testIndexes,]
  conj_training_cv<-datos[trainIndexes,]
  
  x <- cbind(
    as.matrix(conj_training_cv[,-ncol(conj_training_cv)]),
    as.matrix(conj_training_cv[,c(1,2,3,4,5)])**2,
    as.matrix(conj_training_cv[,c(1,2,3,4,5)])**3,
    as.matrix(conj_training_cv[,c(1,2,3,4,5)])**4
  )
  y <- as.matrix(conj_training_cv[,ncol(conj_training_cv)])
  
  
  cv<-cv.glmnet(x,y,alpha=best_alpha_abs,nfolds=10)
  best_lambda<-cv$lambda.min
  
  mdl_lasso <- glmnet(x,y,lambda=best_lambda , family="gaussian",alpha=best_alpha_abs)
  pred_lasso<-predict(mdl_lasso,
                      cbind(
                        as.matrix(conj_test_cv[,-ncol(conj_test_cv)]),
                        as.matrix(conj_test_cv[,c(1,2,3,4,5)])**2,
                        as.matrix(conj_test_cv[,c(1,2,3,4,5)])**3,
                        as.matrix(conj_test_cv[,c(1,2,3,4,5)])**4
                      )
                      ,s=best_lambda)
  error_lasso<-mean( (pred_lasso - conj_test_cv[,ncol(conj_test_cv)])*(pred_lasso - conj_test_cv[,ncol(conj_test_cv)]) )
  
  errors_cv_rl[i]<-error_lasso
}
print(paste("Error en test validacion cruzada 5cv:",mean(errors_cv_rl)))
#######################################################################






##############REGRESION LINEAL CON WEIGHT DECAY RIDGE TRANSF. POLINOMICA QUINTA (VALIDACION CRUZADA)##################
set.seed(199)
library(glmnet)
nparticiones<-5
parts <- cut(seq(1,nrow(datos)),breaks=nparticiones,labels=FALSE)

errors_cv_rl<-numeric(nparticiones)
coef_cor<-numeric(nparticiones)
for (i in 1:nparticiones){
  testIndexes <- which(parts==i,arr.ind=TRUE)
  trainIndexes<- setdiff(as.numeric(rownames(datos)),testIndexes)
  
  conj_test_cv<-datos[testIndexes,]
  conj_training_cv<-datos[trainIndexes,]
  
  x <- cbind(
    as.matrix(conj_training_cv[,-ncol(conj_training_cv)]),
    as.matrix(conj_training_cv[,c(1,2,3,4,5)])**2,
    as.matrix(conj_training_cv[,c(1,2,3,4,5)])**3,
    as.matrix(conj_training_cv[,c(1,2,3,4,5)])**4,
    as.matrix(conj_training_cv[,c(1,3,5)])**5,
    as.matrix(conj_training_cv[,c(1,3,5)])**6,
    as.matrix(conj_training_cv[,c(1,3,5)])**7
  )
  y <- as.matrix(conj_training_cv[,ncol(conj_training_cv)])
  
  
  cv<-cv.glmnet(x,y,alpha=best_alpha_abs,nfolds=10)
  best_lambda<-cv$lambda.min
  
  mdl_lasso <- glmnet(x,y,lambda=best_lambda , family="gaussian",alpha=best_alpha_abs)
  pred_lasso<-predict(mdl_lasso,
                      cbind(
                        as.matrix(conj_test_cv[,-ncol(conj_test_cv)]),
                        as.matrix(conj_test_cv[,c(1,2,3,4,5)])**2,
                        as.matrix(conj_test_cv[,c(1,2,3,4,5)])**3,
                        as.matrix(conj_test_cv[,c(1,2,3,4,5)])**4,
                        as.matrix(conj_test_cv[,c(1,3,5)])**5,
                        as.matrix(conj_test_cv[,c(1,3,5)])**6,
                        as.matrix(conj_test_cv[,c(1,3,5)])**7
                      )
                      ,s=best_lambda)
  error_lasso<-mean( (pred_lasso - conj_test_cv[,ncol(conj_test_cv)])*(pred_lasso - conj_test_cv[,ncol(conj_test_cv)]) )
  coef_cor[i]=cor(pred_lasso,conj_test_cv[,ncol(conj_test_cv)])
  errors_cv_rl[i]<-error_lasso
}

best_error<-mean(errors_cv_rl)
print(paste("Error en test validacion cruzada 5cv:",best_error))
best_coef_cor = mean(coef_cor)
correlacion=best_coef_cor*best_coef_cor
determinacion = sqrt(best_coef_cor)
corq = determinacion*determinacion
#######################################################################


################ CALCULAR EL MEJOR ERROR OBTENIDO CON RL #####################
sqrt(best_error)/diff(range(datos[,ncol(datos)]))
##############################################################################



#####################MODELO DE REGRESION LINEAL RIDGE (LEAVE ONE OUT)#############
errors_cv_rl<-numeric(nrow(datos))
for (i in 1:nrow(datos)){
  testIndexes <- i
  trainIndexes<- setdiff(as.numeric(rownames(datos)),testIndexes)
  
  conj_test_cv<-datos[testIndexes,]
  conj_training_cv<-datos[trainIndexes,]
  
  x <- as.matrix(conj_training_cv[,-ncol(conj_training_cv)])
  y <- as.matrix(conj_training_cv[,ncol(conj_training_cv)])
  
  cv<-cv.glmnet(x,y,alpha=best_alpha_abs,nfolds=10)
  best_lambda<-cv$lambda.min
  
  mdl_lasso <- glmnet(x,y,lambda=best_lambda , family="gaussian",alpha=best_alpha_abs)
  pred_lasso<-predict(mdl_lasso,as.matrix(conj_test_cv[,-ncol(conj_test_cv)]),s=best_lambda)
  error_lasso<-mean( (pred_lasso - conj_test_cv[,ncol(conj_test_cv)])*(pred_lasso - conj_test_cv[,ncol(conj_test_cv)]) )
  
  errors_cv_rl[i]<-error_lasso
  
  print(i)
}
print(paste("Error en test Leave One Out:",mean(errors_cv_rl)))
############################################################





#####################MODELO DE REGRESION LINEAL SIMPLE (VALIDACION CRUZADA)#############
set.seed(199)
nparticiones<-5
parts <- cut(seq(1,nrow(datos)),breaks=nparticiones,labels=FALSE)

errors_cv_rl<-numeric(nparticiones)
for (i in 1:nparticiones){
  testIndexes <- which(parts==i,arr.ind=TRUE)
  trainIndexes<- setdiff(as.numeric(rownames(datos)),testIndexes)
  
  conj_test_cv<-datos[testIndexes,]
  conj_training_cv<-datos[trainIndexes,]
  
  
  mod_lineal <- lm(conj_training_cv$sound~.,data=conj_training_cv)
  pred_lin<-predict(mod_lineal,newdata=conj_test_cv[,-ncol(conj_test_cv)])
  error_lin<-mean((conj_test_cv[,ncol(conj_test_cv)]-pred_lin)*(conj_test_cv[,ncol(conj_test_cv)]-pred_lin))
  
  errors_cv_rl[i]<-error_lin
}
print(paste("Error en test validacion cruzada 5cv:",mean(errors_cv_rl)))
############################################################

#####################MODELO DE REGRESION LINEAL SIMPLE (LEAVE ONE OUT)#############
errors_cv_rl<-numeric(nrow(datos))
for (i in 1:nrow(datos)){
  testIndexes <- i
  trainIndexes<- setdiff(as.numeric(rownames(datos)),testIndexes)
  
  conj_test_cv<-datos[testIndexes,]
  conj_training_cv<-datos[trainIndexes,]
  
  mod_lineal <- lm(conj_training_cv$sound~.,data=conj_training_cv)
  pred_lin<-predict(mod_lineal,newdata=conj_test_cv[,-ncol(conj_test_cv)])
  error_lin<-mean((conj_test_cv[,ncol(conj_test_cv)]-pred_lin)*(conj_test_cv[,ncol(conj_test_cv)]-pred_lin))
  #print(paste("Error de test en el modelo lineal: ", error_lin))
  
  errors_cv_rl[i]<-error_lin
}
print(paste("Error en test Leave One Out:",mean(errors_cv_rl)))
############################################################










































############################################  APARTADO 2 ################################################

########### KNN EN REGRESION vs. REGRESION LINEAL A TOPE

normalizar = function(datosNormalizar) {
  normalizado = datosNormalizar
  for (i in seq(from=1,to=ncol(datosNormalizar)-1)) {
    minimo=min(datosNormalizar[,i])
    maximo=max(datosNormalizar[,i])
    for (j in seq(from=1,to=nrow(datosNormalizar))) {
      variable=datosNormalizar[j,i]
      normalizado[j,i] <- (variable-minimo)/(maximo-minimo)
    }
  }
  return (normalizado)
}

#datos_normalizados = normalizar(datos)



#1. Validacion cruzada para obtener el mejor K.
set.seed(199)
library(FNN)
nparticiones<-5
parts <- cut(seq(1,nrow(datos)),breaks=nparticiones,labels=FALSE)

errors_knn<-numeric(nparticiones)
best_nvecinos<-numeric(nparticiones)
best_error_knn_iter<-numeric(nparticiones)

for (i in 1:nparticiones){
  testIndexes <- which(parts==i,arr.ind=TRUE)
  trainIndexes<- setdiff(as.numeric(rownames(datos)),testIndexes)
  
  conj_test_cv<-datos[testIndexes,]
  conj_training_cv<-datos[trainIndexes,]
  
  errores_num_vecinos<-numeric(10)
  for(j in 1:100){
    m.knn.reg<-knn.reg(train=conj_training_cv[,-ncol(conj_training_cv)],
                        test=conj_test_cv[,-ncol(conj_test_cv)],
                        y=conj_training_cv[,ncol(conj_training_cv)],
                        k=j)
    errores_num_vecinos[j]<-mean((conj_test_cv[,ncol(conj_test_cv)]-m.knn.reg$pred)*(conj_test_cv[,ncol(conj_test_cv)]-m.knn.reg$pred))
  }
  best_k_iter<-min(which(errores_num_vecinos==min(errores_num_vecinos)))
  best_nvecinos[i]<-best_k_iter
  best_error_knn_iter[i]<-min(errores_num_vecinos)
}
best_nvecinos
best_error_knn_iter
print(paste("Error en test validacion cruzada 5cv:",mean(best_error_knn_iter)))

#IGUAL PERO NORMALIZADO

#1. Validacion cruzada para obtener el mejor K.
set.seed(199)
library(FNN)
nparticiones<-5
parts <- cut(seq(1,nrow(datos)),breaks=nparticiones,labels=FALSE)

errors_knn<-numeric(nparticiones)
best_nvecinos<-numeric(nparticiones)
best_error_knn_iter<-numeric(nparticiones)

for (i in 1:nparticiones){
  testIndexes <- which(parts==i,arr.ind=TRUE)
  trainIndexes<- setdiff(as.numeric(rownames(datos_normalizados)),testIndexes)
  
  conj_test_cv<-datos_normalizados[testIndexes,]
  conj_training_cv<-datos_normalizados[trainIndexes,]
  
  errores_num_vecinos<-numeric(10)
  for(j in 1:100){
    m.knn.reg<-knn.reg(train=conj_training_cv[,-ncol(conj_training_cv)],
                       test=conj_test_cv[,-ncol(conj_test_cv)],
                       y=conj_training_cv[,ncol(conj_training_cv)],
                       k=j)
    errores_num_vecinos[j]<-mean((conj_test_cv[,ncol(conj_test_cv)]-m.knn.reg$pred)*(conj_test_cv[,ncol(conj_test_cv)]-m.knn.reg$pred))
  }
  best_k_iter<-min(which(errores_num_vecinos==min(errores_num_vecinos)))
  best_nvecinos[i]<-best_k_iter
  best_error_knn_iter[i]<-min(errores_num_vecinos)
}
best_nvecinos
best_error_knn_iter
print(paste("Error en test validacion cruzada 5cv:",mean(best_error_knn_iter)))







##################################################################
##################################################################
## KNN BUENO

##################################################################
##################################################################
##################################################################
##### PRUEBA MIA KNN

library(FNN)


normalizar = function(datosNormalizar) {
  normalizado = datosNormalizar
  for (i in seq(from=1,to=ncol(datosNormalizar)-1)) {
    minimo=min(datosNormalizar[,i])
    maximo=max(datosNormalizar[,i])
    for (j in seq(from=1,to=nrow(datosNormalizar))) {
      variable=datosNormalizar[j,i]
      normalizado[j,i] <- (variable-minimo)/(maximo-minimo)
    }
  }
  return (normalizado)
}

datos_normalizados = normalizar(datos)

####################################################


##################################################
###################################################
####################################################
###         kd_tree

set.seed(199)
nparticiones<-5
parts <- cut(seq(1,nrow(datos)),breaks=nparticiones,labels=FALSE)

errors_knn<-numeric(5)
errors_knn_iter<-numeric(nparticiones)

for(i in 1:5){
  
  for (j in 1:nparticiones) {
    
    testIndexes <- which(parts==j,arr.ind=TRUE)
    trainIndexes<- setdiff(as.numeric(rownames(datos_normalizados)),testIndexes)
    
    conj_test_cv<-datos[testIndexes,]
    conj_training_cv<-datos[trainIndexes,]
    
    conj_test_cv=normalizar(conj_test_cv)
    conj_training_cv=normalizar(conj_training_cv)

    m.knn.reg<-knn.reg(train=conj_training_cv[,-ncol(conj_training_cv)],
                       test=conj_test_cv[,-ncol(conj_test_cv)],
                       y=conj_training_cv[,ncol(conj_training_cv)],
                       k=i,
                       algorithm="kd_tree")
    
    errors_knn_iter[j]<-mean((conj_test_cv[,ncol(conj_test_cv)]- m.knn.reg$pred)*(conj_test_cv[,ncol(conj_test_cv)]-m.knn.reg$pred))
        
  }
  errors_knn[i]=mean(errors_knn_iter)
}

best_k_kd_tree<-min(which(errors_knn==min(errors_knn)))
errors_knn_kd_tree=errors_knn[best_k_kd_tree]


raiz=sqrt(errors_knn[best_k_kd_tree])
rango=range(datos[,ncol(datos)])
porcentaje_kd_tree=raiz/(rango[2]-rango[1])

errores_kd_tree_plot = errors_knn



###############################################################################
##      cover_tree
set.seed(199)
nparticiones<-5
parts <- cut(seq(1,nrow(datos)),breaks=nparticiones,labels=FALSE)

errors_knn<-numeric(5)
errors_knn_iter<-numeric(nparticiones)

for(i in 1:5){
  
  for (j in 1:nparticiones) {
    
    testIndexes <- which(parts==j,arr.ind=TRUE)
    trainIndexes<- setdiff(as.numeric(rownames(datos_normalizados)),testIndexes)
    
    conj_test_cv<-datos[testIndexes,]
    conj_training_cv<-datos[trainIndexes,]

    conj_test_cv=normalizar(conj_test_cv)
    conj_training_cv=normalizar(conj_training_cv)
        
    m.knn.reg<-knn.reg(train=conj_training_cv[,-ncol(conj_training_cv)],
                       test=conj_test_cv[,-ncol(conj_test_cv)],
                       y=conj_training_cv[,ncol(conj_training_cv)],
                       k=i,
                       algorithm="cover_tree")
    
    errors_knn_iter[j]<-mean((conj_test_cv[,ncol(conj_test_cv)]- m.knn.reg$pred)*(conj_test_cv[,ncol(conj_test_cv)]-m.knn.reg$pred))
    
  }
  errors_knn[i]=mean(errors_knn_iter)
}

best_k_cover_tree<-min(which(errors_knn==min(errors_knn)))
errors_knn_cover_tree=errors_knn[best_k_cover_tree]



raiz=sqrt(errors_knn[best_k_cover_tree])
rango=range(datos[,ncol(datos)])
porcentaje_cover_tree=raiz/(rango[2]-rango[1])

errores_cover_tree_plot = errors_knn

##########################################################################
##       brute
set.seed(199)
nparticiones<-5
parts <- cut(seq(1,nrow(datos)),breaks=nparticiones,labels=FALSE)

errors_knn<-numeric(5)
errors_knn_iter<-numeric(nparticiones)

for(i in 1:5){
  
  for (j in 1:nparticiones) {
    
    testIndexes <- which(parts==j,arr.ind=TRUE)
    trainIndexes<- setdiff(as.numeric(rownames(datos)),testIndexes)
    
    conj_test_cv<-datos[testIndexes,]
    conj_training_cv<-datos[trainIndexes,]
    conj_test_cv=normalizar(conj_test_cv)
    conj_training_cv=normalizar(conj_training_cv)
    
    m.knn.reg<-knn.reg(train=conj_training_cv[,-ncol(conj_training_cv)],
                       test=conj_test_cv[,-ncol(conj_test_cv)],
                       y=conj_training_cv[,ncol(conj_training_cv)],
                       k=i,
                       algorithm="brute")
    
    errors_knn_iter[j]<-mean((conj_test_cv[,ncol(conj_test_cv)]- m.knn.reg$pred)*(conj_test_cv[,ncol(conj_test_cv)]-m.knn.reg$pred))
    
  }
  errors_knn[i]=mean(errors_knn_iter)
}

best_k_brute<-min(which(errors_knn==min(errors_knn)))
errors_knn_brute=errors_knn[best_k_brute]


raiz=sqrt(errors_knn[best_k_brute])
rango=range(datos[,ncol(datos)])
porcentaje_brute=raiz/(rango[2]-rango[1])

errores_brute_plot = errors_knn

#############################################
plot(errores_kd_tree_plot,xlab = "k",ylab = "Error cuadratico" ,type="l",main="¿Cuál es el mejor k?")
plot(errores_kd_tree_plot,xlab = "k",ylab = "Error cuadratico" ,type="l",main="algortimo kd_tree")
plot(errores_cover_tree_plot,xlab = "k",ylab = "Error cuadratico" ,type="l",main="algortimo cover_tree")
plot(errores_brute_plot,xlab = "k",ylab = "Error cuadratico" ,type="l",main="algortimo brute")

print(paste("Error en test validacion cruzada 5cv para kd_tree:",errors_knn_kd_tree, "y mejor k es",best_k_kd_tree ))
print(paste("Error en test validacion cruzada 5cv para cover_tree:",errors_knn_cover_tree, "y mejor k es",best_k_cover_tree ))
print(paste("Error en test validacion cruzada 5cv para brute:",errors_knn_brute, "y mejor k es",best_k_brute ))
print(paste("Porcentaje de error en kd_tree= ",porcentaje_kd_tree*100,"%"))
print(paste("Porcentaje de error en cover_tree= ",porcentaje_cover_tree*100,"%"))
print(paste("Porcentaje de error en brute= ",porcentaje_brute*100,"%"))

#########################################################
#########################################################
#########################################################










######################################################################
##########################################################################
#######################################################################
###########################################################################
##########################################################################
###########################################################################
#########################################################################
datos_normalizados = normalizar(datos)

set.seed(199)

errors_knn<-numeric(100)
errors_knn_cuadratico <- numeric(100)

for(i in 1:100){
      m.knn.reg<-knn.reg(train=datos_normalizados[,-ncol(datos_normalizados)],
                         y=datos_normalizados[,ncol(datos_normalizados)],
                         k=i,
                         algorithm="kd_tree")

      error_otro = mean( (m.knn.reg$pred-datos_normalizados[,ncol(datos_normalizados)])*(m.knn.reg$pred-datos_normalizados[,ncol(datos_normalizados)]))
      
      raiz=sqrt(error_otro)
      porcentaje=raiz/(rango[2]-rango[1])
      porcentajes_error[i] = porcentaje
      errors_knn[i]=porcentaje  
      errors_knn_cuadratico[i]=error_otro
} 
  


best_k<-min(which(errors_knn==min(errors_knn)))
print(paste("Error en test validacion cruzada: ",errors_knn_cuadratico[best_k], " en porcentaje ",errors_knn[best_k], "y mejor k es",best_k ))

plot(errors_knn,type="l")



###############################################################################



#########################

#para kd_tree
set.seed(199)
porcentajes_error=numeric(20)
for (i in 1:20) {
  m.knn.reg<-knn.reg(train=datos_normalizados[,-ncol(datos_normalizados)],
                   y=datos_normalizados[,ncol(datos_normalizados)],
                   k=i,
                   algorithm="kd_tree")
  #summary(m.knn.reg)
  #m.knn.reg$residuals
  #m.knn.reg$pred
  error_otro = mean( (m.knn.reg$pred-datos_normalizados[,ncol(datos_normalizados)])*(m.knn.reg$pred-datos_normalizados[,ncol(datos_normalizados)]))
  #error_otro

  raiz=sqrt(error_otro)
  porcentaje=raiz/(rango[2]-rango[1])
  porcentajes_error[i] = porcentaje

}

porcentajes_error
mejor_kd_tree=min(porcentajes_error)

#para cover_tree
set.seed(199)
porcentajes_error_cover=numeric(20)
for (i in 1:20) {
  m.knn.reg<-knn.reg(train=datos_normalizados[,-ncol(datos_normalizados)],
                     y=datos_normalizados[,ncol(datos_normalizados)],
                     k=i,
                     algorithm="cover_tree")
  #summary(m.knn.reg)
  #m.knn.reg$residuals
  #m.knn.reg$pred
  error_otro_cover = mean( (m.knn.reg$pred-datos_normalizados[,ncol(datos_normalizados)])*(m.knn.reg$pred-datos_normalizados[,ncol(datos_normalizados)]))
  #error_otro
  
  raiz_cover=sqrt(error_otro_cover)
  porcentaje_cover=raiz_cover/(rango[2]-rango[1])
  porcentajes_error_cover[i] = porcentaje_cover
  
}

porcentajes_error_cover
mejor_cover_tree=min(porcentajes_error_cover)

#para brute
set.seed(199)
porcentajes_error_brute=numeric(20)
for (i in 1:20) {
  m.knn.reg<-knn.reg(train=datos_normalizados[,-ncol(datos_normalizados)],
                     y=datos_normalizados[,ncol(datos_normalizados)],
                     k=i,
                     algorithm="brute")
  
  error_otro_brute = mean( (m.knn.reg$pred-datos_normalizados[,ncol(datos_normalizados)])*(m.knn.reg$pred-datos_normalizados[,ncol(datos_normalizados)]))
  
  raiz_brute=sqrt(error_otro_brute)
  porcentaje_brute=raiz_brute/(rango[2]-rango[1])
  porcentajes_error_brute[i] = porcentaje_brute
  
}

porcentajes_error_brute
mejor_brute=min(porcentajes_error_brute)



mejor_kd_tree
mejor_cover_tree
mejor_brute
###############################
# Ein

m.knn.reg<-knn.reg(train=datos_normalizados[,-ncol(datos_normalizados)],
                   y=datos_normalizados[,ncol(datos_normalizados)],
                   k=2,
                   algorithm="kd_tree")
summary(m.knn.reg)

###############################

##### CURVA ROC

m.knn.reg$pred

###########










############SUPPORT VECTOR MACHINE EN REGRESION############
library(e1071)
set.seed(199)

nparticiones<-5
parts <- cut(seq(1,nrow(datos)),breaks=nparticiones,labels=FALSE)

#KERNEL LINEAL
error_svm=numeric(nparticiones)
for (i in 1:nparticiones) {
    
    testIndexes <- which(parts==i,arr.ind=TRUE)
    trainIndexes<- setdiff(as.numeric(rownames(datos)),testIndexes)
    
    conj_test_cv<-datos[testIndexes,]
    conj_training_cv<-datos[trainIndexes,]

    mod_svm <- svm(conj_training_cv$sound~.,
                   data=conj_training_cv,kernel="linear",type="eps-regression")
    pred_svm <- predict(mod_svm, conj_test_cv[,-ncol(conj_test_cv)])
    error_svm[i] <- mean((conj_test_cv[,ncol(conj_test_cv)]-pred_svm)*(conj_test_cv[,ncol(conj_test_cv)]-pred_svm))
}
error_svm_final =mean(error_svm)
print(paste("Error de test svm kernel lineal: ", error_svm_final))

####### EIN
set.seed(199)
mod_svm <- svm(datos$sound~.,
               data=datos,kernel="linear",type="eps-regression")

pred_svm <- predict(mod_svm, datos[,-ncol(conj_test_cv)])
error_svm_ein <- mean((datos[,ncol(datos)]-pred_svm)*(datos[,ncol(datos)]-pred_svm))


##################################################################
#con transformaciones lienales no se puede con el modelo, pide  formula
#######################################################################



#######################OPTIMIZACION DE SVM#######################
tuneResult <- tune(svm, 
                   datos[,-ncol(datos)],
                   datos[,ncol(datos)])

tuneResult$best.model

print(paste("Error de test svm kernel lineal: ", error_svm))

library(e1071)
set.seed(199)

nparticiones<-5
parts <- cut(seq(1,nrow(datos)),breaks=nparticiones,labels=FALSE)

#KERNEL RADIAL
error_svm_mejor=numeric(nparticiones)
for (i in 1:nparticiones) {
  
  testIndexes <- which(parts==i,arr.ind=TRUE)
  trainIndexes<- setdiff(as.numeric(rownames(datos)),testIndexes)
  
  conj_test_cv<-datos[testIndexes,]
  conj_training_cv<-datos[trainIndexes,]
  
  mod_svm <- svm(conj_training_cv$sound~.,
                 data=conj_training_cv,
                 kernel="radial",
                 type="eps-regression",
                 cost=tuneResult$best.model$cost,
                 gamma=tuneResult$best.model$gamma,
                 epsilon=tuneResult$best.model$epsilon)
  
  pred_svm <- predict(mod_svm, conj_test_cv[,-ncol(conj_test_cv)])
  error_svm_mejor[i] <- mean((conj_test_cv[,ncol(conj_test_cv)]-pred_svm)*(conj_test_cv[,ncol(conj_test_cv)]-pred_svm))
  
}
error_svm_final_mejor =mean(error_svm_mejor)
print(paste("Error de test svm kernel radial: ", error_svm_final_mejor))
































































############SUPPORT VECTOR MACHINE EN REGRESION############
library(e1071)
set.seed(199)

  #KERNEL LINEAL
mod_svm <- svm(conj_training$sound~.,
               data=conj_training,kernel="linear",type="eps-regression")
pred_svm <- predict(mod_svm, conj_test[,-ncol(conj_test)])
error_svm <- mean((conj_test[,ncol(conj_test)]-pred_svm)*(conj_test[,ncol(conj_test)]-pred_svm))
print(paste("Error de test svm kernel lineal: ", error_svm))



  #KERNEL POLINOMICO
mod_svm <- svm(conj_training$sound~.,
               data=conj_training,kernel="polynomial",type="eps-regression")
pred_svm <- predict(mod_svm, conj_test[,-ncol(conj_test)])
error_svm <- mean((conj_test[,ncol(conj_test)]-pred_svm)*(conj_test[,ncol(conj_test)]-pred_svm))
print(paste("Error de test svm kernel lineal: ", error_svm))



  #KERNEL RADIAL
mod_svm <- svm(conj_training$sound~.,
               data=conj_training,kernel="radial",type="eps-regression")
pred_svm <- predict(mod_svm, conj_test[,-ncol(conj_test)])
error_svm <- mean((conj_test[,ncol(conj_test)]-pred_svm)*(conj_test[,ncol(conj_test)]-pred_svm))
print(paste("Error de test svm kernel lineal: ", error_svm))


  #KERNEL SIGMOIDAL
mod_svm <- svm(conj_training$sound~.,
               data=conj_training,kernel="sigmoid",type="eps-regression")
pred_svm <- predict(mod_svm, conj_test[,-ncol(conj_test)])
error_svm <- mean((conj_test[,ncol(conj_test)]-pred_svm)*(conj_test[,ncol(conj_test)]-pred_svm))
print(paste("Error de test svm kernel lineal: ", error_svm))
############################################################



#######################OPTIMIZACION DE SVM#######################
library(e1071)
set.seed(199)
tuneResult <- tune(svm, 
                  datos_normalizados[,-ncol(datos_normalizados)],
                  datos_normalizados[,ncol(datos_normalizados)])

tuneResult$best.model

mod_svm <- svm(conj_training$sound~.,
                data=conj_training,
                kernel="radial",
                type="eps-regression",
                cost=tuneResult$best.model$cost,
                gamma=tuneResult$best.model$gamma,
                epsilon=tuneResult$best.model$epsilon)

pred_svm <- predict(mod_svm, conj_test[,-ncol(conj_test)])
error_svm <- mean((conj_test[,ncol(conj_test)]-pred_svm)*(conj_test[,ncol(conj_test)]-pred_svm))
print(paste("Error de test svm kernel lineal: ", error_svm))


#Podemos concluir que esta optimizado al máximo.
#################################################################























########################################### BOOSTING ###########################################
library(randomForest)
library(gbm)

narboles_boost<-numeric(10)
errores_narboles_boost<-numeric(10)
qerrors_boost<-list(10)

cont<-1
for(i in 90:100){
  mod_boost <- gbm(sound ~ .,
                   data=conj_training, 
                   distribution = 'gaussian', 
                   n.trees = 100*i,
                   shrinkage = 0.01,
                   interaction.depth = 3)
  mod_boost
  
  pred_boost<-predict(mod_boost,conj_test,n.trees=100*i)
  qerrors_boost[[cont]]<-(pred_boost-conj_test[,ncol(conj_test)])*(pred_boost-conj_test[,ncol(conj_test)])
  errores_narboles_boost[cont]<-mean(qerrors_boost[[cont]])
  narboles_boost[cont]<-(100*i)
  cont<-cont+1
}

#plot(narboles_boost,errores_narboles_boost, main="Errores test boosting",xlab="numero arboles",ylab="error",type="o")


best_index_boost=which(errores_narboles_boost==min(errores_narboles_boost))
print(paste("Mejor error test boosting: ",errores_narboles_boost[best_index_boost]))
print(paste("Mejor numero de arboles boosting: ",narboles_boost[best_index_boost]))
error_test_boost<-mean(errores_narboles_boost[best_index_boost])

plot(NA, 
     xlim=c(0,length(qerrors_boost[[best_index_boost]])),
     ylim=c(min(qerrors_boost[[best_index_boost]]),max(qerrors_boost[[best_index_boost]])),
     main="Errores Boosting",
     xlab="ejemplos",
     ylab="error test")
grid()
lines(1:length(qerrors_boost[[best_index_boost]]),qerrors_boost[[best_index_boost]],col="green")
abline(h=error_test_boost,col="red")
###############################################################################################
























###################################### BAGGING ######################################
library(randomForest)
x <- as.matrix(datos[,-ncol(datos)])
y <- as.matrix(datos[,ncol(datos)])

tuneRF(x,y,5,ntreeTry = 2000,stepFactor = 1,improve=0.05,doBest=T)

#VALIDACION CRUZADA BAGGING DE R
#cv.tuneRF<-rfcv(datos[,-ncol(datos)],datos[,ncol(datos)],cv.fold = 10,scale="log",step=0.5,
#                mtry = function(p) max(1,p))
#mean((cv.tuneRF$predicted$`5` - datos[,ncol(datos)])*(cv.tuneRF$predicted$`5` - datos[,ncol(datos)]))


#VALIDACION CRUZADA NUESTRA
set.seed(199)

nparticiones<-5
parts <- cut(seq(1,nrow(datos)),breaks=nparticiones,labels=FALSE)

errors_cv_rf<-numeric(nparticiones)
for (i in 1:nparticiones){
  testIndexes <- which(parts==i,arr.ind=TRUE)
  trainIndexes<- setdiff(as.numeric(rownames(datos)),testIndexes)
  
  conj_test_cv<-datos[testIndexes,]
  conj_training_cv<-datos[trainIndexes,]
  
  
  #MODELO Y CALCULO ERROR
  my_rf<-randomForest(conj_training_cv[,-ncol(conj_training_cv)],
                      conj_training_cv[,ncol(conj_training_cv)],
                      mtry = 5,
                      ntree = 500)
  
  pred_myrf<-predict(my_rf,conj_test_cv[,-nrow(conj_test_cv)])
  
  errors_cv_rf[i]<-mean((pred_myrf-conj_test_cv[,ncol(conj_test_cv)])*(pred_myrf-conj_test_cv[,ncol(conj_test_cv)]))
}

print(paste("Error en test validacion cruzada 5cv:",mean(errors_cv_rf)))
#####################################################################################
