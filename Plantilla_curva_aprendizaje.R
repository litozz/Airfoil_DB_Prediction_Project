#############################CURVA E_IN REGRESION LINEAL##############################

error_curva_aprendizaje<-numeric(118)
start<-10-1

for(nmuestras in 10:167){
  datos_copia<-datos[1:(nmuestras*9),]
  
  set.seed(199)
  x <- cbind(
    as.matrix(datos_copia[,-ncol(datos_copia)]),
    as.matrix(datos_copia[,c(1,2,3,4,5)])**2,
    as.matrix(datos_copia[,c(1,2,3,4,5)])**3,
    as.matrix(datos_copia[,c(1,2,3,4,5)])**4,
    as.matrix(datos_copia[,c(1,3,5)])**5,
    as.matrix(datos_copia[,c(1,3,5)])**6,
    as.matrix(datos_copia[,c(1,3,5)])**7,
    as.matrix(datos_copia[,c(1,3,5)])**8
  )
  y <- as.matrix(datos_copia[,ncol(datos_copia)])
  
  
  cv<-cv.glmnet(x,y,alpha=best_alpha_abs,nfolds=10)
  best_lambda<-cv$lambda.min
  
  mdl_elastic <- glmnet(x,y,lambda=best_lambda , family="gaussian",alpha=best_alpha_abs)
  pred_elastic<-predict(mdl_elastic,
                        cbind(
                          as.matrix(datos_copia[,-ncol(datos_copia)]),
                          as.matrix(datos_copia[,c(1,2,3,4,5)])**2,
                          as.matrix(datos_copia[,c(1,2,3,4,5)])**3,
                          as.matrix(datos_copia[,c(1,2,3,4,5)])**4,
                          as.matrix(datos_copia[,c(1,3,5)])**5,
                          as.matrix(datos_copia[,c(1,3,5)])**6,
                          as.matrix(datos_copia[,c(1,3,5)])**7,
                          as.matrix(datos_copia[,c(1,3,5)])**8
                        )
                        ,s=best_lambda)
  error_in_elastic<-mean( 
    (pred_elastic - datos_copia[,ncol(datos_copia)])*
      (pred_elastic - datos_copia[,ncol(datos_copia)]) )
  
  print(nmuestras)
  error_curva_aprendizaje[nmuestras - start]<-error_in_elastic
}

#############################FIN CURVA E_IN REGRESION LINEAL##############################

curva_ein_rl<-error_curva_aprendizaje


plot(curva_ein_rl,type="l")








############################ CURVA E_TEST REGRESION LINEAL################################
error_curva_aprendizaje<-numeric(118)
start<-10-1
for(nmuestras in 10:167){
  datos_copia<-datos[1:(nmuestras*9),]



  set.seed(199)
  nparticiones<-5
  parts <- cut(seq(1,nrow(datos_copia)),breaks=nparticiones,labels=FALSE)
  
  errors_cv_rl<-numeric(nparticiones)
  
  for (i in 1:nparticiones){
    testIndexes <- which(parts==i,arr.ind=TRUE)
    trainIndexes<- setdiff(as.numeric(rownames(datos_copia)),testIndexes)
    
    conj_test_cv<-datos[testIndexes,]
    conj_training_cv<-datos[trainIndexes,]
    
    x <- cbind(
      as.matrix(conj_training_cv[,-ncol(conj_training_cv)]),
      as.matrix(conj_training_cv[,c(1,2,3,4,5)])**2,
      as.matrix(conj_training_cv[,c(1,2,3,4,5)])**3,
      as.matrix(conj_training_cv[,c(1,2,3,4,5)])**4,
      as.matrix(conj_training_cv[,c(1,3,5)])**5,
      as.matrix(conj_training_cv[,c(1,3,5)])**6,
      as.matrix(conj_training_cv[,c(1,3,5)])**7,
      as.matrix(conj_training_cv[,c(1,3,5)])**8
    )
    y <- as.matrix(conj_training_cv[,ncol(conj_training_cv)])
    
    
    cv<-cv.glmnet(x,y,alpha=best_alpha_abs,nfolds=10)
    best_lambda<-cv$lambda.min
    
    mdl_elastic <- glmnet(x,y,lambda=best_lambda , family="gaussian",alpha=best_alpha_abs)
    pred_elastic<-predict(mdl_elastic,
                          cbind(
                            as.matrix(conj_test_cv[,-ncol(conj_test_cv)]),
                            as.matrix(conj_test_cv[,c(1,2,3,4,5)])**2,
                            as.matrix(conj_test_cv[,c(1,2,3,4,5)])**3,
                            as.matrix(conj_test_cv[,c(1,2,3,4,5)])**4,
                            as.matrix(conj_test_cv[,c(1,3,5)])**5,
                            as.matrix(conj_test_cv[,c(1,3,5)])**6,
                            as.matrix(conj_test_cv[,c(1,3,5)])**7,
                            as.matrix(conj_test_cv[,c(1,3,5)])**8
                          )
                          ,s=best_lambda)
    error_elastic<-mean( 
      (pred_elastic - conj_test_cv[,ncol(conj_test_cv)])*
        (pred_elastic - conj_test_cv[,ncol(conj_test_cv)]) )
    
    errors_cv_rl[i]<-error_elastic
  }
  best_error<-mean(errors_cv_rl)
  
  error_curva_aprendizaje[nmuestras - start]<-error_elastic
  print(nmuestras)
}
############################ CURVA E_TEST REGRESION LINEAL################################

curva_etest_rl<-error_curva_aprendizaje


plot(curva_etest_rl,type="l")





######################GRAFICAS##############################
accuracy<-mean(c(curva_etest_rl,curva_ein_rl))

plot(curva_ein_rl,type="l",
     ylim=c(15,35),col="red",
     main="Curva aprendizaje Reg. Lineal Trans. Pol.",
     xlab="numero de muestras",
     ylab="error cuadratico"
     )
lines(curva_etest_rl,col="green")


abline(h=mean(linea_recta),col="orange")

legend(86,35.7, 
       c("e_test","e_in","accuracy"),
       lty=c(1,1),
       lwd=c(1,1),
       col=c("green","red","orange"))
#########################################################################


