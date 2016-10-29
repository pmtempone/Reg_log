----#Library----

library(pROC)

---#separo train y test----

sample.tt <- sample(1:nrow(diab2.3), 482)
diab2.3.training <- diab2.3[sample.tt,]
diab2.3.testing <- diab2.3[-sample.tt,]

diab2.3.training.m1 <- glm(test~(glucose+diastolic+bmi+pedi+age)^2, family=binomial, data = diab2.3.training)
pred.testing <- predict(diab2.3.training.m1, diab2.3.testing[,-7], type="response")
pred.testing <- ifelse(pred.testing >= .5, 1,0)
table("predicho"=pred.testing, "observado"=diab2.3.testing[,"test"])


----#hago la prediccion----

probpred <- predict(diab2.3.training.m1, diab2.3.testing, type=c("response"))

----#se genera la curva roc---
g <- roc(test ~ probpred, data = diab2.3.testing)


plot(g, col="red") 

probpred.training <- predict(diab2.3.training.m1, type=c("response"))
g.training <- roc(test ~ probpred.training, data = diab2.3.training)
lines(g.training, col="blue") 
legend("bottomright", c("entrenamiento","prueba"), col = c("blue", "red"), lty = 1 )
