----#librerias-----

library(MASS)

----#carga de datos----

diabetes <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data", header=F)
names(diabetes) <-  c("npreg", "glucose", "diastolic", "triceps", "insulin", "bmi", "pedi", "age", "test")


----#analisis----

head(diabetes)

str(diabetes)

summary(diabetes)

plot(diabetes,col="blue")

#se buscan los valores 0 porque los valores faltantes se completaron con 0
table(diabetes$glucose==0)

table(diabetes$diastolic==0)

table(diabetes$triceps==0)

table(diabetes$insulin==0)

table(diabetes$bmi==0)

table(diabetes$age==0)

table(diabetes$triceps == 0, diabetes$insulin==0)

#se analiza la correlacion entre las variables
triceps.bmi <- diabetes[diabetes$triceps != 0 & diabetes$bmi != 0, c(4,6)]
with(triceps.bmi, plot(triceps, bmi, pch=20))

with(triceps.bmi, cor.test(triceps, bmi))

#no es una buena correlacion del todo pero se sabe que hay asociacion entre las variables.
#se elimina la variable que tiene menos observaciones

gluco.insu <- diabetes[diabetes$glucose != 0 & diabetes$insulin != 0, c(2,5)]
with(gluco.insu, plot(glucose, insulin, pch=20))

with(gluco.insu, cor.test(glucose, insulin))


----#preparar los datos----

diab2 <- diabetes[,-c(4,5)]
diab2.1 <- diab2[diab2$glucose!=0,]
diab2.2 <- diab2.1[diab2.1$diastolic!=0,]
diab2.3 <- diab2.2[diab2.2$bmi!=0,]

dim(diab2.3)
summary(diab2.3)


----#modelos----

#el elevado al cuadrado permite tener las interacciones entre las variables, más usado es hacer interacciones dobles solamente

diab2.3.m1 <- glm(test~(glucose+diastolic+bmi+pedi+age+npreg)^2, family=binomial, data = diab2.3)
summary(diab2.3.m1)
#media de la deviance tendría que estar cerca de 0, aproximadamente simetrica
# si se deja la interaccion hay que dejar las variables sueltas
#null deviance es el modelo nulo
#residual deviance: tiene que dar menor que el modelo nulo

diab2.3.m2 <- glm(test~(glucose+pedi+age+npreg)^2, family=binomial, data = diab2.3)
summary(diab2.3.m2)


#calcular la probabilidad de una distribucion chi cuadrado, esto sirve como control de las deviances
pchisq(deviance(diab2.3.m1), df.residual(diab2.3.m1), lower = F) #lower=F es mirar la parte alta de la distribucion

#los datos cumplen una chi cuadrado entonces me puedo quedar con el modelo definido, si es menor 0.05 es que no sirve.

pchisq(931.94,723,lower=FALSE) #vemos que las deviance del modelo nulo no sigue una distribucion chi cuadrado, se compara la deviance y los grados de libertad

----#comparacion de modelos-------

anova(diab2.3.m1, diab2.3.m2, test="Chisq")
#se mira si los dos modelos explican cosas distintas. Al dar significativo el modelo es que son distintas

-----#se sacan variables-----
  
drop1(diab2.3.m1,test = "Chisq")

confint(diab2.3.m1)
#se ven los intervalos de confianza

-----#graficar-----

#se analiza la glucosa vs la prob de diabetes suponiendo que el resto de los valores son medios

plot(diab2.3$glucose, diab2.3$test, xlab="glucosa", ylab="diabetes?", pch=20, xlim=c(45,210), cex=0.8, col="darkgray" )
pred.glucosa <- predict(diab2.3.m1, data.frame(glucose=seq(44,210,1),
                                               diastolic=mean(diab2.3$diastolic),
                                               bmi=mean(diab2.3$bmi), 
                                               pedi=mean(diab2.3$pedi),
                                               age=mean(diab2.3$age),npreg=mean(diab2.3$npreg)), type="response")
rango.glucosa <- seq(44,210,1)

lines(rango.glucosa, pred.glucosa, lwd=2, col="brown")

#ver los odds, exp del coeficiente.
exp(7.521e-02)

#Cada aumento de un punto de la glucosa en plasma aumenta en promedio la probabilidad de diabetes en un factor de 1.08

