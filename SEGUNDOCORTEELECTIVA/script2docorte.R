## cargar database

data <- read.delim("clipboard")
data
str(data)
head(data)
tail(data)
summary(data)
#Si quisiera ver el resumen de una variable debo profundizar en el database, con la siguiente funcion.....
attach(data)
#Ahora si puedo pedir el resumen de cada variable, en este caso "BMI"
summary(BMI)
#Si quiero ver la media de alguna variable, utilzo mean y en este caso "Smoker"
mean(Smoker)
#Puedo solicitar el resumen completo o cada una de las variables, min, max, mean, median entre otros.

#Es posible ver la varianza por variable

var(BMI)

#Desviacion estandar
sd(BMI)

#Histograma de Diabetes_012, yo estoy usando BMI para el ejemplo pero podria ser cualquier variable
hist(log(Diabetes_012), freq = F)

#Si quiero agregar una linea de tendencia utilizo la siguiente funcion, puedo escoger el color que mas me guste... 
lines(density(log(Diabetes_012)), col= "blue")

# Empezare a reducir el database ya que es muy amplio, para esto podremos colocar la función "set.seed"
set.seed(15000) #este nos sirve para darnos siempre la misma muestra, en caso de no tenerlo, cada que queramos ver una muestra, mostrara una diferente.

# llamaremos una nueva variable para la muestra aleatoria y seleccionaremos el numero de observaciones a mostrar
muestra_aleatoria <- sample(253680,500, replace = FALSE);muestra_aleatoria

#Seleccionamos la funcion sort para organizar nuestra muestra
orden_muestra <- sort(muestra_aleatoria);orden_muestra

#Para visualizarlo en una tabla seleccionamos un nombre para la muestra, llamamos la base de datos principal, y escribimos "view"
muestra <- data[orden_muestra,]; View(muestra)

#Ahora podemos hacer lo mismo que hicimos antes, pero con la muestra, seleccionar un resumen, la media de una variable, la mediana, el maximo, minimo u otros.
summary(muestra$BMI)

#Puedo mostrar los datos generales de la muestra
str(muestra$Diabetes_012)

#Puedo hacer una tabla de la muestra
table(muestra$Smoker)
tab <- table(muestra$Smoker)  

#Mostrare un grafico de barras 
barplot(muestra$Smoker)

#Tambien se puede mostrar un grafico de pastel pero es mas util en variables categoricas
pie(muestra$Smoker)

# Ahora mostrare la correlacion entre dos variables numericas

cor(log(Diabetes_012), BMI)

#Correlacion por spearman
cor(log(Diabetes_012), BMI, method = "spearman")

#Graficar correlacion, la mas cercana a la diabetes es el indice de masa corporal "BMI"
plot(log(Diabetes_012) ~ BMI)

installed.packages("car")
library(car)

#Para comprobar cuantas personas tienen diabetes, prediabetes o no tienen
table(muestra$Diabetes_012)


#Llamamos una nueva variable nueva para cambiar la variable de numerica a categorica
#La función "as.factor" se utiliza para codificar un vector como factor 
#(los términos "categoría" y "tipo enumerado" también se utilizan para factores).

muestra$Diabetes_012A <- as.factor(muestra$Diabetes_012) 
str(muestra$Diabetes_012A)


#Teniendo en cuenta lo suministrado por el docente, se tiene en cuenta que los
#pacientes sin diabetes se representan como 0, prediabetes 1 y diabetes 2
muestra$Diabetes_012A = factor(muestra$Diabetes_012A, 
                               levels = levels(muestra$Diabetes_012A),
                               labels = c("No Diabetes", "Prediabetes", "Diabetes"),
                               ordered = F)
#Con la funcion levels enlaza el 0, 1 y 2 de la variable Diabetes_012 y luego
#con labels coloco los nombres que cambiare, en este caso, no diabetes, prediabetes
# y diabetes

#nuevamente str para ver si se realizo el cambio a estas variables categoricas
str(muestra$Diabetes_012A)

#Crear una tabla que me muestre cuantos pacientes tienen diabetes, cuantos no y cuantos
#tienen prediabetes.
table(muestra$Diabetes_012A);
print(muestra)
Datosbinarizados <- (muestra)
plot(Datosbinarizados$Diabetes_012A)


#Segundo punto

set.seed(50561)

#Reduje los datos al 1% como lo pide el docente
muestra_Diabetes <- sample(253680,2536, replace = F);muestra_Diabetes

orden_muestra1 <- sort(muestra_Diabetes);orden_muestra1

#LLamo una variable llamada muestra 2 para ver la tabla
muestra2 <- data[orden_muestra1,]; View(muestra2)

#resumen de la variable Diabetes que he creado
summary(muestra2$Diabetes)

str(muestra2$Diabetes)

#Tabla con datos generales
table(muestra2$Diabetes)
tab <- table(muestra2$Diabetes) 

muestra2$Diabetes <- as.factor(muestra2$Diabetes) 
str(muestra2$Diabetes)

#Transformo los datos numericos en categoricos

muestra2$Diabetes = factor(muestra2$Diabetes, 
                               levels = levels(muestra2$Diabetes),
                               labels = c("No Diabetes", "Prediabetes", "Diabetes"),
                               ordered = F)


str(muestra2$Diabetes)



normalise <- function(x){(x-min(x))/(max(x)-min(x))}

# Se realiza binarizacion para pacientes con diabetes o prediabetes les muestra un 1 y los que no tienen se muestra un 0
muestra2$Diabetes1 <- as.numeric(muestra2$Diabetes!="No Diabetes")
