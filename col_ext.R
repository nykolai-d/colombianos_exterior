
datos <- read.csv("p/Colombianos_registrados_en_el_exterior.csv",
                  header = TRUE, fileEncoding = "UTF-8")
View(datos)
head(datos)

#Variables cualitativas:
pais <- datos$País
area <- datos$Área.Conocimiento
genero <- datos$Género

#Variables cuantitativas
edad <- datos$Edad..años.
personas <- datos$Cantidad.de.personas

#################################################################################

## PAQUETES Y LIBRERIAS ##

install.packages("gplots")
install.packages("ggplot2")

library(gplots)
library(ggplot2)

#################################################################################

## FUNCIONES ##
coef_var <- function(x, na.rm = FALSE) {
   100*sd(x, na.rm=na.rm) / mean(x, na.rm=na.rm)
}

Fishasimetria <- function(x) {  
   m3 <- mean((x-mean(x))^3) 
   Fisher <- m3/((sd(x)^3))  
   Fisher}

kurtosis <- function(k) {  
   m4 <- mean((k-mean(k))^4) 
   kurt <- (m4/((sd(k)^4)))-3  
   kurt
}

#################################################################################

### ANÁLISIS ###

## MEDIDAS DE TENDENCIA CENTRAL DE VARIABLES CUANTITATIVAS

#Edad
summary(edad)
mean(edad)
tabla <- table(edad);sort(tabla, decreasing = TRUE) 
median(edad) 
var(edad) 
sd(edad) 
coef_var(edad) 
Fishasimetria(edad) 
kurtosis(edad)
T4=table(edad)
T4c=c(T4)
data.frame(ni=T4c, Ni=cumsum(T4c), fi=T4c/sum(T4c), Fi=cumsum(T4c/sum(T4c)))
plot(T4,type="h",xlab="",ylab="",main="",frame=0,lwd=3)

## EDAD ##
# La media de edad entre los colombianos registrados en el exterior es de 45 años
# La edad que más se repite es 37 años, seguida por 40 y 42.
# La dispersión de los datos es alta (35.84%), es decir los datos no son homogéneos.
# El test de simetría de Fischer nos sugiere que los datos son simétricos.
# Sin embargo, la media, la moda y la mediana no están cercanas entre ellas.
# La curtosis es de 0.1, lo que nos indica que los datos no están cercanos a la media.

T4=table(edad)
T4c=c(T4)
data.frame(ni=T4c, Ni=cumsum(T4c), fi=T4c/sum(T4c), Fi=cumsum(T4c/sum(T4c)))
plot(T4,type="h",xlab="",ylab="",main="",frame=0,lwd=3)

# Un diagrama de palos nos muestra la distribución de los datos, y además señala algunos
# datos atípicos. Por un lado, el -1 se repite aproximadamente 2500 veces (menos del 0.5% del total)
# y hay algunas edades por encima de 100. Convendría rectificar la validez de estos datos.

# Cantidad de personas registradas
summary(personas)
mean(personas)
tabla <- table(personas);sort(tabla, decreasing = TRUE)
median(personas)
var(personas)
sd(personas)
coef_var(personas)
Fishasimetria(personas) 
kurtosis(personas)
487/588
T4=table(personas)
T4c=c(T4)
data.frame(ni=T4c, Ni=cumsum(T4c), fi=T4c/sum(T4c), Fi=cumsum(T4c/sum(T4c)))
plot(T4,type="h",xlab="",ylab="",main="",frame=0,lwd=3)

## CANTIDAD DE PERSONAS REGISTRADAS ##
# El valor más común en este conjunto de datos es 1, seguido por 2 y 3. Es decir,
# es más común que los colombianos se registren en el exterior individualmente,
# o en grupos pequeños. En efecto, la media del conjunto de datos es de 1.72.
# La mediana es 1, y esto tiene sentido si se tiene en cuenta que el aproximadamente
# el 82% de los datos registrados corresponden a 1 sola persona.
# El coeficiente de variación es de 277.67%, lo que indica una dispersión de los
# datos extremadamente alta. El test de Fischer nos indica que los datos tienen una
# alta asimetría positiva. Además, la curtosis es de 894.17.

T4=table(personas)
T4c=c(T4)
data.frame(ni=T4c, Ni=cumsum(T4c), fi=T4c/sum(T4c), Fi=cumsum(T4c/sum(T4c)))
plot(T4,type="h",xlab="",ylab="",main="",frame=0,lwd=3)

# Un diagrama de palos nos muestra la asimetría de los datos.

#################################################################################

## VARIABLES CUALITATIVAS

T1=table(genero)
V1=c(T1) # Esta funcion convierte la tabla en un vector
FA = V1/sum(V1)
Tabla = data.frame(ni=V1,fi = FA, Porcentaje = FA*100)
Tabla
barplot(T1, main = 'Género de colombianos en el exterior')
pie(T1,radius=1.0, main = 'Género de colombianos en el exterior')

tabla <- table(pais);sort(tabla, decreasing = TRUE)
tabla <- table(area);sort(tabla, decreasing = TRUE)

area_df <- data.frame(area)
ggplot(area_df,  aes(x = factor(""), fill = area)) +  geom_bar() +
   coord_polar(theta = "y") + scale_x_discrete("")

# El 55% de los colombianos registrados en el exterior son de género femenino.
# Los países en los que hay mayor número de colombianos registrados son
# Estados Unidos (25,17%), España (18,83%), Venezuela (16,44%) y Ecuador (5,13 %).
# Casi el 63% de los datos corresponden a personas que no indicaron cuál era su
# área del conocimiento o que dijeron no tener ninguna. 
# El área de conocimiento más común es ingenierías, arquitectura y afines (9,62%), 
# seguido por economía y afines (8,9%) y ciencias sociales y humanas (6,28%)

area_genero <- table(area, genero)
genero_df <- data.frame(genero)
ggplot(genero_df) +  geom_bar(aes(genero)) + coord_flip() 
balloonplot(t(area_genero), main ="Tabla de contingencia", xlab ="", ylab="",
            label = FALSE, show.margins = TRUE)

# Hay más hombres que mujeres en el área de ingeniería y afines, pero hay más 
# mujeres en el área de economía, administración y afines. En ciencias sociales
# y humanas hay casi el doble de mujeres que de hombres. En matemáticas y 
# ciencias naturales hay un número parecido de hombres y mujeres.


#################################################################################

# ANEXO: Cuentas análisis cualitativo
148036/588970 # USA
110932/588970 # España
96837/588970 # Venezuela
30260/588970 # Ecuador
(221526+143614)/588970 # No indica o ningún área
56686/588970 # Ingeniería y afines
52456/588970 # Economía y afines
37033/588970 # Ciencias sociales y humanas
2500/588000 # dato atípico (-1)

#################################################################################


