
datos <- read.csv("p/Colombianos_registrados_en_el_exterior.csv",
                  header = TRUE, fileEncoding = "UTF-8")
View(datos)
head(datos)

#Variables cualitativas:
pais <- datos$Pa�s
area <- datos$�rea.Conocimiento
genero <- datos$G�nero

#Variables cuantitativas
edad <- datos$Edad..a�os.
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

### AN�LISIS ###

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
# La media de edad entre los colombianos registrados en el exterior es de 45 a�os
# La edad que m�s se repite es 37 a�os, seguida por 40 y 42.
# La dispersi�n de los datos es alta (35.84%), es decir los datos no son homog�neos.
# El test de simetr�a de Fischer nos sugiere que los datos son sim�tricos.
# Sin embargo, la media, la moda y la mediana no est�n cercanas entre ellas.
# La curtosis es de 0.1, lo que nos indica que los datos no est�n cercanos a la media.

T4=table(edad)
T4c=c(T4)
data.frame(ni=T4c, Ni=cumsum(T4c), fi=T4c/sum(T4c), Fi=cumsum(T4c/sum(T4c)))
plot(T4,type="h",xlab="",ylab="",main="",frame=0,lwd=3)

# Un diagrama de palos nos muestra la distribuci�n de los datos, y adem�s se�ala algunos
# datos at�picos. Por un lado, el -1 se repite aproximadamente 2500 veces (menos del 0.5% del total)
# y hay algunas edades por encima de 100. Convendr�a rectificar la validez de estos datos.

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
# El valor m�s com�n en este conjunto de datos es 1, seguido por 2 y 3. Es decir,
# es m�s com�n que los colombianos se registren en el exterior individualmente,
# o en grupos peque�os. En efecto, la media del conjunto de datos es de 1.72.
# La mediana es 1, y esto tiene sentido si se tiene en cuenta que el aproximadamente
# el 82% de los datos registrados corresponden a 1 sola persona.
# El coeficiente de variaci�n es de 277.67%, lo que indica una dispersi�n de los
# datos extremadamente alta. El test de Fischer nos indica que los datos tienen una
# alta asimetr�a positiva. Adem�s, la curtosis es de 894.17.

T4=table(personas)
T4c=c(T4)
data.frame(ni=T4c, Ni=cumsum(T4c), fi=T4c/sum(T4c), Fi=cumsum(T4c/sum(T4c)))
plot(T4,type="h",xlab="",ylab="",main="",frame=0,lwd=3)

# Un diagrama de palos nos muestra la asimetr�a de los datos.

#################################################################################

## VARIABLES CUALITATIVAS

T1=table(genero)
V1=c(T1) # Esta funcion convierte la tabla en un vector
FA = V1/sum(V1)
Tabla = data.frame(ni=V1,fi = FA, Porcentaje = FA*100)
Tabla
barplot(T1, main = 'G�nero de colombianos en el exterior')
pie(T1,radius=1.0, main = 'G�nero de colombianos en el exterior')

tabla <- table(pais);sort(tabla, decreasing = TRUE)
tabla <- table(area);sort(tabla, decreasing = TRUE)

area_df <- data.frame(area)
ggplot(area_df,  aes(x = factor(""), fill = area)) +  geom_bar() +
   coord_polar(theta = "y") + scale_x_discrete("")

# El 55% de los colombianos registrados en el exterior son de g�nero femenino.
# Los pa�ses en los que hay mayor n�mero de colombianos registrados son
# Estados Unidos (25,17%), Espa�a (18,83%), Venezuela (16,44%) y Ecuador (5,13 %).
# Casi el 63% de los datos corresponden a personas que no indicaron cu�l era su
# �rea del conocimiento o que dijeron no tener ninguna. 
# El �rea de conocimiento m�s com�n es ingenier�as, arquitectura y afines (9,62%), 
# seguido por econom�a y afines (8,9%) y ciencias sociales y humanas (6,28%)

area_genero <- table(area, genero)
genero_df <- data.frame(genero)
ggplot(genero_df) +  geom_bar(aes(genero)) + coord_flip() 
balloonplot(t(area_genero), main ="Tabla de contingencia", xlab ="", ylab="",
            label = FALSE, show.margins = TRUE)

# Hay m�s hombres que mujeres en el �rea de ingenier�a y afines, pero hay m�s 
# mujeres en el �rea de econom�a, administraci�n y afines. En ciencias sociales
# y humanas hay casi el doble de mujeres que de hombres. En matem�ticas y 
# ciencias naturales hay un n�mero parecido de hombres y mujeres.


#################################################################################

# ANEXO: Cuentas an�lisis cualitativo
148036/588970 # USA
110932/588970 # Espa�a
96837/588970 # Venezuela
30260/588970 # Ecuador
(221526+143614)/588970 # No indica o ning�n �rea
56686/588970 # Ingenier�a y afines
52456/588970 # Econom�a y afines
37033/588970 # Ciencias sociales y humanas
2500/588000 # dato at�pico (-1)

#################################################################################


