##-------------------------------------------------##
##-----------      Modelos Linelaes     -----------##
##-----------        Trabajo 01         -----------##
##-- Nombre: Andrea Perez


# 2.1 Leer el archivo de datos data.txt, y analizar de que estructura de datos se trata.
# Utilice la función read.table()
file<-read.table("data.txt", header = TRUE, sep = "\t",dec = ",")
str(data)

# 2.2 Calcular el mínimo, la media, el máximo de la variable Edad.
# Utilice las funciones min(), mean(), max(), de ser necesario utilice 
# el parámetro na.rm = TRUE
Edad<-data[,1]

min(Edad, na.rm = TRUE)
max(Edad, na.rm = TRUE)
mean(Edad, na.rm = TRUE)



# 2.3 Para la variable Genero, contar cuantos sujetos son de Genero: Femenino.
# Utilice la función table()
Genero<-data[,3]
table(Genero)

# 2.4 Encontrar la Edad mínima, media, máxima de los sujetos que Si son dependientes.
subdata <- subset (data, subset=data["Dependiente"]=="Si")

Edaddependientes<-subdata[,"Edad"]
min(Edaddependientes)
max(Edaddependientes)
mean(Edaddependientes)

# 2.5 Identificar el tipo de elementos que contiene cada variable.
# Utilice la función typeof()
tiposdevariable <- numeric(ncol(data))
for (i in 1:ncol(data)){
        tiposdevariable[i] <- typeof(data[,i])
}
tiposdevariable



# 2.6 Identificar la clase de cada variable (columna).
# Utilice la función class()
clase <- numeric(ncol(data))
for (i in 1:ncol(data)){
        clase[i] <- class(data[,i])
}
clase

# 2.7 Calcular la media de todas las variables numéricas (double, integer).
# Recordar que para un factor no es posible obtener la media debido a que 
# éstos representan variables
data_num <- data[,clase!="factor"]
media <- numeric(ncol(data_num))
for (i in 1:ncol(data_num)){
        media[i] <- mean(data_num[,i],na.rm = TRUE)
}
media
# 2.8 Calcular el porcentaje de valores perdidos que contiene cada variable.
# Utilice la función is.na()
vacios <- numeric(ncol(data))
for (i in 1:ncol(data)){
        vacios[i] <- sum(is.na(data[,i]))/nrow(data)
}
vacios

# 3. Selecionando sujetos mediante un determinado criterio:
# 3.1 Seleccione los sujetos con una Edad mayor a 40 años.
# Utilice la función subset()
dataa <- subset (data, subset=data["Edad"]>40)

# 3.2 Seleccione los sujetos que tienen Vivienda Propia.
datab <- subset (data, subset=data["Vivienda"]=="Propia")

# 3.3 Seleccione los sujetos que tienen más ($>$) de dos cargas familiatres.
datac <- subset (data, subset=data["Edad"]>40)

# 3.4 Seleccione los sujetos con una Deuda superior o igual a 500 dólares
# y más ($>$) de 8 Dias_Atraso.
datad <- subset (data, subset=data["Deuda"]>=500 & data["Dias_Atraso"]>8)

# 3.5 Seleccione los sujetos con un Score mayor o igual a 900 puntos, una Edad menor
# o igual a 35 años y con más ($>$) de 3 tarjetas de crédito (Numero_TC).
datae <- subset (data, subset=data["Score"]>=900 & data["Edad"]>=35 & data["Numero_TC"]>3)

# 4. Gráficos:
# 4.1 Realice un histograma de la variable Edad, utilice como color de relleno: red
hist(data[,"Edad"], col="red")

# 4.2 Realice un diagrama de cajas de la variable Edad, utilice como color de relleno: green
# Utilice la función boxplot()
boxplot(data[,"Edad"], col="green")


