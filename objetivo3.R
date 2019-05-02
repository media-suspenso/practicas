Practica1_3 = c(datos$Practica.1..0.2.3.*3*5)
Practica1_3[Practica1_3>10] = 10
Practica1_3[is.na(Practica1_3)] = 0
Practica2_3 = c(datos$Practica.2...0.2.3.*3*5)
Practica2_3[Practica2_3>10] = 10
Practica2_3[is.na(Practica2_3)] = 0
Practica3_3 = c(datos$Practica.3...0.2.3.*3*5)
Practica3_3[Practica3_3>10] = 10
Practica3_3[is.na(Practica3_3)] = 0
PracticaFinal_3 = c(datos$TOTAL.practicas..0.2.*5)
PracticaFinal_3[PracticaFinal_3>10] = 10
PracticaFinal_3[is.na(PracticaFinal_3)] = 0

Cuestionario1_3 = c(datos$Cuestionario.temas.1.y.2..0.10.)
Cuestionario1_3[is.na(Cuestionario1_3)] = 0
Cuestionario2_3 = c(datos$Cuestionario.tipo.test.temas.3.y.4...0.10.)
Cuestionario2_3[is.na(Cuestionario2_3)] = 0
Cuestionario3_3 = c(datos$Cuestionario.tipo.test.temas.5.y.6...0.10.)
Cuestionario3_3[is.na(Cuestionario3_3)] = 0
TotalCuestionarios_3 = c(datos$Total.cuestionarios..0.1.*10)
TotalCuestionarios_3[TotalCuestionarios_3>10] = 10
TotalCuestionarios_3[is.na(TotalCuestionarios_3)] = 0
 modelo2 <- lm(Practica1 ~ Practica2) #Crea un modelo sobre la practica 1 en base a la practica2
 predict(modelo,data.frame(Practica2=5))#Predice la nota de la practica 1 teniendo un 5 en la practica2
 modelo <- lm(PracticaFinal ~ Practica2 + Practica3 +Practica1+TotalCuestionarios) # Crea un modelo para la practicaFinal teniendo en cuenta las 3 practicas y los cuestionarios
 predict(modelo,data.frame(Practica2,Practica3,Practica1,TotalCuestionarios)) #Predice la practica final respecto las 3 practicas y cuestionarios
 PrediccionCualitativa = predict(modelo,data.frame(Practica2,Practica3,Practica1,TotalCuestionarios)) #Guardo en una variable las predicciones en forma cuantitativa
PruebaCuantitativa=cut(Prueba, breaks = c(-1, 5, 10), labels = c("Suspenso", "Aprobado")) #Transformo las variables a cualitativas
#3.3
datos2 <- read.csv("Notas2.csv", sep = ";", dec = ",", header = T)
Cuestionario1_1.1=c(datos2$C1)
Cuestionario1_1.1[is.na(Cuestionario1_1.1)] = 0
Cuestionario1_2.1=c(datos2$C2)
Cuestionario1_2.1[is.na(Cuestionario1_2.1)] = 0
Practica1_1.1=c(datos2$P1)
Practica1_1.1[is.na(Practica1_1.1)] = 0
P1_1.1=cut(Practica1_1.1, breaks = c(-1.1, -0.1, 4.9, 6.9, 8.9, 10), labels = c("No Presentado", "Suspenso", "Aprobado", "Notable", "Sobresaliente"))
C1_1.1=cut(Cuestionario1_1.1, breaks = c(-1.1, -0.1, 4.9, 6.9, 8.9, 10), labels = c("No Presentado", "Suspenso", "Aprobado", "Notable", "Sobresaliente"))
C2_2.1=cut(Cuestionario1_2.1, breaks = c(-1.1, -0.1, 4.9, 6.9, 8.9, 10), labels = c("No Presentado", "Suspenso", "Aprobado", "Notable", "Sobresaliente"))
modelo3 <- lm(NotaFinalJunio_2 ~ Cuestionario1_1.1 + Cuestionario1_2.1 + Practica1_1.1)
predict(modelo3,data.frame(Cuestionario1_1.1,Cuestionario1_2.1,Practica1_1.1))
#Tablas Personales
datos3 <- read.csv("Notas3.csv", sep = ";", dec = ",", header = T)
Cuestionario1_1.2=c(datos3$C1)
Cuestionario1_1.2[is.na(Cuestionario1_1.1)] = -1
Cuestionario1_2.2=c(datos3$C2)
Cuestionario1_2.2[is.na(Cuestionario1_2.1)] = -1
Practica1_1.2=c(datos3$P1)
Practica1_1.2[is.na(Practica1_1.1)] = -1
P1_1.2=cut(Practica1_1.2, breaks = c(-1.1, -0.1, 4.9, 6.9, 8.9, 10), labels = c("No Presentado", "Suspenso", "Aprobado", "Notable", "Sobresaliente"))
table(P1_1.2)
C1_1.2=cut(Cuestionario1_1.2, breaks = c(-1.1, -0.1, 4.9, 6.9, 8.9, 10), labels = c("No Presentado", "Suspenso", "Aprobado", "Notable", "Sobresaliente"))
table(C1_1.2)
C2_2.2=cut(Cuestionario1_2.2, breaks = c(-1.1, -0.1, 4.9, 6.9, 8.9, 10), labels = c("No Presentado", "Suspenso", "Aprobado", "Notable", "Sobresaliente"))
table(C2_2.2)
