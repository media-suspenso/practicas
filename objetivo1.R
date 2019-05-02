datos <- read.csv("Notas.csv", sep = ";", dec = ",", header = T)

install.packages("e1071")
library(e1071)

Practica1 = c(datos$Practica.1..0.2.3.*3*5)
Practica1[Practica1>10] = 10
Practica1[is.na(Practica1)] = 0
Practica2 = c(datos$Practica.2...0.2.3.*3*5)
Practica2[Practica2>10] = 10
Practica2[is.na(Practica2)] = 0
Practica3 = c(datos$Practica.3...0.2.3.*3*5)
Practica3[Practica3>10] = 10
Practica3[is.na(Practica3)] = 0
PracticaFinal = c(datos$TOTAL.practicas..0.2.*5)
PracticaFinal[PracticaFinal>10] = 10
PracticaFinal[is.na(PracticaFinal)] = 0

Cuestionario1 = c(datos$Cuestionario.temas.1.y.2..0.10.)
Cuestionario1[is.na(Cuestionario1)] = 0
Cuestionario2 = c(datos$Cuestionario.tipo.test.temas.3.y.4...0.10.)
Cuestionario2[is.na(Cuestionario2)] = 0
Cuestionario3 = c(datos$Cuestionario.tipo.test.temas.5.y.6...0.10.)
Cuestionario3[is.na(Cuestionario3)] = 0
TotalCuestionarios = c(datos$Total.cuestionarios..0.1.*10)
TotalCuestionarios[TotalCuestionarios>10] = 10
TotalCuestionarios[is.na(TotalCuestionarios)] = 0

P1 = cut(Practica1, breaks = c(-1, 5, 10), labels = c("Suspenso", "Aprobado"))
P2 = cut(Practica2, breaks = c(-1, 5, 10), labels = c("Suspenso", "Aprobado"))
P3 = cut(Practica3, breaks = c(-1, 5, 10), labels = c("Suspenso", "Aprobado"))
PF = cut(PracticaFinal, breaks = c(-1, 5, 10), labels = c("Suspenso", "Aprobado"))

C1 = cut(Cuestionario1, breaks = c(-1, 5, 10), labels = c("Suspenso", "Aprobado"))
C2 = cut(Cuestionario2, breaks = c(-1, 5, 10), labels = c("Suspenso", "Aprobado"))
C3 = cut(Cuestionario3, breaks = c(-1, 5, 10), labels = c("Suspenso", "Aprobado"))
TC = cut(TotalCuestionarios, breaks = c(-1, 5, 10), labels = c("Suspenso", "Aprobado"))

NotaFinalJunio = c(datos$NOTA.FINAL.JUNIO...0.10.)
NotaFinalJunio[is.na(NotaFinalJunio)] = -1
NFJ = cut(NotaFinalJunio, breaks = c(-1, 5, 10), labels = c("Suspenso", "Aprobado"))

m <- naiveBayes(P1 ~ ., data = as.data.frame(NFJ))
View(P1)

mC1 <- naiveBayes(C1 ~ ., data = as.data.frame(NFJ))
mC2 <- naiveBayes(C2 ~ ., data = as.data.frame(NFJ))
mC3 <- naiveBayes(C3 ~ ., data = as.data.frame(NFJ))
mP1 <- naiveBayes(P1 ~ ., data = as.data.frame(NFJ))
mP2 <- naiveBayes(P2 ~ ., data = as.data.frame(NFJ))
mP3 <- naiveBayes(P3 ~ ., data = as.data.frame(NFJ))
mPF <- naiveBayes(PF ~ ., data = as.data.frame(NFJ))
