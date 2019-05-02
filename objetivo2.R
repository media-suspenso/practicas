Practica1_2 = c(datos$Practica.1..0.2.3.*3*5)
Practica1_2[Practica1_2>10] = 10
Practica1_2[is.na(Practica1_2)] = -1
Practica2_2 = c(datos$Practica.2...0.2.3.*3*5)
Practica2_2[Practica2_2>10] = 10
Practica2_2[is.na(Practica2_2)] = -1
Practica3_2 = c(datos$Practica.3...0.2.3.*3*5)
Practica3_2[Practica3_2>10] = 10
Practica3_2[is.na(Practica3_2)] = -1
PracticaFinal_2 = c(datos$TOTAL.practicas..0.2.*5)
PracticaFinal_2[PracticaFinal_2>10] = 10
PracticaFinal_2[is.na(PracticaFinal_2)] = -1

Cuestionario1_2 = c(datos$Cuestionario.temas.1.y.2..0.10.)
Cuestionario1_2[is.na(Cuestionario1_2)] = -1
Cuestionario2_2 = c(datos$Cuestionario.tipo.test.temas.3.y.4...0.10.)
Cuestionario2_2[is.na(Cuestionario2_2)] = -1
Cuestionario3_2 = c(datos$Cuestionario.tipo.test.temas.5.y.6...0.10.)
Cuestionario3_2[is.na(Cuestionario3_2)] = -1
TotalCuestionarios_2 = c(datos$Total.cuestionarios..0.1.*10)
TotalCuestionarios_2[TotalCuestionarios_2>10] = 10
TotalCuestionarios_2[is.na(TotalCuestionarios_2)] = -1

P1_2 = cut(Practica1_2, breaks = c(-1.1, -0.1, 4.9, 6.9, 8.9, 10), labels = c("No Presentado", "Suspenso", "Aprobado", "Notable", "Sobresaliente"))
P2_2 = cut(Practica2_2, breaks = c(-1.1, -0.1, 4.9, 6.9, 8.9, 10), labels = c("No Presentado", "Suspenso", "Aprobado", "Notable", "Sobresaliente"))
P3_2 = cut(Practica3_2, breaks = c(-1.1, -0.1, 4.9, 6.9, 8.9, 10), labels = c("No Presentado", "Suspenso", "Aprobado", "Notable", "Sobresaliente"))
PF_2 = cut(PracticaFinal_2, breaks = c(-1.1, -0.1, 4.9, 6.9, 8.9, 10), labels = c("No Presentado", "Suspenso", "Aprobado", "Notable", "Sobresaliente"))

C1_2 = cut(Cuestionario1_2, breaks = c(-1.1, -0.1, 4.9, 6.9, 8.9, 10), labels = c("No Presentado", "Suspenso", "Aprobado", "Notable", "Sobresaliente"))
C2_2 = cut(Cuestionario2_2, breaks = c(-1.1, -0.1, 4.9, 6.9, 8.9, 10), labels = c("No Presentado", "Suspenso", "Aprobado", "Notable", "Sobresaliente"))
C3_2 = cut(Cuestionario3_2, breaks = c(-1.1, -0.1, 4.9, 6.9, 8.9, 10), labels = c("No Presentado", "Suspenso", "Aprobado", "Notable", "Sobresaliente"))
TC_2 = cut(TotalCuestionarios_2, breaks = c(-1.1, -0.1, 4.9, 6.9, 8.9, 10), labels = c("No Presentado", "Suspenso", "Aprobado", "Notable", "Sobresaliente"))

NotaFinalJunio_2 = c(datos$NOTA.FINAL.JUNIO...0.10.)
NotaFinalJunio_2[is.na(NotaFinalJunio_2)] = -1
NFJ_2 = cut(NotaFinalJunio_2, breaks = c(-1.1, -0.1, 4.9, 6.9, 8.9, 10), labels = c("No Presentado", "Suspenso", "Aprobado", "Notable", "Sobresaliente"))

modelo2 <- lm(NotaFinalJunio ~ Practica2 + Practica3 +Practica1+TotalCuestionarios)
 PrediccionCuantitativa2 = predict(modelo2,data.frame(Practica2,Practica3,Practica1,TotalCuestionarios))
 PrediccionCualitativa2 =cut(PrediccionCuantitativa2, breaks = c(-1,1, 5,7,9,9.99999, 10), labels = c("Suspendidisimo","Suspenso", "Aprobado","Notable","Sobresaliente","Mátricula de honor"))
 
