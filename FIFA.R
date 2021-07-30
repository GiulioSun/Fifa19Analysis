
#===================================================================================

# ANALISI PER COMPONENTI PRINCIPALI  

#===================================================================================

# -------------------------------------
# Installiamo e importiamo le librerie 
# -------------------------------------
#install.packages("FactoMineR")
#install.packages("corrplot")

library(FactoMineR)
library(MASS)
library(corrplot)

# -------------------------------------
# Impostiamo la directory
# -------------------------------------
setwd("/Users/giulio/Projects/DataMiningFifa")

# -------------------------------------
# Importiamo il data set
# -------------------------------------
df <- read.delim2("fifaDf.csv",header = TRUE, sep = ';', row.names = 'long_name')
View(df)
fifaDf <- df[,-21]
fifaDf <- fifaDf[, 1:21]
View(fifaDf)

# -------------------------------------
# Statistiche descrittive
# -------------------------------------
summary(df[,1:21])
boxplot(fifaDf[1:20], las = 2, main = "Distribuzione variabili campionarie", border = 'blue')

# -------------------------------------
# Matrice di correlazione
# -------------------------------------
corMatrix <- cor(fifaDf[, 1:20])
print(corMatrix)
corrplot(corMatrix, method = 'color',
         type = 'upper',bg = 'white', tl.col = 'black')


# -------------------------------------
# Matrice varianza covarianza
# -------------------------------------
covMatrx <- cov(fifaDf[, 1:20])
covMatrx

# -------------------------------------
# Calcoliamo la distrubuzione del nostro campione considerando la variabile Overall che identifica la bravura complessiva di un giocatore
# -------------------------------------
hist(df[,21], main = "Overall distribuction", col = "aquamarine3", xlab = "overall", border ='blue', breaks = 10)

# -------------------------------------
# PCA
# -------------------------------------
fifa.pca <- PCA(fifaDf, scale.unit =  TRUE , quali.sup = 21)

# -------------------------------------
# Stampiamo gli oggetti generati dalla funzione PCA
# -------------------------------------
print(fifa.pca)

# -------------------------------------
# SUMMARY della PCA
# --------------------------------------
summary(fifa.pca)


#-----------------------------------------
#Autovalori e Varianza 
#-----------------------------------------
autovalori <- fifa.pca$eig
autovalori <- autovalori[1:7, 1]
print(autovalori)
barplot(autovalori, main = "Bar Plot Autovalori",col = c(rep("green",2), rep("red", 5)))
abline(h=2, col="blue")
plot(autovalori, type = "lines", main = "Scree Plot", xlab = "Componenti")
plot(fifa.pca$eig[1:7, 3], type = "lines", main = "Varianza cumulata", xlab = "Componenti", ylab = "Percentuale di varianza spiegata dalle componenti")


#-----------------------------------------
#Medie e deviazioni standard
#-----------------------------------------
round(cbind(fifa.pca$call$centre, fifa.pca$call$ecart.type),2)

# -------------------------------------
# Correlazione delle variabili con le componenti principali
# -------------------------------------
CorrCP <- dimdesc(fifa.pca, proba = 1)
CorrCP$Dim.1                          
CorrCP$Dim.2
CorrCP$Dim.3

# Possiamo calcolarla anche in questo modo:
#-----------------------------------------
# Correlazione variabili/componenti                                   #
#-----------------------------------------                            #
fifa.pca$var$cor                                                      #
# NOTA BENE
#-----------------------------------------                            # Essendo che abbiamo standardizzato
# Coordinate delle variabili sulle componenti principali              # le variabili originarie avremo che 
#-----------------------------------------                            # le coordinate = correlazione
fifa.pca$var$coord                                                    #
#
#-----------------------------------------                            #
# Cos2 Qualità delle varibili, quanto i vettori delle variabili sono vicine al cerchio delle correlazioni
# ci indica quantociascuna componente spiega una variabile
#-----------------------------------------
fifa.pca$var$cos2

#-----------------------------------------
# Contributo delle variabili alle componenti principali
# ci indica quanto ogni variabile spiega la compennte
#-----------------------------------------
fifa.pca$var$contrib

#-----------------------------------------
# Caratterizzazione degli assi
#-----------------------------------------
dimdesc(fifa.pca)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Quante PC dovremmo tenere? Questo è probabilmente il problema principale quando si esegue una PCA. 
#Poiché il criterio per una buona proiezione nella PCA è una elevata varianza per quella proiezione, 
#dovremmo tenere solo quelle componenti principali con varianze elevate. Il problema, pertento, 
#coinvolge i valori degli autovalori della matrice di varianza/covarianza (o correlazione) campionaria.
#Diversi criteri sono stati introdoti in letteratura, e qui di seguito ne vediamo i più popolari:

#varianza spiegata: si tiene un numero di componenti sufficiente a riprodurre una grande percentuale
#pre-specificata della variabilità complessiva delle variabili originarie. 
#Sono usualmente suggeriti valori compresi tra il 70% e il 90%.

#scree plot: questo è un grafico che rappresenta gli aoutovalori ordinati in senso decescente (asse y) 
#verso il loro numero d’ordine (assex). Se i più grandi autovalori campionari dominano in dimensione, 
#ed i rimanenti autovalori campionari sono molto piccoli, allora lo scree plot mostrerà un “gomito” nel 
#grafico in corrispondenza alla divisione tra “grandi” e “piccoli” valori degli autovalori campionari. 
#La posizione in cui si presenta il gomito, può essere usata come numero di PC da tenere.

#Regola di Kaiser: tenere solo le PC i cui autovalori superano il valore 1. Questa linea guida si 
#basa sull’idea che, poichè la variabilità totale di tutte le p variabili standardizzate è uguale 
#a p, ne segue che una PCA dovrebbe spiegare almeno una variazione pari al valore medio di una singola
#variabile standardizzata. Questa regola è popolare ma controversa; c’è evidenza che il valore limite 
#di 1 è generalmente troppo alto. Una regola modificata dice di tenere tutte le PC i cui autovalori 
#della correlazione campionaria superano 0.7. Ovviamente, per dati non standardizzati, questa regola
#non ha senso applicarla.
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#===================================================================================

# CLUSTER ANALYSIS

#===================================================================================

DistEuclidean <- dist(fifaDf[, 1:20], method = 'euclidean', diag = TRUE)
DistManhattan <- dist(fifaDf[, 1:20], method = 'manhattan', diag = TRUE)

clust1 <- hclust(DistEuclidean,method = 'complete')
clust2 <- hclust(DistManhattan,method = 'complete')

clust3 <- hclust(DistEuclidean,method = 'single')
clust4 <- hclust(DistManhattan,method = 'single')

clust5 <- hclust(DistEuclidean,method = 'average')
clust6 <- hclust(DistManhattan,method = 'average')

clustK <- kmeans(fifaDf[, 1:20], 3, algorithm = 'MacQueen')

plot(clust1, main = 'Legame completo(Distanza Eucliedea)', xlab = 'Giocatori')
h.c1 <- clust1$height
h.c12 <- c(0, h.c1[-length(h.c1)])
cut1 <- which.max(round(h.c1 - h.c12, 3))
abline(h = clust1$height[cut1], col = 'red')

plot(clust2, main = 'Legame completo(Distanza Manhattan)', xlab = 'Giocatori')
h.c2 <- clust1$height
h.c22 <- c(0, h.c2[-length(h.c2)])
cut2 <- which.max(round(h.c1 - h.c22, 3))
abline(h = clust2$height[cut2], col = 'red')

plot(clust3, main = 'Legame singolo(Distanza Eucliedea)', xlab = 'Giocatori')
h.c3 <- clust3$height
h.c32 <- c(0, h.c3[-length(h.c3)])
cut3 <- which.max(round(h.c3 - h.c32, 3))
abline(h = clust3$height[cut3], col = 'red')

plot(clust4, main = 'Legame singolo(Distanza Manhattan)', xlab = 'Giocatori')
h.c4 <- clust1$height
h.c42 <- c(0, h.c4[-length(h.c4)])
cut4 <- which.max(round(h.c4 - h.c42, 3))
abline(h = clust4$height[cut4], col = 'red')

plot(clust5, main = 'Legame medio(Distanza Eucliedea)', xlab = 'Giocatori')
h.c5 <- clust1$height
h.c52 <- c(0, h.c5[-length(h.c5)])
cut5 <- which.max(round(h.c5 - h.c52, 3))
abline(h = clust5$height[cut5], col = 'red')

plot(clust6, main = 'Legame medio(Distanza Manhattan)', xlab = 'Giocatori')
h.c6 <- clust1$height
h.c62 <- c(0, h.c6[-length(h.c6)])
cut6 <- which.max(round(h.c6 - h.c62, 3))
abline(h = clust6$height[cut6], col = 'red')







