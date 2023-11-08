require(stats)
require(tidyr)
require(tidyverse)
require(dplyr)
require(magrittr)
require(NbClust)
require(knitr)
require(RColorBrewer)
require(plotly)
require(highcharter)
require(cluster)


olive_modificato <- read_csv("olive_modificato.csv")

#View(olive_modificato)

table(olive_modificato$Region)
table(olive_modificato$Area)

# Calcola il numero di colonne nel dataset
num_colonne <- ncol(olive_modificato)

cor(olive_modificato$Area=="North Apulia", olive_modificato$Area=="South Apulia")

summary(olive_modificato$Oleic)
summary(olive_modificato$Palmitic)

# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----

#ANALISI DESCRITTIVA

MedieArea <- olive_modificato %>% 
  group_by(Area) %>% 
  gather(key = "fatty_acid", value = "percentage", -Region, -Area) %>% 
  group_by(Region, Area, fatty_acid) %>% 
  dplyr::summarise(Mean = mean(percentage, na.rm = TRUE), .groups = "keep") %>% 
  spread(key = fatty_acid, value = Mean)

MedieArea %>% kable(digits = 2, align = "c")


#Grafico a Torta Sardegna

datiSardinia <- subset(olive_modificato, Region == "Sardinia")

mediaSardinia <- colMeans(datiSardinia[, c('Palmitic', 'Palmitoleic', 'Stearic', 'Oleic', 'Linoleic', 'Linolenic', 'Arachidic', 'Eicosenoic')])

SDF <- data.frame("Acid" = names(mediaSardinia), "MeanVal" = mediaSardinia) #Sardinia Data Frame

GSardinia <- plot_ly(SDF, labels = ~Acid, values = ~`MeanVal`, type = 'pie') #Grafico Sardinia
GSardinia <- GSardinia %>% layout(title = 'Media dei Valori degli Acidi per la Sardegna',
                                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
GSardinia 


#Grafico a Torta Nord Italia

datiNordItalia <- subset(olive_modificato, Region == "Northern Italy")

mediaNordItalia <- colMeans(datiNordItalia[, c('Palmitic', 'Palmitoleic', 'Stearic', 'Oleic', 'Linoleic', 'Linolenic', 'Arachidic', 'Eicosenoic')])

NDF <- data.frame("Acid" = names(mediaNordItalia), "MeanVal" = mediaNordItalia) #Nord Data Frame

NordItalia <- plot_ly(NDF, labels = ~Acid, values = ~`MeanVal`, type = 'pie')
NordItalia <- NordItalia %>% layout(title = 'Media dei Valori degli Acidi per il Nord Italia',
                                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

NordItalia



#Grafico a Torta Sud Italia

datiSudItalia <- subset(olive_modificato, Region == "Southern Italy")

MediaSud <- colMeans(datiSudItalia[, c('Palmitic', 'Palmitoleic', 'Stearic', 'Oleic', 'Linoleic', 'Linolenic', 'Arachidic', 'Eicosenoic')])

SudDF <- data.frame("Acid" = names(MediaSud), "MeanVal" = MediaSud)

SudItalia <- plot_ly(SudDF, labels = ~Acid, values = ~`MeanVal`, type = 'pie')
SudItalia <- SudItalia %>% layout(title = 'Media dei Valori degli Acidi per il Sud Italia',
                                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

SudItalia



SL <- olive_modificato %>% 
  hchart('scatter', hcaes(x = Stearic, y = Linoleic, group = Area)) %>%
  hc_colors(c("#00bfff", "#ed9121", "#d70a53", "#00cc99", "#d9ad7c", "#36D636", "#588c7e", "#D526BB", "#34AACB")) %>%
  hc_xAxis(title = list(text="Acido Stearico")) %>%
  hc_yAxis(title = list(text="Acido Linoleico"))%>%
  hc_title(text = "Acido Stearico e Linoleico Presenti negli Oli per Area") %>%
  hc_add_theme(hc_theme_smpl())

SL



# ---- Grafico 3 Acidi (con Centroidi) ----

ColorScale <- c("Southern Italy" = "#e74c3c", "Northern Italy" = "#3498db", "Sardinia" = "#2ecc71")

#Calcolo delle medie per ciascuna variabile e regione utilizzando dplyr
CentroidsPoints <- olive_modificato %>%
  group_by(Region) %>%
  summarise(MP = mean(Palmitic), MO = mean(Oleic), ML = mean(Linoleic)) %>%
  mutate(Region = factor(Region, levels = c("Northern Italy", "Sardinia", "Southern Italy")))

#MP = Media Palmitico
#MO = Media Oleico
#ML = Media Linoleico

#Definizione di colori diversi per i centroidi
CentroidsPoints$ColoreMedie <- c("darkblue", "darkgreen", "darkred")

#Creazione del grafico 3D per le osservazioni
G3A <- plot_ly(olive_modificato, x = ~Palmitic, y = ~Oleic, z = ~Linoleic, color = ~Region, colors = ColorScale) %>% add_markers(marker = list(size = 5))

#Aggiunta dei centroidi
G3A <- G3A %>% add_trace(
  data = CentroidsPoints, x = ~MP, y = ~MO, z = ~ML,
  type = "scatter3d", mode = "markers", text = ~Region,
  marker = list(size = 10, color = ~ColoreMedie, symbol = 4))


G3A <- G3A %>%
  layout(scene = list(
    xaxis = list(title = 'Palmitico'),
    yaxis = list(title = 'Oleico'),
    zaxis = list(title = 'Linoleico')),
    
    legend = list(title = 'Region'),
    title = list(
      text = "Presenza Media di Tre Acidi per Olio Raggruppato per Regione",
      font = list(family = "Arial", size = 20), y=0.98),
    annotations = list(
      list(y = 0.99,
           text = "AX: Palmitico | AY: Oleico | AZ: Linoleico",
           showarrow = FALSE)))
G3A


#Grafico Salute Oli
GD <- plot_ly(olive_modificato, x = ~Stearic, y = ~Oleic, z = ~Linoleic, text = ~Area, marker = list(color = ~Stearic, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))

GD <- GD %>% add_markers()
GD <- GD %>% layout(scene = list(xaxis = list(title = 'Stearic'), yaxis = list(title = 'Oleic'), zaxis = list(title = 'Linoleic')), annotations = list(
  x = 1.02,
  y = 1.0,
  text = 'Stearico',
  xref = 'paper',
  yref = 'paper',
  font = list(family="Arial", size=20),
  showarrow = FALSE),
  title = list(
    text = "Presenza di Tre Acidi per Olio",
    font = list(family = "Arial", size = 20), y=0.98))

GD



cor(olive_modificato[, c("Arachidic","Stearic","Palmitic")])

olive_modificato$BA <- olive_modificato$Palmitic + olive_modificato$Stearic + olive_modificato$Arachidic #Bad Acids
olive_modificato$GA <- olive_modificato$Linoleic + olive_modificato$Eicosenoic + olive_modificato$Linolenic + olive_modificato$Palmitoleic #Good Acids
olive_modificato$AS <- olive_modificato$BA/olive_modificato$GA #Acids Score

summary(olive_modificato$AS)


#Boxplot Acids Scores
GAS <- plot_ly(olive_modificato, x = ~Area, y = ~AS, text = ~Area, marker = list(color = ~AS, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE), type = "box", color = ~Area, colors = c("#00bfff", "#ed9121", "#d70a53", "#00cc99", "#d9ad7c", "#36D636", "#588c7e", "#D526BB", "#34AACB"))


GAS <- GAS %>% layout(scene = list(xaxis = list(title = 'Arachidic'), yaxis = list(title = 'Stearic'), zaxis = list(title = 'Palmitic')), annotations = list(
  x = 1.02,
  y = 1.0,
  text = 'Area',
  xref = 'paper',
  yref = 'paper',
  font = list(family="Arial", size=20),
  showarrow = FALSE),
  title = list(
    text = "Acids Scores Boxplot",
    font = list(family = "Arial", size = 20), y=0.98))

GAS




remove(olive_modificato)
olive_modificato <- read_csv("olive_modificato.csv")





# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----

EuDist=dist(olive_modificato[, -c(1,2)], method="euclidean")
MaDist=dist(olive_modificato[, -c(1,2)], method="manhattan")
MiDist=dist(olive_modificato[, -c(1,2)], method="minkowski")


# ---- METODI DI AGGLOMERAMENTO DEI CLUSTER ----

# -- SINGLE (Nearest Neighbour) --

hcEuDistS=hclust(EuDist,method="single",members=NULL) #Euclidean
hcMaDistS=hclust(MaDist,method="single",members=NULL) #Manhattan
hcMiDistS=hclust(MiDist,method="single",members=NULL) #Minkowski

# -- COMPLETE (Farthest Neighbour) --

hcEuDistC=hclust(EuDist,method="complete",members=NULL) #Euclidean
hcMaDistC=hclust(MaDist,method="complete",members=NULL) #Manhattan
hcMiDistC=hclust(MiDist,method="complete",members=NULL) #Minkowski

# -- AVERAGE --

hcEuDistA=hclust(EuDist,method="average",members=NULL) #Euclidean
hcMaDistA=hclust(MaDist,method="average",members=NULL) #Manhattan
hcMiDistA=hclust(MiDist,method="average",members=NULL) #Minkowski

# -- WARD MINIMUM VARIANCE --

hcEuDistW=hclust(EuDist,method="ward.D",members=NULL) #Euclidean
hcMaDistW=hclust(MaDist,method="ward.D",members=NULL) #Manhattan
hcMiDistW=hclust(MiDist,method="ward.D",members=NULL) #Minkowski

# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----



# ---- 3 Clusters Cut - Single Method (Nearest Neighbour) - Manhattan Distance ----

plot(hcEuDistS)
plot(hcMaDistS)
plot(hcMiDistS)

HClusterMaS=cutree(hcMaDistS, k=3) #Inizialmente Supposti 7 (Somiglianze tra Aree)

plot(olive_modificato[,-c(1,2)], col=HClusterMaS, main="Single (Nearest Neighbour) - Manhattan Distance")




# ---- 3 Clusters Cut - Complete Method (Farthest Neighbour) - Euclidean Distance ----


plot(hcEuDistC)
plot(hcMaDistC)
plot(hcMiDistC)

HClusterEuC=cutree(hcEuDistC, k=3) #Inizialmente Supposti 6+1 (Somiglianze tra Aree - 1 Outliar)

plot(olive_modificato[,-c(1,2)], col=HClusterEuC, main="Complete (Farthest Neighbour) - Euclidean Distance")




# ---- 3 Clusters Cut - Average Method - Euclidean Distance ----

plot(hcEuDistA)
plot(hcMaDistA)
plot(hcMiDistA)

HClusterEuA=cutree(hcEuDistA, k=3) #Inizialmente Supposti 6+1 (Somiglianze tra Aree - 1 Outliar)

plot(olive_modificato[,-c(1,2)], col=HClusterEuA, main="Average - Euclidean Distance")



# ---- 6 Clusters Cut - Ward Algorithm - Euclidean Distance ----

plot(hcEuDistW)
plot(hcMaDistW)
plot(hcMiDistW)

HClusterEuW=cutree(hcEuDistW, k=3) #Inizialmente Supposti 6+1 (Somiglianze tra Aree)

plot(olive_modificato[,-c(1,2)], col=HClusterEuW, main="Ward - Euclidean Distance")


# ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----


# ---- KMEANS ----

oilsClusters <- kmeans(olive_modificato[,-c(1,2)], nstart = 10, centers = 5)

plot(olive_modificato[,-c(1,2)], col=oilsClusters$cluster)
print(oilsClusters)


class(olive_modificato[, 2])
class(oilsClusters$cluster)
#table(olive_modificato[, 2], olive_modificato$cluster)

# ---- SILHOUETTE ----

listaCenters <- c(3,4,5,6,7)

KM1 <- kmeans(olive_modificato[, -c(1,2)], centers = listaCenters[1], nstart = 20)
S1 <- silhouette(KM1$cluster, dist(olive_modificato[, -c(1,2)]))
AVGS1 <- mean(S1[, "sil_width"])


KM2 <- kmeans(olive_modificato[, -c(1,2)], centers = listaCenters[2], nstart = 20)
S2 <- silhouette(KM2$cluster, dist(olive_modificato[, -c(1,2)]))
AVGS2 <- mean(S2[, "sil_width"])


KM3 <- kmeans(olive_modificato[, -c(1,2)], centers = listaCenters[3], nstart = 20)
S3 <- silhouette(KM3$cluster, dist(olive_modificato[, -c(1,2)]))
AVGS3 <- mean(S3[, "sil_width"])


KM4 <- kmeans(olive_modificato[, -c(1,2)], centers = listaCenters[4], nstart = 20)
S4 <- silhouette(KM4$cluster, dist(olive_modificato[, -c(1,2)]))
AVGS4 <- mean(S4[, "sil_width"])


KM5 <- kmeans(olive_modificato[, -c(1,2)], centers = listaCenters[5], nstart = 20)
S5 <- silhouette(KM5$cluster, dist(olive_modificato[, -c(1,2)]))
AVGS5 <- mean(S5[, "sil_width"])


RDF <- data.frame(
  nCluster = c(listaCenters[1],listaCenters[2],listaCenters[3],listaCenters[4],listaCenters[5]),
  silhouetteMeans = c(AVGS1, AVGS2, AVGS3, AVGS4, AVGS5)) #Results Data Frame

RDF



# ---- NBCLUST KMEANS ----

set.seed(1111)

NbClust(data = olive_modificato[, -c(1,2)], distance = "euclidean", method = "kmeans")
NbClust(data = olive_modificato[, -c(1,2)], distance = "manhattan", method = "kmeans")
NbClust(data = olive_modificato[, -c(1,2)], distance = "minkowski", method = "kmeans")



# ---- PAM ----

matriceDistEu <- dist(olive_modificato[,-c(1,2)], method = "euclidean")

OilsPAM <- pam(diss = TRUE, x = matriceDistEu, k = 5, metric = "euclidean")

plot(olive_modificato[,-c(1,2)], col = OilsPAM$clustering)




















