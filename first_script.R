#Test Anita
install.packages("readxl")
install.packages("dplyr")
install.packages("psych")

library(readxl)
library(dplyr)
library(cluster)
library(psych)


# Datensatz laden
WVS_data <- read_excel("F00010939-WVS_Wave_7_Germany_Excel_v2.0.xlsx")

# Datacleaning und Vorbereitung der Clustervariablen
# Umbenennung der Clustervariablen
WVS_data <- rename(WVS_data, 
                   `Q158` = `Q158: Science and technology are making our lives healthier, easier, and more comfortable`, 
                   `Q159` = `Q159: Because of science and technology, there will be more opportunities for the next generation`,
                   `Q160` = `Q160: We depend too much on science and not enough on faith`,
                   `Q161` = `Q161: One of the bad effects of science is that it breaks down people's ideas of right and wrong`,
                   `Q162` = `Q162: It is not important for me to know about science in my daily life`,
                   `Q163` = `Q163: The world is better off, or worse off, because of science and technology`)
WVS_data <- rename(WVS_data, `ID` = `D_INTERVIEW: Interview ID`)

# Fehlende Werte (in Clustervariablen nur -1 und -2) in NA umwandeln
WVS_data <- WVS_data %>%
  mutate(Q158 = na_if(Q158, -1))
WVS_data <- WVS_data %>%
  mutate(Q158 = na_if(Q158, -2))
WVS_data <- WVS_data %>%
  mutate(Q159 = na_if(Q159, -1))
WVS_data <- WVS_data %>%
  mutate(Q159 = na_if(Q159, -2))
WVS_data <- WVS_data %>%
  mutate(Q160 = na_if(Q160, -1))
WVS_data <- WVS_data %>%
  mutate(Q160 = na_if(Q160, -2))
WVS_data <- WVS_data %>%
  mutate(Q161 = na_if(Q161, -1))
WVS_data <- WVS_data %>%
  mutate(Q161 = na_if(Q161, -2))
WVS_data <- WVS_data %>%
  mutate(Q162 = na_if(Q162, -1))
WVS_data <- WVS_data %>%
  mutate(Q162 = na_if(Q162, -2))
WVS_data <- WVS_data %>%
  mutate(Q163 = na_if(Q163, -1))
WVS_data <- WVS_data %>%
  mutate(Q163 = na_if(Q163, -2))

table(WVS_data$Q158)
table(WVS_data$Q159)
table(WVS_data$Q160)
table(WVS_data$Q161)
table(WVS_data$Q162)
table(WVS_data$Q163)

# NA pro Person z?hlen
# Subset mit nur den Clustervariablen bilden
Cluster_vars <- WVS_data[c("ID", "Q158", "Q159", "Q160", "Q161", "Q162", "Q163")] 
Cluster_vars$na_count <- apply(is.na(Cluster_vars), 1, sum)
table(Cluster_vars$na_count)


# NA pro Variable z?hlen
table(is.na(Cluster_vars$Q158)) #14
table(is.na(Cluster_vars$Q159)) #16
table(is.na(Cluster_vars$Q160)) #48
table(is.na(Cluster_vars$Q161)) #95
table(is.na(Cluster_vars$Q162)) #23
table(is.na(Cluster_vars$Q163)) #18
# --> auf Variablenebene liegt der Anteil der Missings zwischen 1% und maximal 6% --> nicht problematisch

# Clusteranalyse 1: Gower-Koeffizient ohne Entfernen der Missing Values

g <- daisy(Cluster_vars[,2:7], metric='gower')
g

# Clusteranalyse 1.1 Single Linkage
CA1 <- hclust(g, method = "single", members = NULL)
CA1$merge
CA1$height
CA1$order
par(las=1)
plot(CA1, main='', xlab='', ylab='', sub='')
# --> sehr uneindeutiges Ergebnis, deutliche Kettenbildung

# Clusteranalyse 1.2 Complete Linkage
CA2 <- hclust(g, method = "complete", members = NULL)
CA2$merge
CA2$height
CA2$order
par(las=1)
plot(CA2, main='', xlab='', ylab='', sub='')
# --> evtl 2 Cluster L?sung, aber auch eher uneindeutig, 2. Cluster w?re sehr klein

# Clusteranalyse 1.3 Average Linkage
CA3 <- hclust(g, method = "average", members = NULL)
CA3$merge
CA3$height
CA3$order
par(las=1)
plot(CA3, main='', xlab='', ylab='', sub='')
# --> sehr ?hnlich wie bei Complete Linkage

# Clusteranalyse 1.4 Ward Methode
CA4 <- hclust(g, method = "ward.D", members = NULL)
CA4$merge
CA4$height
CA4$order
par(las=1)
plot(CA4, main='', xlab='', ylab='', sub='')
# --> auf den ersten Blick sehr deutliche 2-Cluster-L?sung erkennbar,
#     ?hnlich gro?e Cluster

# Clusteranalyse 2: Entfernen aller Missing Values aus den Clustervariablen

# ?bertragen der NA-Count Variable aus dem Subset in den Hauptdatensatz
WVS_data <- cbind(WVS_data, Cluster_vars$na_count)
WVS_data <- rename(WVS_data, `na_count_cluster` = `Cluster_vars$na_count`)

# Entfernen der F?lle mit Missing Values in den Clustervariablen
# aus dem Hauptdatensatz
WVS_data <- WVS_data[WVS_data$na_count_cluster == 0, ]
# und aus dem Subset
Cluster_vars <- Cluster_vars[Cluster_vars$na_count == 0, ]

# Erneutes Durchf?hren der Clusteranalysen ohne Missing Values
# Distanzmatrix

# Clusteranalyse 2.1
g2 <- daisy(Cluster_vars[,2:7], metric='euclidean')

# Clusteranalyse 2.1 Single Linkage ohne Missings
CA5 <- hclust(g2, method = "single", members = NULL)
CA5$merge
CA5$height
CA5$order
par(las=1)
plot(CA5, main='', xlab='', ylab='', sub='')
# --> sehr uneindeutiges Ergebnis, deutliche Kettenbildung

# Clusteranalyse 2.2 Complete Linkage ohne Missings
CA6 <- hclust(g2, method = "complete", members = NULL)
CA6$merge
CA6$height
CA6$order
par(las=1)
plot(CA6, main='', xlab='', ylab='', sub='')
# --> uneindeutig, sehr viele kleine Cluster

# Clusteranalyse 2.3 Average Linkage ohne Missings
CA7 <- hclust(g2, method = "average", members = NULL)
CA7$merge
CA7$height
CA7$order
par(las=1)
plot(CA7, main='', xlab='', ylab='', sub='')
# --> sehr ähnlich wie bei Complete Linkage, uneindeutig

# Clusteranalyse 2.4 Ward Methode ohne Missings
CA8 <- hclust(g2, method = "ward.D", members = NULL)
CA8$merge
CA8$height
CA8$order
par(las=1)
plot(CA8, main='', xlab='', ylab='', sub='')
# --> sehr deutliche Lösung, Größe der Cluster hat sich durch Entfernen der Missings angenähert

# Zwei-Cluster-Lösung festlegen
Cluster_vars$cluster1 <- cutree(CA8, k=2)
table(Cluster_vars$cluster1)
rect.hclust(CA8, k=2, border="red")

# Eigenschaften der Cluster beschreiben
describeBy(Cluster_vars[,2:7], group=Cluster_vars$cluster1)
# Cluster 1 ist ablehnender gegenüber Wissenschaft und Technologie eingestellt,
# manche Variablen unterscheiden sich aber kaum,
# Unterschiede vor allem bei Q160 bis Q162
# Ergebnisse insgesamt nicht extrem eindeutig
