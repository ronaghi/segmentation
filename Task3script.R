#ASDM Assignment Task 3: Clustering
#Programmed by S. Mehran Ronaghi

#Setting working directory
mypath = 'C:/ASDM/Assignment/Task3'
setwd(mypath)
getwd()

#Importing G20 Countries Economy Dataset
G20eco <- read.csv('g20-economy.csv', header= TRUE)

dim(G20eco)
names(G20eco)
head(G20eco)
tail(G20eco)
summary(G20eco)
str(G20eco)

#Data Preparation

#Cleaning Variable Names
install.packages('janitor')
library(janitor)

G20eco <- G20eco %>% janitor::clean_names()

#Data Wrangling
install.packages('tidyverse')
library(tidyverse)

G20eco <- G20eco %>% rename('time' = 'i_time')
G20eco <- G20eco %>% rename('FDI_USD' = 'foreign_direct_investment_net_bo_p_current_us_bn_klt_dinv_cd')
G20eco <- G20eco %>% rename('GDP_USD' = 'gdp_current_us_ny_gdp_mktp_cd')
G20eco <- G20eco %>% rename('GDP_growth_pc' = 'gdp_growth_annual_ny_gdp_mktp_kd_zg')
G20eco <- G20eco %>% rename('Inflation_pc' = 'inflation_gdp_deflator_annual_ny_gdp_defl_kd_zg')
G20eco <- G20eco %>% rename('Net_Trade_USD' = 'net_trade_in_goods_and_services_bo_p_current_us_bn_gsr_gnfs_cd')

names(G20eco)
summary(G20eco)

#Missing Values
install.packages('skimr')
library(skimr)

skim(G20eco)

#Outliers
boxplot(G20eco$GDP_USD)
boxplot(G20eco$GDP_growth_pc)


#Skew
plot(density(G20eco$GDP_USD))
abline(v=mean(G20eco$GDP_USD), col='red')
abline(v=median(G20eco$GDP_USD), col='blue')

plot(density(G20eco$GDP_growth_pc))
abline(v=mean(G20eco$GDP_growth_pc), col='red')
abline(v=median(G20eco$GDP_growth_pc), col='blue')

#Preparing the dataset for SAS EG
#write.csv(G20eco, 'g20-economy-SAS-EG.csv', row.names = FALSE)

#Subsetting
G20eco <- subset(G20eco, time==2020, select= c(4:9))

#Preparing the dataset for SAS EM
#G20eco$ID <- seq.int(nrow(G20eco))
#write.csv(G20eco, 'g20-economy-SAS.csv', row.names = FALSE)

#Set country names as row names
rownames(G20eco) <- G20eco$country_code
G20eco$country_code <- NULL
head(G20eco)

#Installing and activating "cluster" package
install.packages('cluster')
library(cluster)

#creating scatterplot matrix to compare the variables
pairs(G20eco)

#Normalisation
install.packages('caret')
library(caret)

preproc<- preProcess(G20eco, method = 'range')
G20eco <- predict(preproc,G20eco)

summary(G20eco)

#creating distance matrix
distance <- dist(G20eco,method = 'euclidean',)
print(distance,digits=1)

#Visualizing the distance matrix
install.packages('factoextra')
library(factoextra)

fviz_dist(distance)

#Assessing clustering tendency
tendency <- get_clust_tendency(G20eco, 18, graph = TRUE)
tendency$hopkins_stat

#choosing the right number of expected clusters
fviz_nbclust(G20eco, kmeans, method = "wss")

#creating K-Means clustering
set.seed(123)
km <- kmeans(G20eco,4,nstart = 30) #k=4
km$cluster
km$size
km$centers


#Plotting the result
clusplot(G20eco, km$cluster, color=TRUE, shade=TRUE, lines=0)

#Visualizing the clusters
fviz_cluster(km,G20eco)
fviz_cluster(km,G20eco,ellipse.type = "norm")

#Modification
G20eco2 <- subset(G20eco, !(rownames(G20eco) %in% c('USA','CHN')))
set.seed(123)
km2 <- kmeans(G20eco2, 2, nstart = 30)
km2$cluster
km2$size
fviz_cluster(km2,G20eco2)

fviz_cluster(km2,G20eco2,ellipse.type = "norm")
