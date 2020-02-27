library(tidyverse)  # data manipulation
library(ggplot2)# to create plots for exploratory analysis
library(gridExtra) #adding extra grid or outlayer plots to ggplots
library(GGally) #  helps in building correlation magtrix
library(ggcorrplot)
library(randomForest)
library(cluster) # for gower similarity and pam
install.packages("Rtsne")
library(Rtsne) # for t-SNE plot
install.packages("ROSE")
library(ROSE)

devtools::install_github("ricardo-bion/ggradar", 
                         dependencies = TRUE)
Bank_clustest_dt<-read.table(file = "bank-additional.csv",sep=';',header = T);

Bank_tr=Bank_clustest_dt %>% distinct
dim(Bank_tr)
Bnk_clust_train<-subset(Bank_tr, housing!='unknown' & loan!='unknown')
Bnk_clust_train$default=ifelse(Bnk_clust_train$default=='yes',1,0)
summary(Bnk_clust_train)
Bnk_clust_train_md <- dplyr::select(Bnk_clust_train,age,job,marital,education,housing,loan,month,duration,campaign,emp.var.rate,y)
#to handle mixed data types we are using a distance metric named Gower distance.
gow_dis <- daisy(Bnk_clust_train_md, metric = "gower")
summary(gow_dis)
gow_mat <- as.matrix(gow_dis)
usethis::edit_r_environ()
Sys.getenv('R_MAX_VSIZE')

Bnk_clust_train_md[
  which(gow_mat == min(gow_mat[gow_mat != min(gow_mat)]),
        arr.ind = TRUE)[1, ], ]

Bnk_clust_train_md[
  which(gow_mat == max(gow_mat[gow_mat != max(gow_mat)]),
        arr.ind = TRUE)[1, ], ]     
sil_width <- c(NA)

for(i in 2:9){
  
  pam_fit <- pam(gow_dis,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot sihouette width (higher is better)

plot(1:9, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:9, sil_width)

k <- 7
pam_fit <- pam(gow_dis, diss = TRUE, k)
# mediods of observations
Bnk_clust_train_md[pam_fit$medoids, ]

# summary of adata
pam_results <- Bnk_clust_train_md %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
#Helps us to get some descriptive statistics of the clusters 
pam_results$the_summary

# clustered data set
clustered_data<-Bnk_clust_train_md %>%
  mutate(cluster = pam_fit$clustering)