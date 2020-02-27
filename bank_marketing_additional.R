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


Bank_train_dt<-read.table(file = "bank-additional-full.csv",sep=';',header = T);
str(Bank_train_dt)
dim(Bank_train_dt)
summary(Bank_train_dt)
#checking for any duplicated rows
duplicated(Bank_train_dt);
sum(duplicated(Bank_train_dt)) ;#so there are totally 12 rows which are duplicated in our training set.

# Now we will remove the duplicated rows from our data set
Bnk_clean_train =Bank_train_dt %>% distinct
dim(Bnk_clean_train)

#verifying the clean dataset
sum(duplicated(Bnk_clean_train)) ; #as we can see there are no duplicated rows or missing values in our dataset.
#By this we have finished of cleaning our dataset.
# summary of our cleaned data set
summary(Bnk_clean_train)

# After carefully observing the summary of bank_cleaned data set as there are some unknowns in default,housing,loan
Bnk_clean_train<-subset(Bnk_clean_train, housing!='unknown' & loan!='unknown')
#removing the rows where both coloumns having unknowns and above that both the coloumns are binary so we remove them.
dim(Bnk_clean_train)
#as default is highly skewed data we will impute the value with the mode.
Bnk_clean_train$default=ifelse(Bnk_clean_train$default=='yes',1,0)

prop.table(table(Bnk_clean_train$y))
head(Bnk_clean_train)
# We can observe that predicted outcome y is mostly skewed to no by 88.73%
# some exploratory analysis on our data set

ggplot(Bnk_clean_train,aes(x=age)) + geom_histogram(color="black", fill="white", binwidth = 5) +
  ggtitle('Age distribution with mean (blue line)') +
  ylab('Frequency') +
  xlab('Age') +
  geom_vline(aes(xintercept = mean(age)),colour="blue", linetype="dashed") +
  scale_x_continuous(breaks = seq(0,100,5));

ggplot(data = Bnk_clean_train, aes(x=education, fill=y)) +
  geom_bar() +
  ggtitle("Term Deposit Subscription based on Education Level") +
  xlab(" Education Level") +
  guides(fill=guide_legend(title="Subscription of Term Deposit"))

ggplot(Bnk_clean_train, aes(x=job,fill=y))+geom_bar()+theme_minimal()+scale_fill_brewer(palette="Blues")+
  ggtitle("Job vs subscription")+labs(fill="subscription")

ggplot(Bnk_clean_train, aes(x=loan,fill=housing))+geom_bar()+scale_fill_brewer(palette="Blues")+facet_grid(cols = vars(y)) +
  ggtitle("housing and personal loan vs subscription")+labs(fill="housing loan")

ggplot(data=Bnk_clean_train, aes(x=campaign, fill=y))+
  geom_histogram()+
  ggtitle("Subscription based on Number of Contact during the Campaign")+
  xlab("Number of Contact during the Campaign")+
  xlim(c(min=1,max=30)) +
  guides(fill=guide_legend(title="Subscription of Term Deposit"))

#transforming the data
Bnk_clean_train$housing=ifelse(Bnk_clean_train$housing=='yes',1,0)
Bnk_clean_train$loan=ifelse(Bnk_clean_train$loan=='yes',1,0)
Bnk_clean_train$job=as.numeric(as.factor(Bnk_clean_train$job))
Bnk_clean_train$marital=as.numeric(as.factor(Bnk_clean_train$marital))
Bnk_clean_train$month=as.numeric(as.factor(Bnk_clean_train$month))
Bnk_clean_train$day_of_week=as.numeric(as.factor(Bnk_clean_train$day_of_week))
Bnk_clean_train$contact=as.numeric(as.factor(Bnk_clean_train$contact))
Bnk_clean_train$education=as.numeric(as.factor(Bnk_clean_train$education))
Bnk_clean_train$poutcome=as.numeric(as.factor(Bnk_clean_train$poutcome))
summary(Bnk_clean_train)

sapply(Bnk_clean_train,class)

Bnk_clean_train<-transform(
  Bnk_clean_train,
  job=as.factor(job),
  marital=as.factor(marital),
  education=as.factor(education),
  default=as.factor(default),
  housing=as.factor(housing),
  loan=as.factor(loan),
  contact=as.factor(contact),
  month=as.factor(month),
  day_of_week=as.factor(day_of_week)
)

sapply(Bnk_clean_train,class)
summary(Bnk_clean_train)
#performing correlation analysis:
ggcorr(Bnk_clean_train, method = c("everything", "pearson"))+  ggtitle("Correlation Analysis")

#random forest technique
#training -1st time
modelform <- as.formula(Bnk_clean_train$y ~ Bnk_clean_train$age+Bnk_clean_train$job+Bnk_clean_train$marital+Bnk_clean_train$education+Bnk_clean_train$housing+Bnk_clean_train$loan+Bnk_clean_train$duration+Bnk_clean_train$campaign+Bnk_clean_train$pdays+Bnk_clean_train$month+Bnk_clean_train$day_of_week+Bnk_clean_train$previous+Bnk_clean_train$poutcome+Bnk_clean_train$cons.price.idx+Bnk_clean_train$cons.conf.idx+Bnk_clean_train$nr.employed+Bnk_clean_train$contact)
RF_fit <- randomForest(modelform,Bnk_clean_train)
varImpPlot(RF_fit,sort = TRUE)
varImpPlot(RF_fit,sort = TRUE,n.var = 15)
summary(RF_fit)
importance = importance(RF_fit)
varImportance = data.frame(Variables = row.names(importance),
                           Importance =round(importance[, "MeanDecreaseGini"],2))
rankImportance=varImportance%>%mutate(Rank=paste('#',dense_rank(desc(Importance))))
ggplot(rankImportance,aes(x=reorder(Variables,Importance),
                          y=Importance,fill=Importance))+ 
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'white') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_classic()

#training - 2nd time with feature selection
# By seeing the importance of features we can actually select the top 15 features because the other features are very less in our plot which won't
# having any effect

modelform1 <- as.formula(Bnk_clean_train$y ~ Bnk_clean_train$age+Bnk_clean_train$job+Bnk_clean_train$marital+Bnk_clean_train$education+Bnk_clean_train$housing+Bnk_clean_train$month+Bnk_clean_train$day_of_week+Bnk_clean_train$duration+Bnk_clean_train$campaign+Bnk_clean_train$pdays+Bnk_clean_train$previous+Bnk_clean_train$poutcome+Bnk_clean_train$cons.price.idx+Bnk_clean_train$cons.conf.idx+Bnk_clean_train$nr.employed)
Bnk_clean_train_md <- dplyr::select(Bnk_clean_train,age,job,marital,education,housing,month,day_of_week,duration,campaign,pdays,previous,poutcome,cons.price.idx,cons.conf.idx,nr.employed,y)
dim(Bnk_clean_train_md)
RF_fit1 <- randomForest(y~.,Bnk_clean_train_md)
a=c()
i=5
for (i in 1:8) {
  model3 <- randomForest(y ~ ., data = Bnk_clean_train_md, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model3, Bnk_clean_train_md, type = "class")
  a[i] = mean(predValid == Bnk_clean_train_md$y)
  i
  a[i]
  
}
classification_accuracy=a
mtry=1:8
plot(mtry,classification_accuracy)

data.balanced.over <- ovun.sample(y~., data=Bnk_clean_train_md, 
                                  p=0.5, seed=1, 
                                  method="over")$data
model5 <- randomForest(y ~ ., data = data.balanced.over, ntree = 500, mtry = 5, importance = TRUE)
summary(data.balanced.over)
#reading test data set
Bank_test_dt<-read.table(file = "bank-additional.csv",sep=';',header = T);
summary(Bank_test_dt)
dim(Bank_test_dt)
# After carefully observing the summary of bank_cleaned data set as there are some unknowns in default,housing,loan
Bnk_clean_test<-subset(Bank_test_dt, housing!='unknown' & loan!='unknown')
#removing the rows where both coloumns having unknowns and above that both the coloumns are binary so we remove them.
dim(Bnk_clean_test)
#as default is highly skewed data we will impute the value with the mode.
Bnk_clean_test$default=ifelse(Bnk_clean_test$default=='yes',1,0)
summary(Bnk_clean_test)
#Transforming the data
Bnk_clean_test$housing=ifelse(Bnk_clean_test$housing=='yes',1,0)
Bnk_clean_test$loan=ifelse(Bnk_clean_test$loan=='yes',1,0)
Bnk_clean_test$job=as.numeric(as.factor(Bnk_clean_test$job))
Bnk_clean_test$marital=as.numeric(as.factor(Bnk_clean_test$marital))
Bnk_clean_test$month=as.numeric(as.factor(Bnk_clean_test$month))
Bnk_clean_test$day_of_week=as.numeric(as.factor(Bnk_clean_test$day_of_week))
Bnk_clean_test$contact=as.numeric(as.factor(Bnk_clean_test$contact))
Bnk_clean_test$education=as.numeric(as.factor(Bnk_clean_test$education))
Bnk_clean_test$poutcome=as.numeric(as.factor(Bnk_clean_test$poutcome))
summary(Bnk_clean_test)

sapply(Bnk_clean_test,class)

Bnk_clean_test<-transform(
  Bnk_clean_test,
  job=as.factor(job),
  marital=as.factor(marital),
  education=as.factor(education),
  default=as.factor(default),
  housing=as.factor(housing),
  loan=as.factor(loan),
  contact=as.factor(contact),
  month=as.factor(month),
  day_of_week=as.factor(day_of_week)
)

sapply(Bnk_clean_test,class)
summary(Bnk_clean_test)
dim(Bnk_clean_test)
Bnk_clean_test_md <- dplyr::select(Bnk_clean_test,age,job,marital,education,housing,month,day_of_week,duration,campaign,pdays,previous,poutcome,cons.price.idx,cons.conf.idx,nr.employed,y)
dim(Bnk_clean_test_md)
summary(Bnk_clean_test_md)
#predicting our test data
pred1 = predict(model5, Bnk_clean_test_md,type="class")
# confusion matrix
table(Bnk_clean_test_md$y,pred1)


#--------------------------##clustering##----------------------------#
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

