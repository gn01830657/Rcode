setwd("C:/Users/L/Desktop/Data-EG")
options(scipen=200)
info23 <- read.csv("info_23.csv",fileEncoding='big5')
info1 <- read.csv("info_1.csv",fileEncoding='big5')
spend <- read.csv("spend.csv",fileEncoding='big5')

infoAll <- merge(info1,info23,all = TRUE)

spendT <- spend[,-1]
spendT <- aggregate(spendT[,-1],by = list(spendT[,1]),FUN = sum,na.rm = TRUE)
colnames(spendT) <- colnames(spend[,-1])
library("plyr")
counting <- count(as.character(spend$card_id))
colnames(counting)[1] <- "card_id"
spendT <- merge(spendT,counting,by = "card_id")
spendT$total <- rowSums(spendT[,2:6])

infoM <- merge(infoAll,spendT,by = "card_id")

#hist(infoM$age)
#ageRange = c("A","B","C","D")
#infoM$age = ageRange[as.numeric(cut(infoM$age,breaks=c(0,40,48,56,100)))]
ageRange = c("1","2","3","4")
infoM$age = ageRange[as.numeric(cut(infoM$age,breaks=c(0,40,48,56,100)))]

#hist(infoM$id)
#N = c("101","3","17","18","20","4")
#for(i in 2:length(N)){
#infoM$id[which(infoM$id==N[i])] = N[1]
#}
#C = c("102","13","14","1","9","21","10")
#for(i in 2:length(C)){
#infoM$id[which(infoM$id==C[i])] = C[1]
#}
#S = c("103","2","15","16","7","12","11")
#for(i in 2:length(S)){
#infoM$id[which(infoM$id==S[i])] = S[1]
#}
#E = c("104","5","6","19")
#for(i in 2:length(E)){
#infoM$id[which(infoM$id==E[i])] = E[1]
#}

Rural = c("R","1","2","3","4","5","6","9","10","11","12","19","21")
for(i in 2:length(Rural)){
  infoM$id[which(infoM$id==Rural[i])] = Rural[1]
}
Metro = c("M","7","13","14","15","16","17","18","20")
for(i in 2:length(Metro)){
  infoM$id[which(infoM$id==Metro[i])] = Metro[1]
}

#write.csv(infoM,"infoM.csv")

dim(infoM)
head(infoM)
cluster_data <- infoM

cluster <- hclust(dist(cluster_data[,10:15]))
plot(cluster)
clusterk <- cutree(cluster,k=11)
for (k in 1:11) {
  cat(paste0("cluster-",as.character(k),": ",
             as.character(length(which(clusterk==k))),"\n"))
}
colMeans(cluster_data[which(clusterk==1),10:15])
colMeans(cluster_data[which(clusterk==2),10:15])
colMeans(cluster_data[which(clusterk==7),10:15])
####################
i=1
table(infoM[which(clusterk==i),3])
table(infoM[which(clusterk==i),4])
table(infoM[which(clusterk==i),5])
table(infoM[which(clusterk==i),8])
colMeans(cluster_data[which(clusterk==i),10:15])
#################
#lv2
cluster_data_lv2 = cluster_data[which(clusterk==1),]
cluster_lv2 <- hclust(dist(cluster_data_lv2[,10:15]))
plot(cluster_lv2)
clusterk_lv2 <- cutree(cluster_lv2,k=6)
for (k in 1:6) {
  cat(paste0("cluster-",as.character(k),": ",
             as.character(length(which(clusterk_lv2==k))),"\n"))
}
colMeans(cluster_data_lv2[which(clusterk_lv2==1),10:15])
colMeans(cluster_data_lv2[which(clusterk_lv2==2),10:15])
colMeans(cluster_data_lv2[which(clusterk_lv2==3),10:15])
colMeans(cluster_data_lv2[which(clusterk_lv2==5),10:15])
#################
#lv3
cluster_data_lv3 = cluster_data_lv2[which(clusterk_lv2==1),]
cluster_lv3 <- hclust(dist(cluster_data_lv3[,10:15]))
plot(cluster_lv3)
clusterk_lv3 <- cutree(cluster_lv3,k=6)
for (k in 1:6) {
  cat(paste0("cluster-",as.character(k),": ",
             as.character(length(which(clusterk_lv3==k))),"\n"))
}
colMeans(cluster_data_lv3[which(clusterk_lv3==1),10:15])
colMeans(cluster_data_lv3[which(clusterk_lv3==2),10:15])
colMeans(cluster_data_lv3[which(clusterk_lv3==3),10:15])
colMeans(cluster_data_lv3[which(clusterk_lv3==4),10:15])
colMeans(cluster_data_lv3[which(clusterk_lv3==5),10:15])
colMeans(cluster_data_lv3[which(clusterk_lv3==6),10:15])
#################
final <- c(as.character(length(cluster_data[which(clusterk==1),1])),summary(cluster_data[which(clusterk==1),3]),colMeans(cluster_data[which(clusterk==1),10:17]))
for(i in 2:11){
  final <- rbind(final,c(as.character(length(cluster_data[which(clusterk==i),1])),summary(cluster_data[which(clusterk==i),3]),colMeans(cluster_data[which(clusterk==i),10:17])))
}
for(i in 1:6){
  final <- rbind(final,c(as.character(length(cluster_data_lv2[which(clusterk_lv2==i),1])),summary(cluster_data_lv2[which(clusterk_lv2==i),3]),colMeans(cluster_data_lv2[which(clusterk_lv2==i),10:17])))
}
for(i in 1:6){
  final <- rbind(final,c(as.character(length(cluster_data_lv3[which(clusterk_lv3==i),1])),summary(cluster_data_lv3[which(clusterk_lv3==i),3]),colMeans(cluster_data_lv3[which(clusterk_lv3==i),10:17])))
}
colnames(final)[1] <- "人數"
rownames(final) <-NULL
final <- data.frame(final)

write.csv(final,"final.csv")

####################
spendD <- spend
library("lubridate")
#spendD$week <- weekdays(as.Date(as.character(spendD$spend_date),format="%Y-%m-%d"))
spendD$week <- wday(as.Date(as.character(spendD$spend_date),format="%Y-%m-%d"))-1
spendD[is.na(spendD)] <- 0

cardid <- cluster_data_lv2[which(clusterk_lv2==1),1]
spendlv2c1 <- spendD[which(spendD$card_id %in% cardid),]
spendlv2c1$card_id <- as.character(spendlv2c1$card_id)
counting <- count(as.character(spendlv2c1$card_id))


#View(counting[rev(order(counting$freq)),])
#View(infoAll[which(infoAll$card_id==counting[rev(order(counting$freq))[1],1]),])
#View(spendlv2c1[which(spendlv2c1$card_id==counting[rev(order(counting$freq))[1],1]),])
#View(infoAll[which(infoAll$card_id==counting[rev(order(counting$freq))[2],1]),])
#View(spendlv2c1[which(spendlv2c1$card_id==counting[rev(order(counting$freq))[2],1]),])
#View(infoAll[which(infoAll$card_id==counting[rev(order(counting$freq))[3],1]),])
#View(spendlv2c1[which(spendlv2c1$card_id==counting[rev(order(counting$freq))[3],1]),])

######################
New_cluster_data = cluster_data_lv2[which(clusterk_lv2==1),]
New_cluster_data$gender <- as.character(New_cluster_data$gender)
gender <- c("男性","女性","1","0")
for(i in 1:2){
  New_cluster_data$gender[which(New_cluster_data$gender==gender[i])] <- gender[i+2]
}

New_cluster_data_subset <- New_cluster_data[,3:16]
New_cluster_data_subset <- New_cluster_data_subset[,-4]
New_cluster_data_subset <- New_cluster_data_subset[,-4]
New_cluster_data_subset <- New_cluster_data_subset[,-4]
New_cluster_data_subset <- New_cluster_data_subset[,-4]
New_cluster <- hclust(dist(New_cluster_data_subset))
plot(New_cluster)
New_clusterk <- cutree(New_cluster,k=4)
for (k in 1:4) {
  cat(paste0("cluster-",as.character(k),": ",
             as.character(length(which(New_clusterk==k))),"\n"))
}

colMeans(New_cluster_data_subset[which(New_clusterk==1),4:10])
colMeans(New_cluster_data_subset[which(New_clusterk==2),4:10])
colMeans(New_cluster_data_subset[which(New_clusterk==3),4:10])
colMeans(New_cluster_data_subset[which(New_clusterk==4),4:10])

count(New_cluster_data$county[which(New_clusterk==1)])
count(New_cluster_data$county[which(New_clusterk==2)])
count(New_cluster_data$county[which(New_clusterk==3)])
count(New_cluster_data$county[which(New_clusterk==4)])


c1 <- colMeans(cluster_data[which(clusterk==1),10:15])
c2 <- colMeans(cluster_data[which(clusterk==2),10:15])
c3 <- colMeans(cluster_data[which(clusterk==3),10:15])
c4 <- colMeans(cluster_data[which(clusterk==4),10:15])
c5 <- colMeans(cluster_data[which(clusterk==5),10:15])
c6 <- colMeans(cluster_data[which(clusterk==6),10:15])
c7 <- colMeans(cluster_data[which(clusterk==7),10:15])
c8 <- colMeans(cluster_data[which(clusterk==8),10:15])
c9 <- colMeans(cluster_data[which(clusterk==9),10:15])
c10 <- colMeans(cluster_data[which(clusterk==10),10:15])
c11 <- colMeans(cluster_data[which(clusterk==11),10:15])
r <- rbind(c1,c2)
r <- rbind(r,c3)
r <- rbind(r,c4)
r <- rbind(r,c5)
r <- rbind(r,c6)
r <- rbind(r,c7)
r <- rbind(r,c8)
r <- rbind(r,c9)
r <- rbind(r,c10)
r <- rbind(r,c11)
a <-dist(r)


###########0608####
##1-1-1
cluster_data_lv3_1 = cluster_data_lv3[which(clusterk_lv3==1),]
cluster_lv3_1 <- hclust(dist(cluster_data_lv3_1[,10:15]))
plot(cluster_lv3_1)
clusterk_lv3_1 <- cutree(cluster_lv3_1,k=6)
for (k in 1:6) {
  cat(paste0("cluster-",as.character(k),": ",
             as.character(length(which(clusterk_lv3_1==k))),"\n"))
}
colMeans(cluster_data_lv3_1[which(clusterk_lv3_1==1),10:15])
colMeans(cluster_data_lv3_1[which(clusterk_lv3_1==2),10:15])
colMeans(cluster_data_lv3_1[which(clusterk_lv3_1==3),10:15])
colMeans(cluster_data_lv3_1[which(clusterk_lv3_1==4),10:15])
colMeans(cluster_data_lv3_1[which(clusterk_lv3_1==5),10:15])
colMeans(cluster_data_lv3_1[which(clusterk_lv3_1==6),10:15])

##1-1-2
cluster_data_lv3_2 = cluster_data_lv3[which(clusterk_lv3==2),]
cluster_lv3_2 <- hclust(dist(cluster_data_lv3_2[,10:15]))
plot(cluster_lv3_2)
clusterk_lv3_2 <- cutree(cluster_lv3_2,k=6)
for (k in 1:6) {
  cat(paste0("cluster-",as.character(k),": ",
             as.character(length(which(clusterk_lv3_2==k))),"\n"))
}
colMeans(cluster_data_lv3_2[which(clusterk_lv3_2==1),10:15])
colMeans(cluster_data_lv3_2[which(clusterk_lv3_2==2),10:15])
colMeans(cluster_data_lv3_2[which(clusterk_lv3_2==3),10:15])
colMeans(cluster_data_lv3_2[which(clusterk_lv3_2==4),10:15])
colMeans(cluster_data_lv3_2[which(clusterk_lv3_2==5),10:15])
colMeans(cluster_data_lv3_2[which(clusterk_lv3_2==6),10:15])
##1-1-4
cluster_data_lv3_4 = cluster_data_lv3[which(clusterk_lv3==4),]
cluster_lv3_4 <- hclust(dist(cluster_data_lv3_4[,10:15]))
plot(cluster_lv3_4)
clusterk_lv3_4 <- cutree(cluster_lv3_4,k=6)
for (k in 1:6) {
  cat(paste0("cluster-",as.character(k),": ",
             as.character(length(which(clusterk_lv3_4==k))),"\n"))
}
colMeans(cluster_data_lv3_4[which(clusterk_lv3_4==1),10:15])
colMeans(cluster_data_lv3_4[which(clusterk_lv3_4==2),10:15])
colMeans(cluster_data_lv3_4[which(clusterk_lv3_4==3),10:15])
colMeans(cluster_data_lv3_4[which(clusterk_lv3_4==4),10:15])
colMeans(cluster_data_lv3_4[which(clusterk_lv3_4==5),10:15])
colMeans(cluster_data_lv3_4[which(clusterk_lv3_4==6),10:15])

##1-1-2-1
cluster_data_lv3_2_1 = cluster_data_lv3_2[which(clusterk_lv3_2==1),]
cluster_lv3_2_1 <- hclust(dist(cluster_data_lv3_2_1[,10:15]))
plot(cluster_lv3_2_1)
clusterk_lv3_2_1 <- cutree(cluster_lv3_2_1,k=6)
for (k in 1:6) {
  cat(paste0("cluster-",as.character(k),": ",
             as.character(length(which(clusterk_lv3_2_1==k))),"\n"))
}
colMeans(cluster_data_lv3_2_1[which(clusterk_lv3_2_1==1),10:15])
colMeans(cluster_data_lv3_2_1[which(clusterk_lv3_2_1==2),10:15])
colMeans(cluster_data_lv3_2_1[which(clusterk_lv3_2_1==3),10:15])
colMeans(cluster_data_lv3_2_1[which(clusterk_lv3_2_1==4),10:15])
colMeans(cluster_data_lv3_2_1[which(clusterk_lv3_2_1==5),10:15])
colMeans(cluster_data_lv3_2_1[which(clusterk_lv3_2_1==6),10:15])