library(ggplot2)
library(fpc)
library(NbClust)
library(pheatmap)
library(cluster)

set.seed(1234)

micro_6plus.mod <- read.csv("micro_6plus.csv")
#micro_6plus.mod <- cbind(micro_6plus[1:4], micro_6plus[-1:-4]*100)
mid_6plus.mod <- read.csv("mid_6plus_1000min.csv")
#mid_6plus.mod <- cbind(mid_6plus[1:4], mid_6plus[-1:-4]*100)
high_6plus.mod <- read.csv("high_6plus_1000min.csv")
#high_6plus.mod <- cbind(high_6plus[1:4], high_6plus[-1:-4]*100)

all_6plus <- rbind(micro_6plus.mod, mid_6plus.mod, high_6plus.mod)


all_6plus$winrate <- ifelse(all_6plus$Net.Won > 0, 1,0)
high_6plus.mod$winrate <- ifelse(high_6plus.mod$Net.Won > 0, 1,0)
mid_6plus.mod$winrate <- ifelse(mid_6plus.mod$Net.Won > 0, 1,0)
micro_6plus.mod$winrate <- ifelse(micro_6plus.mod$Net.Won > 0, 1,0)

summary(all_6plus$VP.IP)

mean(all_6plus$VP.IP)
mean(high_6plus.mod$VP.IP)
mean(mid_6plus.mod$VP.IP)
mean(micro_6plus.mod$VP.IP)

mean(all_6plus$PFR)
mean(high_6plus.mod$PFR)
mean(mid_6plus.mod$PFR)
mean(micro_6plus.mod$PFR)

#look for number of clusters, vpip, pfr
wss <- (nrow(high_6plus.mod[c(5,6)])-1)*sum(apply(high_6plus.mod[c(5,6)],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(high_6plus.mod[c(5,6)], centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

wss <- (nrow(mid_6plus.mod[c(5,6)])-1)*sum(apply(mid_6plus.mod[c(5,6)],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mid_6plus.mod[c(5,6)], centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

wss <- (nrow(micro_6plus.mod[c(5,6)])-1)*sum(apply(micro_6plus.mod[c(5,6)],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(micro_6plus.mod[c(5,6)], centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

wss <- (nrow(all_6plus[c((5,6)])-1)*sum(apply(all_6plus[c((5,6)],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(all_6plus[c((5,6)], centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

#look for number of clusters expanded variables
wss <- (nrow(high_6plus.mod[c(-1:-4)])-1)*sum(apply(high_6plus.mod[c(-1:-4)],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(high_6plus.mod[c(-1:-4)], centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

wss <- (nrow(mid_6plus.mod[c(-1:-4)])-1)*sum(apply(mid_6plus.mod[c(-1:-4)],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mid_6plus.mod[c(-1:-4)], centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

wss <- (nrow(micro_6plus.mod[c(-1:-4)])-1)*sum(apply(micro_6plus.mod[c(-1:-4)],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(micro_6plus.mod[c(-1:-4)], centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

wss <- (nrow(all_6plus[c(-1:-4)])-1)*sum(apply(all_6plus[c(-1:-4)],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(all_6plus[c(-1:-4)], centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

nc <- NbClust(all_6plus[c(5,6)], min.nc = 2, max.nc = 15, method = "kmeans")
nc <- NbClust(all_6plus[c(-1:-4)], min.nc = 2, max.nc = 15, method = "kmeans")

o <- order(all_6plus[,30])
all6plus.order <- all_6plus[o,]

clusGap(all_6plus[c(-1:-4)], kmeans, K.max = 12, B = 100, verbose = interactive())

gap <- rep(0,10)
gap_stat <- clusGap(all_6plus[c(-1:-4)], kmeans, K.max=10, B=100, verbose = interactive())
ELogW <- gap_stat$Tab[,2]
logW <- gap_stat$Tab[,1]
gap <- ELogW - logW
error <- gap_stat$Tab[,4]
plot(1:10, gap, type = "b", pch = 19, frame = FALSE, xlab = "Number of clusters k",ylab="Gap stat")
arrows(1:10, gap-error/sqrt(nrow(all_6plus[c(-1:-4)])), 1:10, gap+error/sqrt(nrow(all_6plus[c(-1:-4)])),length=0.05, angle=90, code=3)

gap <- rep(0,10)
gap_stat <- clusGap(high_6plus.mod[c(-1:-4)], kmeans, K.max=10, B=100, verbose = interactive())
ELogW <- gap_stat$Tab[,2]
logW <- gap_stat$Tab[,1]
gap <- ELogW - logW
error <- gap_stat$Tab[,4]
plot(1:10, gap, type = "b", pch = 19, frame = FALSE, xlab = "Number of clusters k",ylab="Gap stat")
arrows(1:10, gap-error/sqrt(nrow(high_6plus.mod[c(-1:-4)])), 1:10, gap+error/sqrt(nrow(high_6plus.mod[c(-1:-4)])),length=0.05, angle=90, code=3)

gap <- rep(0,10)
gap_stat <- clusGap(mid_6plus.mod[c(-1:-4)], kmeans, K.max=10, B=100, verbose = interactive())
ELogW <- gap_stat$Tab[,2]
logW <- gap_stat$Tab[,1]
gap <- ELogW - logW
error <- gap_stat$Tab[,4]
plot(1:10, gap, type = "b", pch = 19, frame = FALSE, xlab = "Number of clusters k",ylab="Gap stat")
arrows(1:10, gap-error/sqrt(nrow(mid_6plus.mod[c(-1:-4)])), 1:10, gap+error/sqrt(nrow(mid_6plus.mod[c(-1:-4)])),length=0.05, angle=90, code=3)

gap <- rep(0,10)
gap_stat <- clusGap(micro_6plus.mod[c(-1:-4)], kmeans, K.max=10, B=100, verbose = interactive())
ELogW <- gap_stat$Tab[,2]
logW <- gap_stat$Tab[,1]
gap <- ELogW - logW
error <- gap_stat$Tab[,4]
plot(1:10, gap, type = "b", pch = 19, frame = FALSE, xlab = "Number of clusters k",ylab="Gap stat")
arrows(1:10, gap-error/sqrt(nrow(micro_6plus.mod[c(-1:-4)])), 1:10, gap+error/sqrt(nrow(micro_6plus.mod[c(-1:-4)])),length=0.05, angle=90, code=3)

                            
heatmap(test)
pheatmap(test, kmeans_k = 3)
pheatmap(test, kmeans_k = 4)
test <- as.matrix(all6plus.order[c(-1:-4)])

all_6plus.clus <- kmeans(all_6plus[c(-1:-4)],4, nstart=50)
all_6plus$cluster <- as.factor(all_6plus.clus$cluster)

high_6plus.clus <- kmeans(high_6plus.mod[c(-1:-4)],4, nstart=50)
high_6plus.mod$cluster <- as.factor(high_6plus.clus$cluster)

mid_6plus.clus <- kmeans(mid_6plus.mod[c(-1:-4)],4, nstart=50)
mid_6plus.mod$cluster <- as.factor(mid_6plus.clus$cluster)

micro_6plus.clus <- kmeans(micro_6plus.mod[c(-1:-4)],4, nstart=50)
micro_6plus.mod$cluster <- as.factor(micro_6plus.clus$cluster)

ggplot(all_6plus, aes(all_6plus$PFR, all_6plus$VP.IP, color = all_6plus$cluster)) + geom_point()
ggplot(high_6plus.mod, aes(high_6plus.mod$PFR, high_6plus.mod$VP.IP, color = high_6plus.mod$cluster)) + geom_point()
ggplot(mid_6plus.mod, aes(mid_6plus.mod$PFR, mid_6plus.mod$VP.IP, color = mid_6plus.mod$cluster)) + geom_point()
ggplot(micro_6plus.mod, aes(micro_6plus.mod$PFR, micro_6plus.mod$VP.IP, color = micro_6plus.mod$cluster)) + geom_point()

barplot(table(high_6plus.mod$winrate, high_6plus.mod$cluster), col = c("red", "blue"), beside = TRUE)
barplot(table(mid_6plus.mod$winrate, mid_6plus.mod$cluster), col = c("red", "blue"), beside = TRUE)
barplot(table(micro_6plus.mod$winrate, micro_6plus.mod$cluster), col = c("red", "blue"), beside = TRUE)
