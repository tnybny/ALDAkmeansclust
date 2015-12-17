##############################################################
# CSC 522 : Automated Learning and Data Analysis
# Bharathkumar Ramachandra
# bramach2/tnybny
##############################################################

library(mclust)
library(lattice)
library(MASS)
library(car)
library(utils)          # for read/write csv files
library(flexclust)      # for kcca

#the bisecting k-means function which calls mykmeans
bikmeans <- function(c1, maxclusters = 2)
{
	numclusters <- 1
	while (numclusters < maxclusters)
	{
		repeat  #to guard against the case where we pick the singleton cluster from the list of clusters (so we won't be able to bisect it)
		{
			k = sample(1:numclusters, 1)     #pick a cluster at random from the list  
			s = subset(c1, c1$cluster == k)  #of clusters available at this instant
			if(nrow(s) != 1)        #to guard against the case where we pick the singleton cluster from the list of clusters (so we won't be able to bisect it)
			{
			break
			}
		}		
		c2 <- mykmeans (s, 2)  #call mykmeans on the subset of data s that we picked, asking for k=2 and store it in c2
		la<-subset(c2, c2$cluster == 1)
		lb<-subset(c2, c2$cluster == 2)
		la$cluster = k			           #assign the proper cluster numbers to the clusters that are returned from kmeans
		lb$cluster = numclusters + 1
		for(i in 1:nrow(c1))                   #copy the cluster results from kmeans on the subset back onto the master clustering
		{
			for(j in 1:nrow(la))
			{
				if(c1[i,1]==la[j,1]&&c1[i,2]==la[j,2])
				{
					c1$cluster[i]=la$cluster[j]
				}
			}
		}
		for(i in 1:nrow(c1))				#copy the cluster results from kmeans on the subset back onto the master clustering
		{
			for(j in 1:nrow(lb))
			{
				if(c1[i,1]==lb[j,1]&&c1[i,2]==lb[j,2])
				{
					c1$cluster[i]=lb$cluster[j]
				}
			}
		}
		numclusters = numclusters + 1 #increment the count that keeps track of the number of clusters we currently have
	}
	return (c1)
}

#my implementation of the k-means algorithm
mykmeans <- function(c3, p = 2)
{
	mi = c3[sample(1:nrow(c3), p, replace = FALSE),]
	for(i in 1:nrow(c3))
	{
		dmin <- 1000
		for(j in 1:p)
		{
			p1 <- data.frame(matrix(c(c3[i,1],c3[i,2]),1,2)) 	#so that we avoid considering the cluster column in distance calculation
			p2 <- data.frame(matrix(c(mi[j,1],mi[j,2]),1,2))
			d1 <- dist(rbind(p1,p2))
			if(d1 < dmin)
			{
				dmin <- d1
				c3[i,3] <- j
			}
		}
	}
	return (c3)	
}

#so called main
du = read.table(file = "d-c4hw2.csv", header=T, sep=",")
#du = d[,-c(1,4)]
du=data.frame(du)		#as data frame
nc = 2					#specify desired number of centroids
cluster <- matrix(1, nrow(du), 1)	#new column "cluster" with all values = 1, this is what we want to start off with
du <- data.frame(du, cluster)		#append new column
#clust <- mykmeans(du, nc)			#uncomment to run kmeans
clust <- bikmeans(du, nc)			#uncomment to run bisecting kmeans
#time to plot the data
da=data.frame(clust[,1],clust[,2])
colnames(da)[1] <- "X1"
colnames(da)[2] <- "X2"
x1 = min(du[,1])
x2 = max(du[,1])
y1 = min(du[,2])
y2 = max(du[,2])
plot(da, col=c(1), main = "Cluster Distributions", xlim=c(x1,x2), ylim=c(y1,y2), pch=c(19), xlab=(names(du))[1], ylab=(names(du))[2], sub="My clustering")
for(i in 1:nc)
{
	points(subset(da,clust$cluster==i), col=c(i+10))
}
#SSE calculation. ssei is SSE of clustering and ssej hold the sse of a specific cluster
ssei <- 0
for(i in 1:nc)
{
	ssej <- 0
	xmean = data.frame(matrix(c(mean(subset(da,clust$cluster==i)[,1]),mean(subset(da,clust$cluster==i)[,2])),1,2))
	for(j in 1:nrow(subset(da,clust$cluster==i)))
	{
		ssej = ssej + (dist(rbind(xmean[1,],subset(da,clust$cluster==i)[j,]))^2)
	}
	ssei = ssei + ssej
}
#mapply(write.table, x = l, file = c("df1.txt", "df2.txt"))
