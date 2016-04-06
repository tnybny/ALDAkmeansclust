##################
# Author: tnybny #
##################

# clear workspace
rm(list = ls(all = T))

cent <- NULL

mykmeans <- function(dat, nc = 2)
{
    # this function performs k-means clustering
    cent <<- dat[sample(1:nrow(dat), nc), -3] # randomly pick centroids from data
    repeat{
        for(i in 1:nrow(dat))
        {
            dmin <- 1000
            for(j in 1:nc)
            {
                point1 <- dat[i, -3]
                point2 <- cent[j, ]
                dist <- dist(rbind(point1, point2))
                if(dist < dmin)
                {
                    dmin <- dist
                    dat[i, 3] <- j
                }
            }
        }
        dat <- dat[order(dat$cluster), ]
        centPrime <<- t(as.data.frame(
            lapply(split(dat[, -3], dat$cluster), colMeans)))
        if(all(cent == centPrime))
        {
            # prevent singleton clusters
            # if any cluster has only 1 point, repick centroids
            if(any(unlist(lapply(split(dat[, -3], dat$cluster), nrow)) == 1))
            {
                cent <<- dat[sample(1:nrow(dat), nc), -3]
                next
            }
            break
        }
        else
            cent <<- centPrime
    }
    return(dat)
}

# load and preprocess the data
dat <- read.table(file = "data.csv", header = T, sep = ",")
nc <- 4				# specify desired number of centroids
cluster <- matrix(1, nrow(dat), 1)	# new column "cluster" with all values = 1,
# this is what we want to start off with
dat <- data.frame(dat, cluster)		# append new column

# perform k-means clustering
dat <- mykmeans(dat, nc)
cluster <- dat$cluster

# plot the result of the clustering
plot(dat[, -3], main = "Cluster Distributions", sub = "My clustering")
for(i in 1:nc)
{
    points(subset(dat[, -3], cluster == i), col = i, pch = 16)
}
points(cent, pch = "+", cex = 3)

# SSE calculation. ssei is SSE of clustering and ssej holds the sse of a specific cluster
ssei <- 0
for(i in 1:nc)
{
     ssej <- 0
     xmean = colMeans(subset(dat, cluster == i)[, -3])
     for(j in 1:nrow(subset(dat, cluster == i)))
     {
         ssej = ssej + dist(rbind(xmean, subset(dat, cluster == i)[j, ])) ^ 2
     }
     ssei = ssei + ssej
}
print(ssei)