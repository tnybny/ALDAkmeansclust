##################################################
# CSC 522 : Automated Learning and Data Analysis #
# Bharathkumar Ramachandra			 #
# bramach2/tnybny				 #
##################################################

mykmeans <- function(dat, nc = 2)
{
    # this function performs k-means clustering
	cent = dat[sample(1:nrow(dat), nc), ] # randomly pick centroids from data
	for(i in 1:nrow(dat))
	{
		dmin <- 1000
		for(j in 1:nc)
		{
			point1 <- dat[i, -3]
			point2 <- cent[j, -3]
			dist <- dist(rbind(point1, point2))
			if(dist < dmin)
			{
				dmin <- dist
				dat[i, 3] <- j
			}
		}
	}
	return(dat[, -3])
}

dat <- read.table(file = "data.csv", header = T, sep = ",")
nc <- 4				# specify desired number of centroids
cluster <- matrix(1, nrow(dat), 1)	# new column "cluster" with all values = 1,
                                    # this is what we want to start off with
dat <- data.frame(dat, cluster)		# append new column
clust <- mykmeans(du, nc)

# plot the result of the clustering
plot(dat[, -3], main = "Cluster Distributions", sub = "My clustering")
for(i in 1:nc)
{
	points(subset(dat[, -3], clust == i), col = i, pch = 16)
}

# SSE calculation. ssei is SSE of clustering and ssej holds the sse of a specific cluster
ssei <- 0
for(i in 1:nc)
{
    ssej <- 0
    xmean = c(mean(subset(dat, clust == i)[, 1]),
              mean(subset(dat, clust == i)[, 2]))
    for(j in 1:nrow(subset(dat, clust == i)))
    {
        ssej = ssej + dist(rbind(xmean, subset(dat, clust == i)[j, ]))^2
    }
    ssei = ssei + ssej
}
