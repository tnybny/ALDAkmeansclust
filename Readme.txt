Author - bramach2/tnybny

Just one file for code - "hw2.R"
Edit the line nc = X (line 85) (replace X with the number of clusters desired)
Comment out and uncomment whichever of clust <- mykmeans(du, nc) (line 88) 
and clust <- bikmeans(du, nc) (line 89) depending on which function you want.
To run - from R console: source("hw2.R") after navigating to the correct working directory.
Change line 82 appropriately to specify different data file.

Assumptions - My bisecting k-means chooses next cluster to be clustered in random from the list of clusters available at any instant. K-means chooses centroids at random from the data points. Both the functions stop on given K (number of clusters required). 

In k-means, centroids are not being recomputed and data points are not being reassigned to centroids. However, in SSE calculation, mean of the cluster has been considered as the representative of the cluster to account for this problem of the centroid not being recomputed.

Accounted for: singleton clusters cannot be further bisected.
