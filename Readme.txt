Author - bramach2/tnybny

Assumptions - K-means chooses centroids at random from the data points. Stop on given K (number of clusters required). 

In k-means, centroids are not being recomputed and data points are not being reassigned to centroids. However, in SSE calculation, center of the cluster has been considered as the representative of the cluster to account for this problem of the centroid not being recomputed.
