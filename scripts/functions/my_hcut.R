my_hcut <- function (x, k = 2, isdiss = inherits(x, "dist"), hc_func = c("hclust", 
                                                              "agnes", "diana"), hc_method = "ward.D2", hc_metric = "euclidean", 
          stand = FALSE, graph = FALSE, ...) 
{
  # Ge the silhouette information
  # cluster: the cluster assignement of observation
  # diss the dissimilarity matrix
  .get_silinfo <- function(cluster, diss){
    
    k <- length(unique(cluster))
    silinfo <- list()
    if(k > 1) {
      sil.obj <- cluster::silhouette(cluster, diss)
      widths <- as.data.frame(sil.obj[, 1:3])
      rownames(widths) <- colnames(as.matrix(diss))
      widths <- widths[order(widths$cluster, -widths$sil_width), ]
      widths$cluster <- as.factor(widths$cluster)
      
      # summary
      silinfo$widths <- widths
      silinfo$clus.avg.widths <- as.vector(tapply(widths$sil_width, widths$cluster, mean))
      silinfo$avg.width <- mean(widths$sil_width)
    }
    
    silinfo
  }
  
  if (!inherits(x, c("matrix", "data.frame", "dist"))) 
    stop("The data must be of class matrix, data.frame, or dist")
  if (!is.numeric(k) || length(k) != 1L || is.na(k) || k%%1 != 
      0) 
    stop("k must be a single integer >= 1")
  if (isdiss && !inherits(x, "dist")) 
    stop("When isdiss = TRUE, x must be an object of class dist")
  if (stand && !inherits(x, "dist")) {
    x <- scale(x)
    if (anyNA(x)) 
      stop("Scaling produced NA values. Check for constant columns or non-finite values.")
  }
  data <- x
  hc_func <- match.arg(hc_func)
  hc_func <- hc_func[1]
  if (!isdiss) 
    x <- get_dist(x, method = hc_metric)
  if (hc_func == "hclust") 
    hc <- stats::hclust(x, method = hc_method)
  else if (hc_func == "agnes") {
    if (hc_method %in% c("ward.D", "ward.D2")) 
      hc_method = "ward"
    hc <- cluster::agnes(x, method = hc_method)
  }
  else if (hc_func == "diana") 
    hc <- cluster::diana(x)
  else stop("Don't support the function ", hc_func)
  hc.cut <- stats::cutree(hc, k = k)
  hc$cluster = hc.cut
  hc$nbclust <- k
  if (k > 1) 
    hc$silinfo <- .get_silinfo(hc.cut, x)
  hc$size <- as.vector(table(hc.cut))
  hc$data <- data
  class(hc) <- c(class(hc), "hcut")
  if (graph) 
    fviz_dend(hc)
  hc
}
# debugonce(my_hcut)