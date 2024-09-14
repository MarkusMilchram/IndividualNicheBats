#' hampel filter
#'
#' @description detect outliers in bearings using hampel filter in a moving time window
#'
#'
#' @author Jannis Gottwald
#'
#'
#' @param col string, string indicating the column containing bearings
#' @param k numeric, window size n seconds
#' @param t0 numeric,threshold for median filter. A high threshold makes the filter more forgiving, a low one will declare more points to be outliers. Default=0.5
#'
#' @import foreach
#'
#' @export
#'
#' @examples

hampel_time <- function(data, col, k, t0 = 0.5) {

  data <- as.data.frame(data)
  
  colnames_actual <- colnames(data)
  
  colnames_expected <- c("timestamp", col)
  
  if (!all(colnames_expected  %in%   colnames_actual )) {
    idx <- colnames_expected %in%   colnames_actual 
    
    stop(paste0("Required column ", colnames_expected [!idx], " not found! "))
  }
  
  
  
  
  data <- data[!is.na(data[, col]), ]
  
  if (nrow(data) >= k) {
   
    
    data$bearings_filtered <- data[, col]
    
    n <- nrow(data)
    indc <- c() # indices of outliers
    
    bear <- c()
    
    L <- 1.4826 # constants for normal distributions
    # t0 <- 3        # Pearson's 3 sigma edit rule
    
    # we don't look at outliers at the end parts of x !
    
    
    for (i in 1:nrow(data)) {
      x0 <- median(data[, col][data$timestamp >= data$timestamp[i] - k & data$timestamp <= data$timestamp[i] + k])
      S0 <- L * median(abs(data[, col][data$timestamp >= data$timestamp[i] - k & data$timestamp <= data$timestamp[i] + k] - x0))
      
      
      if (abs(data[, col][i] - x0) > t0 * S0) {
        data$bearings_filtered[i] <- x0
      }
    }
    # return a list with 2 components
    
    
    
    
  } else{
    data[1,]<-NA
    data<-cbind(data,bearings_filtered=NA)
  }
  return(data)
}
