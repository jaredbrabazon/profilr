#' Identifies significant changes in a changing data set with time 
#' 
#' \code{lineprof} identifies the most significant statistical change in mean, variance, or both mean and variance in the data (often pixel data) as specified by user on changing datasets
#' 
#' @param data a dataframe by read.csv of a .csv created from using the ImageJ macro found in vignette. Or simply a dataframe with each column representing a new instance (with potential changes) of a data set of same N
#' @param L_per_pix a unit conversion of length per pixel, or desired unit per data unit default = 1 or no conversion
#' @param type type of changepoint desired. Choose between "meanvar" (mean and variance), "mean", "variance", and "all" (changepoints for meanvar, mean, and variance); note the default is "meanvar" see Killick et al., 2013 for more details
#' @return results in a dataframe with time interval, filename, changepoint in units of original data, a conversion labeled as unit.length
#' @examples 
#' changepoint_df <- lineprof(data = df, L_per_pix = 0.123, type = "mean")
#' changepoint_df <- lineprof(data = df, L_per_pix = 500, type = "variance")
#' @export

#create lineprof() function 

lineprof <- function(data, L_per_pix = 1, type = "meanvar"){
  
  if(type == "meanvar"){
  df <- select(data, -ends_with("x"))
  mat <- as.matrix(df)
  trans <- t(mat)
  filename <- row.names(trans)
  z <- data.frame(cpt.meanvar(trans, class = FALSE))
  z <- select(z, -ends_with("value"))
  name_cpt <- cbind(filename, z)
  px_height = ncol(trans)
  
  final_meanvar <- name_cpt %>%
    mutate(unit.length = cpt*L_per_pix)%>%
    rowid_to_column("time.interval")
  colnames(final_meanvar)[colnames(final_meanvar) == "cpt"] <- "pixel.changepoint"
  final_meanvar
  
  } else if(type == "mean"){
    df <- select(data, -ends_with("x"))
    mat <- as.matrix(df)
    trans <- t(mat)
    filename <- row.names(trans)
    z <- data.frame(cpt.mean(trans, class = FALSE))
    z <- select(z, -ends_with("value"))
    name_cpt <- cbind(filename, z)
    px_height = ncol(trans)
    
    final_mean <- name_cpt %>%
      mutate(unit.length = cpt*L_per_pix) %>%
      rowid_to_column("time.interval")
    colnames(final_mean)[colnames(final_mean) == "cpt"] <- "pixel.changepoint"
    final_mean
  
    } else if(type == "variance") {
    df <- select(data, -ends_with("x"))
    mat <- as.matrix(df)
    trans <- t(mat)
    filename <- row.names(trans)
    z <- data.frame(cpt.var(trans, class = FALSE, method = "AMOC", test.stat = "CSS", penalty = "Asymptotic", pen.value = 0.05))
    name_cpt <- cbind(filename, z)
    px_height = ncol(trans)
    
    coln <- colnames(name_cpt)[2]
    colnames(name_cpt)[colnames(name_cpt) == paste(coln)] <- "pixel.changepoint"
    
    final_variance <- name_cpt %>%
      mutate(unit.length = pixel.changepoint*L_per_pix) %>%
      rowid_to_column("time.interval")
    
    final_variance
  
    } else if(type == "all"){
      df <- select(data, -ends_with("x"))
      mat <- as.matrix(df)
      trans <- t(mat)
      filename <- row.names(trans)
      xmeanvar <- data.frame(cpt.meanvar(trans, class = FALSE))
      ymean <- data.frame(cpt.mean(trans, class = FALSE))
      zvar <- data.frame(cpt.var(trans, class = FALSE, method = "AMOC", test.stat = "CSS", penalty = "Asymptotic", pen.value = 0.05))
      xmeanvar <- select(xmeanvar, -ends_with("value"))
      ymean <- select(ymean, -ends_with("value"))
      zvar <- select(zvar, -ends_with("value"))
      df1 <- cbind(filename, xmeanvar)
      df2 <- cbind(filename, ymean)
      df3 <- cbind(filename, zvar)
      px_height = ncol(trans)
      
      dfmeanvar <- df1 %>%
        mutate(mv.unit.length = cpt*L_per_pix) %>%
        rowid_to_column("time.interval")
      colnames(dfmeanvar)[colnames(dfmeanvar) == "cpt"] <- "meanvar.cpt"
      
      dfmean <- df2 %>%
        mutate(m.unit.length = cpt*L_per_pix)%>%
        select(-filename)
      colnames(dfmean)[colnames(dfmean) == "cpt"] <- "mean.cpt"
      
      coln <- colnames(df3)[2]
      colnames(df3)[colnames(df3) == paste(coln)] <- "var.cpt"
      
      dfvar <- df3 %>%
        mutate(v.unit.length = var.cpt*L_per_pix) %>%
        select(-filename)
      
      
    final <- cbind(dfmeanvar, dfmean, dfvar)
    final
  }
  
}


