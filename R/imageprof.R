#' Identifies significant changes for each column of a matrix image over time
#' 
#' \code{imageprof} identifies the most significant statistical change in mean, variance, or both mean and variance in the data (often pixel data) as specified by user on changing datasets
#' 
#' @param basepath a file directory to a folder containing matrix images (e.g. TIFF or text images) 
#' @param savepath a file directory to a desired save folder
#' @param samplename desired name of the sample, enter in quotes (e.g. "sample-x"), if left as NULL then the folder name will be used
#' @param L_per_pix a unit conversion of length per pixel, or desired unit per data unit; default = 1 which implies no conversion
#' @param stats indicates if a dataframe of statistics should be saved in place of changepoint dataframe. This stats dataframe includes the mean, median, variance, and sdev of all the changepoints taken within each column in the matrix, default =FALSE
#' @param type type of changepoint desired. Choose between "meanvar" (mean and variance), "mean", and "variance"(changepoints for meanvar, mean, and variance); note the default is "meanvar" see Killick et al., 2013 for more details
#' @param filetail to add a desired save file tail after the samplename, default includes "samplename"_"meanvar(or other type)".csv for stats = FALSE and "samplename"_"meanvar(or other type)"_"stats".csv; change filetail by inputing a character string = "yourfiletail"
#' @return two possible return options. 1)results in a dataframe where changepoints are the rows and the columns are the matrix image filename; changepoints are potentially converted to unit of choice as indicated by user. 2) stats of each image in stack (e.g. mean, median, variance, stdev, of all changepoints in each image)
#' @examples 
#' changepoint_df <- imageprof(basepath = "./../file/directory/foldercontainingmatriximages/", savepath = "./../file/directory/desiredsavefolder/", samplename = NULL, L_per_pix = 1, stats = FALSE, type = "meanvar", filetail = NULL)
#' 
#' stats_df <- imageprof(basepath = "./../file/directory/foldercontainingmatriximages/", savepath = "./../file/directory/desiredsavefolder/", samplename = "samplenumber123", L_per_pix = 1, stats = TRUE, type = "meanvar", filetail = NULL)
#' 
#' variation_change_df <- imageprof(basepath = "./../../University/Research/rockimages/", savepath = "./../../University/Research/save/", samplename = "samplenumber", L_per_pix = 0.123, stats = FALSE, type = "variance", filetail = "conversion0.123")
#' 
#' change_mean_df <- imageprof(basepath = "./../../University/Research/rockimages/", savepath = "./../../University/Research/save/", samplename = "rock-CO-1", L_per_pix = 0.5, stats = FALSE, type = "mean", filetail = "2018-5-14-changepoints")
#' 
#' stats_df <- imageprof(basepath = "./../../University/Research/rockimages/", savepath = "./../../University/Research/save/", samplename = "rock-CO-1", L_per_pix = 0.5, stats = TRUE, type = "meanvar", filetail = "2018-5-14-stats")
#' @export



imageprof <- function(basepath = NULL, savepath = NULL, samplename = NULL, L_per_pix = 1, stats = FALSE, type = "meanvar", filetail = NULL){
 
  change_meanvar <- function(data){
    
    mat <- as.matrix(data)
    trans <- t(t(t(mat)))
    z <- data.frame(cpt.meanvar(trans, class = FALSE))
  }
  
  change_mean <- function(data){
    
    mat <- as.matrix(data)
    trans <- t(t(t(mat)))
    z <- data.frame(cpt.mean(trans, class = FALSE))
  }
  
  change_var <- function(data){
    
    mat <- as.matrix(data)
    trans <- t(t(t(mat)))
    z <- data.frame(cpt.var(trans, class = FALSE, method = "AMOC", test.stat = "CSS", penalty = "Asymptotic", pen.value = 0.05))
  }
  
  if((is.null(basepath) == FALSE) && (is.null(savepath) == FALSE)){
    base.path <- basepath
    
    
    #get character strings for each csv within the directory
    path.delim <- dir(base.path, full.names = TRUE)
    get_dimens <- read.delim(path.delim[1])
    px_width = ncol(get_dimens)
    px_height = nrow(get_dimens)
    
    df <- 1:px_width
    
      if(type == "meanvar"){
        
        for (i in path.delim){
          
          k <- change_meanvar(read.delim(i))
          df <- cbind(df, k)
        }
        all_data <- data.frame(df)
        
        column_names <- dir(base.path, full.names = FALSE)
        
        final <- all_data %>%
          select_(~contains("cpt")) %>%
          mutate_all(funs(px_height - .)) %>%
          mutate_all(funs(.*L_per_pix)) %>%
          `colnames<-`(c(column_names)) %>%
          rownames_to_column(var = "matrix.column.number")
        
        save.path <- savepath
        
        if(stats == FALSE){
          ##connect save path with samplename
          save <- file.path(save.path, if(is.null(samplename) == TRUE){paste(basename(basepath))}else{paste(samplename)})
          filename.path <- file.path(save, if(is.null(filetail) == TRUE){paste(type)}else{paste(filetail)}, fsep = "_")
          csv.ending <- file.path(filename.path, "csv", fsep = ".")
          
          ##write a csv to the save path folder
          write.csv(x = final, file = csv.ending, row.names = FALSE)
          final
        } else if(stats == TRUE){
          mean_df <- final %>%
            select(-matrix.column.number) %>%
            summarise_all(funs(mean)) %>%
            t()%>%
            data.frame()
          colnames(mean_df)[colnames(mean_df) == "."] <- "mean"
          
          median_df <- final %>%
            select(-matrix.column.number) %>%
            summarise_all(funs(median)) %>%
            t()%>%
            data.frame()
          colnames(median_df)[colnames(median_df) == "."] <- "median"
          
          var_df <- final %>%
            select(-matrix.column.number) %>%
            summarise_all(funs(var)) %>%
            t()%>%
            data.frame()
          colnames(var_df)[colnames(var_df) == "."] <- "variance"
          
          sd_df <- final %>%
            select(-matrix.column.number) %>%
            summarise_all(funs(sd)) %>%
            t()%>%
            data.frame()
          colnames(sd_df)[colnames(sd_df) == "."] <- "standard.deviation"
          
          z <- nrow(mean_df)
          image.order <- seq(1:z)
          stats <- cbind(image.order, mean_df, median_df, var_df, sd_df)
          
          ##connect save path to the filename folder
          save <- file.path(save.path, if(is.null(samplename) == TRUE){paste(basename(basepath))}else{paste(samplename)})
          filename.path <- file.path(save, if(is.null(filetail) == TRUE){paste(type, "stats", sep = "_")}else{paste(filetail)}, fsep = "_")
          csv.ending <- file.path(filename.path, "csv", fsep = ".")
          
          ##write a csv to the save path folder
          write.csv(x = stats, file = csv.ending, row.names = FALSE)
          stats
        }

      } else if(type == "mean"){
        
        for (i in path.delim){
          
          k <- change_mean(read.delim(i))
          df <- cbind(df, k)
        }
        all_data <- data.frame(df)
        
        column_names <- dir(base.path, full.names = FALSE)
        
        final <- all_data %>%
          select_(~contains("cpt")) %>%
          mutate_all(funs(px_height - .)) %>%
          mutate_all(funs(.*L_per_pix)) %>%
          `colnames<-`(c(column_names)) %>%
          rownames_to_column(var = "matrix.column.number")
        
        save.path <- savepath
        
        if(stats == FALSE){
          ##connect save path with samplename
          save <- file.path(save.path, if(is.null(samplename) == TRUE){paste(basename(basepath))}else{paste(samplename)})
          filename.path <- file.path(save, if(is.null(filetail) == TRUE){paste(type)}else{paste(filetail)}, fsep = "_")
          csv.ending <- file.path(filename.path, "csv", fsep = ".")
          
          ##write a csv to the save path folder
          write.csv(x = final, file = csv.ending, row.names = FALSE)
          final
        } else if(stats == TRUE){
          mean_df <- final %>%
            select(-matrix.column.number) %>%
            summarise_all(funs(mean)) %>%
            t()%>%
            data.frame()
          colnames(mean_df)[colnames(mean_df) == "."] <- "mean"
          
          median_df <- final %>%
            select(-matrix.column.number) %>%
            summarise_all(funs(median)) %>%
            t()%>%
            data.frame()
          colnames(median_df)[colnames(median_df) == "."] <- "median"
          
          var_df <- final %>%
            select(-matrix.column.number) %>%
            summarise_all(funs(var)) %>%
            t()%>%
            data.frame()
          colnames(var_df)[colnames(var_df) == "."] <- "variance"
          
          sd_df <- final %>%
            select(-matrix.column.number) %>%
            summarise_all(funs(sd)) %>%
            t()%>%
            data.frame()
          colnames(sd_df)[colnames(sd_df) == "."] <- "standard.deviation"
          
          z <- nrow(mean_df)
          image.order <- seq(1:z)
          stats <- cbind(image.order, mean_df, median_df, var_df, sd_df)
          
          ##connect save path to the filename folder
          save <- file.path(save.path, if(is.null(samplename) == TRUE){paste(basename(basepath))}else{paste(samplename)})
          filename.path <- file.path(save, if(is.null(filetail) == TRUE){paste(type, "stats", sep = "_")}else{paste(filetail)}, fsep = "_")
          csv.ending <- file.path(filename.path, "csv", fsep = ".")
          
          ##write a csv to the save path folder
          write.csv(x = stats, file = csv.ending, row.names = FALSE)
          stats
        }
      } else if(type == "variance"){
        
        for (i in path.delim){
      
          k <- change_var(read.delim(i))
          df <- cbind(df, k)
        }
        all_data <- data.frame(df)
        
        column_names <- dir(base.path, full.names = FALSE)
        
        final <- all_data %>%
          select_(~contains("cpt")) %>%
          mutate_all(funs(px_height - .)) %>%
          mutate_all(funs(.*L_per_pix)) %>%
          `colnames<-`(c(column_names)) %>%
          rownames_to_column(var = "matrix.column.number")
        
        save.path <- savepath
      
        if(stats == FALSE){
          ##connect save path with samplename
          save <- file.path(save.path, if(is.null(samplename) == TRUE){paste(basename(basepath))}else{paste(samplename)})
          filename.path <- file.path(save, if(is.null(filetail) == TRUE){paste(type)}else{paste(filetail)}, fsep = "_")
          csv.ending <- file.path(filename.path, "csv", fsep = ".")
          
          ##write a csv to the save path folder
          write.csv(x = final, file = csv.ending, row.names = FALSE)
          final
        } else if(stats == TRUE){
          mean_df <- final %>%
            select(-matrix.column.number) %>%
            summarise_all(funs(mean)) %>%
            t()%>%
            data.frame()
          colnames(mean_df)[colnames(mean_df) == "."] <- "mean"
          
          median_df <- final %>%
            select(-matrix.column.number) %>%
            summarise_all(funs(median)) %>%
            t()%>%
            data.frame()
          colnames(median_df)[colnames(median_df) == "."] <- "median"
          
          var_df <- final %>%
            select(-matrix.column.number) %>%
            summarise_all(funs(var)) %>%
            t()%>%
            data.frame()
          colnames(var_df)[colnames(var_df) == "."] <- "variance"
          
          sd_df <- final %>%
            select(-matrix.column.number) %>%
            summarise_all(funs(sd)) %>%
            t()%>%
            data.frame()
          colnames(sd_df)[colnames(sd_df) == "."] <- "standard.deviation"
          
          z <- nrow(mean_df)
          image.order <- seq(1:z)
          stats <- cbind(image.order, mean_df, median_df, var_df, sd_df)
          
          ##connect save path with samplename
          save <- file.path(save.path, if(is.null(samplename) == TRUE){paste(basename(basepath))}else{paste(samplename)})
          filename.path <- file.path(save, if(is.null(filetail) == TRUE){paste(type, "stats", sep = "_")}else{paste(filetail)}, fsep = "_")
          csv.ending <- file.path(filename.path, "csv", fsep = ".")
          
          ##write a csv to the save path folder
          write.csv(x = stats, file = csv.ending, row.names = FALSE)
          stats
        }
      } 
      
  } else warning("You must enter a basepath and savepath")
}


