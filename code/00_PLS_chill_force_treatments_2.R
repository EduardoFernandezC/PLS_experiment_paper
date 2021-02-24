library(pls)


PLS_chill_force_treatments_2 <- function (daily_chill_obj, bio_data_frame, expl.var = 30, 
                                        ncomp.fix = NULL, return.all = FALSE, crossvalidate = "none",
                                        end_at_pheno_end = TRUE,
                                        chill_models = c("Chilling_Hours", "Utah_Chill_Units",
                                                         "Chill_Portions"),
                                        heat_models = c("GDH"),
                                        runn_means = 1, metric_categories = c("Chill", "Heat")) 
{
  
  # Check the sort of 'class' for daily_chill_obj
  
  # if (!daily_chill_obj[[1]] == "daily_chill") 
  #   stop("Error: not a daily chill object; use function daily_chill to make one from hourly temperature data")
  
  # Extract the chilling data frame from the daily_chill list
  
  dc <-  as.data.frame(daily_chill_obj)
  
  # Repeat the same as above (no clue yet)
  
  weather_file <- as.data.frame(daily_chill_obj)
  
  # Change the name of the bio_data_frame
  
  bio_data <- bio_data_frame
  
  # Define the seasons according to the split_month
  
  # weather_file[which(weather_file$Month <= split_month), "Season"] <- weather_file[which(weather_file$Month <= 
  #                                                                                          split_month), "Year"]
  # weather_file[which(weather_file$Month > split_month), "Season"] <- weather_file[which(weather_file$Month > 
  #                                                                                         split_month), "Year"] + 1
  
  # Add the date column in format MODA
  
  weather_file[, "Date"] <- weather_file$Month * 100 + weather_file$Day
  
  # Add the column JDay to the weather data frame
  
  weather_file[, "JDay"] <- strptime(paste(weather_file$Month, "/", weather_file$Day, "/",
                                           weather_file$Year, sep = ""),
                                     "%m/%d/%Y")$yday + 1
  
  # Define an internal function to determine the number of components used in the PLS analysis
  
  pls_ncomp_old <- function(indep, dep, threshold = 30) {
    
    dat <- data.frame(dep)
    dat$runn <- indep
    
    if (length(dep) > 15) {
      suppressWarnings(pls_out <- plsr(dep ~ runn, data = dat, 
                                       ncomp = 10, validation = "CV"))
      suppressWarnings(pls_cv <- crossval(pls_out, segments = 10))
      res <- data.frame(ncomp = c(1:10), explvar = explvar(pls_out), 
                        cumul = NA)
      res$cumul[1] <- res$explvar[1]
      for (i in 2:nrow(res)) {
        res$cumul[i] <- res$cumul[i - 1] + res$explvar[i]
      }
      ncomp <- which(res$cumul > threshold)[1]
      if (is.na(ncomp)) 
        ncomp <- 10
    }
    else ncomp <- 2
    return(ncomp)
  }
  
  # Same as above but using a newer version?
  
  pls_ncomp <- function(indep, dep, threshold) {
    dat <- data.frame(dep)
    dat$runn <- indep
    if (length(dep) > 15) {
      suppressWarnings(pls_out <- plsr(dep ~ runn, data = dat, 
                                       ncomp = 10, validation = "none"))
      ncomp <- which(cumsum(explvar(pls_out)) > threshold)[1]
      if (is.na(ncomp)) 
        ncomp <- 10
    }
    else ncomp <- 2
    return(ncomp)
  }
  
  # Get the seasons vector. In this case, I will modify it to the treatments
  
  treatments <- unique(weather_file$Treatment)
  
  # Define all models used in this PLS_chill_force analysis
  
  all_models <- c(chill_models, heat_models)
  
  # Check the length of runn_means parameter (not idea yet of its use)
  
  if (!length(runn_means) %in% c(1, length(all_models))) 
    stop("runn_means has to be either 1 or an integer vector with as many elements as\n           the number of models considered (chill and heat models combined)")
  
  # Define the 'runner' vector depending on the runn_means parameter
  
  if (length(runn_means) == 1) 
    runner <- rep(runn_means, length(all_models))
  
  if (length(runn_means) == length(all_models)) 
    runner <- runn_means
  
  
  # Apply the running mean to each column of the models in the weather data frame
  
  for (i in 1 : length(all_models)){
    
    weather_file[, all_models[i]] <- runn_mean(weather_file[, all_models[i]], runner[i])
  }
  
  
  
  # Filter the data frame to keeping the data starting on October
  
  weather_file <- weather_file[weather_file$Month %in% c(1 : 5, 10 : 12), ]
  
  
  
  
  # Define the list for collecting the PLS outputs
  
  all_outputs <- list(object_type = "PLS_chillforce_pheno")
  
  
  # Perform the matrix for implementing the PLS analysis
  
  for (CM in chill_models)
    for (HM in heat_models){
      for (yy in treatments) {
        
        # Subset the weather according to each treatment (yy index)
        yearweather <- weather_file[which(weather_file$Treatment == yy), ]
        
        # Keep only 243 days
        yearweather <- yearweather[1 : 243, ]
        
        # Define the weather vector according to each Chill and Heat model (CM and HM, respectively)
        weathervector <- c(yearweather[1 : 243, CM], yearweather[1 : 243, HM])
        
        # Merge the weather vectors depending on the index 'yy'. The vector 'year_res' is collecting
        # all weather values
        if (yy == treatments[1]) year_res <- weathervector else
          
          year_res <- rbind(year_res, weathervector)
        
        # Define the labels for dates and JDays
        if (nrow(yearweather) == 243) {
          labdates <- yearweather$Date
          labJdates <- yearweather$JDay
        }
      }
      
      
      # Set the names of the matrix for each combination of Chill model and Heat model
      
      colnames(year_res) <- c(paste("Chill_", 1 : 243, sep = ""), paste("Heat_", 1 : 243, sep = ""))
      
      # Add the column Seasons/Treatments to the matrix
      
      year_res <- cbind(Season = treatments, year_res)
      
      # Duplicate the year_res matrix
      
      data_set <- year_res
      
      # Identify the seasons/treatments having complete data
      
      full_seasons <- which(!is.na(rowMeans(data_set)))
      
      # Remove the seasons/treatments for which the rowMeans function retrieves NA... (any missing)
      
      data_set <- data_set[full_seasons, ]
      
      # Define the newSeasons for which data is complete
      
      newseasons <- data_set[, "Season"]
      
      # Remove the missing seasons in the weather data frame from the bio_data data frame
      
      suppressWarnings(bio_data <- bio_data[which(bio_data[, "Treatment"] %in% newseasons), ])
      
      # Keep only the complete seasons in the bio_data data set
      
      suppressWarnings(bio_data <- bio_data[which(!is.na(as.numeric(as.character(bio_data$pheno)))), ])
      
      # Include the bio_data data frame in the PLS outputs
      
      all_outputs[["pheno"]] <- bio_data
      
      # Define a new bio data frame
      
      suppressWarnings(bio <- as.numeric(as.character(bio_data$pheno)))
      
      # Define the independent variables
      
      indep <- as.matrix(data_set[which(data_set[, "Season"] %in% bio_data$Treatment), ])
      
      # Remove the column Season
      
      indep <- indep[, 2 : ncol(indep)]
      
      # Define the end of the analysis
      
      pheno_end <- get_last_date(as.numeric(as.character(bio_data$pheno)))
      
      # pheno_end defined by the user through end_at_pheno_end parameter
      
      if (is.numeric(end_at_pheno_end)) pheno_end <- end_at_pheno_end
      
      # Not really sure about this step. It seems re-define the independent variables at the end
      
      if (end_at_pheno_end)
        if (pheno_end %in% labJdates) {
          
          dayskeep <- 1 : which(labJdates == pheno_end)
          labJdates <- labJdates[dayskeep]
          labdates <- labdates[dayskeep]
          indep <- indep[, c(dayskeep, 243 + dayskeep)]
        }
      
      # Define the number of components according to ncomp.fix parameter or with the internal 
      # function
      
      if (is.null(ncomp.fix)) {
        
        ncomp <- pls_ncomp(indep = indep, dep = bio, threshold = expl.var)} else {
          
          ncomp <- ncomp.fix}
      
      # Compute the standar deviation for the independent variables
      
      sdindep <- apply(indep, 2, sd)
      
      # Change 0 to 1 in sdindep
      
      sdindep[which(sdindep == 0)] <- 1
      
      # Perform the PLS analysis
      
      PLS_output <- plsr(bio ~ indep, ncomp, validation = "none", method = "oscorespls",
                         scale = sdindep)
      
      # Define the d1 value to determine the cutpoint in JDays
      
      #d1 <- switch(split_month, 31, 59, 89, 120, 151, 181, 212, 243, 274, 304, 335, 365)
      
      d1 <- labJdates[1]
      
      # Set the JDays labels
      
      labJdates[labJdates >= d1] <- labJdates[labJdates >= d1] - 365
      
      # Define the output data frame for VIP, Coef and others
      
      out <- data.frame()
      
      # Set the length of the table
      
      tablength <- length(coef(PLS_output))
      
      # Add the dates to the Date column in the output data frame
      
      out[1 : tablength, "Date"] <- labdates
      
      # Add the type to the output data frame
      
      out[1 : tablength, "Type"] <- c(rep(metric_categories[1], tablength / 2),
                                      rep(metric_categories[2], tablength / 2))
      
      
      # Add the JDay labels to the output data frame
      
      out[1:tablength, "JDay"] <- labJdates
      
      # Add the coefficients 
      
      out[1:tablength, "Coef"] <- coef(PLS_output)
      
      # Add the VIP score depending on the number of components used in the PLS analysis
      
      if (ncomp == 1) out[1 : tablength, "VIP"] <- VIP(PLS_output)
      if (ncomp > 1) out[1 : tablength, "VIP"] <- VIP(PLS_output)[ncomp, ]
      
      # Ad the Metric mean
      
      out[1:tablength, "MetricMean"] <- colMeans(indep)
      
      # Add the metric standard deviation
      
      out[1:tablength, "MetricStdev"] <- apply(indep, 2, sd, na.rm = TRUE)
      
      if (return.all)
        all_outputs[[CM]][[HM]] <- list(PLS_summary = out,
                                        PLS_output = PLS_output)
      else
        all_outputs[[CM]][[HM]] <- list(PLS_summary = out)
    }
  return(all_outputs)
}