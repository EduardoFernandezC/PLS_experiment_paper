library(pls)

pls_treatments_2 <- function(weather_data, bio_data, split_month = 7, runn_mean = 11, 
                           expl.var = 30, ncomp.fix = NULL, use_Tmean = FALSE, return.all = FALSE, 
                           crossvalidate = "none", end_at_pheno_end = TRUE) 
{
  
  # Transform the weather tibble into a dataframe
  
  weather_data <- as.data.frame(weather_data)
  
  # If the weather data only include Tmin and Tmax, this step will compute Tmean after some checks
  
  if (!use_Tmean) {
    
    # Get the row numbers for complete cases of Tmin and Tmax
    cl.dat <- which(!is.na(weather_data[, "Tmax"]) |
                      !is.na(weather_data[, "Tmin"]))
    
    # Select the rows having either complete Tmin or Tmax
    weather_file <- weather_data[min(cl.dat) : max(cl.dat), ]
    
    # Add a check step incase Tmin is greater than Tmax and add NA if TRUE
    weather_file[which(weather_file$Tmin > weather_file$Tmax), c("Tmin", "Tmax")] <- NA
    
    # Fill gaps (linearly) in Tmin and Tmax with interpolate_gaps function
    # Get the filling gaps
    Tmin_gaps <- interpolate_gaps(weather_file$Tmin)
    
    # Replace the Tmin vector with Tmin_gaps from above
    weather_file$Tmin <- Tmin_gaps[[1]]
    
    # Same as above for Tmax
    Tmax_gaps <- interpolate_gaps(weather_file$Tmax)
    
    weather_file$Tmax <- Tmax_gaps[[1]]
    
    
    # Compute Tmean from extreme records
    weather_file[, "Tmean"] <- (weather_file$Tmax + weather_file$Tmin) / 2
  }
  
  
  
  # If the weather dataframe contains Tmean already computed, this step will identify and fill missing
  # values
  
  if (use_Tmean) {
    
    # Identify missing values in the Tmean column
    cl.dat <- which(!is.na(weather_data[, "Tmean"]))
    
    # Filter the complete cases in the weather dataframe
    weather_file <- weather_data[min(cl.dat) : max(cl.dat), ]
    
    # Fill missing values using a linear interpolation
    Tmean_gaps <- interpolate_gaps(weather_file$Tmean)
    
    # Paste the Tmean_gaps vector in the Tmean column of the weather dataframe
    weather_file$Tmean <- Tmean_gaps[[1]]
  }
  
  
  # Define the season column in the weather dataframe according to the split_month parameter
  
  # weather_file[weather_file$Month <= split_month, "Season"] <- weather_file[weather_file$Month <= 
  #                                                                             split_month, "Year"]
  # weather_file[weather_file$Month > split_month, "Season"] <- weather_file[weather_file$Month > 
  #                                                                            split_month, "Year"] + 1
  
  
  # Add the Date column as MODA (month and day)
  
  weather_file[, "Date"] <- weather_file$Month * 100 + weather_file$Day
  
  
  # Add the JDay column to the weather dataframe
  
  weather_file[, "JDay"] <- strptime(paste(weather_file$Month, 
                                           "/", weather_file$Day, "/", weather_file$Year, 
                                           sep = ""), "%m/%d/%Y")$yday + 1
  
  
  
  # Compute the running mean vector. Since all the trees were exposed at the same conditions at the begining
  # of the experiment, I modified a little the running mean function by computing the vector separatelly 
  # for each treatment. In this way, all the treatments share the same running mean until the begining of the
  # experiment.
  
  # Define the treatments vector
  
  treatments <- unique(weather_file$Treatment)
  
  # Create an empty running mean vector to collect the outputs of the for loop
  
  runn_mean_vector <- numeric()
  
  for (treatment in treatments){
    
    # This step get the Tmean for the complete year of each treatment
    ww <- weather_file[weather_file$Treatment == treatment, "Tmean"]
    rr <- ww
    
    # This computes the running mean depending on the position of the 'dd' index.
    for (dd in 1 : length(ww)) {
      
      # At the begining of the vector ('runn_mean/2' positions), the mean is computed from 1 up to runn_mean
      # parameter
      if (dd < ceiling(runn_mean / 2)) {
        rr[dd] <- mean(ww[1 : (dd + floor(runn_mean/2))])
      }
      
      # After the first rows, the mean is computed from runn_mean/2 to runn_mean/2 from the position dd
      if ((dd >= ceiling(runn_mean/2)) & (dd <= length(ww) - ceiling(runn_mean/2))) {
        rr[dd] <- mean(ww[(dd - floor(runn_mean/2)):(dd + floor(runn_mean/2))])
      }
      
      # At the end of the vector, mean is computed with the last runn_mean/2 numbers
      if (dd > (length(ww) - ceiling(runn_mean/2))) {
        rr[dd] <- mean(ww[(dd - floor(runn_mean/2)):length(ww)])
      }
    }
    
    # Collect all the means in a single vector
    runn_mean_vector <- c(runn_mean_vector, rr)
  }
  
  # Add the running mean to the weather dataframe
  
  weather_file[, "runn"] <- runn_mean_vector
  
  
  
  
  
  ## Internal function that computes the number of components used in the PLS model
  
  pls_ncomp <- function(indep, dep, threshold) {
    
    dat <- data.frame(dep)
    dat$runn <- indep
    
    
    if (length(dep) > 15) {
      
      suppressWarnings(pls_out <- pls::plsr(dep ~ runn, data = dat, ncomp = 10, validation = "none"))
      
      ncomp <- which(cumsum(explvar(pls_out)) > threshold)[1]
      
      if (is.na(ncomp)) ncomp <- 10
    } else ncomp <- 2
    
    return(ncomp)
  }
  ## End of the internal function
  
  
  # Get the seasons used in the weather dataframe. I will change this to "Treatment" since this
  # is more appropriate for my experimental design
  
  seasons <- unique(weather_file$Treatment) # The original is: "unique(weather_file$Season)"
  
  
  
  # Filter the data frame to keeping the data starting on October
  
  weather_file <- weather_file[weather_file$Month %in% c(1 : 5, 10 : 12), ]
  
  
  
  
  
  
  # Create a matrix with ndays columns and ntreatments rows
  
  for (yy in seasons) {
    
    # Get the weather data for each treatment at a time
    yearweather <- weather_file[weather_file$Treatment == yy, ] # The original is: "$Season"
    
    # This step only keeps 365 days, so the leap years are missing at this point... (?)
    weathervector <- yearweather$runn[1 : 243] # The original is: [1:365]
    
    # Creates the matrix after two runs of the foor loop
    if (yy == seasons[1]) 
      
      year_res <- weathervector else
        
        year_res <- rbind(year_res, weathervector)
    
    # Get the labels for non-leap years. Beware that this step will not work if all your treatments have
    # 366 days of data
    if (yy == seasons[1]) { # The original is: 365
      
      labdates <- yearweather$Date
      labJdates <- yearweather$JDay
    }
  }
  
  
  # Change the name of the columns for the year_res matrix according to the running mean of the day 'i'
  
  colnames(year_res) <- paste("runn_", 1 : 243, sep = "")
  
  # Add the season column to the matrix... In this case is the treatments
  
  year_res <- cbind(Season = seasons, year_res)
  
  # Copy the year_res matrix into a 'data_set' object
  
  data_set <- year_res
  
  
  # Get the seasons ('Treatment') that have complete data
  
  full_seasons <- which(!is.na(rowMeans(data_set)))
  
  # Filter data_set matrix keeping the seasons ('Treatments') that have complete data for the whole year
  
  data_set <- data_set[full_seasons, ]
  
  
  # Get a new vector containing the complete seasons
  
  newseasons <- data_set[, "Season"]
  
  # Select the rows in the phenological dataset having weather data by matching the newseason vector.
  # In this case, I will use treatment as my season argument
  
  suppressWarnings(bio_data <- bio_data[which(bio_data[, "Treatment"] %in% newseasons), ])
  
  # Remove the rows having missing values in the phenological dataset
  
  suppressWarnings(bio_data <- bio_data[which(!is.na(as.numeric(as.character(bio_data$pheno)))), ])
  
  # Get a numeric vector of JDays from the pheno column in the bio_data dataframe
  
  suppressWarnings(bio <- as.numeric(as.character(bio_data$pheno)))
  
  # Remove the row for phenological missing values from the weather dataframe. Define the indep 
  # (independent variable?)
  
  indep <- as.matrix(data_set[which(data_set[, "Season"] %in% bio_data$Treatment), ]) 
  
  # Remove the season column from the matrix for independent variables (?)
  
  indep <- indep[, 2 : ncol(indep)]
  
  
  # Define the end of the phenological records
  
  pheno_end <- get_last_date(as.numeric(as.character(bio_data$pheno)))
  
  
  # If the end_at_pheno_end parameter is provided as a JDay, this will overwrite the pheno_end vector
  
  if (is.numeric(end_at_pheno_end)) pheno_end <- end_at_pheno_end
  
  
  # If the end_at_pheno_end parameter is set to TRUE, this will remove the days after the last measured
  # phenological record from the labels (JDay and Date) as well as from the independent variable matrix
  
  if (end_at_pheno_end){
    
    if (pheno_end %in% labJdates) {
      
      dayskeep <- 1 : which(labJdates == pheno_end)
      
      labJdates <- labJdates[dayskeep]
      
      labdates <- labdates[dayskeep]
      
      indep <- indep[, dayskeep]
    }
  }
  
  # If the number of components for the PLS model is not provided by the user, the function pls_ncomp
  # will automatically determine that input
  
  if (is.null(ncomp.fix)) {
    
    ncomp <- pls_ncomp(indep = indep, dep = bio, threshold = expl.var)
  } else {
    ncomp <- ncomp.fix
  }
  
  
  # Compute the standard deviation across the columns (parameter 2 in apply) in the independent matrix
  
  sdindep <- apply(indep, 2, sd)
  
  
  # Define sd as 1 for those cells in which the step above produced sd of 0 (reason?). It seems it is
  # important for scaling the PLS analysis later...
  
  sdindep[which(sdindep == 0)] <- 1
  
  
  # Implement the PLS model
  
  PLS_output <- plsr(bio ~ indep, ncomp, validation = crossvalidate, 
                     method = "oscorespls", scale = sdindep)
  
  
  # Define the point in JDays when the season starts
  
  d1 <- switch(split_month, 31, 59, 89, 120, 151, 181, 212, 
               243, 274, 304, 335, 365)
  
  # Implement the backward counting relative to the last day of the year (Dec 31)
  
  labJdates[labJdates > d1] <- labJdates[labJdates > d1] - 365
  
  
  # Define the output dataframe
  
  out <- data.frame()
  
  # The dimensions (i.e. nrows) depend on the total numbers of coefficients determined in the PLS model
  
  tablength <- length(coef(PLS_output))
  
  
  # Fill the output dataframe with the dates, the JDays (in backward counting) and Coefficients from the 
  # PLS output
  
  out[1:tablength, "Date"] <- labdates
  
  out[1:tablength, "JDay"] <- labJdates
  
  out[1:tablength, "Coef"] <- coef(PLS_output)
  
  # In the case of the VIP results, this will depend on the number of components used in the PLS model.
  
  if (ncomp == 1) 
    out[1:tablength, "VIP"] <- VIP(PLS_output)
  
  if (ncomp > 1) 
    out[1:tablength, "VIP"] <- VIP(PLS_output)[ncomp, ]
  
  
  # Add the Tmean vector by summarizing all the rows in the indep matrix (seasons or treatments in this
  # case)
  
  out[1:tablength, "Tmean"] <- colMeans(indep)
  
  # Add the standard deviation for Tmean 
  
  out[1:tablength, "Tstdev"] <- apply(indep, 2, sd, na.rm = TRUE)
  
  
  # Make different outputs depending on the return.all parameter...
  
  if (return.all) 
    return(list(object_type = "PLS_Temp_pheno",
                pheno = bio_data, 
                PLS_summary = out,
                PLS_output = PLS_output)) else
                  
                  
                  return(list(object_type = "PLS_Temp_pheno",
                              pheno = bio_data, 
                              PLS_summary = out))
}
