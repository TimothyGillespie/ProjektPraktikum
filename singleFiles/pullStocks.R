library(quantmod)

#-------------------------------------------------------

# Hilfsfunktion: + ist eine String-Konkatenation
# Das k?nnt ihr zun?chst mal ignorieren
"+" <- function(...) UseMethod("+") 
"+.default" <- .Primitive("+") 
"+.character" <- function(...) paste(...,sep="")

#-------------------------------------------------------

get_stock_data <- function (symbol, symbol_naming = F, cache = T)
{
  if (symbol == "GDAXI") symbol <- "^GDAXI"
  
  a <- suppressWarnings (getSymbols (symbol, src = "yahoo", auto.assign = F))
  a <- na.omit (a)
  if (!symbol_naming)
  {
    colnames (a) <- c ("Open", "High", "Low", "Close", "Volume", "Adjusted")
  }
  else
  {
    colnames (a) <- c (symbol + "_Open", symbol + "_High", symbol + "_Low", symbol + "_Close", symbol + "_Volume", symbol + "_Adjusted")
    colnames (a) <- str_replace_all (colnames (a), fixed ("^"), "")
  }
  
  return (a)
}

add_moving_average <- function (df, moving_average_range) {
  for(i in (moving_average_range + 1):nrow(df))
    df[i, "Moving_Average"] <- mean(df$Close[i-moving_average_range:i])
  
  # Remove all NA elements (cropping method for edge treatment)
  is_na <- !is.na(df$Moving_Average)
  df <- df[is_na,]
  
  return(df)
  
}

calculateAngle <- function (df, i) {
  
  m1 <- df[i, "Close"] - df[i-1, "Close"]
  m2 <- df[i, "Moving_Average"] - df[i - 1, "Moving_Average"]
  
  intermediate_result <- ((m1 - m2) / (1 + m1 * m2))
  angle <- atan(intermediate_result)
  
  return(angle)
  
}

add_angle_information <- function (df) {
  if(df[1, "Moving_Average"] > df[1, "Close"]) {
    average_over_close = TRUE
  } else {
    average_over_close = FALSE
  }
  
  current_last_angle <- NA
  n <- NA
  
  # Angle in radians and counting the days since it occured
  for(i in 2:nrow(df)) {
    
    
    if(df[i, "Moving_Average"] > df[i, "Close"]) {
      updated_average_over_close = TRUE
    } else {
      updated_average_over_close = FALSE
    }
    
    if(average_over_close != updated_average_over_close) {
      current_last_angle <- calculateAngle(df, i)
      n <- 0 
    }
    
    average_over_close <- updated_average_over_close
    
    df[i, "Last_Angle"] <- current_last_angle
    df[i, "Days_Since_Last_Cross"] <- n
    
    if(!is.na(n))
      n <- n + 1
    
  }
  
  is_na <- !is.na(df$Days_Since_Last_Cross)
  df <- df[is_na,]
  
  return(df)
  
}

add_MA_close <- function (df) {
  df[, "MA_Close"] <- df[, "Close"] - df[,"Moving_Average"]

  return(df)  
}

# to the previous day
add_percentage_change <- function (df) {
  for(i in 2:nrow(df))
    df[i, "Percentage_Change_Close"] <- (df[i, "Close"] / df[i - 1, "Close"]) - 1
  
  return(df)
}

add_other_stock <- function (stock1, stock2, prefix) {
  for(i in c(1:nrow(stock2))) {
      if(!is.na(stock1[rownames(stock2)[i],"Close"])) {
        stock1[i, prefix + "_Percentage_Change_Close"] <- stock2[i, "Percentage_Change_Close"]
        stock1[i, prefix + "_MA_Close"] <- stock2[i, "MA_Close"]
        stock1[i, prefix + "_Last_Angle"] <- stock2[i, "Last_Angle"]
        stock1[i, prefix + "_Days_Since_Last_Cross"] <- stock2[i, "Days_Since_Last_Cross"]
         
      }
  }
  
  return(na.omit(stock1))
}

# k is the dataframe
# i is the row in the dataframe
generate_class <- function (k, i) {
  up <- c (Inf, which ((k[, "High"] > k[i, "Close"] * 1.03) & (k > i)))
  down <- c (Inf, which ((k[, "Low"] < k[i, "Close"] * 0.97) & (k > i)))
  if (min (up) < min (down)) { result <- "Steigt"; } else { result <- "Fällt"; };
  return (result)
}

add_classes <- function(df) {
  for (i in 1:nrow(df))
    df[i, "Class"] <- generate_class (k = df, i = i)

  return(df)
}

#ma_range meaning moving average range
processing_pipelin <- function(df, ma_range = 150) {
  df <- add_percentage_change(df)
  df <- add_moving_average(df, ma_range)
  df <- add_angle_information(df)
  df <- add_MA_close(df)
  df <- add_classes(df)
  
  return(df)
}


# Pulling AMD stocks since they are currently hugely winning against Intel in the market 
amd <- as.data.frame(get_stock_data("AMD"))
amd <- processing_pipelin(amd)

intel <- as.data.frame(get_stock_data("INTC"))
intel <- processing_pipelin(intel)

apple <- as.data.frame(get_stock_data("AAPL"))
apple <- processing_pipelin(apple)

qualcomm <- as.data.frame(get_stock_data("QCOM"))
qualcomm <- processing_pipelin(qualcomm)

nvidia <- as.data.frame(get_stock_data("NVDA"))
nvidia <- processing_pipelin(nvidia)

amd_compared <- add_other_stock(amd, intel, "Intel")
amd_compared <- add_other_stock(amd_compared, apple, "Apple")
amd_compared <- add_other_stock(amd_compared, qualcomm, "Qualcomm")
#amd_compared <- add_other_stock(amd_compared, nvidia, "Nvidia")

# Save as csv in a linux system
write.csv (amd[, c("MA_Close", "Last_Angle", "Days_Since_Last_Cross", "Class")], file = "/home/timothy/amd.csv")
write.csv (intel[, c("MA_Close", "Last_Angle", "Days_Since_Last_Cross", "Class")], file = "/home/timothy/intel.csv")
write.csv (apple[, c("MA_Close", "Last_Angle", "Days_Since_Last_Cross", "Class")], file = "/home/timothy/apple.csv")
write.csv (nvidia[, c("MA_Close", "Last_Angle", "Days_Since_Last_Cross", "Class")], file = "/home/timothy/nvidia.csv")
write.csv (qualcomm[, c("MA_Close", "Last_Angle", "Days_Since_Last_Cross", "Class")], file = "/home/timothy/qualcomm.csv")
write.csv (amd_compared[, c("Percentage_Change_Close", "MA_Close", "Last_Angle", "Days_Since_Last_Cross", "Intel_Percentage_Change_Close", "Intel_MA_Close", "Intel_Last_Angle", "Intel_Days_Since_Last_Cross", "Class")], file = "/home/timothy/compare_amd_intel.csv")
