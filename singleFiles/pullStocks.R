library(quantmod)
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

# Pulling AMD stocks since they are currently hugely winning against Intel in the market 

df <- as.data.frame(get_stock_data("AMD"))

generate_class <- function (k, i)
{
  up <- c (Inf, which ((k[, "High"] > k[i, "Close"] * 1.03) & (k > i)))
  down <- c (Inf, which ((k[, "Low"] < k[i, "Close"] * 0.97) & (k > i)))
  if (min (up) < min (down)) { result <- "Steigt"; } else { result <- "Fällt"; };
  return (result)
}

calculateAngle <- function (df, i) {
  
  m1 <- df[i, "Close"] - df[i-1, "Close"]
  m2 <- df[i, "Moving_Average"] - df[i - 1, "Moving_Average"]
  
  intermediate_result <- ((m1 - m2) / (1 + m1 * m2))
  angle <- atan(intermediate_result)
  
  return(angle)
  
}


moving_average_range <- 150

# Calculate the moving average with a range of 150. The first 150 will be NA
for(i in (moving_average_range + 1):nrow(df))
  df[i, "Moving_Average"] <- mean(df$Close[i-moving_average_range:i])

# Remove all NA elements (cropping method for edge treatment)
is_na <- !is.na(df$Moving_Average)
df <- df[is_na,]


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

df[, "MA_Close"] <- df[, "Close"] - df[,"Moving_Average"]

for (i in 1:nrow(df))
  df[i, "Class"] <- generate_class (k = df, i = i)



# Als Arff-Datei abspeichern
write.csv (df[, c("MA_Close", "Last_Angle", "Days_Since_Last_Cross", "Class")], file = "/home/timothy/data.csv")