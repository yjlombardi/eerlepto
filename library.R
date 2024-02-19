remove_empty_cols <- function(my_df)
{
  empty_columns <- colSums(is.na(my_df) | my_df == "") == nrow(my_df)
  return(my_df[, !empty_columns])
}

convert <- function(data)
{
  # Conversion des caractères en factor
  for(i in 1:dim(data)[2])
  {
    curcol <- data[,i]
    if(is.character(curcol))
    {
      data[which(data[,i] == ''),i] <- NA
      data[,i] <- as.factor(data[,i])
    }
    else if(is.integer(curcol))
    {
      x <- levels(as.factor(curcol))
      if(length(x) == 2 && x[1] == 0 && x[2] == 1)
      {
        data[,i] <- as.logical(data[,i])
      }
      else if(length(x) <= 10)
      {
        data[,i] <- as.factor(data[,i])
      }
    }
  }
  
  # Conversion des dates
  for(i in 1:dim(data)[2])
  {
    if(substr(names(data)[i], 0,4) == 'date')
    {
      data[,i] <- as.Date(data[,i], format = '%d/%m/%Y')
    }
  }
  
  return(data)
}