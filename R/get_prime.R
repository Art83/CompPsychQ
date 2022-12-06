#' get_prime function
#' @param dataset original dataset "PRIME-R" from the bundle
#' @param completers boolean parameter, if True filters out participants that are not labeled as completers
#' @return dataframe with 4 columns:
#'         PIN, cutoff_6, cutoff_5, prime_cat
#' @export
get_prime <- function(dataset, subscales=F, completers=T){
  if(nrow(dataset) == 0 | ncol(dataset) == 0){
    stop("Empty dataset")
  }
  dataset$PIN <- gsub("'", "", dataset$PIN)
  essential_cols <- c("pin", "complete",  "item", "response")
  colnames(dataset) <- tolower(colnames(dataset))
  if(!all(essential_cols %in% colnames(dataset))){
    stop(essential_cols[!essential_cols %in% colnames(dataset)]," column(s) not found in the dataset")
  }
  if(any(is.na(dataset["pin"])) | any(dataset["pin"] == "")){
    stop("Missed data in pin column")
  }
  if(any(is.na(dataset["item"])) | any(dataset["pin"] == "")){
    stop("Missed data in item column")
  }
  if(any(is.na(dataset$response))){
    stop("You have NAs in response columns!")
  }
  
  if(!all(dataset$response %in% c("Definitely disagree", "Somewhat disagree","Slightly disagree", "Not sure","Definitely agree", "Somewhat agree","Slightly agree"))){
    stop("Range constraints are broken!")
  }
  if(completers){
    num_participants <- unique(dataset[dataset$complete == 'y', "pin"])
    dataset <- dataset[dataset$complete == "y", ]
    if(nrow(dataset) == 0){
      stop("There are no completers in your dataset")
    }
  } else {
    num_participants <- unique(dataset$pin)
  }
  
  
  
  dataset$item <- as.numeric(dataset$item)
  dataset$response <- as.character(dataset$response)
  
  dataset$response <- prime_scale$score[match(dataset$response, prime_scale$response)] 
  

  df_6 <- aggregate(response ~ pin, data = dataset, function(x) sum(x == 6))
  colnames(df_6)[2] <- "prime_sum_cutoff_6"
  df_5_3 <- aggregate(response ~ pin, data = dataset, function(x) sum(x == 5))
  colnames(df_5_3)[2] <- "prime_sum_cutoff_5"
  
  df_sum <- merge(df_6, df_5_3, by="pin")
  
  df_sum$prime_cat <- ifelse(df_sum$prime_sum_cutoff_6 > 0 | df_sum$prime_sum_cutoff_5 > 0, 1, 0)
  colnames(df_sum)[1] <- "PIN" 
  return(df_sum)
}