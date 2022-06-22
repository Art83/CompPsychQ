#' get_pgsi function
#' @param dataset original dataset "PGSI" from the bundle
#' @param completers boolean parameter, if True filters out participants that are not labeled as completers
#' @return dataframe with 4 columns:
#'         PIN, pgsi_sum, pgsi_cat, pgsi_severity
#' @export
get_pgsi <- function(dataset, subscales=F, completers=T){
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
  
  if(!all(dataset$response %in% c("Never", "Sometimes","Most of the time", "Almost always"))){
    stop("Range constraints are broken!")
  }
  if(!all(as.character(dataset$item) %in% as.character(c(1:9)) )){
    stop("Item constraints are broken!")
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
  
  dataset$response <- pgsi_scale$score[match(dataset$response, pgsi_scale$response)] 
  
  
  df_sum <- aggregate(response ~ pin, data = dataset, sum, na.action = NULL)

  
  df_sum$pgsi_cat <- ifelse(df_sum$response >= 8, 1, 0)
  df_sum$pgsi_sev <- ifelse(df_sum$response >= 8, 3,
                             ifelse((df_sum$response>= 3 & df_sum$response <= 7), 2,
                                     ifelse((df_sum$response >= 1 & df_sum$response <= 2), 1, 0)))
  
  
  colnames(df_sum) <- c("PIN", "pgsi_sum", "pgsi_sev") 
  return(df_sum)
}