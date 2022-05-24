#' get_asrs function
#' @param dataset original dataset "ASRM" from the bundle
#' @param completers boolean parameter, if True filters out participants that are not labeled as completers
#' @return dataframe with 3 columns:
#'         PIN, asrm_sum, asrm_cat
#' @export
get_asrm <- function(dataset, completers=T){
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
  
  if(!all(dataset$response %in% c(0:4))){
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
  dataset$response <- as.numeric(dataset$response)
  
  
  df_sum <- aggregate(response ~ pin, data=dataset, sum)
  df_sum$asrm_cat <- ifelse(df_sum$response >= thr_asrm, 1, 0)
  colnames(df_sum) <- c("PIN", "asrm_sum", "asrm_cat")
  return(df_sum)
}