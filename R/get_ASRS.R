#' get_asrs function
#' @param dataset original dataset "ASRS" from the bundle
#' @param completers boolean parameter, if True filters out participants that are not labeled as completers
#' @return dataframe with 3 columns:
#'         PIN, asrs_sum, asrs_cat
#' @export
get_asrs <- function(dataset, subscales=F, completers=T){
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
  
  if(!all(dataset$response %in% c("Never", "Rarely", "Sometimes","Often","Very often"))){
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
  
  dataset$response <- ifelse(as.character(dataset$response) == 'Never', 0, 
                             ifelse(as.character(dataset$response) == 'Rarely', 1,
                                    ifelse(as.character(dataset$response) == 'Sometimes', 2,
                                           ifelse(as.character(dataset$response) == 'Often', 3, 4)) ))  
  
  
  df_sum <- aggregate(response ~ pin, data=dataset, sum)
  df_sum$asrs_cat <- ifelse(df_sum$response >= thr_asrs, 1, 0)
  colnames(df_sum) <- c("PIN", "asrs_sum", "asrs_cat")
  return(df_sum)
}