#' get_isi function
#' @param dataset original dataset "ASRM" from the bundle
#' @param completers boolean parameter, if True filters out participants that are not labeled as completers
#' @return dataframe with 4 columns:
#'         PIN, isi_sum, isi_cat, isi_sev
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
  
  dataset$response <- as.numeric(dataset$response)
  
  
  df_sum <- aggregate(response ~ pin, data=dataset, sum)
  df_sum$isi_cat <- ifelse(df_sum$response >= thr_isi, 1, 0)
  df_sum$isi_sev <- ifelse(df_sum$response > 22, 3,
                            ifelse((df_sum$response <= 21 & df_sum$response >= 15), 2,
                                    ifelse((df_sum$response <= 14 & df_sum$response >= 8), 1, 0)))
  
  colnames(df_sum) <- c("PIN", "isi_sum", "isi_cat", "isi_sev")
  return(df_sum)
}