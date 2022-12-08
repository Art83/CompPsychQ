#' get_sds function
#' @param dataset original dataset "PC-PTSD-5" from the bundle
#' @param completers boolean parameter, if True filters out participants that are not labeled as completers
#' @return dataframe with 3 columns:
#'         PIN, ptsd_sum, ptsd_cat
#' @export
get_ptsd <- function(dataset, completers=T){
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
  if(completers){
    num_participants <- unique(dataset[dataset$complete == 'y', "pin"])
    dataset <- dataset[dataset$complete == "y", ]
    if(nrow(dataset) == 0){
      stop("There are no completers in your dataset")
    }
  } else {
    num_participants <- unique(dataset$pin)
  }
  if(!all(dataset$response %in% c("YES", "NO", NA))){
    stop("Range constraints are broken!")
  }
  if(!all(dataset$item %in% c("1", "2", "3", "4", "5", "screen"))){
    stop("Range constraints are broken!")
  }

  dataset <- dataset[dataset$item != "screen", ]
  dataset$response <- ifelse(dataset$response == "YES", 1, 0)
  ds <- aggregate(response ~ pin, dataset, sum, na.action = NULL)
  ds$ptsd_cat <- ifelse(ds$response >= thr_ptsd, 1, 0)
  ds$ptsd_cat <- ifelse(is.na(ds$ptsd_cat), 0, ds$ptsd_cat)
  colnames(ds) <- c("PIN", "ptsd_sum", "ptsd_cat")
  return(ds)
}