#' get_smoking function
#' @param dataset original dataset "smoking_status" and "FTND" from the bundle
#' @param completers boolean parameter, if True filters out participants that are not labeled as completers
#' @return dataframe with 4 columns:
#'         "PIN", "smoking_status", "smoking_cat", "ftnd_sum", "ftnd_cat"
#' @export
get_smoking <- function(dataset1,dataset2, subscales=F, completers=T){
  if(nrow(dataset1) == 0 | ncol(dataset1) == 0){
    stop("Empty dataset smoking status")
  }
  if(nrow(dataset2) == 0 | ncol(dataset2) == 0){
    stop("Empty dataset ftnd")
  }
  dataset1$PIN <- gsub("'", "", dataset1$PIN)
  essential_cols <- c("pin", "complete",  "item", "response")
  colnames(dataset1) <- tolower(colnames(dataset1))
  
  dataset2$PIN <- gsub("'", "", dataset2$PIN)
  essential_cols <- c("pin", "complete",  "item", "response")
  colnames(dataset2) <- tolower(colnames(dataset2))
  
  if(!all(essential_cols %in% colnames(dataset1))){
    stop(essential_cols[!essential_cols %in% colnames(dataset1)]," column(s) not found in the dataset smoking")
  }
  if(any(is.na(dataset1["pin"])) | any(dataset1["pin"] == "")){
    stop("Missed data in pin column of smoking dataset")
  }
  if(any(is.na(dataset1["item"])) | any(dataset1["pin"] == "")){
    stop("Missed data in item column of smoking dataset")
  }
  
  if(!all(essential_cols %in% colnames(dataset2))){
    stop(essential_cols[!essential_cols %in% colnames(dataset2)]," column(s) not found in the dataset ftnd")
  }
  if(any(is.na(dataset2["pin"])) | any(dataset2["pin"] == "")){
    stop("Missed data in pin column of ftnd dataset")
  }
  if(any(is.na(dataset2["item"])) | any(dataset2["pin"] == "")){
    stop("Missed data in item column of ftnd dataset")
  }
  
  if(any(!unique(dataset1$pin) %in% unique(dataset2$pin)) | any(!unique(dataset2$pin) %in% unique(dataset1$pin))){
    stop("The pins in dataset don't match")
  }
  
  if(completers){
    num_participants <- unique(dataset1[dataset1$complete == 'y', "pin"])
    dataset1 <- dataset1[dataset1$complete == "y", ]
    dataset2 <- dataset2[dataset2$complete == "y", ]
    if(nrow(dataset1) == 0){
      stop("There are no completers in your smoking dataset")
    }
    if(nrow(dataset2) == 0){
      stop("There are no completers in your ftnd dataset")
    }
  } else {
    num_participants <- unique(dataset1$pin)
  }
  
  if(any(is.na(dataset1$response))){
    warning("You have NAs in response columns of smoking dataset!")
  }

  
  # Smoking table
  if(!all(dataset1$response %in% c("Yes", "No", "Every day", "Some days", "Not at all"))){
    stop("Range constraints are broken!")
  }
  if(typeof(dataset1$item) == "factor"){
    dataset1$item <- as.numeric(as.character(dataset1$item))
  } else {
    dataset1$item <- as.numeric(dataset1$item)
  }
  if(typeof(dataset$response) == "factor"){
    warning("response is factor.conversion")
    dataset1$response <- as.character(dataset1$response)
  } else {
    dataset1$response <- as.character(dataset1$response)
  }
  dataset1 <- dataset1[dataset1$item == 2,]
  dataset1$status_ss <- ifelse(dataset1$response == 'Every day', 2,
                               ifelse(dataset1$response == 'Some days', 1, 0))
  df_sum <- dataset1[,c("pin", "status_ss")]
  df_sum$smoking_cat <- ifelse(df_sum$status_ss == 0 , 0, 1)
  
  
  if(typeof(dataset2$item) == "factor"){
    dataset2$item <- as.numeric(as.character(dataset2$item))
  } else {
    dataset2$item <- as.numeric(dataset2$item)
  }                       
  
  dataset2$ftnd_sum <- sapply(1:nrow(dataset2), function(x) if(is.na(dataset2$response[x])){return(NA)} 
                           else {return(unlist(ftnd_scale[[as.character(dataset2$item[x])]][[as.character(dataset2$response[x])]]))} )
  
  
  df_sum2 <- aggregate(ftnd_sum ~ pin, dataset2, sum, na.action=NULL)
  
  df <- merge(df_sum, df_sum2, by="pin")
  df$ftnd_cat <- ifelse(df$ftnd_sum >= thr_ftnd, 1,0)
  colnames(df) <- c("PIN", "smoking_status", "smoking_cat", "ftnd_sum", "ftnd_cat")
  return(df)
}