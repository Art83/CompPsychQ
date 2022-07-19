#' get_smoking function
#' @param dataset original dataset "smoking_status" and "FTND" from the bundle
#' @param completers boolean parameter, if True filters out participants that are not labeled as completers
#' @return dataframe with 4 columns:
#'         "PIN", "smoking_status", "smoking_cat", "ftnd_sum", "ftnd_cat"
#' @export
get_smoking <- function(smoking,ftnd, subscales=F, completers=T){
  if(nrow(smoking) == 0 | ncol(smoking) == 0){
    stop("Empty dataset smoking status")
  }
  if(nrow(ftnd) == 0 | ncol(ftnd) == 0){
    stop("Empty dataset ftnd")
  }
  smoking$PIN <- gsub("'", "", smoking$PIN)
  essential_cols <- c("pin", "complete",  "item", "response")
  colnames(smoking) <- tolower(colnames(smoking))
  
  ftnd$PIN <- gsub("'", "", ftnd$PIN)
  essential_cols <- c("pin", "complete",  "item", "response")
  colnames(ftnd) <- tolower(colnames(ftnd))
  
  if(!all(essential_cols %in% colnames(smoking))){
    stop(essential_cols[!essential_cols %in% colnames(smoking)]," column(s) not found in the dataset smoking")
  }
  if(any(is.na(smoking["pin"])) | any(smoking["pin"] == "")){
    stop("Missed data in pin column of smoking dataset")
  }
  if(any(is.na(smoking["item"])) | any(smoking["pin"] == "")){
    stop("Missed data in item column of smoking dataset")
  }
  
  if(!all(essential_cols %in% colnames(ftnd))){
    stop(essential_cols[!essential_cols %in% colnames(ftnd)]," column(s) not found in the dataset ftnd")
  }
  if(any(is.na(ftnd["pin"])) | any(ftnd["pin"] == "")){
    stop("Missed data in pin column of ftnd dataset")
  }
  if(any(is.na(ftnd["item"])) | any(ftnd["pin"] == "")){
    stop("Missed data in item column of ftnd dataset")
  }
  
  if(any(!unique(smoking$pin) %in% unique(ftnd$pin)) | any(!unique(ftnd$pin) %in% unique(smoking$pin))){
    stop("The pins in dataset don't match")
  }
  
  if(completers){
    num_participants <- unique(smoking[smoking$complete == 'y', "pin"])
    smoking <- smoking[smoking$complete == "y", ]
    ftnd <- ftnd[ftnd$complete == "y", ]
    if(nrow(smoking) == 0){
      stop("There are no completers in your smoking dataset")
    }
    if(nrow(ftnd) == 0){
      stop("There are no completers in your ftnd dataset")
    }
  } else {
    num_participants <- unique(smoking$pin)
  }
  
  if(any(is.na(smoking$response))){
    warning("You have NAs in response columns of smoking dataset!")
  }

  
  # Smoking table
  if(!all(smoking$response %in% c("Yes", "No", "Every day", "Some days", "Not at all"))){
    stop("Range constraints are broken!")
  }
  if(typeof(smoking$item) == "factor"){
    smoking$item <- as.numeric(as.character(smoking$item))
  } else {
    smoking$item <- as.numeric(smoking$item)
  }
  if(typeof(smoking$response) == "factor"){
    warning("response is factor.conversion")
    smoking$response <- as.character(smoking$response)
  } else {
    smoking$response <- as.character(smoking$response)
  }
  smoking <- smoking[smoking$item == 2,]
  smoking$status_ss <- ifelse(smoking$response == 'Every day', 2,
                               ifelse(smoking$response == 'Some days', 1, 0))
  df_sum <- smoking[,c("pin", "status_ss")]
  df_sum$smoking_cat <- ifelse(df_sum$status_ss == 0 , 0, 1)
  
  
  if(typeof(ftnd$item) == "factor"){
    ftnd$item <- as.numeric(as.character(ftnd$item))
  } else {
    ftnd$item <- as.numeric(ftnd$item)
  }                       
  
  ftnd$ftnd_sum <- sapply(1:nrow(ftnd), function(x) if(is.na(ftnd$response[x])){return(NA)} 
                           else {return(unlist(ftnd_scale[[as.character(ftnd$item[x])]][[as.character(ftnd$response[x])]]))} )
  
  
  df_sum2 <- aggregate(ftnd_sum ~ pin, ftnd, sum, na.action=NULL)
  
  df <- merge(df_sum, df_sum2, by="pin")
  df$ftnd_cat <- ifelse(df$ftnd_sum >= thr_ftnd, 1,0)
  colnames(df) <- c("PIN", "smoking_status", "smoking_cat", "ftnd_sum", "ftnd_cat")
  return(df)
}