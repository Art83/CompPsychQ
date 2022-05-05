#' get_OCI function
#' @param dataset original dataset "OCI" from the bundle
#' @param completers boolean parameter, if True filters out participants that are not labeled as completers
#' @param subscales boolean parameter, if True includes to the returned dataframe OCI subscales 
#' @return either dataframe with 3 columns:
#'         PIN, response, oci_cat or dataframe with 9 columns: PIN, response, oci_cat, sym_wash, sym_obsess,sym_hoard,sym_order,sym_check,sym_ment
#' @export

get_oci <- function(dataset, subscales=F, completers=T){
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
  if(any(is.na(dataset["response"])) | any(dataset["pin"] == "")){
    stop("Missed data in response column")
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
  
  if(any(is.na(dataset$response))){
    warning("You have NAs in response columns!")
  }
  df_sum <- aggregate(response ~ pin, data=dataset, sum)
  df_sum$oci_cat <- ifelse(df_sum$response >= CompPsychQ::thr_OCI, 1, 0)
  
  
  if(subscales == F){
    return(df_sum)
  } else {
    subsc <- data.frame(matrix(ncol = length(names(CompPsychQ::contingency_oci))+1, nrow = length(num_participants)))
    colnames(subsc) <- c("pin", names(CompPsychQ::contingency_oci))
    subsc[,1] <- as.character(num_participants)
    for(i in names(CompPsychQ::contingency_oci)){
     subsc[,i] <- aggregate(response ~ pin, data=dataset[dataset$item %in% CompPsychQ::contingency_oci[[i]],], sum)[,2]
    }
    answer <- merge(df_sum, subsc, by="pin")
    return(answer)
  }
  
}











