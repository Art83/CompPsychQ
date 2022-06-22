#' get_lsas function
#' @param dataset original dataset "LSAS" from the bundle
#' @param completers boolean parameter, if True filters out participants that are not labeled as completers
#' @param subscales boolean parameter, if True includes to the returned dataframe moves subscales 
#' @return either dataframe with 4 columns:
#'         PIN, lsas_sum, lsas_cat, lsas_sev or dataframe with 8 columns: "PIN", "lsas_sum", "lsas_cat", "lsas_sev", "lsas_sym_sif" "lsas_sym_sia" "lsas_sym_pf"  "lsas_sym_pa"
#' @export
get_lsas <- function(dataset, subscales=F, completers=T){
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
  
  if(any(is.na(dataset$response))){
    warning("You have NAs in response columns!")
  }
  if(!all(as.numeric(dataset$response) %in% c(0:3))){
    stop("Range constraints are broken!")
  }
  
  if(!all(as.character(dataset$item) %in% c(paste0(rep("A",24),1:24), paste0(rep("F",24),1:24)) )){
    stop("Item constraints are broken!")
  }
  
  
  dataset$response <- as.numeric(dataset$response)
  
  df_sum <- aggregate(response ~ pin, data=dataset, sum, na.action = NULL)
  df_sum$lsas_cat <- ifelse(df_sum$response >= thr_lsas, 1, 0)
  df_sum$lsas_sev = ifelse(df_sum$response > 95, 4, 
                            ifelse(df_sum$response >= 81 & df_sum$response <= 95, 3, 
                                    ifelse(df_sum$response >= 66 & df_sum$response <= 80, 2, 
                                            ifelse(df_sum$response >= 51 & df_sum$response <= 65, 1, 0))))
  
  
  
  if(subscales == F){
    colnames(df_sum) <- c("PIN", "lsas_sum", "lsas_cat", "lsas_sev")
    return(df_sum)
  } else {
    subsc <- data.frame(matrix(ncol = length(names(contingency_lsas))+1, nrow = length(num_participants)))
    colnames(subsc) <- c("pin", names(contingency_lsas))
    subsc$pin <- as.character(subsc$pin)
    subsc[,1] <- as.character(num_participants)
    for(i in names(contingency_lsas)){
      agreg_t <- aggregate(response ~ pin, data=dataset[dataset$item %in% contingency_lsas[[i]],], sum, na.action=NULL)
      subsc[,i] <- unname(sapply(subsc$pin, function(x) agreg_t[agreg_t$pin == x, "response"]))
    }
    answer <- merge(df_sum, subsc, by="pin")
    colnames(answer) <- c("PIN", "lsas_sum", "lsas_cat", "lsas_sev", "lsas_sym_sif", "lsas_sym_sia", "lsas_sym_pf", "lsas_sym_pa")
    return(answer)
  }
  
}