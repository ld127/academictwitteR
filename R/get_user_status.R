#' Get user status
#' 
#' This function fetches user-level information about the status of the Twitter user account (suspended, deleted, privated) for a vector of user IDs.
#' It is written to resemble that of "get_user_profile.R" closely.
#'
#' @param x string containing one user id or a vector of user ids
#' @param bearer_token string, bearer token
#'
#' @return a data frame
#' @export
#'
#' @examples
#' \dontrun{
#' bearer_token <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
#' users <- c("2244994945", "6253282")
#' get_user_status(users, bearer_token)
#' }

get_user_status <- function(x, bearer_token = get_bearer()){
  bearer_token <- check_bearer(bearer_token)
  #endpoint
  url <- "https://api.twitter.com/2/users"
  
  new_df <- data.frame()
  # dividing into slices of 100 each
  slices <- seq(1, length(x), 100) 
  for(i in slices){
    if(length(x) < (i+99)){
      end <- length(x)
    } else {
      end <- (i+99)
    }
    cat(paste0("Processing from ",i," to ", end,"\n"))
    slice <- x[i:end]
    #parameters
    params <- list(
      "ids" = paste(slice, collapse = ","),
      "user.fields" = "verified,protected,withheld"
    )
    dat <- make_query(url = url, params = params, bearer_token = bearer_token, verbose = TRUE)
    
    container <- data.frame(author_id = slice)
    
    # users without errors are not suspended or undetectable (deleted), retrieve only verified and protected status
    if (length(dat$data) > 0) {
      container <- dplyr::left_join(container, dat$data %>% dplyr::select("id", "verified", "protected"), by = c("author_id" = "id"))
    }
    
    # users with errors have a special status, retrieve the status (forbidden = suspended, not found = no user with the ID or deleted user if ID has been recorded before)
    if (length(dat$errors) > 0) {
      dat$errors <- dat$errors %>% dplyr::mutate(suspended = ifelse(title == "Forbidden", T, F),
                                                 undetectable = ifelse(title == "Not Found Error", T, F))
      container <- dplyr::left_join(container, dat$errors %>% dplyr::select("resource_id", "suspended", "undetectable"), by = c("author_id" = "resource_id"))
    }
    
    # if there are observations in the df, change all from boolean to integer for better overview
    if (ncol(container > 1)) {
    container <- container %>% dplyr::mutate_at(c("verified", "protected", "suspended", "undetectable"), as.integer)
    
    # set all NA to 0 because status are mutually exclusive
    container[is.na(container)] <- 0
    
    new_df <- dplyr::bind_rows(new_df, container) # add new rows
    }
    
  }
  return(new_df)
}

