#' loads monthly and daily Fama-French portfolio data
#' 
#' @param file_name
#' @param path
#' @returns list with daily and monthly data
#' 

get_data <- function(
    path, 
    file_name
) {
  data <- rio::import(
    file.path(
      path, 
      file_name
    )
  ) %>% 
    suppressWarnings %>% 
    # get correct format for dates
    mutate(Date = as.Date(paste0(V1,"01"), format = "%Y%m%d")) %>% 
    dplyr::select(
      Date, everything(), 
      -V1
    ) %>% 
    dplyr::filter(Date >= from_date & Date <= to_date) %>% 
    dplyr::mutate(across(-Date,~as.numeric(.)))
  # remove columns with missing data
  sum_of_col <- as.data.frame(data == -99.99) %>% 
    colSums %>% 
    tibble(num = ., name = names(.)) %>% 
    dplyr::filter(num > 0)
  
  data_clean <- data %>% 
    dplyr::select(-sum_of_col$name)
  return(data_clean)
}
