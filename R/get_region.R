

# Get WHO Regions using ISO
#' @name get_who.region
#' @title get_whoregion
#' @description Identify WHO region from ISO
#' @param ISO vector of ISO3 codes
#' @return vector of WHO regions 
#' @export
get_whoregion <- function(ISO){
  # Get WHO Regions using ISO
  data('region_data', package = 'globaltoolboxlite')
  return(as.character(region_data$who.region[match(toupper(ISO), region_data$ISO3)]))
}

 
# Get Regions using ISO
#' @name get_region
#' @title get_region
#' @description Identify region from ISO
#' @param ISO vector of ISO3 codes
#' @return vector of regions 
#' @export
get_region <- function(ISO){
  data('region_data', package = 'globaltoolboxlite')
  return(as.character(region_data$region[match(toupper(ISO), region_data$ISO3)]))
}


# Get Sub Regions using ISO
#' @name get_subregion
#' @title get_subregion
#' @description Identify subregion from ISO
#' @param ISO vector of ISO3 codes
#' @return vector of subregions 
#' @export
get_subregion <- function(ISO){
  data('region_data', package = 'globaltoolboxlite')
  return(as.character(region_data$sub.region[match(toupper(ISO), region_data$ISO3)]))
}

