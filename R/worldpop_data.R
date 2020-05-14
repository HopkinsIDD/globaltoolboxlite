





##' Function to get population data from a worldpop .tif and geounit shapefile
##' This function extracts raster values for overlayed polygons
##' 
##' @param wp_file WorldPop population file path for the country of interest
##' @param shp Shapefile file path for shapefile of geunits (i.e., admin2)
##'
##' @return 
##'
##' @import dplyr
##' @importFrom exactextractr exact_extract
##' @importFrom raster raster
##' @importFrom sf read_sf
##' @importFrom tibble as_tibble
##' 
##' @references WorldPop (https://www.worldpop.org/geodata)
##'
##' 
load_worldpop <- function(wp_file, shp) {
    
    #load data
    pop <- raster::raster(wp_file) #raster from world pop
    adm2 <- sf::read_sf(shp)  #district level shapefile
    
    #extract raster values by summing across 100m grids within shapefile polygons
    loc_values <- adm2 %>% 
        dplyr::mutate(sum = exactextractr::exact_extract(pop, adm2, 'sum')) %>%
        dplyr::select(ADM2_EN,sum) %>%
        tibble::as_tibble()
    
}


# # EXAMPLE
# # Download the files and save them
# 
# library(doParallel)
# library(foreach)
# library(tidyverse)
# 
# wp_file <- "raw_data/BGD/BGD_ppp_2015_adj_v2.tif"
# shp <- "raw_data/BGD/bgd_admbnda_adm2_bbs_20180410/bgd_admbnda_adm2_bbs_20180410.shp"
# 
# wp_pop_BGD <- covidSeverity:::load_worldpop(wp_file, shp)







##' Function to get population data from a worldpop geotiff and geounit shapefile
##' This function extracts raster values for overlayed polygons
##' 
##' @param shp Shapefile file path for shapefile of geunits (i.e., admin2)
##' @param country ISO3 of country of interest
##' @param year Year of population data (2000 to 2020)
##' @param save_dir directory where to save geotiff files
##' @param cores number of cores to parallelize over
##'
##' @return file names of the geotiffs downloaded and saved.
##'
##' @import dplyr doParallel foreach
##' @importFrom RCurl getURL
##' 
##' @references WorldPop (https://www.worldpop.org/geodata)
##'
##' @export
##' 
download_worldpop_agetifs <- function(country="BGD", year="2020", save_dir="raw_data", cores=4){
    
    dir.create(file.path(save_dir, country), recursive = TRUE, showWarnings = FALSE)
    
    url <- paste0("ftp://ftp.worldpop.org.uk/GIS/AgeSex_structures/Global_2000_2020/", year, "/", country, "/")
    filenames = RCurl::getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
    filenames <- unlist(strsplit(filenames, "\\r\\n"))
    
    doParallel::registerDoParallel(cores)
    foreach(f=seq_len(length(filenames))) %dopar% {
        url1 <- file.path(url, filenames[f])
        download.file(url1, destfile = file.path(save_dir, country, filenames[f]), mode="wb")
    }
    doParallel::stopImplicitCluster()
    
    print(paste0("Successfully downloaded age population files from Worldpop and save to ", save_dir,"/",country))
    
    return(filenames)
    
}






##' Function to get population data from a worldpop geotiff and geounit shapefile
##' This function extracts raster values for overlayed polygons
##' 
##' @param shp Shapefile file path for shapefile of geunits (i.e., admin2)
##' @param country ISO3 of country of interest
##' @param year Year of population data (2000 to 2020)
##' @param save_dir directory where to save geotiff files
##' @param cores number of cores to parallelize over
##'
##' @return long age population data by admin level 2
##'
##' @import dplyr tidyr doParallel foreach
##' @importFrom RCurl getURL
##' @importFrom exactextractr exact_extract
##' @importFrom raster raster
##' @importFrom sf read_sf
##' @importFrom tibble as_tibble
##' 
##' @references WorldPop (https://www.worldpop.org/geodata)
##'
##' @export
##' 
load_worldpop_age <- function(shp, country="BGD", year="2020", save_dir="raw_data", cores=4) {
    
    url <- paste0("ftp://ftp.worldpop.org.uk/GIS/AgeSex_structures/Global_2000_2020/", year, "/", country, "/")
    filenames = RCurl::getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
    filenames <- unlist(strsplit(filenames, "\\r\\n"))
    
    age_grps <- sort(unique(as.integer(data.frame(matrix(unlist(strsplit(filenames, "_")), ncol=4, byrow=TRUE), stringsAsFactors = FALSE)[,3])))
    age_grps_full <- paste(age_grps, c(age_grps[-1],100), sep="_")
    
    
    #load data
    adm2 <- sf::read_sf(shp)  #district level shapefile
    
    doParallel::registerDoParallel(cores)
    age_pop_data <- foreach(f=seq_len(length(filenames)), .combine=rbind,
                            .packages = c("dplyr", "tibble", 
                                          "exactextractr", "raster", "tidyr")) %dopar% {
                                              
                                              wp_file = file.path(save_dir, country, filenames[f])
                                              pop <- raster::raster(wp_file) #raster from world pop
                                              
                                              male_female <- unlist(strsplit(filenames[f], "_"))[2]
                                              age_grp_ <- as.integer(unlist(strsplit(filenames[f], "_")))[3]
                                              age_grps_full_ <- age_grps_full[which(age_grp_==age_grps)]
                                              
                                              #extract raster values by summing across 100m grids within shapefile polygons
                                              loc_values <- adm2 %>% 
                                                  dplyr::mutate(sum = exactextractr::exact_extract(pop, adm2, 'sum')) %>%
                                                  tibble::as_tibble() %>% 
                                                  dplyr::select(ADM2_EN, sum) %>%
                                                  dplyr::mutate(sex = male_female,
                                                                age = age_grps_full_)
                                              
                                              loc_values <- loc_values %>%
                                                  tidyr::separate(age, into=c("age_l","age_r"), sep="_", remove=FALSE) %>%
                                                  dplyr::mutate(age_r = as.integer(age_r) - 1)
                                              
                                              loc_values
                                              
                                          }
    doParallel::stopImplicitCluster()    
    
    
    age_pop_data <- age_pop_data %>% 
        tibble::as_tibble() %>%
        dplyr::rename(adm2 = ADM2_EN, pop = sum) %>% 
        tidyr::pivot_wider(names_from="sex", values_from = pop) %>%
        dplyr::rename(pop_m = m, pop_f = f) %>%
        dplyr::mutate(pop = pop_m + pop_f)
    
    return(age_pop_data)
    
}




##' Function to transform population data from a worldpop age groups to 10-year age groups
##' 
##' @param age_pop_data data pulled from worldpop using `load_worldpop_age`
##'
##' @return long age population data by admin level 2, in 10-year age groups
##'
##' @import dplyr
##' @importFrom tibble as_tibble
##' 
##' @references WorldPop (https://www.worldpop.org/geodata)
##'
##' @export
##' 
convert_wp_10yr <- function(age_pop_data){
    
    age_pop_10yr <- age_pop_data %>% 
        dplyr::mutate(age10 = floor(as.integer(age_l)/10)*10) %>%
        dplyr::group_by(age10, adm2) %>%
        dplyr::summarise(pop = sum(pop), pop_m = sum(pop_m), pop_f = sum(pop_f)) %>%
        dplyr::mutate(age_l = age10, age_r = age10+9) %>% 
        tibble::as_tibble() %>%
        dplyr::mutate(age = paste(age_l, age_r, sep="_")) %>%
        dplyr::select(-age10)
    
    return(age_pop_10yr)
    
}



