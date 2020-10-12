





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
##' @param country ISO3 of country of interest
##' @param year Year of population data (2000 to 2020)
##'
##' @return vector of filenames of the WorldPop age geotiffs
##'
##' @import dplyr doParallel foreach
##' @importFrom RCurl getURL
##' 
##' @references WorldPop (https://www.worldpop.org/geodata)
##'
##' @export
##' 
wp_geotiff_filenames <- function(country="BGD", year="2020") {
        
        url <- paste0("ftp://ftp.worldpop.org.uk/GIS/AgeSex_structures/Global_2000_2020/", year, "/", country, "/")
        filenames = RCurl::getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
        filenames <- unlist(strsplit(filenames, "\\n"))
        
        filenames <- gsub("\\r", "", filenames) # for some users there will also be "\r"
        
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
    
    country <- toupper(country)
    dir.create(file.path(save_dir, country), recursive = TRUE, showWarnings = FALSE)
    
    url <- paste0("ftp://ftp.worldpop.org.uk/GIS/AgeSex_structures/Global_2000_2020/", year, "/", country, "/")
    filenames <- wp_geotiff_filenames(country, year)
        
    
    doParallel::registerDoParallel(cores)
    foreach(f=seq_len(length(filenames))) %dopar% {
        url1 <- file.path(url, filenames[f])
        download.file(url1, destfile = file.path(save_dir, country, filenames[f]), mode="wb")
    }
    doParallel::stopImplicitCluster()
    
    print(paste0("Successfully downloaded age population files from Worldpop and save to ", save_dir,"/",country))
    
    return(filenames)
    
}


## to be finished
download_worldpop_globalmosaics <- function(country="BGD", year="2020", save_dir="raw_data", cores=4){
    
    country <- toupper(country)
    dir.create(file.path(save_dir, country), recursive = TRUE, showWarnings = FALSE)
    
    url <- paste0("ftp://ftp.worldpop.org.uk/GIS/AgeSex_structures/Global_2000_2020/", year, "/", country, "/")
    filenames <- wp_geotiff_filenames(country, year)
    
    
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
##' @param shp Shapefile file path for shapefile of geounits (i.e., admin2)
##' @param country ISO3 of country of interest
##' @param year Year of population data (2000 to 2020)
##' @param save_dir directory where to save geotiff files
##' @param cores number of cores to parallelize over
##' @param loc_var name of location name or id variable, if want to reduce data to this and population data
##' @param add_pop_to_shapefile logical, whether to add total population to the shapefile
##' @param shp_country_var Variable in the shapefile denoting the country ISO3 code. For GADM shapefiles, this is GID_0. If NULL, no subsetting done
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
load_worldpop_age <- function(shp, country="BGD", year="2020", save_dir="raw_data", cores=4, 
                              loc_var="NAME_2",
                              shp_vars = c("GID_0", "NAME_0", "GID_1", "NAME_1", "GID_2", "NAME_2"),
                              add_pop_to_shapefile = TRUE, shp_country_var="GID_0") {
    
    country <- toupper(country)
    filenames <- wp_geotiff_filenames(country, year)
    
    age_grps <- sort(unique(as.integer(data.frame(matrix(unlist(strsplit(filenames, "_")), ncol=4, byrow=TRUE), stringsAsFactors = FALSE)[,3])))
    age_grps_full <- paste(age_grps, c(age_grps[-1],100), sep="_")
    
    
    #load data (if character string, load it, otherwise just rename shapefile)
    if (is.character(shp)){
        adm2 <- sf::read_sf(shp) 
    } else {
        adm2 <- shp
    }
    
    if (!is.null(shp_country_var)){
        adm2 <- adm2 %>%   #admin-2 / district level shapefile
            dplyr::filter(!!as.name(shp_country_var)==country)
    }
    
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
                                              tmp_sum <- exactextractr::exact_extract(pop, adm2, 'sum')
                                              
                                              if ("pop" %in% colnames(adm2)){
                                                  adm2 <- adm2 %>%
                                                      dplyr::select(-pop)
                                              }
                                              
                                              loc_values <- adm2 %>% 
                                                  dplyr::mutate(sum = tidyselect::all_of(tmp_sum)) %>%
                                                  tibble::as_tibble() %>% 
                                                  dplyr::mutate(sex = male_female,
                                                                age = age_grps_full_) %>%
                                                  dplyr::rename(pop = sum) %>% 
                                                  dplyr::select(-geometry)
                                              
                                              if (!is.na(shp_vars[1])) {
                                                  loc_values <- loc_values %>% dplyr::select(tidyselect::all_of(shp_vars), sex, age, pop)
                                              }
                                              
                                              loc_values <- loc_values %>%
                                                  tidyr::separate(age, into=c("age_l","age_r"), sep="_", remove=FALSE) %>%
                                                  dplyr::mutate(age_r = as.integer(age_r) - 1)
                                              
                                              loc_values
                                              
                                          }
    doParallel::stopImplicitCluster()    
    
    age_pop_data <- age_pop_data %>% 
        tibble::as_tibble() %>%
        dplyr::mutate(loc = get(loc_var)) %>%
        dplyr::group_by(sex) %>%
        dplyr::mutate(row = row_number()) %>%
        tidyr::pivot_wider(names_from="sex", values_from = pop) %>%
        dplyr::rename(pop_m = m, pop_f = f) %>%
        dplyr::mutate(pop_m = as.integer(pop_m), pop_f = as.integer(pop_f)) %>%
        dplyr::mutate(pop = pop_m + pop_f) %>%
        dplyr::select(-row)
    
    age_pop_tot <- age_pop_data %>%
        dplyr::group_by(dplyr::across(tidyselect::all_of(unique(shp_vars[shp_vars!=loc_var]))),loc) %>%
        dplyr::summarise(pop = sum(pop)) %>%
        tibble::as_tibble() %>%
        dplyr::rename(setNames("loc", loc_var))
    
    # Add data to shapefile if desired
    if (add_pop_to_shapefile){
        
        if (!is.null(shp_country_var)){
            print("Not adding population to shapefile due to subsetting of shapefile")
        } else {
            
            adm2 <- adm2 %>% left_join(age_pop_tot)
            
            # Save it back in the same name
            sf::st_write(adm2, shp, delete_layer=TRUE)
        }
    }
    
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
        dplyr::mutate(age_r = ifelse(age_l==80, 100, age_r)) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(age = paste(age_l, age_r, sep="_")) %>%
        dplyr::select(-age10)
    
    return(age_pop_10yr)
    
}



##' Function to transform population data from a worldpop age groups to user-defined age groups
##' 
##' @param age_pop_data data pulled from worldpop using `load_worldpop_age`
##' @param age_groups lower age limit of user-defined age groups. For 5 year age groups, this would be 0, 5, 10, etc.
##' @param max_age max age to include (default is 100)
##'
##' @return long age population data by admin level 2, in user-defined age groups
##'
##' @import dplyr
##' @importFrom tibble as_tibble
##' 
##' @references WorldPop (https://www.worldpop.org/geodata)
##'
##' @export
##' 
convert_wp_agegroups <- function(age_pop_data, age_groups = seq(0, 80, 5), max_age=110){
    
    age_groups_l <- age_groups # left limit
    age_groups_r <- c(age_groups[-1]-1, max_age) #right limit (not inclusive)
    age_groups <- paste0(age_groups_l, "_", age_groups_r)
    
    # Old function
    # smooth_fun <- function(x, y, age_groups_r) {
    #     x <- c(x, max(x)+5)
    #     y <- c(y, max(y))
    #     diff(c(0, as.integer(unlist(predict(smooth.spline(x, y, all.knots = TRUE), data.frame(x=age_groups_r))$y))))
    # }
    
    # New function(monotonic)
    smooth_fun <- function(x, y, age_groups_r) {
        x <- c(x, max(x)+5)
        y <- c(y, max(y))
        splinefit_ <- splinefun(x, y, method="hyman")
        diff(c(0, as.integer(splinefit_(age_groups_r))))
    }
    
    
    if("loc" %in% colnames(age_pop_data)){
        age_pop_data <- age_pop_data %>% dplyr::select(-loc)
    }
    
    age_pop_data <- age_pop_data %>% 
        dplyr::mutate(age_l = as.numeric(age_l),
                      age_r = as.numeric(age_r) + 1, # add 1 for the spline fitting
                      pop = as.numeric(pop),
                      pop_f = as.numeric(pop_f),
                      pop_m = as.numeric(pop_m)) %>%
        dplyr::arrange(adm2, age_l) %>%
        group_by(adm2) %>%
        dplyr::mutate(pop_cum = cumsum(pop),
                      pop_f_cum = cumsum(pop_f),
                      pop_m_cum = cumsum(pop_m)) %>%
        dplyr::ungroup()
    
    # Adjust max age (change max age of data to match input. so we dont overestimate total population)
    max_age_data <- max(age_pop_data$age_r)
    age_pop_data <- age_pop_data %>% 
        dplyr::mutate(age_r = ifelse(age_r==max_age_data, max_age, age_r))
    
    age_pop_fit <- age_pop_data %>% 
        tidyr::nest(data = c(age, age_l, age_r, pop_f, pop_m, pop, pop_cum, pop_f_cum, pop_m_cum)) %>%
        dplyr::mutate(age_groups = list(age_groups),
                      age_l = list(age_groups_l),
                      age_r = list(age_groups_r),
                      pop = purrr::map(data, ~smooth_fun(x=.$age_r, y=.$pop_cum, age_groups_r)),
                      pop_f = purrr::map(data, ~smooth_fun(x=.$age_r, y=.$pop_f_cum, age_groups_r))) %>% 
        tidyr::unnest(c(age_groups, age_l, age_r, pop, pop_f)) %>%
        tibble::as_tibble() %>%
        dplyr::select(-data) %>%
        dplyr::mutate(pop_m = pop - pop_f)
    
    return(age_pop_fit)
}




