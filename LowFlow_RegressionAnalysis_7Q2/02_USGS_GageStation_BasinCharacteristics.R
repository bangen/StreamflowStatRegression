#' @name 02_USGS_GageStation_BasinCharacteristics
#' 
#' @title Append USGS gage station basin characteristics to discharge data
#'
#' @author Sara Bangen
#'
#' @param q.data Path to Baseflow_7Q2.csv
#' @param gagesii.f Path of folder containing GagesII csvs
#' @param gages.shp Path to shapefile of USGS gage stations
#' @param huc8.hr.shp Path to HUC8 shapefile with attributed values including hydrologic region
#' @param out.f Output folder path


# Set user defined arguments:
q.data = "C:/.../LowFlow_RegressionAnalysis_7Q2/Baseflow_7Q2.csv"
gagesii.f = "C:/.../ExampleData/Inputs/basinchar_and_report_sept_2011/spreadsheets-in-csv-format"
gages.shp = "C:/.../ExampleData/Inputs/USGS_GagesII.shp"
huc8.hr.shp = "C:/.../ExampleData/Inputs/WBDHU8_Hydrologic_Regions.shp"
out.f = "C:/.../LowFlow_RegressionAnalysis_7Q2"

# ----------------------------------------


# Import required packages
library(sf)
library(tidyverse)
library(purrr)


get_basin_characteristics = function(q.data, gagesii.f, gages.shp, huc8.hr.shp, out.f){
  
  # read in discharge data
  q.data = read_csv(q.data)
  
  # read in gages data
  gages = st_read(gages.shp) %>% st_drop_geometry() %>%
    select(STAID, HUC8)
  
  # read in huc8 hydro regions data
  hydro.regions = st_read(huc8.hr.shp) %>% st_drop_geometry() %>%
    select(HUC8, HR_CODE, HR_NAME, NFDName, NFDCode, DWRName, DWRCode, ELEV_FT, SLOPE_PCT, PRECIP_IN, RELIEF_FT) %>%
    rename(elev.ft.8 = ELEV_FT, slope.pct.8 = SLOPE_PCT, precip.in.8 = PRECIP_IN, relief.ft.8 = RELIEF_FT)
  
  # read in gagesII csvs and subset to variables we want
  climate = read_csv(file.path(gagesii.f, "conterm_climate.csv")) %>%
    mutate(precip.in = round(PPTAVG_BASIN * 0.393701, 3)) %>%
    select(STAID, precip.in) 
  
  topo = read_csv(file.path(gagesii.f, "conterm_topo.csv")) %>%
    mutate(elev.ft = round(ELEV_MEAN_M_BASIN * 3.28084, 3), 
           elev.max = round(ELEV_MAX_M_BASIN * 3.28084, 3),
           elev.min = round(ELEV_MIN_M_BASIN * 3.28084, 3),
           relief.ft = elev.max - elev.min,
           slope.pct = round(SLOPE_PCT, 3)) %>%
    select(STAID, elev.ft, relief.ft, slope.pct)
  
  # join gagesII variables with stations df
  vars.df = list(climate, topo) %>%
    reduce(left_join, by = 'STAID') %>%
    mutate(STAID = as.factor(STAID))
  
  # join with discharge data and hydrologic region from gage data
  out.df = q.data %>% 
    mutate(STAID = as.factor(site_no)) %>%
    left_join(vars.df, by = "STAID") %>%
    left_join(gages, by = "STAID") %>%
    left_join(hydro.regions, by = "HUC8") %>%
    select(-STAID) %>%
    write_csv(file.path(out.f, "Baseflow_7Q2_Regression_Variables.csv"), col_names = TRUE)
  
}


get_basin_characteristics(q.data, gagesii.f, gages.shp, huc8.hr.shp, out.f)


