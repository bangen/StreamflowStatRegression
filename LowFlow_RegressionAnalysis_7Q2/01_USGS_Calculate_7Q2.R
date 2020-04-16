#' @name 01_USGS_Calculate_7Q2
#' 
#' @title Calculate 7Q2 Low Flow Statistic for USGS Stream Gages
#'
#' @author Sara Bangen
#'
#' @param gages.shp Path to shapefile of USGS gage stations
#' @param out.f Output folder path


# Set user defined Arguments:
gages.shp = "C:/.../ExampleData/Inputs/USGS_GagesII.shp"
out.f = "C:/.../LowFlow_RegressionAnalysis_7Q2"


# ----------------------------------------


# Import required packages
library(EGRET)
library(dataRetrieval)
library(sf)
library(tidyverse)
library(stringr)
library(purrr)


#' Calculate 7Q2 Low Flow 
#'
#' @note Called by run_7q2 
#'
#' @param staid USGS gage station ID
#'
#' @return Dataframe with following columns:
#' \itemize{
#' \item site_no: gage station ID 
#' \item da.sq.mi: upstream drainage area in square miles
#' \item q.cfs: 7Q2 in cubic feet per second
#' \item record.start: first year of record 
#' \item record.end: last year of record
#' \item record.length: number of years in record
#' \item record.gap: maximum year gap in record
#' }
#'           
calc_7q2 = function(staid){
  
  # get gage station annual 7 day min discharge in cfs for each year post 1950
  siteNumber = as.character(staid)
  daily.q = readNWISDaily(siteNumber, "00060", startDate = "", endDate = "")
  station.info = readNWISInfo(siteNumber, "", interactive = FALSE)
  eList = as.egret(station.info, daily.q, NA, NA)
  eList = setPA(eList)
  annual.max = printSeries(eList, istat = 2) %>% select(-qSmooth) %>% rename(q.cfs = qActual) %>%
    filter(years >= 1950)
  
  # calculate max record gap
  n.gaps = annual.max %>%
    mutate(year.diff = years - lag(years)) %>%
    summarize(record.gap = max(year.diff, na.rm = TRUE))
  
  # calculate 7q2
  # which.min grabs q closest to ri of 2
  n = nrow(annual.max)
  q2 = annual.max %>%
    mutate(q.rank = rank(desc(q.cfs), ties.method = "first"),
           q.prob = (q.rank / (n + 1)) * 100,
           ri = (n + 1) / q.rank) %>%
    slice(which.min(abs(ri - 2.0))) %>% 
    select(q.cfs)
  
  # output dataframe
  q2.info = station.info %>% select(site_no, drainSqKm) %>%
    mutate(q.cfs = q2$q.cfs,
           da.sq.mi = drainSqKm * 0.386102,
           record.start = min(annual.max$years),
           record.end = max(annual.max$years),
           record.length = record.end - record.start,
           record.gap = n.gaps$record.gap) %>%
    select(-drainSqKm)
  
  return(q2.info)
}


#' Run 7Q2 
#'
#' @param gages.shp Path to shapefile of USGS gage stations
#' @param out.f Output folder path
#'
#' @export "Baseflow_7Q2.csv" written to output folder. See calc.7q2 for column descriptions.
#'
run_7q2 = function(gages.shp, out.f){
  
  # read in station data shapefile as dataframe
  stations = st_read(gages.shp) %>% st_drop_geometry()
  
  # run calc.7q2 function for each station
  q2.results = map_dfr(stations$STAID, calc_7q2) %>%
    bind_rows()
  
  # remove gages without record
  q2.results = q2.results %>%
    filter(record.length > 0) %>%
    write_csv(file.path(out.f, "Baseflow_7Q2.csv"), col_names = TRUE)
  
}


run_7q2(gages.shp, out.f)

                 



