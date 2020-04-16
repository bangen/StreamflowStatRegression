#' @name 03_USGS_Q2Regression
#' 
#' @author Sara Bangen
#'
#' @param in.data Path to Q2_Regression_Variables.csv
#' @param out.f Output folder path


# Set user defined arguments:
in.data = "C:/.../HighFlow_RegressionAnalysis/Q2_Regression_Variables.csv"
out.f = "C:/.../HighFlow_RegressionAnalysis"


# ----------------------------------------


# Import required packages
library(GGally)
library(tidyverse)
library(broom)

# Read in input data csv
df = read_csv(in.data)

# Filter to gages with record year of at least 20 years and with record gaps less than 3 years
vars = df %>% 
  filter(record.length >= 20, record.gap < 3)

# Get quick summary of count of gages by hydro region
dwr.count = vars %>%
  group_by(DWRName, DWRCode) %>%
  summarize(no.gages = n()) %>%
  ungroup()

nfd.count = vars %>%
  group_by(NFDName, NFDCode) %>%
  summarize(no.gages = n()) %>%
  ungroup()

# Log tranform numerical vars
vars.log10 = vars 
cols = c("q.cfs", "da.sq.mi", "elev.ft", "relief.ft", "slope.pct", "precip.in", "elev.ft.8", "relief.ft.8", "slope.pct.8", "precip.in.8")
vars.log10[cols] = log10(vars.log10[cols])

# Run regression models

# Nest dataframes
by.dwr = vars.log10 %>% 
  group_by(DWRName, DWRCode) %>%
  nest()

by.nfd = vars.log10 %>% 
  group_by(NFDName, NFDCode) %>%
  nest()


# Creat dataframe of regression models to run
create.model.df = function(x){  
    tribble(
      ~model.name,    ~f,     ~params,

      "da.sq.mi + precip.in","lm", list(formula = as.formula("q.cfs ~ da.sq.mi + precip.in"),data = x),
      "da.sq.mi + elev.ft + precip.in","lm", list(formula = as.formula("q.cfs ~ da.sq.mi + elev.ft + precip.in"),data = x),
      "da.sq.mi + relief.ft + precip.in","lm", list(formula = as.formula("q.cfs ~ da.sq.mi + relief.ft + precip.in"),data = x),
      "da.sq.mi + slope.pct + precip.in","lm", list(formula = as.formula("q.cfs ~ da.sq.mi + slope.pct + precip.in"),data = x),
      "da.sq.mi + elev.ft + slope.pct + precip.in","lm", list(formula = as.formula("q.cfs ~ da.sq.mi + elev.ft + slope.pct + precip.in"),data = x),
      "da.sq.mi + relief.ft + slope.pct + precip.in","lm", list(formula = as.formula("q.cfs ~ da.sq.mi + relief.ft + slope.pct + precip.in"),data = x),
      
      "da.sq.mi + precip.in.8","lm", list(formula = as.formula("q.cfs ~ da.sq.mi + precip.in.8"),data = x),
      "da.sq.mi + elev.ft.8 + precip.in.8","lm", list(formula = as.formula("q.cfs ~ da.sq.mi + elev.ft.8 + precip.in.8"),data = x),
      "da.sq.mi + relief.ft.8 + precip.in.8","lm", list(formula = as.formula("q.cfs ~ da.sq.mi + relief.ft.8 + precip.in.8"),data = x),
      "da.sq.mi + slope.pct.8 + precip.in.8","lm", list(formula = as.formula("q.cfs ~ da.sq.mi + slope.pct.8 + precip.in.8"),data = x),
      "da.sq.mi + elev.ft.8 + slope.pct.8 + precip.in.8","lm", list(formula = as.formula("q.cfs ~ da.sq.mi + elev.ft.8 + slope.pct.8 + precip.in.8"),data = x),
      "da.sq.mi + relief.ft.8 + slope.pct.8 + precip.in.8","lm", list(formula = as.formula("q.cfs ~ da.sq.mi + relief.ft.8 + slope.pct.8 + precip.in.8"),data = x)

    )     
  }

# Function to apply regression models on each row in dataframe
apply.models = function(x){        
  x %>% 
    mutate(model.fit = invoke_map(f, params)) 
}


# Function to run regressions and create output csvs
# df: input data frame
# csv.prefix: previx for each of the model output csvs
run.regression = function(df, csv.prefix, group.a, group.b){
  
  # run regression models
  model.nested = df %>% 
    mutate(model.df = data %>% 
             map(create.model.df) %>%
             map(apply.models)) 
  
  # get coefficients and summaries
  model.coeff = model.nested %>% unnest(model.df) %>%
    mutate(model.tidy = map(model.fit, tidy)) %>%
    unnest(model.tidy, .drop = TRUE) %>%
    select(-data, -f, -params, -model.fit)
  
  model.coeff.sign = model.coeff %>%
    group_by(!!sym(group.a), !!sym(group.b), model.name) %>%
    filter(term != '(Intercept)') %>%
    summarise(all.coeff.sign = ifelse(max(p.value) <= 0.05, 1, 0))
  
  model.summary = model.nested %>% unnest(model.df) %>%
    mutate(model.glance = map(model.fit, glance)) %>%
    unnest(model.glance, .drop = TRUE) %>%
    select(-data, -f, -params, -model.fit) %>%
    left_join(model.coeff.sign, by = c(group.a, group.b, 'model.name'))
  
  model.summary.sign = model.summary %>%
    group_by(!!sym(group.a), !!sym(group.b)) %>%
    filter(p.value <= 0.05 & all.coeff.sign == 1) %>%
    mutate(best.adj.r.squared = ifelse(adj.r.squared == max(adj.r.squared), 1, 0),
           best.sigma = ifelse(sigma == min(sigma), 1, 0),
           best.AIC = ifelse(AIC == min(AIC), 1, 0))
  
  # model.resid = model.nested %>% unnest(model.df) %>%
  #   mutate(model.aug = map(model.fit, augment)) %>%
  #   unnest(model.aug)
  
  write_excel_csv(model.coeff, file.path(out.f, paste(csv.prefix, 'ModelCoefficients.csv', sep = '_')), append = FALSE, col_names = TRUE)
  write_excel_csv(model.summary, file.path(out.f, paste(csv.prefix, 'ModelSummary.csv', sep = '_')), append = FALSE, col_names = TRUE)
  write_excel_csv(model.summary.sign, file.path(out.f, paste(csv.prefix, 'ModelSummary_Significant.csv', sep = '_')), append = FALSE, col_names = TRUE)
  # write_excel_csv(model.resid, file.path(out.f, paste(csv.prefix, ModelResids.csv', sep = '_')), append = FALSE, col_names = TRUE)
  
  return(model.summary.sign)
}


nfd.region = run.regression(by.nfd, 'Q2_NFDRegions', 'NFDName', 'NFDCode')
dwr.region = run.regression(by.dwr, 'Q2_DWRRegions', 'DWRName', 'DWRCode')
