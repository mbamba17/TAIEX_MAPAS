# Required packages installation
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
if (!requireNamespace("Factoshiny", quietly = TRUE)) install.packages("Factoshiny")

# Loading required packages
library(tidyverse)
library(lubridate)
library(Factoshiny)

# 1. Principal Component Analysis application example -  more info at: http://factominer.free.fr/graphs/factoshiny.html ####
# data
load("stres_test_podaci.Rda")
Data <- podaci %>% select(datum,dunemp,dbdp_r,wage,infl,unemp,infl_eu,pdebt,pdeficit,bdp_y,ciss_bond,dbdp_y,yld,yld_de,investicijski) %>% drop_na()
chart_data <- Data %>% gather("variable","value",-datum)
ggplot(chart_data,aes(x=datum,y = value,color=variable)) + geom_line(linewidth=1.4) + facet_wrap(~variable,scales = "free_y")
# application launch
PCAshiny(Data)

# Cleaning Workspace
rm(list = ls())

