## Causal Inference in R
## author: BragatteMAS
## ref:: Matt Dancho


## A/B testing

pacman::p_load(tidyverse, # data manipulation
               timetk, # time series
               plotly, # interactive plots
               infer, # statistical inference
               CasualImpact, # causal inference
               
               )

hotel_booking_raw_tbl <-
tibble::tibble(hotel_booking)
