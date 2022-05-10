rm(list=ls())

library(tidyverse)

rexp <- "^(\\S+) (\\S)/6 (\\S+)\n(\\S+) (\\S+)\n(.*)$" # we use a regular expression to split the copied results
#                score    time   length solution results     
#         \\1     \\2     \\3     \\4     \\5     \\6
#                 Y_s     Y_t      Z  

dat <- read_csv("rawdata.csv") %>% # emails have been converted into IDs to anonymize data
  rename(result=`Paste your results here.`, block=ID) 


dat <- dat %>% mutate(Y_s=sub(rexp,"\\2",dat$result), 
         Y_t=as.double(sub(rexp,"\\3",dat$result)), 
         Z=as.numeric(sub(rexp,"\\4",dat$result)),
         l4=if_else(Z == 4, 1, 0),
         l5=if_else(Z == 5, 1, 0),
         l6=if_else(Z == 6, 1, 0),
         l7=if_else(Z == 7, 1, 0)) %>%
  subset(select = c(block, Y_s, Y_t, Z, l4, l5, l6, l7))

saveRDS(dat, "clean.rds")
  
  

