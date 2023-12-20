library(tidyverse)
library(readr)

#setwd("D:/shared/experiments/eyecontrast_project/eyecontrast_analyses/scripts")

path <- getwd()


# this function adds subject name to a new column in a given dataset (input must be a filename)
addsubname <- function(file){
  df <- tibble(read_delim(file))
  filename <- tail(str_split(file,"/")[[1]],1)
  subname <- str_split(filename,"_")[[1]][1]
  df <- df %>% mutate(subject=subname)
  return(df)
}

# this function "wrangles" the data into a simpler format and puts it in a new dataframe
wrangledf <- function(df){
  if("ori" %in% colnames(df)){
    df2 <- df %>% select(cond, ori, rt, acc, contrast, trialno, subject, staircase) %>%
      mutate(context = case_when(cond=="ss" | cond == "sd" ~ "same",
                                 cond=="ds" | cond == "dd" ~ "diff",
                                 cond=="isos" | cond == "isod" ~ "iso"),
             congruency = case_when(cond=="ss" | cond == "dd" ~ "cong",
                                    cond=="sd" | cond == "ds" ~ "inc",
                                    cond=="isos" | cond == "isod" ~ "iso"),
             condition = case_when(cond=="ss" | cond == "dd" ~ "plus",
                                   cond=="sd" | cond == "ds" ~ "minus",
                                   cond=="isos" | cond == "isod" ~ "iso"),
             contrast = as.numeric(contrast),
             acc = as.numeric(acc),
             task = ori)
  } else {
    df2 <- df %>% select(cond, rt, acc, contrast, TrialNumber, participant, staircase) %>%
      mutate(contrast=as.numeric(contrast), 
             acc=as.numeric(acc),
             task="low",
             condition=case_when(cond==1 ~ "minus",
                               cond==2 ~ "plus",
                               cond==3 ~ "iso"),
             congruency=case_when(cond==1 ~ "cong",
                                  cond==2 ~ "inc",
                                  cond==3 ~ "iso")) %>%
      select(-cond) %>% 
      rename(subject=participant)
  }
 return(df2)
}

# this function gets the file names of csvs in a folder
getfilenames <- function(foldername){
  path <- getwd()
  #path <- dirname(path)
  path2 <- paste(path,"/",foldername,"/", sep="")
  datafiles=list.files(path = path2, pattern = "csv", all.files = TRUE,
                       full.names = FALSE, recursive = TRUE,
                       ignore.case = FALSE, include.dirs = FALSE, no.. = TRUE)
  datafiles <- paste(path2,datafiles,sep="")
  return(datafiles)
}

# this function puts every csv in a folder (names taken from getfilenames function) into a single data frame
singledf <- function(filenames, singledfname){
  singledfname <- c()
  for(i in 1:length(filenames)){
    tempdf <- addsubname(filenames[i])
    tempdf2 <- wrangledf(tempdf)
    singledfname <- rbind(singledfname, tempdf2)
  }
  return(singledfname)
}

# this function formats both datasets into a same arrangement and combines them
combinelowhigh <- function(lowdf, highdf){
  lo1 <- lowdf %>% select(subject, contrast, condition, acc, task)
  hi1 <- highdf %>% select(subject, contrast,  condition, acc, task)
  lohi_ec <- rbind(lo1,hi1)
  return(lohi_ec)
}
#gratings

lowfiles <- getfilenames("data_low")

lowdf <- c()

lowdf <- singledf(lowfiles, lowdf)

# eyes

highfiles <- getfilenames("data_high")

highdf <- c()

highdf <- singledf(highfiles, highdf)

#combining both data

lohi_ec <- combinelowhigh(lowdf, highdf)

# some filters
lohi_ec_full_set <- lohi_ec %>% group_by(subject) %>% tally() %>% filter(n>768)

lohi_ec_target_names <- unique(lohi_ec_full_set$subject) %>% paste(collapse="|")

lohi_ec_pilot <- lohi_ec %>% filter(str_detect(subject,lohi_ec_target_names))

#saving the data to an rda file to run in R

save(lohi_ec_pilot, file="ec_1sub.rda")

highdf %>% ggplot(aes(x=rt))+
  facet_wrap(~condition)+
  geom_density()
lowdf %>% ggplot(aes(x=rt))+
  facet_wrap(~condition)+
  geom_density()

lohi_ec_pilot %>% ggplot(aes(x=contrast)) +
  geom_histogram()

summdata <- lohi_ec_pilot %>% group_by(task) %>% 
  mutate(contrast=log10(contrast)) %>%
  group_by(condition, task) %>% summarise(normcontrast=mean(contrast),sd=sd(contrast), se=sd/sqrt(n_distinct(subject)))


summdata %>% ggplot(aes(y=normcontrast, x=condition, color=condition))+
  geom_boxplot()+
  geom_errorbar(aes(ymin=normcontrast-se, ymax=normcontrast+se))+
  facet_wrap(~task)+
  theme_classic()
  
