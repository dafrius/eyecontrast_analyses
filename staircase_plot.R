library(tidyverse)


wrangledf <- function(df){
  if("ori" %in% colnames(df)){
    df2 <- df %>% select(cond, ori, rt, acc, contrast, trialno) %>%
      mutate(cong = case_when(cond=="ss" | cond == "dd" ~ "cong",
                              cond=="sd" | cond == "ds" ~ "inc",
                              cond=="isos" | cond == "isod" ~ "iso"),
             contrast = as.numeric(contrast),
             acc = as.numeric(acc),
             task = ori)
  } else {
    df2 <- df %>% select(cond, rt, acc, contrast, TrialNumber) %>%
      mutate(contrast=as.numeric(contrast), 
             acc=as.numeric(acc),
             task="low",
             cong=case_when(cond==1 ~ "cong",
                            cond==2 ~ "inc",
                            cond==3 ~ "iso")) %>%
      select(-cond)
  }
  return(df2)
}

setwd("D:/shared/experiments/eyecontrast_project/eyecontrast_analyses/scripts/final/")
path <- getwd()
path <- dirname(path)
# eyes
#path2 <- paste(path, "/data_high/", sep="")
# gratings
path2 <- paste(path, "/data_low/", sep="")

datafiles=list.files(path = path2, pattern = "csv", all.files = TRUE,
                     full.names = FALSE, recursive = TRUE,
                     ignore.case = FALSE, include.dirs = FALSE, no.. = TRUE)
datafiles <- paste(path2,datafiles,sep="")

# i is the number of the file you want to check the staircase of 

i=1

df <- tibble(read_delim(datafiles[i], delim = ",", escape_double = FALSE, trim_ws = TRUE))

#df2 <- wrangledf(df)

# JERO MAHE ROVE UC

if (length(df) == 23) { # low-level task)
  df2 <- df %>% mutate(cond= case_when(bgori==90 ~ "minus", # 90 degrees context is parallel to the target
                                       bgori==0 ~ "plus", # 0 degrees is orthogonal
                                       TRUE ~ "iso"),
                         acc=as.factor(acc),
                       task="low") %>% rename(trialno=TrialNumber)
} else { # high-level task
  df2 <- df %>% mutate(cond=case_when(cond=="sd" | cond == "ds" ~ "minus",#incongruent
                                      cond=="dd" | cond == "ss" ~ "plus",#congruent
                                      TRUE ~ "iso"),
                       acc=as.factor(acc),
                       task=case_when(ori=="up" ~ "upright",
                                      ori=="inv" ~ "inverted"))}


df2 %>% group_by(cond, ori, staircase) %>% tally() # checking the number of trials per condition
#df2 %>% group_by(im1name) %>% tally()

df2 %>% ggplot(aes(x=trialno, y=contrast))+
  geom_point(aes(color=acc), size=3)+
  geom_line(aes(group=staircase))+
  facet_wrap(ori~cond)



df2plusup1 <- df2 %>% filter(task=="up", cond=="plus", staircase==1) %>% mutate(row_id=row_number())
df2plusup2 <- df2 %>% filter(task=="up", cond=="plus", staircase==2) %>% mutate(row_id=row_number())


df2plusup1 %>% ggplot(aes(x=row_id, y=contrast))+
  geom_point(aes(color=acc), size=3)+
  geom_line()

df2congup2 %>% ggplot(aes(x=row_id, y=contrast))+
  geom_point(aes(color=acc), size=3)+
  geom_line()

df2minusup1 <- df2 %>% filter(ori=="up", cond=="minus", staircase==1) %>% mutate(row_id=row_number())
df2minusup2 <- df2 %>% filter(ori=="up", cond=="minus", staircase==2) %>% mutate(row_id=row_number())

df2incup1 %>% ggplot(aes(x=row_id, y=contrast))+
  geom_point(aes(color=acc), size=3)+
  geom_line()

df2incup2 %>% ggplot(aes(x=row_id, y=contrast))+
  geom_point(aes(color=acc), size=3)+
  geom_line()

df2minuslow1 <- df2 %>% filter(task=="low", cond=="minus", staircase==1) %>% mutate(row_id=row_number())
df2minuslow2 <- df2 %>% filter(task=="low", cond=="minus", staircase==2) %>% mutate(row_id=row_number())

df2minuslow1 %>% ggplot(aes(x=row_id, y=contrast))+
  geom_point(aes(color=acc), size=3)+
  geom_line()

df2minuslow2 %>% ggplot(aes(x=row_id, y=contrast))+
  geom_point(aes(color=acc), size=3)+
  geom_line()


