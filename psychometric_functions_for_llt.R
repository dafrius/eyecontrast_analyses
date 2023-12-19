library(tidyverse)
library(ggplot2)
library(emdbook)
library(psyphy)
library(gridExtra)
library(readr)
library(modelfree)


# This script uses Daniel Linares' psyphy functions to make predictions and 
# plot a psychometric function

# this function takes as an input a data frame, beginning of the prediction 
# sequence, end of the sequence, length of the sequence, guess rate, and lapse rate

# the data frame to be provided must only contain a "dv" and an "iv" column (with the names 
# written exactly as dv and iv)


wrangledf <- function(df){
  if("ori" %in% colnames(df)){
    df2 <- df %>% select(cond, ori, rt, acc, contrast, trialno) %>%
      mutate(condition = case_when(cond=="ss" | cond == "dd" ~ "plus",#cong
                                 cond=="sd" | cond == "ds" ~ "minus",#inc
                                 cond=="isos" | cond == "isod" ~ "iso"),#iso
             contrast = as.numeric(contrast),
             acc = as.numeric(acc),
             task = ori)
  } else {
    df2 <- df %>% select(cond, rt, acc, contrast, TrialNumber) %>%
      mutate(contrast=as.numeric(contrast), 
             acc=as.numeric(acc),
             task="low",
             condition=case_when(cond==1 ~ "minus",
                               cond==2 ~ "plus",
                               cond==3 ~ "iso")) %>%
      select(-cond)
  }
 return(df2)
}

psyfunc_llt <- function(df, seqbegin, seqend, seqlength, guessrate, lapserate){
  # here we take the number of 0 (numNo) and 1 (numYes) responses using summarise
  aggr <- df %>% group_by(iv) %>% summarise(dv = mean(dv),n = n()) %>%
    mutate(numYes = n * dv) %>%  mutate(numNo = n - numYes) %>% mutate(task=df$task[1])
  
  # we create our prediction sequence
  xseq<- lseq(seqbegin,seqend,len=seqlength)
  
  # running a logistic regression model on the aggregate data, implementing the guess rate and lapse rate
  aggr.glm <- glm(formula = cbind(numYes,numNo) ~ log(iv),binomial(probit.lambda(m = 1/guessrate, lambda = lapserate)),aggr)

  # 'predict' function gives us the predictions of the model, given the sequence
  aggr.pred<- predict(aggr.glm,newdata = data.frame(iv = xseq), type = "response",se.fit = TRUE)
  
  # we merge the predictions with the standard error, for plotting and ribboning later
  preds_aggr <- data.frame(aggr.pred$fit,xseq,aggr.pred$se.fit)
  preds_aggr <- preds_aggr %>%  rename(dv = aggr.pred.fit) %>%
    rename(iv = xseq) %>%  rename(se = aggr.pred.se.fit)
  
  mod <- glm(dv ~ log(iv),binomial(probit.lambda(m=1/guessrate, lambda=lapserate)),df)
  mdata <- with(df, tibble(iv = seq(seqbegin, seqend, length = seqlength)))
  mdata <- add_column(mdata, fit = predict(mod, newdata = mdata, type = 'response'))
  fam <- family(mod)
  ilink <- fam$linkinv
  ilink <- family(mod)$linkinv
  mdata <- bind_cols(mdata, setNames(as_tibble(predict(mod, mdata, se.fit = TRUE)[1:2]),
                                     c('fit_link','se_link')))
  mdata <- mutate(mdata,
                  fit_resp  = ilink(fit_link),
                  right_upr = ilink(fit_link + (2 * se_link)),
                  right_lwr = ilink(fit_link - (2 * se_link)))
  
  thresh1 <- threshold_slope(preds_aggr$dv,preds_aggr$iv,.75)
  
  a <- list(aggr=aggr, preds=preds_aggr, modeldata=mdata, thresholds=thresh1)
  
  return(a)
}

psyfunc_plot <- function(modeloutput, title){
  if(modeloutput$aggr$task[1] == "low"){
  modeloutput$preds %>% ggplot(aes(x = iv,y = dv)) + 
  geom_line(size = 1.2) +
  geom_count(data = modeloutput$aggr,aes(size = n,group = iv),alpha = .6,colour = "blue", stroke=1,shape = 16, show.legend  =  FALSE)+
  geom_ribbon(data = modeloutput$modeldata, aes(x=iv, y= fit,ymin=right_lwr, ymax=right_upr),
              alpha=0.4, inherit.aes=FALSE)+
  scale_size_area(max_size = 7)+
  theme_classic()+
  scale_x_continuous(name = "Target contrast",trans = "log10",limits = c(0.001,1)) +
  scale_y_continuous(name = "Accuracy (prop. correct)", breaks = c(0,.25,.5,.75,1))+
  geom_segment(aes(x = modeloutput$thresholds$x_th,y = 0,xend = modeloutput$thresholds$x_th,yend = 0.75),linetype = "dashed")+
  geom_segment(aes(x = 0,y = 0.75,xend = modeloutput$thresholds$x_th,yend = 0.75),linetype = "dashed")+
  ggtitle(title)+
  annotation_logticks(sides="b")
  } else {
  modeloutput$preds %>% ggplot(aes(x = iv,y = dv)) + 
    geom_line(size = 1.2) +
    geom_count(data = modeloutput$aggr,aes(size = n,group = iv),alpha = .6,colour = "blue", stroke=1,shape = 16, show.legend  =  FALSE)+
    geom_ribbon(data = modeloutput$modeldata, aes(x=iv, y= fit,ymin=right_lwr, ymax=right_upr),
                alpha=0.4, inherit.aes=FALSE)+
    scale_size_area(max_size = 7)+
    theme_classic()+
    scale_x_continuous(name = "Target contrast",trans = "log10", limits= c(1,40), breaks=c(1, 5, 10, 40)) +
    scale_y_continuous(name = "Accuracy (prop. correct)", breaks = c(0,.25,.5,.75,1))+
    geom_segment(aes(x = modeloutput$thresholds$x_th,y = 0,xend = modeloutput$thresholds$x_th,yend = 0.75),linetype = "dashed")+
    geom_segment(aes(x = 0,y = 0.75,xend = modeloutput$thresholds$x_th,yend = 0.75),linetype = "dashed")+
    ggtitle(title)+
    annotation_logticks(sides="b")  
  }
}

bin_df <- function(df){
  df2 <- df2 %>% ungroup() %>% group_by(condition, task) %>%
   # I bin everything to 5 levels so that the comparison across the data and the model is simpler
   mutate(contbin = ntile(contrast, n=10)) %>% group_by(task, condition, contbin) %>% 
   summarise(contrast= mean(contrast), tot_n=n(),acc=mean(acc)) %>% ungroup(condition,task)
  return(df2)
}



#setwd("D:/shared/analyses_R/lohi_eyecontrast/scripts")
path <- getwd()
#path <- dirname(path)
# eyes
#path2 <- paste(path, "/data_high/", sep="")
# gratings
path2 <- paste(path, "/data_low/", sep="")

datafiles=list.files(path = path2, pattern = "csv", all.files = TRUE,
             full.names = FALSE, recursive = TRUE,
             ignore.case = FALSE, include.dirs = FALSE, no.. = TRUE)
datafiles <- paste(path2,datafiles,sep="")

i=1
df <- tibble(read_delim(datafiles[1], delim = ",", escape_double = FALSE, trim_ws = TRUE))

df2 <- wrangledf(df)

df3 <- bin_df(df2)

df4 <- df3 %>% filter(task=="inv")
df5 <- df3 %>% filter(task=="low")

c1 <- df5 %>%  filter(condition ==  "plus") %>%  select(acc,contrast, task) %>% rename(iv=contrast, dv=acc) 
c2 <- df5 %>%  filter(condition ==  "minus") %>%  select(acc,contrast, task) %>% rename(iv=contrast, dv=acc) 
c3 <- df5 %>%  filter(condition ==  "iso") %>%  select(acc,contrast, task) %>% rename(iv=contrast, dv=acc)

# for gratings
c1s <- psyfunc_llt(c1, 0.001, 1, 350, 0.5, 0.05)
c2s <- psyfunc_llt(c2, 0.001, 1, 350, 0.5, 0.05)
c3s <- psyfunc_llt(c3, 0.001, 1, 350, 0.5, 0.05)




# for eyes
c1s <- psyfunc_llt(c1, 1, 40, 100, 0.5, 0.05)
c2s <- psyfunc_llt(c2, 1, 40, 100, 0.5, 0.05)
c3s <- psyfunc_llt(c3, 1, 40, 100, 0.5, 0.05)

modeloutput <- c1s


a <- psyfunc_plot(c1s, "Plus")
b <- psyfunc_plot(c2s, "Minus")
c <- psyfunc_plot(c3s, "Isolated")

#grid.arrange(a,b,c, top=paste("Ecc = ", df2$eccentricity[1], " Size = ", df2$Stim_size[1], sep=""))
grid.arrange(a,b,c)












df3 <- df2 %>% select(TrialNumber, contrast, acc, condition, staircase)


cond1_sc1 <- df3 %>% filter(cond == 1, staircase == 1) %>% mutate(row_id=row_number(),
                                              acc= as.character(acc))

cond1_sc2 <- df3 %>% filter(cond == 1, staircase == 2) %>% mutate(row_id=row_number(),
                                                                  acc= as.character(acc))


cond1_sc1 %>% ggplot(aes(x=row_id, y=contrast, fill=acc, color=acc))+ 
  geom_point(size=5)+
  geom_line(aes(x=row_id, y=contrast), inherit.aes=FALSE)


