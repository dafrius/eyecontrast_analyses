library(tidyverse)
library(readr)
library(ggplot2)
library(brms)
library(rstan)

load("ec_1sub.rda")

df <- lohi_ec_pilot

df_trns <- df %>% mutate(contrast2= log10(contrast)) %>%
  group_by(task) %>%
  mutate(contrast3=(contrast2-mean(contrast2))/sd(contrast2),
         task=as.factor(task),
         guess=case_when(task=="low" ~ 1,
                         TRUE ~ 0))

fit_ec <- brm(
  bf(acc ~ 0.5*guess + (1-0.5*guess)*inv_logit(eta),
     eta ~ contrast3*task*condition + (contrast3*task*condition||subject), nl=TRUE),
  data=df_trns,
  family=bernoulli("identity"),
  prior=c(prior(normal(0,5), nlpar="eta"),
          prior(lognormal(0,.1), nlpar="eta", class="sd", group="subject")),
  cores=12,
  warmup = 300,
  iter=600)
save(fit_ec, file="fit_ec.rda", compress="xz")

load("fit_ec.rda")
pred_df <- expand.grid(contrast3 = seq(-3,3, l = 101),
                       task = unique(df_trns$task),
                       subject = unique(df_trns$subject),
                       condition = unique(df_trns$condition))

pred_df <- pred_df %>% mutate(guess=1)
pred_df <- bind_cols(pred_df, as_tibble(fitted(fit_ec, newdata = pred_df, 
                                               re_formula = NULL,scale="linear")))
save(pred_df, file="preds_fit_ec.rda", compress="xz")


df_trns_avg <- df_trns %>% group_by(condition, task) %>% 
  summarise(acc=mean(acc))

df_trns_avg %>% ggplot(aes(x=condition, y=acc, color=condition))+
  geom_point()+
  geom_line(aes(group=task))+
  facet_wrap(~task)
