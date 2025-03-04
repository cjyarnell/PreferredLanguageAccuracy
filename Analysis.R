# Better Access By Equity in Language #
# Camron Ford, Christopher Yarnell #
# 2024-07-18 #

############## libraries, packages, functions ##################

library(tidyverse)
library(tableone)
library(brms)

############## functions #######################################

MOR <- function(v){exp(sqrt(2*v)*qnorm(0.75))} # for variance v

############## read in data ####################################

df <- read_csv("BABEL_Deidentified 2025-03-04.csv") %>%
  select(language_emr, language_interview)

############## descriptive results #############################

# how many
nrow(df)


# how many English by EMR

table(df$language_emr)
table(df$language_interview)

# two by two table

df %>%
  filter(language_emr != 3) %>%
  group_by(language_emr, language_interview) %>%
  mutate(
    label = 
      case_when(
        language_emr == 1 & language_interview == 1 
        ~ "English EMR - English interview",
        language_emr == 2 & language_interview == 2 
        ~ "Non-English EMR - Non-English interview",
        language_emr == 1 & language_interview == 2 
        ~ "English EMR - Non-English interview",
        language_emr == 2 & language_interview == 1 
        ~ "Non-English EMR - English interview")
    ) %>%
  ungroup() %>%
  group_by(label) %>%
  summarise(
    count = n(),
    prop  = n()/321
  )


############## Frequentist comparisons ###############

# two with "unknown" language in EMR counted as "non-English preference"

prop.test(x=c(93,124), n=c(323,323), alternative="two.sided", conf.level =0.95, correct = FALSE)



############### Predictors of correctly identifying non-English preference ############

# Code included for interest
# Full dataset cannot be shared, according to the REB stipulations for the data

head(df)

df_nep <- df %>% 
  filter(language_interview == 2) %>%
  mutate(age = age/10,
         language_emr = language_emr - 1,
         language_interview_detail = factor(language_interview_detail),
         hospital = factor(hospital),
         length_of_stay = log2(length_of_stay))

fit_nep <- brm(
  data = df_nep,
  formula = language_emr ~ 1 + hospital + 
    unit + age + sex + length_of_stay + 
    (1|language_interview_detail),
  family = bernoulli(link = "logit"),
  prior =  c(set_prior("normal(0, 0.5)", class = "b"),
             set_prior("normal(0, 0.5)", class = "sd"))
)

saveRDS(fit_nep, "fit_nep.RDS")

fit_nep <- readRDS("fit_nep.RDS")

post <- as_draws_df(fit_nep)

# Fixed effects

ylim = c(0.05, 3)

ref <- data.frame(
  name = c("Hospital 1",
           "ICU"),
  post_mean = 1, 
  post_lb95 = NA,
  p_leq_1 = NA,
  ORlabel = "Reference",
  group = c("Hospital","Service")
)

post_or_fe <-
post %>%
  select(starts_with("b_")) %>%
  select(-b_Intercept) %>%
  pivot_longer(everything()) %>%
  mutate(name = gsub("b_","",name)) %>%
  group_by(name) %>%
  summarise(post_mean = mean(value),
            post_lb95 = quantile(value, 0.025),
            post_ub95 = quantile(value, 0.975),
            p_leq_1   = mean(value < 0)) %>%
  mutate(across(post_mean:post_ub95,~ exp(.x))) %>%
  mutate(ORlabel = paste0(round(post_mean, 2), " (",
                          round(post_lb95,2), " to ",
                          round(post_ub95,2), ")")) %>%  
  mutate(group = c("Demographics","Hospital","Hospital",
                   "Demographics", "Demographics", "Service","Service")) %>%
  bind_rows(ref) %>%
  mutate(name = factor(name,
                       levels = c("Hospital 1","hospital3","hospital2",
                                  "unitNephro","ICU","unitMed",
                                  "age","length_of_stay","sex"),
                       ordered = T,
                       labels = c("Hospital 1","Hospital 3","Hospital 2",
                                  "Nephrology","Critical Care","Medicine",
                                  "Age**","Length of stay*","Male sex"))) %>%
  arrange(name)
  
post_or_fe %>%
  ggplot(aes(x = as.integer(name), y= post_mean, ymin = post_lb95, ymax = post_ub95)) +
  geom_rect(aes(xmin = as.integer(name)-0.5,
                xmax = as.integer(name)+0.5,
                ymin = ylim[1],
                ymax = ylim[2],
                fill = factor(as.integer(name)%%2)))+
  geom_hline(yintercept = 1, color = "gray") +
  geom_pointrange(size = 0.25) + 
  geom_text(
    data = post_or_fe,
    aes(label = name,
        x = as.integer(name)), 
    y = log(ylim[1]+0.005),
    hjust = 0,
    vjust = 0.5,
    size = 3) +
  geom_text(
    aes(label = ORlabel,
        x = as.integer(name)), 
    y = log(0.25),
    hjust = 1,
    vjust = 0.5,
    size = 3) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = 
          element_text(margin = 
                         margin(
                           t = 10, 
                           r = 0, 
                           b = 0, 
                           l = 0)),
        strip.text.x = element_text(hjust=0, 
                                    margin=margin(l=25),
                                    face = "bold")) +
  coord_flip() +
  scale_fill_manual(values = c("grey95","white"),
                    guide = "none") +
  scale_y_continuous(trans = "log",
                     limits = ylim,
                     breaks = c(0.5, 1, 2)) +
  labs(y = "                         <<< less accurate            Odds ratio (95% credible interval)            more accurate >>>",
       x = "",
       title = "Odds ratios for correctly identifying non-English language preference") +
  facet_wrap(.~group,
             scales = "free_y",
             ncol = 1)

ggsave("BABEL_PosteriorOR_fixed.svg",
       width = 7, height = 4)


# Languages

# MOR

MOR_samples <- MOR(post$sd_language_interview_detail__Intercept^2)

c(mean = mean(MOR_samples),
CILB = quantile(MOR_samples, 0.025),
CIUB = quantile(MOR_samples, 0.975))


ylim2 = c(0.25, 3)

post_or_languages <- post %>%
  select(starts_with("r_")) %>%
  pivot_longer(everything()) %>%
  mutate(name = gsub("r_language_interview_detail\\[","",name)) %>%
  mutate(name = gsub(",Intercept\\]","",name)) %>%
  group_by(name) %>%
  summarise(post_mean = mean(value),
            post_lb95 = quantile(value, 0.025),
            post_ub95 = quantile(value, 0.975),
            p_leq_1   = mean(value < 0)) %>%
  mutate(across(post_mean:post_ub95,~ exp(.x))) %>%
  mutate(ORlabel = paste0(round(post_mean, 2), " (",
                          round(post_lb95,2), " to ",
                          round(post_ub95,2), ")")) %>%
  arrange(desc(post_mean)) %>%
  mutate(name = factor(name, levels = name, ordered = T))
    
post_or_languages %>% 
  ggplot(aes(x = as.integer(name), y= post_mean, ymin = post_lb95, ymax = post_ub95)) +
    geom_rect(aes(xmin = as.integer(name)-0.5,
                  xmax = as.integer(name)+0.5,
                  ymin = ylim2[1],
                  ymax = ylim2[2],
                  fill = factor(as.integer(name)%%2)))+
    geom_hline(yintercept = 1, color = "gray") +
    geom_pointrange(size = 0.25) + 
    geom_text(
      data = post_or_languages,
      aes(label = name,
          x = as.integer(name)), 
      y = log(ylim2[1]+0.005),
      hjust = 0,
      vjust = 0.5,
      size = 3) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = 
            element_text(margin = 
                           margin(
                             t = 10, 
                             r = 0, 
                             b = 0, 
                             l = 0)),
          strip.text.x = element_text(hjust=0, 
                                      margin=margin(l=25),
                                      face = "bold")) +
    coord_flip() +
    scale_fill_manual(values = c("grey95","white"),
                      guide = "none") +
    scale_y_continuous(trans = "log",
                       limits = ylim2,
                       breaks = c(0.5, 1, 2)) +
    labs(y = "                         <<< less accurate        Odds ratio (95% credible interval)        more accurate >>>",
         x = "",
         title = "Odds of correctly identifying non-English preference by language")
    
ggsave("BABEL_PosteriorOR_languages.svg",
       width = 7, height = 6)

