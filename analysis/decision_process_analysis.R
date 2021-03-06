####### DECISION PROCESS ANALYSIS #########
### Jennifer L Beaudry, Jamal K Mansour, Roy Groncki, & Mai-Tram Nguyen

##### LOAD PACKAGES #####
library(here)
library(box)
library(tidyverse)
library(magrittr)
library(effectsize) # for eta & Cohen's d with CIs
library(chisq.posthoc.test) # for chi-square
library(epitools) # for OR
library(psych) # from Jam's code - check distribution
library(corpcor) # from Jam's code - why
library(GPArotation) # from Jam's code - why
library(lavaan) # from Jam's code - why
library(semTools) # from Jam's code - why
library(semPlot) # from Jam's code - why
box::use(plyr[mapvalues]) #use mapvalues from plyr without installing full package

##### LOAD DATA #####
df <- read.csv(here::here("experiment", "data", "decision_process_data.csv")) %>%
  mutate(id = 1:n()) %>% # add a unique id number
  relocate(id, .before = ParticipantID) %>% # move it to the first column
  select(-ParticipantID) # delete the random number id

# load in metadata [breadcrumb]

# metadata <- here::here("survey", "data", "os_metadata_raw_data.csv") %>%
#   read_csv(col_names = TRUE, skip_empty_rows = TRUE) %>%
#   filter(!is.na(OldVariable))

##### RECODE VARIABLE NAMES #####

# recode variable labels according to metadata [breadcrumb]

# df <- meta_rename(df, metadata, old = old_variable, new = new_variable)


##### CODE VARIABLES ####

# turn into factors with labels
df$IV_target_presence_lab <- factor(df$IV_target_presence) %>%
  mapvalues (
    c("0", "1"),
    c("target absent", "target present")
  )

df$IV_mem_strength_lab <- factor(df$IV_mem_strength) %>%
  mapvalues (
    c("0", "1"),
    c("weak", "strong")
  )

df$id_decision_lab <- factor(df$id_decision) %>%
  mapvalues (
    c("0", "1", "2"),
    c("reject", "suspect", "filler")
  )

# rearrange the data set

df <- df %>%
  relocate (IV_target_presence_lab, .after = IV_target_presence) %>%
  relocate (IV_mem_strength_lab, .after = IV_mem_strength) %>%
  relocate (id_decision_lab, .after = id_decision)

##### MEMORY STRENGTH PROCEDURAL MANIPULATION CHECK ####

# select the relevant variables [manova will drop cases with NA values,
  # but we need to drop them here so we have the correct values for the
  # means]

mem_check <- df %>%
  select(
    c(
      IV_mem_strength,
      IV_mem_strength_lab,
      view,
      makeoutfeat,
      memory,
      time,
      distance
    )
  ) %>%
  drop_na()

# run the manova & follow-up anovas

res.man <-
  manova(cbind(view, makeoutfeat, memory, time, distance) ~ IV_mem_strength_lab,
         data = mem_check)
summary.manova(res.man,
        test = "Wilks")
summary.aov(res.man) #follow-up ANOVAS

# eta squared for manova [need to hard code it, unfortunately]

eta <- F_to_eta2(f = 79.98, df = 5, df_error = 557, ci = .90)

# calculate the means & sds

view <- mem_check %>%
  dplyr::group_by(IV_mem_strength_lab) %>%
  summarise(mean = mean(view),
            sd = sd(view),
            n = n()) %>%
  ungroup()

features <- mem_check %>%
  dplyr::group_by(IV_mem_strength_lab) %>%
  summarise(mean = mean(makeoutfeat),
            sd = sd(makeoutfeat),
            n = n()) %>%
  ungroup()

memory <- mem_check %>%
  dplyr::group_by(IV_mem_strength_lab) %>%
  summarise(mean = mean(memory),
            sd = sd(memory),
            n = n()) %>%
  ungroup()

time <- mem_check %>%
  dplyr::group_by(IV_mem_strength_lab) %>%
  summarise(mean = mean(time),
            sd = sd(time),
            n = n()) %>%
  ungroup()

distance <- mem_check %>%
  dplyr::group_by(IV_mem_strength_lab) %>%
  summarise(mean = mean(distance),
            sd = sd(distance),
            n = n()) %>%
  ungroup()

  # Cohens d for the follow-up tests

    # reorder the factor levels so that d is positive

mem_check$IV_mem_strength_lab <- relevel(mem_check$IV_mem_strength_lab, "strong")

      # VIEW
cohens_d(view ~IV_mem_strength_lab,
         data = mem_check,
         correction = FALSE,
         pooled_sd = TRUE,
         paired = FALSE,
         ci = 0.95)

      # FEATURES
cohens_d(makeoutfeat ~IV_mem_strength_lab,
         data = mem_check,
         correction = FALSE,
         pooled_sd = TRUE,
         paired = FALSE,
         ci = 0.95)

    # MEMORY
cohens_d(memory ~IV_mem_strength_lab,
         data = mem_check,
         correction = FALSE,
         pooled_sd = TRUE,
         paired = FALSE,
         ci = 0.95)

    # TIME
cohens_d(time ~IV_mem_strength_lab,
         data = mem_check,
         correction = FALSE,
         pooled_sd = TRUE,
         paired = FALSE,
         ci = 0.95)

    # DISTANCE
cohens_d(distance ~IV_mem_strength_lab,
         data = mem_check,
         correction = FALSE,
         pooled_sd = TRUE,
         paired = FALSE,
         ci = 0.95)


##### IDENTIFICATION DECISIONS #####

# TARGET PRESENT #
  # select Target Present Data [cannot use _lab data for prop tables] &
  # then turn it into a table

tp_num <- df %>%
  filter(IV_target_presence_lab == 'target present') %>%
  select(IV_mem_strength, id_decision) %>%
  table %>% # turn it into a table
  addmargins(1)  # add margins to figure out the ns for each memory strength

tp_num %>% addmargins(2) # use this to get ns for the rows, but don't use it for the actual analysis

# proportions of id decisions for each memory strength conditions plus overall
prop.table(tp_num,1) %>%
  round(2) # round to two digits

tp_lab <- df %>%
  filter(IV_target_presence_lab == 'target present') %>%
  select(IV_mem_strength_lab, id_decision_lab) %>%
  table

chisq.test(tp_lab) # use the n value from prop_table
chisq.posthoc.test(tp_lab) # this gives the same values as the follow-up z tests

# Build tables for each of the id decisions & then calculate the odds ratios

# suspect id
S <- as.table(rbind(c(tp_num[1,2],(tp_num[1,1]+tp_num[1,3])), c(tp_num[2,2],(tp_num[2,1]+tp_num[2,3]))))
dimnames(S) <- list ("memory strength" = c("weak", "strong"),
                     "ID Decision" = c("suspect", "not suspect"))
oddsratio.wald(S, rev = "rows") #use 'rev' to change the reference value so the OR value is > 1

# filler id
F <- as.table(rbind(c(tp_num[1,3],(tp_num[1,1]+tp_num[1,2])), c(tp_num[2,3],(tp_num[2,1]+tp_num[2,2]))))
dimnames(F) <- list ("memory strength" = c("weak", "strong"),
                     "ID Decision" = c("filler", "not filler"))
oddsratio.wald(F) #use 'rev' to change the reference value so the OR value is > 1

# rejections
R <- as.table(rbind(c(tp_num[1,1],(tp_num[1,2]+tp_num[1,3])), c(tp_num[2,1],(tp_num[2,2]+tp_num[2,3])))) # build 2 x 2 tables from P
dimnames(R) <- list ("memory strength" = c("weak", "strong"),
                     "ID Decision" = c("rejection", "not rejection"))
oddsratio.wald(R)


# TARGET ABSENT #

# select Target Absent Data [cannot use _lab data for prop tables] &
# then turn it into a table

ta_prop <- df %>%
  filter(IV_target_presence_lab == 'target absent') %>%
  select(IV_mem_strength, id_decision) %>%
  table %>% # turn it into a table
  addmargins(1)  # add margins to figure out the ns for each memory strength

ta_prop %>% addmargins(2) # use this to get ns for the rows, but don't use it for the actual analysis

# proportions of id decisions for each memory strength conditions plus overall]
prop.table(ta_prop,1) %>%
  round(2) # round to two digits

ta_lab <- df %>%
  filter(IV_target_presence_lab == 'target absent') %>%
  select(IV_mem_strength_lab, id_decision_lab) %>%
  table

chisq.test(ta_lab) # use the n value from prop_table
# no need for follow up tests because no sig differences


##### EXPLORATORY FACTOR ANALYSIS #####

# create dataframes for relevant variables for choosers only
  # decision process items
dp <- df %>%
  filter(id_decision_lab != "reject") %>%
  select(c(id,ease:plausible))

  # absolute vs. relative item
absrel <- df %>%
  filter(id_decision_lab != "reject") %>%
  select(c(id, absrel))

# Step 1: check for and delete missing values
sapply(dp, function(x) sum(is.na(x)))
dp %<>% drop_na
  # only 1 missing for mem variable in dp df

sapply(absrel, function(x) sum(is.na(x)))
absrel %<>% drop_na
  # only 1 missing for absrel df


# Step 2a: check if variables are normally distributed (univariate) - not required but helps

# get info about skewness (should be 0) and kurtosis (should be < 3)
describe(dp)

# visualise the data
hist_dp <- dp %>%
  select(-c(id)) %>%
  gather() %>%
  ggplot(aes(value)) +
  geom_histogram(bins=6)+
  facet_wrap(~key, scales = 'free_x')

hist_dp


  # negatively skewed variables: mem, familiar, remembered, compared
  # positively skewed variables: poppedout, easily, knew, clues, confused,
    # plausible
  # negative kurtosis: eliminated & standout

hist_absrel <- absrel %>%
  select(-c(id)) %>%
  ggplot(aes(absrel)) +
  geom_histogram(bins=6)

hist_absrel

  # positively skewed: absrel

# Step 2b: Attempt to transform variables (see Tabachnik & Fiddel, p. 86)

# [breadcrum: work on this later. I'm wasting a lot of time trying to find the
  # best transformation]

  # need to look at different transformations for the various items
dp_trans <- dp %>%
  mutate(
    popedout_log = log(poppedout),
    easily_log10 = log10(easily),
    knew_log10 = log10(knew),
    clues_log10 = log10(clues),
    clues_sqrt = sqrt(clues),
    clues_inv = (1/clues),
    confused_log10 = log10(confused),
    plausible_log10 = log10(plausible),
    eliminated_log10 = log10(eliminated)
  )

  # for those negatively skewed, first reflect, then use log transformation
# figure out max score for each variable [all had 7s]
dp %>%
  select(c(mem, familiar,remembered,compared)) %>%
  sapply(function (x) max(x))

# reflect the variable

# dp %<>%
#   mutate(
#     mem_r = ((7 - mem)+1)
#   )

# [breadcrumb: will need to reflect the variables and then log transform]
dp_trans <- dp %>%
  mutate(
    mem_log = log2(mem_r))
    familiar_log = log(familiar_r),
    remembered_log = log(remembered_r),
    compared_log = log(compared_r)
  )

hist_trans_dp <- dp_trans %>%
  select(-c(id)) %>%
  gather() %>%
  ggplot(aes(value)) +
  geom_histogram(bins=6)+
  facet_wrap(~key) # fix the scales to 0 to 8 so it's easier to see shape

hist_trans_dp

# Step 3: Check for pairwise linearity

#skewness in histograms suggest there may be
#curvilinear relationships between vars
#check worst case scenario by doing scatterplot for most skewed variables
  # familiar & easily

dp %>%
  ggplot(aes(x = familiar, y = easily))+
  geom_point()+
  geom_smooth(method="loess")+
  geom_smooth(method="lm")
#may be some weak non-linearity here judging by best-fitting line (geom_smooth)
#but seems  minimal
