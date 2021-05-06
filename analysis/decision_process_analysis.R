####### DECISION PROCESS ANALYSIS #########
### Jennifer L Beaudry, Jamal K Mansour, Roy Groncki, & Mai-Tram Nguyen

##### LOAD PACKAGES #####
library(here)
library(box)
library(tidyverse)
library(effectsize) # for eta & Cohen's d with CIs
library(chisq.posthoc.test) # for chi-square
library(epitools) # presumably for OR?
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

  # select Target Present Data [cannot use _lab data for prop tables] &
  # then turn it into a table

tp_prop <- df %>%
  filter(IV_target_presence_lab == 'target present') %>%
  select(IV_mem_strength, id_decision) %>%
  table %>% # turn it into a table
  addmargins(1) # add margins to figure out the ns for each memory strength
# [remove 1 to get ns for the rows] , but delete it for the actual analysis

# proportions of id decisions for each memory strength conditions plus overall
prop.table(tp_prop,1) %>%
  round(2) # round to two digits

tp_lab <- df %>%
  filter(IV_target_presence_lab == 'target present') %>%
  select(IV_mem_strength_lab, id_decision_lab) %>%
  table

chisq.test(tp_lab)
chisq.posthoc.test(tp_lab) # this gives the same values as the follow-up z tests

# [breadcrumb: I need to build my tables for each of the id decisions & then
 # use the odds ratio code]

# select Target Absent Data [cannot use _lab data for prop tables] &
# then turn it into a table

ta_prop <- df %>%
  filter(IV_target_presence_lab == 'target absent') %>%
  select(IV_mem_strength, id_decision) %>%
  table %>% # turn it into a table
  addmargins(1)  # add margins to figure out the ns for each memory strength
# [remove 1 to get ns for the rows] , but delete it for the actual analysis

# proportions of id decisions for each memory strength conditions plus overall]
prop.table(ta_prop,1) %>%
  round(2) # round to two digits

ta_lab <- df %>%
  filter(IV_target_presence_lab == 'target absent') %>%
  select(IV_mem_strength_lab, id_decision_lab) %>%
  table

chisq.test(ta_lab)
chisq.posthoc.test(ta_lab) # this gives the same values as the follow-up z tests

# [breadcrumb: I need to build my tables for each of the id decisions & then
# use the odds ratio code]


