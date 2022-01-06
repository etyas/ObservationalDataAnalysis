#______________________________________________________________________________________#
# Project: BresMed - 3454 Capability enhancement for updated NICE methods
# Purpose: Standardised Propensity Score Weighting example
# Author: Hannah Kilvert
# Date: 6 January 2022
#______________________________________________________________________________________#


# PREAMBLE: PACKAGES, PATHS, AND FUNCTIONS --------------------------------
message("PREAMBLE: PACKAGES, PATHS, AND FUNCTIONS")

## Import packages
message("Load packages and data")

library(dplyr)
library(survival)
# library(devtools)
# devtools::install_github("ngreifer/WeightIt")
library(WeightIt)
# install.packages("cobalt")
library(cobalt)
library(ggplot2)
library(survtools)
library(survminer)
library(table1)
library(tibble)
library(rvest)
library(xlsx)
library(fastDummies)
library(tidyr)
## Check that all of these libraries are used



## BresMed Colours
message("BresMed Colours")

BM.blue = rgb(69, 185, 209, max=255)
BM.red = rgb(225, 55, 60, max=255)
BM.yellow = rgb(238, 224, 30, max=255)
BM.pink = rgb(211,78,147,max=255)
BM.lightPink<-rgb(229,151,192,max=255)



## File path
message("Set file paths")

base.dir <- "G:/Clients/BresMed (internal)/3454 Capability enhancement for updated NICE methods/Project/Observational data/Code/GitHub/PSWDemoExample"
data.dir <- file.path(base.dir, "Simulated datasets")
res.dir <- file.path(base.dir, "PSW Results")
if(!dir.exists(res.dir)){
  dir.create(res.dir, recursive = TRUE)
}

# output.file <- file.path(res.dir, paste0("Output tables.xlsx"))
# if(file.exists(output.file)) {
#   file.remove(output.file)
# }



## Import data 
message("Import data")

Data1 <- read.csv(file.path(data.dir, "SimulatedData1.csv"))
Data2 <- read.csv(file.path(data.dir, "SimulatedData2.csv"))



# Choose characteristics for inclusion in PSW -----------------------------
message("Characteristics for inclusion in PSW")

# Select the continuous characteristics for inclusion in propensity score
# calculation
Continuous.characteristics <- c("Age")
# Select the categorical characteristics for inclusion in propensity score
# calculation separately as these require more formatting than continuous
# variables
Categoricals.characteristics <- c("Sex", "Smoke")
# Select variable that specifies the two groups that should be balanced over
# i.e. this will probably be a variable that states the source of the data
Group.variable <- "GROUP"
# Select other variable that you would like to keep in your data set. For
# example you may wish to keep outcomes in the data sets for easy use in later
# codes
Other.variables <- c("Treatment", "Outcome")

# Derive formula for propensity score calculation
Characteristics <- c(Continuous.characteristics, Categoricals.characteristics)
for(i in 2:length(Characteristics)){
  RHS <- paste(RHS, Characteristics[i], sep = " + ")
}
Formula <- formula(paste0("Group ~ ", RHS))
# this may be adjusted to include non-linear terms e.g. I(age)^2



# Format data sets --------------------------------------------------------
message("Format data sets")

# Select Group.variable and Characteristics
data1.select <- Data1 %>%
  mutate(GROUP = "Data 1") %>%
  rename(Treatment = Group) %>%
  select(all_of(Group.variable),
         all_of(Continuous.characteristics),
         all_of(Categoricals.characteristics),
         all_of(Other.variables))

data2.select <- Data2 %>%
  mutate(GROUP = "Data 2") %>%
  rename(Treatment = Group) %>%
  select(all_of(Group.variable),
         all_of(Continuous.characteristics),
         all_of(Categoricals.characteristics),
         all_of(Other.variables))

# Merge data sets together
data <- full_join(data1.select, data2.select)


# Check whether there are any missing data in your weighting variables
input <- as.formula(paste0("~", RHS, "| GROUP"))
char.table.before.weighting <- table1(input, data = data)
char.table.before.weighting
###OUTPUT TABLE?
char.table.before.weighting.dataframe <- as.data.frame(read_html(char.table.before.weighting) %>% html_table(fill=TRUE))

# You could impute the information or set the variable to have "Missing" factor


# Set the factor variables as 0 or 1 for each level
df <- data %>%
  rename(Group = all_of(Group.variable)) %>%
  select(Group, all_of(Continuous.characteristics), 
         all_of(Categoricals.characteristics),
         all_of(Other.variables)) %>%
  dummy_cols(select_columns = Categoricals.characteristics) # ensure data is called df

colnames(df)

# Formatted names for balance plots
new.names <- c(`Sex_0` = "Sex (Female)",
               `Sex_1` = "Sex (Male)",
               `Smoke_0` = "Smoker (No)",
               `Smoke_1` = "Smoker (Yes)"
)



# Set input options for propensity score calculation ----------------------
message("Set input options for propensity score calculation")

# estimand specifies the desired estimand for propensity score calculation
# Options include ...
# For binary and multi-category treatments
  # "ATE" = the average treatment effect in the population
  # "ATT" = the average treatment effect in the treated
  # "ATC" = the average treatment effect in the control 
# For some methods
  # "ATO" = the average treatment effect in the overlap (i.e., equipoise
  # population)
  # "ATM" = the average treatment effect in the remaining matched sample
  # "ATOS" = the average treatment effect in the optimal subset
# In the weighit package, "ATE" is the default
# For this code, we have set "ATT" as the default
estimand <- "ATT"

# focal refers to the reference category for the propensity score calculation
# When multi-category treatments are used or ATT/ATC weights are requested, this
# specifies which group to consider the "treated" or focal group. This group
# will not be weighted, and the other groups will be weighted to be more like
# the focal group. Must be non-NULL if estimand = "ATT" or "ATC".
focal <- "Data 1"

# threshold refers to the difference between the data sets for the specified
# covariates to assess balance
# standardised mean difference
threshold <- 0.1
# variance ratio
v.threshold <- 2
# balance tables can be used to identify whether non-linear terms e.g. I(age)^2

# weighting.methods refers to the method used to estimate weights
# Options include ...
  # "ps" binary regression
  # "gbm" generalised boosted modeling
  # "cbps" covariate balancing
  # "super" propensity score weighting using SuperLearner
  # "bart" bayesian additive regression trees 
  # "ebal" entropy balancing
  # "ebcw" empirical balancing calibration weighting
  # "energy" energy balancing
  # "ncbps" non-parametric covariate balancing
  # "optweight" optimisation-based weighting
  # "user" user-defined functions for estimating weights
# The default option is "ps"
weighting.method <- "ps"

# missing refers to how missing data should be handled
# Options include ...
  # "ind" - for each variable with missingness, a new missingness indicator
  # variable is created which takes the value 1 if the original covariate is NA
  # and 0 otherwise. The missingness indicators are added to the model formula
  # as main effects. The missing values in the covariates are then replaced with
  # 0s. The weight estimation then proceeds with this new formula and set of
  # covariates.
  # " " - covariates will not be checked for NA values; this can be faster when
  # it is known there are none.
missing <- "ind"



# DO NOT EDIT FROM HERE ---------------------------------------------------

# Assess balance before matching ------------------------------------------
message("Assess balance before matching")

balance.before.weighting <- bal.tab(Formula, data = df,
                                   estimand = estimand,
                                   thresholds = c(m = .1, v = 2)
                                   )$Balance[,c(1,2,3,5)] %>%
  as.data.frame()

## WRITE OUT



# Fit propensity score model, generate balancing weights ------------------
message("Fit propensity score model")

weighting <- weightit(formula = Formula, data = df,
                      estimand = estimand,
                      method = weighting.method,
                      missing = missing,
                      focal = focal)

# Bind weights and propensity scores to analysis dataset 
df.weights <- df %>% mutate(wt = weighting$weights,
                            ps = weighting$ps)

## WRITE OUT



# Propensity score model diagnostics --------------------------------------
message("Propensity score model diagnostics")

# Assessing balance after weighting ---------------------------------------
message("Assessing balance after weighting")

message("Summary of propensity scores")

weight.summary <- summary(weighting)

Measure <- data.frame(Measure = c("Min", "Max", 
                                  "Greatest 1", "Greatest 2", "Greatest 3",
                                  "Greatest 4", "Greatest 5",
                                  "ESS Unweighted", "ESS Weighted"))
ESS <- as.data.frame(weight.summary$effective.sample.size) %>% 
  remove_rownames() %>%
  rename(Data.1 = `Data 1`, Data.2 = `Data 2`) # this isn't standardised
weight.summary.table <- rbind(as.data.frame(weight.summary$weight.range),
                              as.data.frame(weight.summary$weight.top)) %>%
  rbind(ESS) %>% cbind(Measure) %>% select(Measure, Data.1, Data.2) # this isn't standardised

## WRITE OUT

message("Propensity score histogram")

ps_hist <- ggplot(df.weights, aes(x = ps)) +
  geom_histogram(aes(fill = Group), binwidth=0.05, alpha = 0.4, position="identity", color = "black") +
  scale_color_manual(values = c(BM.blue, BM.pink)) +
  scale_fill_manual(values = c(BM.blue, BM.pink)) +
  ggtitle("Histogram of propensity scores by study") +
  scale_x_continuous(breaks = seq(0,1,0.25)) +
  scale_y_continuous(expand = expansion(add = c(0, 2))) +
  xlab("Propensity score") +
  ylab("Number of patients") +
  theme_bw() +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        title = element_text(size = 14),
        legend.text = element_text(size = 12))
ps_hist

# Save propensity scores
jpeg(file.path(output.dir, "plots/Histogram of propensity scores.jpg"), width = 25, height = 15,
     units = 'cm', res = 300, quality=100)
ps_hist
graphics.off()


message("Weight histogram")

hist.data.wt <- df.weights %>%
  filter(Group != focal) %>%
  mutate(n = n(),
         wt.type = paste0("Weights (sum = ", round(sum(wt),0), ")"))

ESS.adjusted <- weight.summary.table[weight.summary.table$Measure == "ESS Weighted","Data.2"] # not standardised

hist.data.rescaled.wt <- df.weights %>%
  filter(Group != focal) %>%
  mutate(n = n(),
         wt = wt * round(ESS.adjusted,0) / sum(wt),
         wt.type = paste0("Rescaled weights using ESS (sum = ", round(sum(wt),1), ")"))

weights.data.long <- rbind(hist.data.wt, hist.data.rescaled.wt)

wt_hist <- ggplot(weights.data.long) +
  geom_histogram(aes(x = wt), color = "black", fill = BM.blue, alpha = 0.6, position="identity") +
  scale_color_manual(values = c(BM.blue, BM.pink)) +
  scale_fill_manual(values = c(BM.blue, BM.pink)) +
  scale_y_continuous(expand = expansion(add = c(0, 2))) +
  ggtitle(paste0("Histogram of weights for ", unique(hist.data.rescaled.wt$Group))) +
  xlab("Weight") +
  ylab("Number of patients") +
  facet_grid(~wt.type, scales = "free_x") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        title = element_text(size = 14),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12))
wt_hist

# Save weights
jpeg(file.path(output.dir, "plots/Histogram of weights.jpg"), width = 25, height = 15,
     units = 'cm', res = 300, quality=100)
wt_hist
graphics.off()


message("Balance table after weighting")

balance.after.weighting <- bal.tab(weighting, thresholds = c(m = .1, v = 2)
                                   )$Balance[,c(1,4,5,7)] %>%
  as.data.frame()

balance.table <- merge(balance.before.weighting, balance.after.weighting, by = c(0), all = TRUE)
names(balance.table) <- c("Characteristic", "Type", 
                          "Unadjusted Difference", "Unadjusted Mean Balance", "Unadjusted Variance Ratio Balance",
                          "Type2", "Adjusted Difference", "Adjusted Mean Balance", "Adjusted Variance Ratio Balance")
balance.table <- balance.table %>% select(!Type2)

## WRITE OUT

# Baseline_summary_bin_weighted <- df.weights %>%
#   select(-all_of(Continuous.characteristics), -all_of(Categoricals.characteristics), -all_of(Other.variables), -ps) %>%
#   dplyr::group_by(Group) %>%
#   summarise_each(funs(weighted.mean(.*100, wt)), -wt)  %>%
#   mutate(Analysis = "Weighted") %>%
#   left_join(select(ESS_forcombining, Group,`N/ESS` = Adjusted), by = "Group")
# 
# Baseline_summary_bin_nonweighted <- df.weights %>%
#   select(-all_of(Characteristics), -all_of(OutcomeNames), -ps, -wt) %>%
#   dplyr::group_by(Group) %>%
#   summarise_each(funs(mean(.*100)))  %>%
#   mutate(Analysis = "Non weighted") %>%
#   left_join(select(ESS_forcombining, Group, `N/ESS` = Unadjusted), by = "Group")
# 
# Baseline_summary_bin_all <- bind_rows(Baseline_summary_bin_weighted, Baseline_summary_bin_nonweighted)   %>%
#   select(Analysis, Group, `N/ESS`, everything())

#Summary of weighted data, weighted means etc
base.summ.wt <- df.weights %>% 
  select(-all_of(Continuous.characteristics), -all_of(Categoricals.characteristics), -all_of(Other.variables), -ps) %>%
  dplyr::group_by(Group) %>%
  summarise(across(.cols = everything(), ~weighted.mean(., wt, na.rm = TRUE)),) %>%
  mutate(across(.cols = ends_with('_bin'), ~.*100), .keep = 'unused') %>%
  mutate(across(.cols = where(is.numeric), ~round(., 2)), .keep = 'unused') %>%
  mutate(Analysis = "Weighted")

#Summary of unweighted data, means etc
base.summ.uwt <- df.weights %>% 
  select(-all_of(Continuous.characteristics), -all_of(Categoricals.characteristics), -all_of(Other.variables), -ps) %>%
  dplyr::group_by(Group) %>%
  summarise(across(.cols = everything(), ~mean(., na.rm = TRUE)),) %>%
  mutate(across(.cols = ends_with('_bin'), ~.*100), .keep = 'unused') %>%
  mutate(across(.cols = where(is.numeric), ~round(., 2)), .keep = 'unused') %>%
  mutate(Analysis = "Weighted")

base.summ.all <- bind_rows(base.summ.wt, base.summ.uwt)%>%
  as.data.frame()

## NEEDS TO BE ADJUSTED SUCH THAT WEIGHTED SUMMARY OF CONTINUOUS VARIABLES ARE INCLUDED AND ADD THE `N/ESS` COLUMN

## WE NEED TO CREATE OUTPUT

## WE NEED TO STANDARDISED SOME BITS FLAGGED ABOVE AND LOVEPLOTS AND BALANCE PLOTS

# 
# 
# # Save dataset with PS weights --------------------
# # Add standardized weights to data set
# df.weights <- df.weights %>%
#   mutate(ESS = ifelse(Group == focal, ESS.adjusted[[focal]],
#                       ifelse(Group == setdiff(colnames(ESS.adjusted), c("TYPE", focal)), ESS.adjusted[[setdiff(colnames(ESS.adjusted), c("TYPE", focal))]], NA))) %>%
#   group_by(Group) %>%
#   mutate(wt.standardized.ESS = (wt * ESS) / sum(wt))
# 
# # check weights (sum should equal ESS)
# weights.sum <- df.weights %>%
#   group_by(Group, ESS) %>%
#   summarize(check = sum(wt.standardized.ESS))
# 
# write.csv(df.weights, file.path(output.dir, "PLD with weights.csv"), row.names = F)
# 
# # Save Diagnostics
# write.csv(balance.table, file.path(output.dir, "Balance Statistics.csv"), row.names = F)
# write.csv(Baseline_summary_bin_all, file.path(output.dir, "Baseline summary weighted and non weighted.csv"), row.names = F)
# 
# 
# # Balance plots
# # Absolute mean differences
# 
# love.plot.MD <- love.plot(weighting,
#                           #line = TRUE,
#                           stats = c("mean.diffs"),
#                           threshold = c(m = threshold),
#                           drop.distance = TRUE,
#                           binary = "raw",
#                           abs = TRUE,
#                           #var.order = "unadjusted",
#                           limits = c(0, 1),
#                           grid = FALSE,
#                           wrap = 100,
#                           #sample.names = c("Unmatched", "Matched"),
#                           position = "top",
#                           shapes = c("circle", "triangle"),
#                           colors = c(BM.blue, BM.pink),
#                           var.names = new.names) +
#   ggtitle("") +
#   xlab("Absolute mean differences") +
#   theme(legend.title = element_blank(),
#         axis.title = element_text(size=14),
#         axis.text = element_text(size=12),
#         title = element_text(size = 14),
#         legend.text = element_text(size = 12))
# 
# love.plot.MD
# 
# jpeg(file.path(output.dir, "plots/Covariate balance absolute MD.jpg"), width = 25, height = 15,
#      units = 'cm', res = 300, quality=100)
# love.plot.MD
# graphics.off()
# 
# 
# 
# # Absolute standardised mean differences
# love.plot.SMD <- love.plot(weighting,
#                            #line = TRUE,
#                            stats = c("mean.diffs"),
#                            threshold = c(m = threshold),
#                            drop.distance = TRUE,
#                            binary = "std",
#                            abs = TRUE,
#                            #var.order = "unadjusted",
#                            limits = c(0, 1),
#                            grid = FALSE,
#                            wrap = 100,
#                            #sample.names = c("Unmatched", "Matched"),
#                            position = "top",
#                            shapes = c("circle", "triangle"),
#                            colors = c(BM.blue, BM.pink),
#                            var.names = new.names) +
#   ggtitle("") +
#   xlab("Absolute standardised mean differences") +
#   theme(legend.title = element_blank(),
#         axis.title = element_text(size=14),
#         axis.text = element_text(size=12),
#         title = element_text(size = 14),
#         legend.text = element_text(size = 12))
# 
# love.plot.SMD
# 
# jpeg(file.path(output.dir, "plots/Covariate balance absolute SMD.jpg"), width = 25, height = 15,
#      units = 'cm', res = 300, quality=100)
# love.plot.SMD
# graphics.off()
# 
# 
# 
# # Balance plots
# bal.plots.data <- pivot_longer(Baseline_summary_bin_all, cols = colnames(Baseline_summary_bin_all)[4:length(colnames(Baseline_summary_bin_all))]) %>%
#   mutate(var = ifelse(grepl("AGE", name), "Age",
#                       ifelse(grepl("MUFC", name), "mUFC",
#                              ifelse(grepl("TIMEDIAG", name), "Time since diagnosis",
#                                     ifelse(grepl("IIRD", name), "Prior irridation",
#                                            ifelse(grepl("MED", name), "Prior medications",
#                                                   ifelse(grepl("RACE", name), "Race", "Prior surgery")))))),
#          x = gsub(".*_", "", name),
#          x = ifelse(x == "2.0 x ULN to <5.0 x ULN", "2.0 x ULN to\n<5.0 x ULN", x),
#          x = ifelse(x == "Yes (over 43 months ago)", "Yes\n(>=43 months)",
#                     ifelse(x == "Yes (within 43 months)", "Yes\n(<43 months)", x)),
#          x = gsub("<", "< ", x),
#          x = gsub(">=", "\u2265 ", x),
#          Group = ifelse(grepl("LINC-3", Group), "Osilodrostat\n(LINC-3)", "Pasireotide LAR \n(Lacroix 2018)"))
# 
# 
# 
# # mUFC
# jpeg(file.path(output.dir, "plots/MUFC distribution.jpg"), width = 25, height = 15,
#      units = 'cm', res = 300, quality=100)
# 
# ggplot(filter(bal.plots.data, var == "mUFC")) +
#   geom_col(aes(x = factor(x, level = c("< 2.0 x ULN", "2.0 x ULN to\n< 5.0 x ULN", "\u2265 5.0 x ULN", "Missing")), y = value, fill = Group), position = "dodge", alpha = 0.6, color = "black") +
#   scale_fill_manual(values = c(BM.blue, BM.pink)) +
#   scale_color_manual(values = c(BM.blue, BM.pink)) +
#   ylab("Proportion of patients (%)") +
#   xlab("mUFC (nmol/24h)") +
#   facet_wrap("Analysis") +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.title = element_text(size=14),
#         axis.text = element_text(size=10),
#         title = element_text(size = 14),
#         legend.text = element_text(size = 12),
#         strip.text = element_text(size = 12))
# 
# graphics.off()
# 
# 
# # Age cat
# jpeg(file.path(output.dir, "plots/Age distribution.jpg"), width = 25, height = 15,
#      units = 'cm', res = 300, quality=100)
# 
# 
# ggplot(filter(bal.plots.data, var == "Age")) +
#   geom_col(aes(x = factor(x, levels = c("< 40 years", "40 to < 60 years", "\u2265 60 years")), y = value, fill = Group), position = "dodge", alpha = 0.6, color = "black") +
#   scale_fill_manual(values = c(BM.blue, BM.pink)) +
#   scale_color_manual(values = c(BM.blue, BM.pink)) +
#   ylab("Proportion of patients (%)") +
#   xlab("Age") +
#   facet_wrap("Analysis") +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.title = element_text(size=14),
#         axis.text = element_text(size=12),
#         title = element_text(size = 14),
#         legend.text = element_text(size = 12),
#         strip.text = element_text(size = 12))
# 
# graphics.off()
# 
# 