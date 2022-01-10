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

packages <- c("tidyverse", "tableone", "rvest", "fastDummies", "cobalt",
              "WeightIt")
lapply(packages, library, character.only = TRUE)

## BresMed Colours
message("BresMed Colours")

BM.blue = rgb(69, 185, 209, max=255)
BM.pink = rgb(211,78,147,max=255)

## File paths
message("Set file paths")

base.dir <- "C:/Users/etyas/OneDrive - Enterprise/Documents/Standardisation/GitHub/ObservationalDataAnalysis-main_10JAN22"
data.dir <- file.path(base.dir, "Simulated datasets")
res.dir <- file.path(base.dir, "PSW Results")

# Create folders if needed
if(!dir.exists(res.dir)){
  dir.create(res.dir, recursive = TRUE)
}

if(!dir.exists(file.path(res.dir, "Plots"))){
  dir.create(file.path(res.dir, "Plots"), recursive = TRUE)
}

if(!dir.exists(file.path(res.dir, "Tables"))){
  dir.create(file.path(res.dir, "Tables"), recursive = TRUE)
}

# output.file <- file.path(res.dir, paste0("Output tables.xlsx"))
# if(file.exists(output.file)) {
#   file.remove(output.file)
# }


## Import data 
message("Import data")

Data1 <- read.csv(file.path(data.dir, "SimulatedData1_v2.csv"))
Data2 <- read.csv(file.path(data.dir, "SimulatedData2_v2.csv"))


# Choose characteristics for inclusion in PSW -----------------------------
message("Characteristics for inclusion in PSW")

# Select the continuous characteristics for inclusion in propensity score
# calculation
Continuous.characteristics <- c("Age")
# Select the categorical characteristics for inclusion in propensity score
# calculation separately as these require more formatting than continuous
# variables
Categorical.characteristics <- c("Sex", "Smoke")
# Select variable that specifies the two groups that should be balanced over
# i.e. this will probably be a variable that states the source of the data
Group.variable <- "GROUP"
# Select other variable that you would like to keep in your data set. For
# example you may wish to keep outcomes in the data sets for easy use in later
# codes
Other.variables <- c("Outcome")

# Derive formula for propensity score calculation
Characteristics <- c(Continuous.characteristics, Categorical.characteristics)
RHS <- Characteristics[1]
for(i in 2:length(Characteristics)){
  RHS <- paste(RHS, Characteristics[i], sep = " + ")
}
Formula <- formula(paste0("Group ~ ", RHS))
# this may be adjusted to include non-linear terms e.g. I(age)^2



# Format data sets --------------------------------------------------------
message("Format data sets")

# Select Group.variable and Characteristics
# Note: group names must not include special characters (spaces are allowed)
data1.select <- Data1 %>%
  filter(Group == "A") %>%
  mutate(GROUP = "TRTA") %>%
  rename(Treatment = Group) %>%
  select(all_of(Group.variable),
         all_of(Continuous.characteristics),
         all_of(Categorical.characteristics),
         all_of(Other.variables))

data2.select <- Data2 %>%
  filter(Group == "C") %>%
  mutate(GROUP = "TRTC") %>%
  rename(Treatment = Group) %>%
  select(all_of(Group.variable),
         all_of(Continuous.characteristics),
         all_of(Categorical.characteristics),
         all_of(Other.variables))

# Merge data sets together
data <- full_join(data1.select, data2.select)

# Check whether there are any missing data in your weighting variables
(char.table.before.weighting <- CreateTableOne(vars = Characteristics, strata = Group.variable, factorVars = Categorical.characteristics, data = data))

# Output formatted table (comment out if not required)
write.csv(print(char.table.before.weighting, showAllLevels = TRUE, formatOptions = list(big.mark = ",")),
          file.path(res.dir, "Tables/Patient baseline characteristics.csv"))

# If you have missing data you could impute the information or set the variable
# to have "Missing" factor

# Set the factor variables as 0 or 1 for each level
df <- data %>%
  rename(Group = all_of(Group.variable)) %>%
  select(Group, all_of(Continuous.characteristics), 
         all_of(Categorical.characteristics),
         all_of(Other.variables)) %>%
  dummy_cols(select_columns = Categorical.characteristics) # ensure data is called df

colnames(df)

# Format characteristic names for balance plots
formatted.names <- data.frame(name = c("Age", colnames(df)[which(grepl("_", colnames(df)))]),
                              formatted = c("Age (years)",
                                            "Sex (Female)",
                                            "Sex (Male)",
                                            "Smoker (No)",
                                            "Smoker (Yes)")
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
focal <- "TRTA"

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

# Assess balance before weighting ------------------------------------------
message("Assess balance before weighting")

(balance.before.weighting <- bal.tab(Formula, data = df,
                                   estimand = estimand,
                                   thresholds = c(m = threshold, v = v.threshold)
                                   )$Balance[,c(1,2,3,5)] %>%
  as.data.frame())


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
  rename_with(make.names)

weight.summary.table <- rbind(as.data.frame(weight.summary$weight.range),
                              as.data.frame(weight.summary$weight.top)) %>%
  rbind(ESS) %>% cbind(Measure) %>% select(Measure, everything())

# Save output
write.csv(weight.summary.table, file.path(res.dir, "Tables/Weight summary.csv"),
          row.names = F)


message("Weight histogram")

hist.data.wt <- df.weights %>%
  filter(Group != focal) %>%
  mutate(n = n(),
         wt.type = paste0("Weights (sum = ", round(sum(wt),0), ")"))

ESS.adjusted <- weight.summary.table[weight.summary.table$Measure == "ESS Weighted",
                                     gsub(" ", ".", setdiff(unique(df$Group), focal))]

hist.data.rescaled.wt <- df.weights %>%
  filter(Group != focal) %>%
  mutate(n = n(),
         wt = wt * round(ESS.adjusted,0) / sum(wt),
         wt.type = paste0("Rescaled weights using ESS (sum = ", round(sum(wt),1), ")"))

weights.data.long <- rbind(hist.data.wt, hist.data.rescaled.wt)

(wt_hist <- ggplot(weights.data.long) +
  geom_histogram(aes(x = wt), color = "black", fill = BM.blue, alpha = 0.6, position="identity") +
  scale_color_manual(values = c(BM.blue, BM.pink)) +
  scale_fill_manual(values = c(BM.blue, BM.pink)) +
  scale_y_continuous(expand = expansion(add = c(0, 2))) +
  ggtitle(paste0("Histogram of weights for ", unique(hist.data.rescaled.wt$Group))) +
  xlab("Weight") +
  ylab("Number of patients") +
  facet_grid(~wt.type, scales = "free_x") +
  theme_bw() +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        title = element_text(size = 14),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12)))

# Save weights
jpeg(file.path(res.dir, "Plots/Histogram of weights.jpg"), width = 25, height = 15,
     units = 'cm', res = 300, quality=100)
wt_hist
graphics.off()


message("Propensity score density plot before and after weighting")

ps_plot_dataset <- df.weights %>%
  mutate(wt = 1,
         status = "Before weighting") %>%
  rbind(df.weights %>% mutate(status = "After weighting"))
ps_plot_dataset$status <- factor(ps_plot_dataset$status, levels = c("Before weighting", "After weighting"))
  
(ps_plot <- ggplot(ps_plot_dataset, aes(x = ps, weights = wt)) +
    geom_density(aes(fill = Group), alpha = 0.4, position="identity", color = "black") +
    scale_color_manual(values = c(BM.blue, BM.pink)) +
    scale_fill_manual(values = c(BM.blue, BM.pink)) +
    ggtitle("Propensity score distribution") +
    scale_x_continuous(breaks = seq(0,1,0.25), limits = c(0,1)) +
    xlab("Propensity score") +
    ylab("Density") +
    theme_bw() +
    theme(legend.title = element_blank(),
          axis.title = element_text(size=14),
          axis.text = element_text(size=12),
          title = element_text(size = 14),
          legend.text = element_text(size = 12),
          strip.text = element_text(size = 12)) +
  facet_wrap("status"))

# Save propensity scores
jpeg(file.path(res.dir, "Plots/Propensity score density plot.jpg"), width = 25, height = 15,
     units = 'cm', res = 300, quality=100)
ps_plot
graphics.off()


message("Balance table after weighting")

balance.after.weighting <- bal.tab(weighting, thresholds = c(m = threshold, v = v.threshold)
                                   )$Balance[,c(1,4,5,7)] %>%
  as.data.frame()

balance.table <- merge(balance.before.weighting, balance.after.weighting, by = c(0), all = TRUE)
names(balance.table) <- c("Characteristic", "Type", 
                          "Unadjusted Difference", "Unadjusted Mean Balance",
                          "Unadjusted Variance Ratio Balance",
                          "Type2", "Adjusted Difference", "Adjusted Mean Balance",
                          "Adjusted Variance Ratio Balance")
(balance.table.output <- balance.table %>% select(!Type2) %>% arrange(Type))

# Save output
write.csv(balance.table.output, file.path(res.dir, "Tables/Balance table.csv"),
          row.names = F)

message("Patient characteristics before and after weighting")

#Summary of weighted data, weighted means etc
N_ESS_tab <- as.data.frame(t(ESS)) %>%
  rownames_to_column() %>%
  mutate(rowname = gsub("\\.", " ", rowname)) %>%
  rename(Group = rowname, Unweighted = V1, Weighted = V2) %>%
  pivot_longer(cols = -Group, values_to = "N/ESS", names_to = "Analysis")

base.summ.wt <- df.weights %>% 
  select(-all_of(Continuous.characteristics), -all_of(Categorical.characteristics),
         -all_of(Other.variables), -ps) %>%
  rename_at(vars(-Group, -wt), ~ paste0(.x, "_bin")) %>%
  cbind(select(df.weights, all_of(Continuous.characteristics)) %>%
          rename_all(~ paste0(.x, "_mean"))) %>%
  dplyr::group_by(Group) %>%
  summarise(across(.cols = everything(), ~weighted.mean(., wt, na.rm = TRUE)),) %>%
  mutate(across(.cols = ends_with('_bin'), ~.*100), .keep = 'unused') %>%
  mutate(across(.cols = where(is.numeric), ~round(., 2)), .keep = 'unused') %>%
  mutate(Analysis = "Weighted")

#Summary of unweighted data, means etc
base.summ.uwt <- df.weights %>% 
  select(-all_of(Continuous.characteristics), -all_of(Categorical.characteristics),
         -all_of(Other.variables), -ps) %>%
  rename_at(vars(-Group, -wt), ~ paste0(.x, "_bin")) %>%
  cbind(select(df.weights, all_of(Continuous.characteristics)) %>%
          rename_all(~ paste0(.x, "_mean"))) %>%
  dplyr::group_by(Group) %>%
  summarise(across(.cols = everything(), ~mean(., na.rm = TRUE)),) %>%
  mutate(across(.cols = ends_with('_bin'), ~.*100), .keep = 'unused') %>%
  mutate(across(.cols = where(is.numeric), ~round(., 2)), .keep = 'unused') %>%
  mutate(Analysis = "Unweighted")
  

(base.summ.all <- bind_rows(base.summ.uwt, base.summ.wt)%>%
  as.data.frame() %>%
    left_join(N_ESS_tab) %>%
    mutate(`N/ESS` = round(`N/ESS`, 1)) %>%
    select(Group, `N/ESS`, everything(), -wt, -Analysis, wt, Analysis))

# Save output
write.csv(base.summ.all,
          file.path(res.dir, "Tables/Patient characteristics before and after weighting.csv"),
          row.names = F)


# Output final dataset -----------------------------------------------------
message("Save dataset with derived weights")

# Add standardized weights to data set
df.weights.output <- df.weights %>%
  left_join(select(filter(N_ESS_tab, Analysis == "Weighted"), -Analysis), by = "Group") %>%
  rename(ESS = `N/ESS`) %>%
  group_by(Group) %>%
  mutate(wt.standardized.ESS = (wt * ESS) / sum(wt))

# check weights (sum should equal ESS)
df.weights.output %>%
  group_by(Group, ESS) %>%
  summarize(check = sum(wt.standardized.ESS))

# Save output
write.csv(df.weights.output, file.path(res.dir, "Tables/PLD with weights.csv"), row.names = F)


# Plots for assessing balance -----------------------------------------------

message("Produce plot to assess covariate balance")
(love.plot <- love.plot(weighting,
                          threshold = c(m = threshold),
                          drop.distance = TRUE,
                          stars = "raw",
                          abs = TRUE,
                          grid = FALSE,
                          wrap = 100,
                          sample.names = c("Unweighted", "Weighted"),
                          position = "top",
                          shapes = c("circle", "triangle"),
                          colors = c(BM.blue, BM.pink),
                          var.names = data.frame(t(column_to_rownames(formatted.names, 'name')))
                        ) +
  ggtitle("") +
  xlab("Absolute standardized mean differences") +
  theme(legend.title = element_blank(),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        title = element_text(size = 14),
        legend.text = element_text(size = 12)))

jpeg(file.path(res.dir, "Plots/Covariate balance Love plot.jpg"), width = 20, height = 15,
     units = 'cm', res = 300, quality=100)
love.plot
graphics.off()


message("Produce balance plots for binary characteristics")

# Balance plots data - categorical
balance.plot.data.cat <- base.summ.all %>%
  select(-ends_with("mean")) %>%
  pivot_longer(cols = ends_with("bin")) %>%
  mutate(name = sub("_bin", "", name)) %>%
  left_join(formatted.names, by = "name") %>%
  mutate(input.var = gsub("\\_.*","", name),
         var = gsub("\\ .*","", formatted),
         level = sub(".*\\(","", formatted),
         level = sub(")","", level))

balance.plot.data.cat[balance.plot.data.cat$Analysis == "Unweighted",]$Analysis <- "Before weighting"
balance.plot.data.cat[balance.plot.data.cat$Analysis == "Weighted",]$Analysis <- "After weighting"


for(i in 1:length(Categorical.characteristics)){
  
  bal.plot.var <- filter(balance.plot.data.cat, input.var == Categorical.characteristics[i])
  bal.plot.var$Analysis <- factor(bal.plot.var$Analysis, levels = c("Before weighting", "After weighting"))
  message(paste0("Produce ", unique(bal.plot.var$var)," balance plot"))
  
  bal.plot <- ggplot(bal.plot.var) +
    geom_col(aes(x = formatted, y = value, fill = Group), position = "dodge",
             alpha = 0.6, color = "black") +
    scale_x_discrete(name = unique(bal.plot.var$var),
                     labels = unique(bal.plot.var$level)) +
    scale_fill_manual(values = c(BM.blue, BM.pink)) +
    scale_color_manual(values = c(BM.blue, BM.pink)) +
    ylab("Proportion of patients (%)") +
    facet_wrap("Analysis") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title = element_text(size=14),
          axis.text = element_text(size=10),
          title = element_text(size = 14),
          legend.text = element_text(size = 12),
          strip.text = element_text(size = 12),
          panel.spacing.x = unit(5, "mm"))
  
  jpeg(file.path(res.dir, paste0("Plots/Balance plot_", unique(bal.plot.var$var),".jpg")),
       width = 25, height = 15,
       units = 'cm', res = 300, quality=100)
  print(bal.plot)
  dev.off()
  
}


message("Produce density plots for continuous characteristics")

# Balance plots data - continuous
balance.plot.data.con <- df.weights.output %>%
  select(Group, Continuous.characteristics, wt) %>%
  mutate(Analysis = "Weighted") %>%
  rbind(select(df.weights.output, Group, Continuous.characteristics, wt) %>%
          mutate(Analysis = "Unweighted",
                 wt = 1))

balance.plot.data.con[balance.plot.data.con$Analysis == "Unweighted",]$Analysis <- "Before weighting"
balance.plot.data.con[balance.plot.data.con$Analysis == "Weighted",]$Analysis <- "After weighting"
balance.plot.data.con$Analysis <- factor(balance.plot.data.con$Analysis, levels = c("Before weighting", "After weighting"))


for(i in 1:length(Continuous.characteristics)){
  
  name <- filter(formatted.names, name == Continuous.characteristics[i])$formatted
  message(paste0("Produce ", name," density plot"))
  
  bal.plot <- ggplot(balance.plot.data.con, aes_string(x = Continuous.characteristics[i], weights = "wt")) +
    geom_density(aes(fill = Group), alpha = 0.4, position="identity", color = "black") +
    scale_fill_manual(values = c(BM.blue, BM.pink)) +
    xlab(name) +
    ylab("Density") +
    theme_bw() +
    theme(legend.title = element_blank(),
          axis.title = element_text(size=14),
          axis.text = element_text(size=12),
          title = element_text(size = 14),
          legend.text = element_text(size = 12),
          strip.text = element_text(size = 12),
          panel.spacing.x = unit(5, "mm")) +
    facet_wrap("Analysis")
  
  jpeg(file.path(res.dir, paste0("Plots/Balance plot_", name,".jpg")),
       width = 25, height = 15,
       units = 'cm', res = 300, quality=100)
  print(bal.plot)
  dev.off()
  
}

