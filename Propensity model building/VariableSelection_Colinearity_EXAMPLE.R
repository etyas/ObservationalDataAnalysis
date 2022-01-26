message("Load packages")
library(dplyr)
library(corrplot)
library(pedometrics)
library(ggcorrplot)

# Bresmed colours
BM.lightPink<-rgb(229,151,192,max=255)
BM.blue = rgb(69, 185, 209, max=255)

# Set file paths
base.dir <- ""
data.dir <- file.path(base.dir, "")
output.dir <- file.path(base.dir, "")

message("Load data")
Surv.data <- read.csv(file.path(data.dir, "")) %>%
  mutate(AGEGR3= ifelse(AAGE < 60 , as.character("< 60 years"), ifelse(AAGE >= 60, as.character(">=60 years"), NA)), 
         AGEGR4= ifelse(AAGE < 40 , as.character("< 40 years"), ifelse(AAGE >= 40 & AAGE < 60, as.character("40 to < 60 years"), as.character(">=60 years")))) %>%
  mutate_at(vars(c(AGEGR1, AGEGR2, AGEGR3, AGEGR4, SEX, RACE, BMIGR1, MUFCGR1, PRIMED, PRIRRD, STATCDIS, TIMEDIAGGR, TIMESURGR)), as.factor)

Surv.data$MUFCGR1 <- factor(Surv.data$MUFCGR1, levels = c("< 300 nmol/24h", "300 to < 1,000 nmol/24h", ">=1,000 nmol/24h"))
Surv.data$MUFCGR2 <- factor(Surv.data$MUFCGR2, levels = c("< 2.0 x ULN", "2.0 x ULN to < 5.0 x ULN", ">=5.0 x ULN"))
Surv.data$MUFCGR3 <- factor(Surv.data$MUFCGR3, levels = c("< 2.0 x ULN", "2.0 x ULN to < 5.0 x ULN", ">=5.0 x ULN"))

subgroups <- select(Surv.data,
                    #"AGEGR1",
                    #"AGEGR2", 
                    #"AGEGR3",
                    "AGEGR4",
                    #"SEX",
                    #"RACEGR1",
                    #"BMIGR1",
                    #"MUFCGR1",
                    "MUFCGR2.138",
                    #"MUFCGR3.166",
                    "TIMESURGR",
                    #"PRIMED", 
                    #"PRIRRD",
                    #"STATCDIS",
                    "TIMEDIAGGR"
                    ) %>%
  mutate(MUFCGR2.138 = ifelse(is.na(MUFCGR2.138), "Missing", MUFCGR2.138)) %>%
  mutate_all(as.factor)

subgroups.full <- select(Surv.data,
                    #"AGEGR1",
                    #"AGEGR2", 
                    #"AGEGR3",
                    "AGEGR4",
                    #"SEX",
                    #"RACEGR1",
                    #"BMIGR1",
                    #"MUFCGR1",
                    "MUFCGR2.138",
                    #"MUFCGR3.166",
                    "TIMESURGR",
                    #"PRIMED", 
                    #"PRIRRD",
                    #"STATCDIS",
                    "TIMEDIAGGR",
                    "RACE",
                    "PRIMED"
) %>%
  mutate(RACE = ifelse(RACE == "WHITE", "White", "Other"),
         MUFCGR2.138 = ifelse(is.na(MUFCGR2.138), "Missing", MUFCGR2.138)) %>%
  mutate_all(as.factor)

## VARIABLES FOR 0.1 CUT OFF IN P-VALUE ----------------------------
#### create data matrix
subgroup.matrix <- model.matrix(~ . - 1, data=subgroups,
                     contrasts.arg = lapply(subgroups, contrasts, contrasts = FALSE))

# correlation matrix
corr <- cor(subgroup.matrix)

write.csv(print(corr), file.path(output.dir, "Correlation matrix.jpeg"))

#### plot
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

jpeg(file.path(output.dir, "Correlation plot.jpeg"), width = 25, height = 15,
     units = 'cm', res = 300, quality=100)

corr %>%
  ggcorrplot(show.diag = F,
           type = "lower",
           lab = TRUE, lab_size = 3.5) +
  scale_fill_gradient2(high = BM.lightPink, low = BM.blue, limits = c(-1,1), name = "Correlation coefficient") +
  scale_x_discrete(labels = c("Age (\u2265 60 years)", "Age (40 to < 60 years)", "mUFC (< 2.0 x ULN)", "mUFC (\u2265 5.0 x ULN)", "mUFC (2.0 x ULN to < 5.0 x ULN)", "mUFC (missing)", "Prior surgery (N)", "Prior surgery (\u2265 43 months ago)", "Prior surgery (< 43 months ago)", "Time since diagnosis (< 47 months)", "Time since diagnosis (\u2265 47 months)")) +
  scale_y_discrete(labels = c("Age < 40 years", "Age (\u2265 60 years)", "Age (40 to < 60 years)", "mUFC (< 2.0 x ULN)", "mUFC (\u2265 5.0 x ULN)", "mUFC (2.0 x ULN to < 5.0 x ULN)", "mUFC (missing)", "Prior surgery (N)", "Prior surgery (\u2265 43 months ago)", "Prior surgery (< 43 months ago)", "Time since diagnosis (< 47 months)")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 14),
        axis.title = element_blank())
graphics.off()