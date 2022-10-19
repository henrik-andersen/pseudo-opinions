
# Survey Research Methods 
# Replication file for: 
# Pseudo-Opinions in Online Surveys: Evidence to Recontextualize the Imputed Meaning Hypothesis
# Henrik K. Andersen, Jochen Mayerl, Felix Wolter, & Justus Junkermann
# 19 October 2022 

# Needed data files
# - dfl.Rda
# - dfw.Rda
# available at: https://github.com/henrik-andersen/pseudo-opinions

# Packages ----------------------------------------------------------------

# install.packages("lme4")
# install.packages("margins")
# install.packages("ggplot2")
# install.packages("dplyr")

library(lme4)
library(margins)
library(ggplot2)
library(dplyr)

# Setup -------------------------------------------------------------------

# Clear working directory 
rm(list = ls())

# Fill in path to folder containing files dfl.Rda and dfw.Rda 
setwd("F:/nextcloud/Jochen-Henrik/Papers/fictitious-issues/final-files-publication") # Add path to files 

# Add path to files 
dfl = readRDS("dfl.Rda")
dfw = readRDS("dfw.Rda")


# Data preparation  -------------------------------------------------------

# --- Outlier treatment 

# Remove any RLs less than 0 
dfl$rl <- ifelse(dfl$rl < 0, NA, dfl$rl)

# Make into seconds 
dfl$rl <- dfl$rl / 1000

# Option 1: remove greater then 2000 seconds, then RLs > mean RL + 2sd
dfl$rl_out <- ifelse(dfl$rl > 2000, NA, dfl$rl)
dfl$rl_out <- ifelse(dfl$rl > mean(dfl$rl, na.rm = TRUE) + 2 * sd(dfl$rl, na.rm = TRUE), NA, dfl$rl)

# Option 2: remove RLs > median RL + 2sd 
# dfl$rl_out <- ifelse(dfl$rl > median(dfl$rl, na.rm = TRUE) + 2 * sd(dfl$rl, na.rm = TRUE), NA, dfl$rl)

# --- Create RL measures 

# Aggregate RLs per id
mean_rls_id <- aggregate(dfl$rl_out, by = list(dfl$id), FUN = mean, na.rm = TRUE, na.action = "na.pass")
names(mean_rls_id) <- c("id", "rl_out_m_id")
dfl <- merge(dfl, mean_rls_id, by = "id")

# Aggregate RLs per item
mean_rls_item <- aggregate(dfl$rl_out, by = list(dfl$item_fac), FUN = mean, na.rm = TRUE, na.action = "na.pass")
names(mean_rls_item) <- c("item_fac", "rl_out_m_item")
dfl <- merge(dfl, mean_rls_item, by = "item_fac")
 
# Demean RLs 
dfl$rl_out_demean <- dfl$rl_out - dfl$rl_out_m_id

# --- Center control variables 

dfl$nsa_c <- dfl$nsa - mean(dfl$nsa, na.rm = TRUE)
dfl$anon_c <- dfl$anon - mean(dfl$anon, na.rm = TRUE)
dfl$yob_c <- dfl$yob - mean(dfl$yob, na.rm = TRUE)
dfl$polint_c <- dfl$polint - mean(dfl$polint, na.rm = TRUE)
dfl$lr_c <- dfl$lr - mean(dfl$lr, na.rm = TRUE)

# --- Other recodes 

# Make numeric DV
dfl$response_d <- as.numeric(dfl$response) - 1

# Change dummy coding for experiment 
dfl$exp <- relevel(dfl$exp, ref = "implicit DK")


# Separate dataset into fictitious and real items  ------------------------

# Fictitious issues 
dflfi <- dfl[dfl$fi == 1, ]

# Non-fictitious issues 
dflnfi <- dfl[dfl$fi != 1, ]


# Histograms for Figure 1 -------------------------------------------------

ggplot(dfl, aes(x = rl_out_demean)) +
  geom_histogram(color = "black", fill = "grey") +
  theme_bw() +
  scale_x_continuous(name = "Within-unit RL") +
  scale_y_continuous(name = "") 
ggsave("fig-1a.pdf", device = "pdf", dpi = 600)

ggplot(dfl, aes(x = rl_out_m_id)) +
  geom_histogram(color = "black", fill = "grey") +
  theme_bw() +
  scale_x_continuous(name = "Between-unit RL") +
  scale_y_continuous(name = "") 
ggsave("fig-1b.pdf", device = "pdf", dpi = 600)


# Distributions for Table 1 -----------------------------------------------

# Environmental Court (EC)
dfw$EC_impl_dk = dplyr::coalesce(dfw$FI01_04orig, dfw$FI03_04orig)
dfw$EC_expl_dk = dplyr::coalesce(dfw$FI02_04orig, dfw$FI04_04orig)

prop.table(table(dfw$EC_expl_dk)); prop.table(table(dfw$EC_impl_dk))

# Coastal Aid Agency (CAA)
dfw$CAA_impl_dk = dplyr::coalesce(dfw$FI01_06orig, dfw$FI03_06orig)
dfw$CAA_expl_dk = dplyr::coalesce(dfw$FI02_06orig, dfw$FI04_06orig)

prop.table(table(dfw$CAA_expl_dk)); prop.table(table(dfw$CAA_impl_dk))

# Prague Energy Transition Initiative (PETI)
dfw$PETI_impl_dk = dplyr::coalesce(dfw$FI01_08orig, dfw$FI03_08orig)
dfw$PETI_expl_dk = dplyr::coalesce(dfw$FI02_08orig, dfw$FI04_08orig)

prop.table(table(dfw$PETI_expl_dk)); prop.table(table(dfw$PETI_impl_dk))

# German Nuclear Forum (GNF)
dfw$GNF_impl_dk = dplyr::coalesce(dfw$FI01_09orig, dfw$FI03_09orig)
dfw$GNF_expl_dk = dplyr::coalesce(dfw$FI02_09orig, dfw$FI04_09orig)

prop.table(table(dfw$GNF_expl_dk)); prop.table(table(dfw$GNF_impl_dk))

# Herbert-Schmaar Foundation (HSF)
dfw$HSF_impl_dk = dplyr::coalesce(dfw$FI01_13orig, dfw$FI03_13orig)
dfw$HSF_expl_dk = dplyr::coalesce(dfw$FI02_13orig, dfw$FI04_13orig)

prop.table(table(dfw$HSF_expl_dk)); prop.table(table(dfw$HSF_impl_dk))

# World Space Agency (WSA)
dfw$WSA_impl_dk = dplyr::coalesce(dfw$FI01_15orig, dfw$FI03_15orig)
dfw$WSA_expl_dk = dplyr::coalesce(dfw$FI02_15orig, dfw$FI04_15orig)

prop.table(table(dfw$WSA_expl_dk)); prop.table(table(dfw$WSA_impl_dk))


# Models  -----------------------------------------------------------------

m1 = glmer(response_d ~ 
              exp +
              rl_out_demean + 
              rl_out_m_id +
              I(rl_out_demean^2) + 
              I(rl_out_m_id^2) +
              item_fac + 
              (1 | id),
            data = dflfi, 
            nAGQ = 0, family = binomial(link = "probit"))
summary(m1)
margins_m1 = margins(m1)
sum_margins_m1 = summary(margins_m1); sum_margins_m1

m2 = glmer(response_d ~ 
              exp +
              rl_out_demean + 
              rl_out_m_id +
              I(rl_out_demean^2) + 
              I(rl_out_m_id^2) +
              yob_c + male + german + educ +
              polint_c +
              academic + job + nrsurvey + lr_c +
              item_fac + 
              (1 + rl_out_demean | id),
            data = dflfi, 
            nAGQ = 0, family = binomial(link = "probit"))
summary(m2)
margins_m2 = margins(m2)
sum_margins_m2 = summary(margins_m2); sum_margins_m2

m3 = glmer(response_d ~ 
              exp +
              rl_out_demean + 
              rl_out_m_id +
              I(rl_out_demean^2) + 
              I(rl_out_m_id^2) +
              exp:rl_out_demean +
              exp:rl_out_m_id +
              exp:I(rl_out_demean^2) +
              exp:I(rl_out_m_id^2) +
              yob_c + male + german + educ +
              polint_c +
              academic + job + nrsurvey + lr_c +
              item_fac + 
              (1 + rl_out_demean | id),
            data = dflfi, 
            nAGQ = 0, family = binomial(link = "probit"))
summary(m3)
margins_m3 = margins(m3)
sum_margins_m3 <- summary(margins_m3); sum_margins_m3



# Marginal effects for Figure 2 -------------------------------------------

sum_margins_m1 <- subset(sum_margins_m1,
                         factor == "expexplicit DK" | 
                           factor == "item_facCAA" |
                           factor == "item_facGNF" |
                           factor == "item_facHSF" |
                           factor == "item_facPETI" |
                           factor == "item_facWSA" |
                           factor == "rl_out_demean" |
                           factor == "rl_out_m_id")

sum_margins_m2 <- subset(sum_margins_m2,
                         factor == "expexplicit DK" | 
                           factor == "item_facCAA" |
                           factor == "item_facGNF" |
                           factor == "item_facHSF" |
                           factor == "item_facPETI" |
                           factor == "item_facWSA" |
                           factor == "rl_out_demean" |
                           factor == "rl_out_m_id")

sum_margins_m3 <- subset(sum_margins_m3,
                         factor == "expexplicit DK" | 
                           factor == "item_facCAA" |
                           factor == "item_facGNF" |
                           factor == "item_facHSF" |
                           factor == "item_facPETI" |
                           factor == "item_facWSA" |
                           factor == "rl_out_demean" |
                           factor == "rl_out_m_id")

sum_margins_m1$model <- 1
sum_margins_m2$model <- 2
sum_margins_m3$model <- 3

sum_margins <- rbind(sum_margins_m1, sum_margins_m2, sum_margins_m3)
sum_margins$model <- factor(sum_margins$model, levels = 1:3, labels = c("Model 1", "Model 2", "Model 3"))
sum_margins

ggplot(sum_margins, aes(x = factor, y = AME, color = factor(model))) + 
  geom_point(size = 2, position = position_dodge(width = 0.9)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.5, position = position_dodge(width = 0.9)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme_bw() +
  scale_x_discrete(name = "", labels = c("Explicit DK", 
                                                 "Item: CAA", "Item: GNF", "Item: HSF", "Item: PETI", "Item: WSA", 
                                                 "Within-unit RL", "Between-unit RL")) + 
  scale_color_discrete(name = "") + 
  coord_flip() 
ggsave("fig-2.pdf", device = "pdf", dpi = 600)

# Subgroup analysis -------------------------------------------------------

# For the subgroup analysis, M1 was estimated separately for each level of 
# the control variables. This entails estimating many models and the entire code
# is not shown here. Rather, for each control variable, check the levels using 
# table() and then change the model code accordingly for the specific subgroup. 

# --- Control variables 
# Sex: male
# Year of birth: yob_c
# Citizenship: german 
# Education: educ
# Academic: academic_c
# Employment: job
# Number surveys completed: nrsurvey

# Check levels here
table(dflfi$male)

# Change subset for specific levels below
glmer(response_d ~ 
        exp + 
        rl_out_demean + 
        rl_out_m_id +
        I(rl_out_demean^2) + 
        I(rl_out_m_id^2) + 
        item_fac + 
        (1 | id), 
      data = subset(dflfi, dflfi$male == "female"),     # Change variable and level here  
      nAGQ = 0, family = binomial(link = "probit")) |> 
  margins() |>
  summary()


