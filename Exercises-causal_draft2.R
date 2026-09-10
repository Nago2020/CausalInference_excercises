
# Clear workspace
rm(list = ls())

# Load dependencies
library(mediation)
library(dplyr)

# View the data
View(jobs)

#### Exercise 1: Inspecting and preparing the data ####

# Check the documentation
?jobs

# Only keep the required variables
jobs <- jobs[, c(
  "treat",
  "comply",
  "depress1",
  "econ_hard",
  "sex",
  "depress2"
)]

# Rename the variables
jobs <- jobs %>% rename(
  assignment       = treat,
  participation    = comply,
  depression_before = depress1,
  depression_after  = depress2
)

# Drop treatment assignment from the data (but keep it for later)
assignment <- jobs$assignment
jobs <- jobs[, colnames(jobs) != "assignment"]

#### Exercise 2: Average treatment effect ####

# 1. Linear regression without interaction
model_1 <- lm(
  depression_after ~ participation + depression_before,
  data = jobs
)
summary(model_1)

# Estimated effect of participation
coef(model_1)["participation"]

# 2. Linear regression with interaction
model_2 <- lm(
  depression_after ~ participation * depression_before,
  data = jobs
)
summary(model_2)

# Compute the estimated ATE:
# beta_1 + beta_3 * mean(depression_before)
beta_1 <- coef(model_2)["participation"]
beta_3 <- coef(model_2)["participation:depression_before"]
beta_1 + beta_3 * mean(jobs$depression_before)

#### Exercise 3: ... ####

# ToDo

#### Exercise 4: ... ####

# 0. Running the same steps as in Exercise 1 but not removing the treatment
#    assignment variable

# This can be achieved by simply adding the variable back to the data set
jobs$assignment <- assignment

# 1. Investigate non-compliance
sum(jobs$assignment == 0 & jobs$participation == 1)
sum(jobs$assignment == 0 & jobs$participation == 0)
sum(jobs$assignment == 1 & jobs$participation == 1)
sum(jobs$assignment == 1 & jobs$participation == 0)
# No-one who was assigned to the control arm actually received treatment
# All subjects assigned to the control arm did not receive treatment
# 228 subject assigned to treatment arm did not actually participate.
# --> One-sided non-compliance

# Alternatively, we could have simply called
table(jobs$assignment, jobs$participation)

# 2. Test for instrument relevance
first.stage.Ftest <- lm(participation ~ assignment + depression_before + econ_hard + sex, data = jobs)
summary(first.stage.Ftest)
# We find t-value = 22.009, so F = (22.009)^2 = 484.400 >> 10.
# Hence, the instrument is relevant




