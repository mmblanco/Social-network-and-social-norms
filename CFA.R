library('stats')
library('lavaan')
library('resample')
library('readxl')
library('naniar')

#---------------------- FACTOR ANALYSIS - PRE ----------------------
#--------------- DESCRIPTIVE EXP ---------------
all = read_excel("v4_pre_input_R_factorAnalysis.xlsx", sheet = "LATENT", range = "A1:J1621")

rownames(all) = all$StudyID
colnames(all)
all = subset(all, select=-c(StudyID, School, I_INJUCTIVE, CGI_TOBCONTROL, CGI_SUPCONTROL, CGI_ADVCONTROL, CGI_PEERS, CGD_PEERS))
colnames(all)
all = all %>% replace_with_na_all(condition = ~.x == 9999)
all = na.omit(all)

# CONFIRMATORY FACTOR ANALYSIS!
colnames(all)
HS.model <- ' descriptive  =~ D_PEERS + D_FAMILY '

# fit the GLOBAL model
fit <- cfa(HS.model, data=all)
summary(fit, fit.measures=TRUE, standardized = TRUE)

#--------------- DESCRIPTIVE CG ---------------
all = read_excel("v4_pre_input_R_factorAnalysis.xlsx", sheet = "LATENT", range = "A1:J1621")

rownames(all) = all$StudyID
colnames(all)
all = subset(all, select=-c(StudyID, School, I_INJUCTIVE, CGI_TOBCONTROL, CGI_SUPCONTROL, CGI_ADVCONTROL, CGI_PEERS, D_PEERS, D_FAMILY))
colnames(all)
all = all %>% replace_with_na_all(condition = ~.x == 9999)
all = na.omit(all)

# CONFIRMATORY FACTOR ANALYSIS!
colnames(all)
HS.model <- ' descriptive  =~ CGD_PEERS'

# fit the GLOBAL model
fit <- cfa(HS.model, data=all)
summary(fit, fit.measures=TRUE, standardized = TRUE)

#--------------- INJUCTIVE_EXP ---------------
all = read_excel("v4_pre_input_R_factorAnalysis.xlsx", sheet = "LATENT", range = "A1:J1621")

rownames(all) = all$StudyID
colnames(all)
all = subset(all, select=-c(StudyID, School, D_PEERS, D_FAMILY, CGD_PEERS, CGI_TOBCONTROL, CGI_SUPCONTROL, CGI_ADVCONTROL, CGI_PEERS))
colnames(all)
all = all %>% replace_with_na_all(condition = ~.x == 9999)
all = na.omit(all)

# CONFIRMATORY FACTOR ANALYSIS!
colnames(all)
HS.model <- ' injuctive  =~ I_INJUCTIVE '

# fit the GLOBAL model
fit <- cfa(HS.model, data=all)
summary(fit, fit.measures=TRUE, standardized = TRUE)

#--------------- INJUCTIVE_CG ---------------
all = read_excel("v4_pre_input_R_factorAnalysis.xlsx", sheet = "LATENT", range = "A1:J1621")

rownames(all) = all$StudyID
colnames(all)
all = subset(all, select=-c(StudyID, School, D_PEERS, D_FAMILY, CGD_PEERS, I_INJUCTIVE))
colnames(all)
all = all %>% replace_with_na_all(condition = ~.x == 9999)
all = na.omit(all)

# CONFIRMATORY FACTOR ANALYSIS!
colnames(all)
HS.model <- ' injuctive  =~ CGI_TOBCONTROL + CGI_SUPCONTROL + CGI_ADVCONTROL + CGI_PEERS'

# fit the GLOBAL model
fit <- cfa(HS.model, data=all)
summary(fit, fit.measures=TRUE, standardized = TRUE)


#---------------------- FACTOR ANALYSIS - POST ----------------------
#--------------- DESCRIPTIVE survey ---------------
all = read_excel("v4_post_input_R_factorAnalysis.xlsx", sheet = "LATENT", range = "A1:J1621")

rownames(all) = all$StudyID
colnames(all)
all = subset(all, select=-c(StudyID, School, I_INJUCTIVE, CGI_TOBCONTROL, CGI_SUPCONTROL, CGI_ADVCONTROL, CGI_PEERS, CGD_PEERS))
colnames(all)
all = all %>% replace_with_na_all(condition = ~.x == 9999)
all = na.omit(all)

# CONFIRMATORY FACTOR ANALYSIS!
colnames(all)
HS.model <- ' descriptive  =~ D_PEERS + D_FAMILY '

# fit the GLOBAL model
fit <- cfa(HS.model, data=all)
summary(fit, fit.measures=TRUE, standardized = TRUE)

#--------------- DESCRIPTIVE experiment ---------------
all = read_excel("v4_post_input_R_factorAnalysis.xlsx", sheet = "LATENT", range = "A1:J1621")

rownames(all) = all$StudyID
colnames(all)
all = subset(all, select=-c(StudyID, School, I_INJUCTIVE, CGI_TOBCONTROL, CGI_SUPCONTROL, CGI_ADVCONTROL, CGI_PEERS, D_PEERS, D_FAMILY))
colnames(all)
all = all %>% replace_with_na_all(condition = ~.x == 9999)
all = na.omit(all)

# CONFIRMATORY FACTOR ANALYSIS!
colnames(all)
HS.model <- ' descriptive  =~ CGD_PEERS'

# fit the GLOBAL model
fit <- cfa(HS.model, data=all)
summary(fit, fit.measures=TRUE, standardized = TRUE)

#--------------- INJUCTIVE_survey ---------------
all = read_excel("v4_post_input_R_factorAnalysis.xlsx", sheet = "LATENT", range = "A1:J1621")

rownames(all) = all$StudyID
colnames(all)
all = subset(all, select=-c(StudyID, School, D_PEERS, D_FAMILY, CGD_PEERS, CGI_TOBCONTROL, CGI_SUPCONTROL, CGI_ADVCONTROL, CGI_PEERS))
colnames(all)
all = all %>% replace_with_na_all(condition = ~.x == 9999)
all = na.omit(all)

# CONFIRMATORY FACTOR ANALYSIS!
colnames(all)
HS.model <- ' injuctive  =~ I_INJUCTIVE '

# fit the GLOBAL model
fit <- cfa(HS.model, data=all)
summary(fit, fit.measures=TRUE, standardized = TRUE)

#--------------- INJUCTIVE_experiment ---------------
all = read_excel("v4_post_input_R_factorAnalysis.xlsx", sheet = "LATENT", range = "A1:J1621")

rownames(all) = all$StudyID
colnames(all)
all = subset(all, select=-c(StudyID, School, D_PEERS, D_FAMILY, CGD_PEERS, I_INJUCTIVE))
colnames(all)
all = all %>% replace_with_na_all(condition = ~.x == 9999)
all = na.omit(all)

# CONFIRMATORY FACTOR ANALYSIS!
colnames(all)
HS.model <- ' injuctive  =~ CGI_TOBCONTROL + CGI_SUPCONTROL + CGI_ADVCONTROL + CGI_PEERS'

# fit the GLOBAL model
fit <- cfa(HS.model, data=all)
summary(fit, fit.measures=TRUE, standardized = TRUE)
