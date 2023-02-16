##### Load Data, Create Variables #####
dat <- read.csv("Data/analytic_20220210.csv", stringsAsFactors = F)
dat <- dat[,-1]

# Create categorical variables
dat <- dat %>% 
  mutate(svi_rank_q = factor(svi_rank_q),
         GEOID = factor(GEOID),
         sex = factor(sex),
         birth_year = scale(year(as.Date(dob))),
         ga_NY = scale(ga_NY),
         edip_insur = factor(edip_insur),
         los = scale(los),
         bpd = factor(bpd),
         race_NY = factor(race_NY,
                          levels = c("White", "Black", "Other/Missing")),
         eth_NY = factor(eth_NY,
                         levels = c("Not Hispanic or Latino", "Hispanic or Latino")),
         svi_rank_q4 = ifelse(svi_rank_q == "Q4", 1, 0),
         svi_rank_sd = svi_rank/sd(svi_rank),
         svi_rank_t = cut(svi_rank, breaks = quantile(dat$svi_rank, probs = seq(0,1,1/3), na.rm = T), 
                          include.lowest = T,
                          labels = c("T1", "T2", "T3")),
         race_2 = ifelse(race_NY == "Black", "Black", "Non-Black"),
         raceeth_NY = factor(case_when(race_NY == "White" & eth_NY == "Not Hispanic or Latino" ~ "NHW",
                                       race_NY == "Black" & eth_NY == "Not Hispanic or Latino" ~ "NHB",
                                       TRUE ~ "Other"),
                             levels = c("NHW", "NHB", "Other"))
  )


for(i in 1:nrow(dat)){
    if(dat$city[i] %in% c("PHILA","PHILADELPHIA")){ dat$loc[i] <- "cphl" }
    else if(dat$metrophl[i] == 1) { dat$loc[i] <- "mphl" }
    else { dat$loc[i] <- "other" }
    
    if(dat$ed_visit[i] == 1){dat$readm_multi[i] <- "ed_visit"}
    else if(dat$ip_readm[i] == 1){dat$readm_multi[i] <- "ip_readm"}
    else{dat$readm_multi[i] <- "no_readm"}
}

dat$loc_ind = ifelse(dat$loc == "mphl", 1, 0)

dat$readm_multi <- relevel(as.factor(dat$readm_multi), ref = "no_readm")

# Subset to metro philly cohort
dat_mphl <- dat %>% filter(metrophl == 1)

# Subset to city of philly cohort
dat_cphl <- dat %>% filter(city %in% c("PHILA","PHILADELPHIA"))

## Functions for model fitting
fit_logit <- function(targetVar, ds){
    cat(paste0("Model 0: ", targetVar, " only\n"))
    mod0_formula <- reformulate(termlabels = targetVar, response = "readm_flag")
    logit_mod0 <- glm(mod0_formula, data = ds, family = binomial())
    print(logit_res(logit_mod0))
    
    cat("\n\n")

    cat(paste0("Model 1: ", targetVar, " and control variables\n"))
    controlVars <- c("ga_NY", "sex", "birth_year", "bpd", "edip_insur")
    mod1_formula <- reformulate(termlabels = c(targetVar, controlVars), response = "readm_flag")
    logit_mod1 <- glm(mod1_formula, data = ds, family = binomial())
    print(logit_res(logit_mod1))

    cat("\n\n")

    cat(paste0("Model 2: ", targetVar, ", control variables, and race/ethnicity variables\n"))
    mod2_formula <- reformulate(termlabels = c(targetVar, controlVars, "raceeth_NY"), response = "readm_flag")
    logit_mod2 <- glm(mod2_formula, data = ds, family = binomial())
    print(logit_res(logit_mod2))
}

fit_multinom <- function(targetVar, ds){
    cat(paste0("Model 0: ", targetVar, " only\n"))
    mod0_formula <- reformulate(termlabels = targetVar, response = "readm_multi")
    multinom_mod0 <- multinom(mod0_formula, data = ds)
    print(multinom_res(multinom_mod0, targetVar))

    cat("\n\n")

    cat(paste0("Model 1: ", targetVar, " and control variables\n"))
    controlVars <- c("ga_NY", "sex", "birth_year", "bpd", "edip_insur")
    mod1_formula <- reformulate(termlabels = c(targetVar, controlVars), response = "readm_multi")
    multinom_mod1 <- multinom(mod1_formula, data = ds)
    print(multinom_res(multinom_mod1, targetVar))

    cat("\n\n")

    cat(paste0("Model 2: ", targetVar, ", control variables, and race/ethnicity variables\n"))
    mod2_formula <- reformulate(termlabels = c(targetVar, controlVars, "raceeth_NY"), response = "readm_multi")
    multinom_mod2 <- multinom(mod2_formula, data = ds)
    print(multinom_res(multinom_mod2, targetVar))
}

## Function for getting robust standard error results
logit_res <- function(modfit){
    
    modsum <- summary(modfit)
    modsum_tab <- modsum$coefficients
    
    q.val <- qnorm(0.975)

    r.est <- as.data.frame(cbind(
      Estimate = coef(modfit),
      OR = exp(coef(modfit)),
      SE = modsum_tab[,"Std. Error"],
      "Pr(>|z|) " = modsum_tab[,"Pr(>|z|)"]
    ))
    
    r.est$OR_LL <- exp(r.est$Estimate - q.val * r.est$SE)
    r.est$OR_UL <- exp(r.est$Estimate + q.val * r.est$SE)

    return(r.est)
}

# multinom_res <- function(modfit){
#   coefs <- exp(coef(modfit))
#   z <- summary(modfit)$coefficients/summary(modfit)$standard.errors
#   p <- (1 - pnorm(abs(z), 0, 1)) * 2
#   
#   
#   return(list(odds_ratio = coefs, p = p))
# }

multinom_res <- function(modfit, targetVar){
    modSum <- summary(modfit)
    odds_ratios <- exp(modSum$coefficients)
    z <- modSum$coefficients/modSum$standard.errors
    p <- (1 - pnorm(abs(z), 0, 1)) * 2
    
    # Match columns with the same starting pattern as the target variable (needed for when target variable is a factor)
    c <- grep(paste0("^", targetVar), colnames(modSum$coefficients), value = TRUE)
    
    # Get coefficients and standard errors
    coefs <- modSum$coefficients[,c]
    ses <- modSum$standard.errors[,c]
    
    if(length(c) > 1){ 
        resp <- rownames(coefs)
        
        coefs <- data.frame(response = resp, coefs)
        coefs <- gather(coefs, variable, coefficient, -c(response))
        
        ses <- data.frame(response = resp, ses)
        ses <- gather(ses, variable, standard.error, -c(response))
        
        targetVarData <- merge(coefs, ses, by = c('variable', 'response'))
        
    } else{
        targetVarData <- data.frame(variable = targetVar, response = names(coefs), coefficient = coefs, standard.error = ses, row.names = NULL)
    }
    
    
    # Calculate the upper and lower limits for the 95% CI of the odds ratio
    targetVarData$OR_LL <- exp(targetVarData$coefficient - qnorm(0.975) * targetVarData$standard.error)
    targetVarData$OR_UL <- exp(targetVarData$coefficient + qnorm(0.975) * targetVarData$standard.error)
    targetVarData <- targetVarData[,c('variable','response','OR_LL','OR_UL')]
    
    return(list(odds_ratio = odds_ratios, p = p, OR_CI = targetVarData))
}

mediation <- function(targetVar, mediator, response, ds){
    # controlVars <- c("ga_NY", "sex", "birth_year", "bpd", "edip_insur")
    fit.mediator <- lm(formula = reformulate(termlabels = c(c("ga_NY", "sex", "birth_year", "bpd", "edip_insur"), mediator), response = targetVar), data = ds)
    cat("Mediator Fit Summary:")
    print(summary(fit.mediator))
    
    dv_mod <- reformulate(termlabels = c(targetVar, c("ga_NY", "sex", "birth_year", "bpd", "edip_insur"), mediator), response = response)
    fit.dv <- glm(dv_mod, data = ds, family = binomial())
    cat("DV Fit Summary:")
    print(logit_res(fit.dv))
    
    med <- mediate(fit.mediator, fit.dv,
                treat = mediator, mediator = targetVar,
                covariates = c("ga_NY", "sex", "birth_year", "bpd", "edip_insur"),
                boot = T,
                robustSE = T)
    cat("Mediation Summary:")
    summary(med)
}
