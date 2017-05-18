library (foreign); library(psych); library(randomForest); library(stargazer)
InstitutionsData <- read.dta("MeasuringInstitutionsRecognizedStates20170314.dta")
attach(InstitutionsData)
stargazer(as.data.frame(cbind(pitf_eth, pitf_rev, frac, polariz, yrsoffc, stabs, herfgov, checks, liec, eiec, 
                              fraud, polity2, durable, domestic1, domestic2, domestic4, domestic5, domestic6, 
                              domestic8, polit03, polit04, polit11, polit12, polit15, Currencycrises, Inflationcrises, 
                              Sovereigndebtcrisesdomestic, Bankingcrises)), 
          type = "html", out = "Table1SumStats.html", title = "Table 1: Summary Statistics")
Institutions.EFA <- fa(cbind(pitf_eth, pitf_rev, frac, polariz, yrsoffc, stabs, herfgov, checks, 
                       liec, eiec, fraud, polity2, durable, domestic1, domestic2, domestic3, domestic4,  
                       domestic6, domestic8, polit03, polit04, polit11, polit12, polit15, Currencycrises, 
                       Inflationcrises, Sovereigndebtcrisesdomestic, Bankingcrises), 
              nfactors = 9, fm = "gls", rotate = "varimax", max.iter = 500)
rownames(Institutions.EFA$loadings) <- c("Ethnic Conflict", "Revolutionary Conflict", "Legislative Fractionalization", 
                                         "Government Polarization", "Executive Tenure", "Change in Veto Players", 
                                         "Government Concentration", "Checks on Power", 
                                         "Legislative Electoral Competition", "Executive Electoral Competition", 
                                         "Electoral Fraud", "Polity2 Index", "Regime Durability", 
                                         "Assassinations", "General Strikes", "Government Crises", "Political Purges", 
                                         "Riots", "Anti-Government Demonstrations", "Coups d'Etat", "Constitutional Changes", 
                                         "Number of Cabinet Changes", "Changes in Executive", "Legislative Elections", 
                                         "Currency Crises", "Inflation Crises", "Domestic Sovereign Debt Crises", 
                                         "Banking Crises")
colnames(Institutions.EFA$loadings) <- c("Democracy", "Protest", "Within", "Credibility", 
                                         "Adverse", "Flexibility", "Regime", "Violence", "Transparency")
colnames(Institutions.EFA$scores) <- c("Democracy", "Protest", "Within", "Credibility", 
                                       "Adverse", "Flexibility", "Regime", "Violence", "Transparency")
print(Institutions.EFA$loadings)
fa.diagram(Institutions.EFA, e.size = 0.04)
InstitutionsData$Stability.C1 <- princomp(~ pitf_eth + pitf_rev + domestic1NM + domestic2NM + domestic3NM + 
                                            domestic4NM + domestic5NM + domestic6NM + domestic7NM + 
                                            domestic8NM + fraudNM + polit03NM + polit04NM,
                                          cor = TRUE)$scores[,1] 
attach(InstitutionsData)
set.seed(8976)
InstitutionsData.rfImpute <- rfImpute(Stability.C1 ~ pitf_eth + pitf_rev + frac + polariz + yrsoffc + stabs + 
                                        herfgov + checks + liec + eiec + fraud + polity2 + durable + 
                                        domestic1 + domestic2 + domestic3 + domestic4 + domestic5 + 
                                        domestic6 + domestic7 + domestic8 + polit03 + polit04 + 
                                        polit11 + polit12 + polit15 + Currencycrises + Inflationcrises + 
                                        Sovereigndebtcrisesdomestic + Bankingcrises, iter = 10, ntree = 500)
InstitutionsData.rfImpute <- cbind(wbcode, year, InstitutionsData.rfImpute)
detach(InstitutionsData)
attach(InstitutionsData.rfImpute)
stargazer(as.data.frame(cbind(pitf_eth, pitf_rev, frac, polariz, yrsoffc, stabs, herfgov, checks, liec, eiec, 
                              fraud, polity2, durable, domestic1, domestic2, domestic4, domestic5, 
                              domestic6, domestic8, polit03, polit04, polit11, polit12, polit15, Currencycrises, 
                              Inflationcrises, Sovereigndebtcrisesdomestic, Bankingcrises)), 
          type = "html", out = "Table3SumStats.html", title = "Table 3: Summary Statistics (Imputed)", style = "aer")
Institutions.EFA.rfI <- fa(cbind(pitf_eth, pitf_rev, frac, polariz, yrsoffc, stabs, herfgov, checks, 
                                       liec, eiec, fraud, polity2, durable, domestic1, domestic2, domestic3, 
                                       domestic4, domestic6, domestic8, polit03, polit04, polit11, 
                                       polit12, polit15, Currencycrises, Inflationcrises, 
                                       Sovereigndebtcrisesdomestic, Bankingcrises), 
                                 nfactors = 9, fm = "pa", rotate = "varimax", max.iter = 500)
rownames(Institutions.EFA.rfI$loadings) <- c("Ethnic Conflict", "Revolutionary Conflict", "Legislative Fractionalization", 
                                         "Government Polarization", "Executive Tenure", "Change in Veto Players", 
                                         "Government Concentration", "Checks on Power", 
                                         "Legislative Electoral Competition", "Executive Electoral Competition", 
                                         "Electoral Fraud", "Polity2 Index", "Regime Durability", 
                                         "Assassinations", "General Strikes", "Government Crises", "Political Purges", 
                                         "Riots", "Anti-Government Demonstrations", "Coups d'Etat", "Constitutional Changes", 
                                         "Number of Cabinet Changes", "Changes in Executive", "Legislative Elections", 
                                         "Currency Crises", "Inflation Crises", "Domestic Sovereign Debt Crises", 
                                         "Banking Crises")
colnames(Institutions.EFA.rfI$loadings) <- c("Democracy", "Protest", "Credibility", "Regime", 
                                             "Within", "Adverse", "Violence", "Flexibility", "Transparency")
colnames(Institutions.EFA.rfI$scores) <- c("Democracy", "Protest", "Credibility", "Regime", 
                                           "Within", "Adverse", "Violence", "Flexibility", "Transparency")
print(Institutions.EFA.rfI$loadings)
fa.diagram(Institutions.EFA.rfI, cut = 0.29, e.size = 0.04)
plot(Institutions.EFA.rfI$e.values, ylab = "Eigen Value", xlab = "Factor", pch = 4, ylim = c(0,5.1)) 
par(new = T); plot(Institutions.EFA$e.values, ylab = "", xlab = "", pch = 1, ylim = c(0,5.1))
lines(Institutions.EFA.rfI$e.values); lines(Institutions.EFA$e.values); abline(a = 1, b = 0, lty = 3)
legend("topright", legend = c("Observed Data", "Imputed Data"), pch = c(1, 4))

# Correlations among Scores and with ICRG
R.mlpi.icrg <- cor(na.omit(cbind(Institutions.EFA$scores, InstitutionsData$demacct, InstitutionsData$gstab, InstitutionsData$invprof,
InstitutionsData$corrupt, InstitutionsData$burqual, InstitutionsData$intconf, InstitutionsData$extconf,
InstitutionsData$ethten, InstitutionsData$socecon, InstitutionsData$milpol, InstitutionsData$relpol, InstitutionsData$law)))
colnames(R.mlpi.icrg) <- c("Democracy", "Protest", "Within", "Credibility", "Adverse", "Flexibility", "Regime", "Violence", 
                           "Transparency", "Dem. Acct.", "Gov. Stab.", "Inv. Prof.", "Corrupt", "Bur. Qual", "Int. Conf.", 
                           "Ext. Conf.", "Eth. Ten.", "Soc. Econ.", "Mil. Pol.", "Rel. Pol.", "Rule of Law")
rownames(R.mlpi.icrg) <- c("Democracy", "Protest", "Within", "Credibility", "Adverse", "Flexibility", "Regime", "Violence", 
                           "Transparency", "Dem. Acct.", "Gov. Stab.", "Inv. Prof.", "Corrupt", "Bur. Qual", "Int. Conf.", 
                           "Ext. Conf.", "Eth. Ten.", "Soc. Econ.", "Mil. Pol.", "Rel. Pol.", "Rule of Law")
R.mlpi.icrg[upper.tri(R.mlpi.icrg)] <- NA

# Write scores to CSV file
MLPI <- as.data.frame(cbind(InstitutionsData$wbcode, InstitutionsData$year, Institutions.EFA$scores, Institutions.EFA.rfI$scores))
colnames(MLPI) <- c("wbcode", "year", colnames(Institutions.EFA$scores), "Democracy.rfI", "Protest.rfI", "Within.rfI", 
                                               "Credibility.rfI", "Adverse.rfI", "Flexibility.rfI", "Regime.rfI", 
                                               "Violence.rfI", "Transparency.rfI")
write.csv(MLPI, file = "MLPI.csv")
save.image("InstitutionsEFA.RData")
