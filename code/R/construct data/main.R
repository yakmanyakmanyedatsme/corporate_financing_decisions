rm(list=ls())
library(dplyr)
library(data.table)
library(RStata)
library(haven)
library(Hmisc)
setwd("C:/Users/wfran/Dropbox/Research Projects/Financing Decisions/code/R/construct data/Functions")
df_list <- list()
source("compustat.R")
source("crsp.R")
source("ibes.R")
df_def <- df_list[[1]]
df_nom <- df_list[[2]]
####Levels Variables####
df_def <- compustat_levels(df_def)
df_nom <- compustat_levels(df_nom)
#####Flow Variables#####
df_def <- compustat_flow(df_def)
df_nom <- compustat_flow(df_nom)
#####Change of Variables#####
df_def <- change_of_levels(df_def)
df_nom <- change_of_levels(df_nom)
#####Ratio Levels#####
df_def <- ratio_levels(df_def)
df_nom <- ratio_levels(df_nom)
#####Ratio Flow#####
df_def <- ratio_of_flow(df_def)
df_nom <- ratio_of_flow(df_nom)
#######################################################################
#######################################################################
#######################DETERMINANTS CALCULATIONS#######################
#######################################################################
#######################################################################
df_def <- determinants(df_def)
df_nom <- determinants(df_nom)



  
#####################################################
############information Asymmetry####################
#####################################################
hot_cold <- done
Analyst_Coverage <- done
Forecast_dispersion <- done
#Halov and Heider?
Recent Asset Volatilities 
Change_of_implied_volatility <- #####Halov and Heider
####Credit_Ratings <-  #####
#####################################################
############Measure of Corporate Taxes###############
#####################################################
#Marginal_Tax_Rate <- done

#####################################################
###########Measure of Transaction costs##############
#####################################################
###Equity_Spread
###Equity_Debt_Spread
#####################################################
##############Measure of Agency Costs################
#####################################################
#######G_Index
#####################################################
##################Lifeycyle Stage####################
#####################################################

#####################################################
#################Market_Timing#######################
#####################################################
#EFWA = 
########Recent_and_Future_Abnormal_Returns = 
#####################################################
########Tightness of Financial Constraints###########
#####################################################
df <- df %>% mutate(net_issues = delta_csho*prcc_f)
df <- df %>% rowwise() %>% mutate(Internal_Funds = sum(che_t_1, Cash_Flow_7, -dv, -change_nwc_7, na.rm = T))
df <- df %>% rowwise() %>% mutate(Near_term_cash_shortfall = sum(delta_che, -delta_debt, -net_issues, na.rm = T)/at)
df <- df %>% rowwise() %>% mutate(Z_score = sum(3.3*sum(ib,xint,txt,na.rm = T),sale,1.48*re,1.2*sum(act,-lct,na.rm = T),na.rm = T)/at)
####Financing Deficit
#####################################################
##################BankRuptcy Risk####################
#####################################################
df <- df %>% rowwise() %>% mutate(Profitability= sum(ib,xint,txt/at_t_1,na.rm = T)/at)###Measure of corporate taxes
df <- df %>% mutate(depr_ammor = dp/at)
df <- df %>% rowwise() %>% mutate(fin_def = sum(dv, Investment_7, change_nwc_7, -Cash_Flow_7, dlc, na.rm = T))
#####################################################
##################Intangible Assets##################
#####################################################
#df <- df %>% mutate(knowlede_capital)
#df <- df %>% mutate(organizational_capital)
#df <- df %>% mutate(information_capital)


temp <- c("BookEquity", "Debt", "MV", "mvat", "Cash_Flow_7", "Investment_7", "delta_che",
  "delta_debt", "change_nwc_7", "StockReturn", "Current_Ratio", "Market_Leverage",
  "Book_Leverage", "cash_to_assets", "Debt_to_assets", "RD_Sales", "Ammortization",
  "Selling_Expense", "firmage", "Tangible_Assets", "Market_to_Book_AT", "capx_t_1", 
  "capx_at", "volatility", "Operating_Loss_Carryforward", "Firm_Size", "Dividend",
  "Dividend_Payer", "Internal_Funds", "Near_term_cash_shortfall", "Z_score", "Profitability",
  "depr_ammor", "fin_def", "Debt_Issuance_Decision_Variable", "Equity_Issuance_Decision_Variable",
  "Internal", "External", "Equity", "Debtiss", "External_tp_1", "Internal_tp_1", "Equity_tp_1", "Debt_tp_1")

df <- df %>% select(colnames(df)[which(colnames(df) %in% temp)])