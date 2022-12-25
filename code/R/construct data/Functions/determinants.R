 determinants <- function(df){
   information_assymetry <- function(df){
     ############information Asymmetry####################
     df <- df %>% mutate(firmage = fyear - minyear)
     df <- df %>% mutate(Tangible_Assets = ppent/at)###Asset Tangibility
     df <- df %>% rowwise() %>% mutate(Market_to_Book_AT = sum(at,-BookEquity,MV,na.rm = T)/at)
     df <- df %>% group_by(gvkey) %>% mutate(capx_t_1 = ifelse(fyear == fyear_t_1 + 1,Lag(capx, -1),NA))
     df <- df %>% mutate(capx_at = capx/at)
     df  <- df %>% mutate(volatility = abs(ni-ni_t_1)/at)
     return(df)
   }
   corporate_taxes <- function(df){
     ############Measure of Corporate Taxes###############
     df <- df %>% mutate(Operating_Loss_Carryforward = tlcf/sale)
     return(df)
   }
   
   transaction_costs <- function(){
     
   }
   
   agency_cost <- function(df){
     ##############Measure of Agency Costs################
     df <- df %>% mutate(Firm_Size = log(at))
     return(df)
   }
   lifecycle_stages <- function(df){
     df <- df %>% mutate(Dividend = dv/prcc_f)
     df <- df %>% mutate(Dividend_Payer = if_else(Dividend != 0,1,0))
     return(df)
   }
   
   market_timing <- function(df){
     return(df)
   }
   
   tightness_of_financial_constraints <- function(df){
     ########Tightness of Financial Constraints###########
     df <- df %>% mutate(net_issues = delta_csho*prcc_f)
     df <- df %>% rowwise() %>% mutate(Internal_Funds = sum(che_t_1, Cash_Flow_7, -dv, -change_nwc_7, na.rm = T))
     df <- df %>% rowwise() %>% mutate(Near_term_cash_shortfall = sum(delta_che, -delta_debt, -net_issues, na.rm = T)/at)
     df <- df %>% rowwise() %>% mutate(Z_score = sum(3.3*sum(ib,xint,txt,na.rm = T),sale,1.48*re,1.2*sum(act,-lct,na.rm = T),na.rm = T)/at)
     return(df)
   }
   
   bankruptcy_risk <- function(df){
     ##################BankRuptcy Risk####################
     df <- df %>% rowwise() %>% mutate(Profitability= sum(ib,xint,txt/at_t_1,na.rm = T)/at)###Measure of corporate taxes
     df <- df %>% mutate(depr_ammor = dp/at)
     df <- df %>% rowwise() %>% mutate(fin_def = sum(dv, Investment_7, change_nwc_7, -Cash_Flow_7, dlc, na.rm = T))
     return(df)
   }
   
   intangible_assets <- function(df){
     df <- df %>% mutate(knowlede_capital)
     df <- df %>% mutate(organizational_capital)
     df <- df %>% mutate(information_capital)
     return(df)
   }
   
   outcome_calculations <- function(df){
     #######################OUTCOME VARS CALCULATIONS#######################
     df <- df %>% mutate(Debt_Issuance_Decision_Variable = delta_debt/at_t_1) 
     df <- df %>% mutate(Equity_Issuance_Decision_Variable = net_issues/at_t_1)
     ################Financing Decisions##################
     df <- df %>% mutate(Internal = ifelse(Debt_Issuance_Decision_Variable <= .05 && Equity_Issuance_Decision_Variable <= .05,1,0))
     df <- df %>% mutate(External = ifelse(Debt_Issuance_Decision_Variable >= .05 || Equity_Issuance_Decision_Variable >= .05,1,0))
     df <- df %>% mutate(Equity = ifelse(Equity_Issuance_Decision_Variable >= .05, 1,0))
     df <- df %>% mutate(Debtiss = ifelse(Debt_Issuance_Decision_Variable >= .05,1,0))
     df <- df %>% group_by(gvkey) %>% mutate(External_tp_1 = ifelse(fyear == fyear_tp_1 - 1,Lag(External, +1),NA))
     df <- df %>% group_by(gvkey) %>% mutate(Internal_tp_1 = ifelse(fyear == fyear_tp_1 - 1,Lag(Internal, +1),0))
     df <- df %>% group_by(gvkey) %>% mutate(Equity_tp_1 = ifelse(fyear == fyear_tp_1 - 1,Lag(Equity, +1),0))
     df <- df %>% group_by(gvkey) %>% mutate(Debt_tp_1 = ifelse(fyear == fyear_tp_1 - 1,Lag(Debtiss, +1),0))
     return(df)
   }
 }