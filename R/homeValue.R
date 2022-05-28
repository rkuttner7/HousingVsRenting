
#' khan Academy; Application of the geometric series
#' @references https://www.khanacademy.org/math/precalculus/x9e81a4f98389efdf:series/x9e81a4f98389efdf:geo-series-notation/v/geometric-series-sum-to-figure-out-mortgage-payments
#' @param P principal (down payment)
#' @param i annual interest rate
#' @param T number of years to pay off loan
#' @param k compounding periods (eg. 12 for monthly)
#' @keywords internal
find_fixedMortgagePayment <- function(P, i, T, k){
  r <- 1 /(1 + i/k)
  P * (1 - r) / (r - (r ^ ((T * k) + 1)))
}


#' The debt remaining after each payment.
#' @param t numeric vector for time
#' @param P principal (down payment)
#' @param i annual interest rate
#' @param payment monthly payment installment
#' @param k compounding periods (eg. 12 for monthly)
#' @keywords internal
#' @references 
#' Shiny Mortgage Calculator; https://github.com/lcolladotor/mortgage/blob/master/server.R
#' Blog on Shiny Mortgage; https://www.r-bloggers.com/simple-mortgage-calculator/
debtRemaining <- function(t, P, i, payment, k){ 
  r <- 1 + (i / k)
  P * r ^ t -
    payment * cumsum(I(t > 0) * r ^ (t - 1))
}

## Calculate payment
pay <- function(principal, interest, duration, payfreq, compoundfreq) {
  
  r <- interest / (100 * 12 / compoundfreq ) 
  
  payment <- principal * r / ( 1 - ( 1 + r)^(-duration * 12 / compoundfreq) ) * payfreq / compoundfreq
  res <- list(r=r, payment=payment, principal=principal)
  return(res)
}

#' Increase the value either monthly or yearly
#' @details 
#' The initial value increases by an annual rate
#' adjusted incrementally every month or yearly. The interest
#' is compounded annually, in either case.
#' @inheritParams compoundInterest
#' @param adjustment either "monthly" or "yearly"
#' @keywords internal
inflateValue <- function(time, principal, interest_rate, 
                         adjustment = c("yearly", "monthly")){
  
  rlang::arg_match(adjustment)
  
  if(adjustment == "monthly"){
    compoundInterest(time,
                     principal, 
                     interest_rate / 12,
                     1 / 12)
    } else if (adjustment == "yearly") {
      # n_years <- max(floor(time/12))
      years_floor <- floor(time/12)
      # units_per_floor <- as.integer(table(years_floor))
      compoundInterest(years_floor,
                                       principal, 
                                       interest_rate,
                                       1)
      
    } else{
      stop(sprintf("Argument 'adjustment' has unexpected value: {%s}", 
                   adjustment))
    }
}

#' user helper constructor for kahn model
#' Model proposed in lecture from Kahn Academy for projecting 
#' the home value over the course of its mortgage.
#' @param purchasePrice cost of the house at time of purchase
#' @param downpayment cash on hand for home purchase
#' @param interestRate the mortgage interest rate
#' @param principalAmortizaion number of years to pay off mortgage
#' @references 
#' Object Oriented R for Programmers
#' https://rstudio-pubs-static.s3.amazonaws.com/150296_904158e070594471864e89c10c0d14f9.html
#' 
#' Advanced R (2nd edition); S3 
#' https://adv-r.hadley.nz/s3.html
#' 
#' R Language Definition
#' https://cran.r-project.org/doc/manuals/r-release/R-lang.html
#' 
#' Class Methods
#' https://stat.ethz.ch/R-manual/R-devel/library/base/html/UseMethod.html
#' 
#' Methods for S3 and S4 Dispatch
#' https://stat.ethz.ch/R-manual/R-devel/library/methods/html/Methods_for_S3.html
#' @examples
#' kahnModel(purchasePrice = 800, downpayment = 280, 
#'           interestRate = 0.03, principalAmortizaion = 30,
#'           compound_periods = 12)
kahnModel <- function(purchasePrice,
                      downpayment, 
                      #pctDown, 
                      interestRate, principalAmortizaion,
                      compound_periods){
  x <- validate_kahnModel(new_kahnModel(purchasePrice,
                                        downpayment,
                                        #pctDown, 
                                        interestRate,
                                        principalAmortizaion,
                                        compound_periods))
  x
}

#' constructor function
#' Have one argument for the base object, and one for each attribute.
#' @examples 
#'  new_kahnModel(purchasePrice = 750000, 
#'                downpayment = 280000,
#'                interestRate = 0.06, 
#'                principalAmortizaion = 30,
#'                compound_periods = 12)
new_kahnModel <- function(purchasePrice, downpayment, interestRate, 
                          principalAmortizaion,
                          compound_periods){
  
  
  #downpayment <- purchasePrice * pctDown
  
  loan_0 <- purchasePrice - downpayment
  
  time_intervals <- 0:(compound_periods * principalAmortizaion)
  
  fixedMortgagePayment <- find_fixedMortgagePayment(loan_0,
                                                    interestRate,
                                                    principalAmortizaion,
                                                    compound_periods)
  
  debt_t <- debtRemaining(t = time_intervals, P = loan_0, 
                          i = interestRate, fixedMortgagePayment, 
                          compound_periods)
  
  interestDebt <-  dplyr::lag(debt_t, default = 0) * interestRate / compound_periods
    #c(0, tail(debt_t, -1)) * interestRate / compound_periods
  paid_principal <- fixedMortgagePayment - interestDebt
  paid_principal[1] <- 0 # no payment at time of purchase [t = 0]
  
  structure(class = "kahnModel", list(
    # attributes
    purchasePrice = purchasePrice, 
    downpayment = downpayment,
  #  pctDown = pctDown, 
    interestRate = interestRate, 
    principalAmortizaion = principalAmortizaion,
    compound_periods = compound_periods,
    fixedMortgagePayment = fixedMortgagePayment,
    # methods
    get_paymentsDF = function() {
      data.frame(Time = time_intervals,
                 Debt = debt_t,
                 Interest_on_debt = interestDebt,
                 Paid_principal = paid_principal
      )
    }
  ))
}

#' Rather than encumbering the constructor with 
#' complicated checks, it’s better to put them 
#' in a separate function.
#' Check the type of the base object and the types of each attribute
#' @examples 
#' validate_kahnModel( new_kahnModel(purchasePrice = 750000,
#'                                   #pctDown = 0.2, 
#'                                   downpayment = 280000,
#'                                   interestRate = 0.06,
#'                                   principalAmortizaion = 30,
#'                                   compound_periods = 12))
validate_kahnModel <- function(x){
  if(!is.numeric(x[[1]])) {
    stop(
      "The `purchasePrice` must be numeric",
      call. = FALSE
    )
    x
  }
x
}

#' Value of house and costs associated with owning of a home
#' @param mortgageModel model of mortgage payments
#' @param propertyTaxRate Local tax on property value
#' @param maintenance average maintenance and repair per year
#' @param housingAssociationDues annual dues to Home Owners Association
#' @param insurance annual home owners insurance
#' @param appreciationHome average annual appreciation in home value
#' @param marginalIncomeTaxRate annual tax on income earned
#' @param inflation average annual increase in the cost of goods
#' @param time_years number of years to model home value
#' @examples 
#' mortgageModel <- kahnModel(purchasePrice = 750000, downpayment = 280000,
#'                            #pctDown = 0.2,
#'                            interestRate = 0.06, principalAmortizaion = 15,
#'                            compound_periods = 12)
#'  homeValue(mortgageModel,
#'            propertyTaxRate = 0.0125, maintenance = 1000,
#'            housingAssociationDues = 0, insurance = 1500,
#'            appreciationHome = 0.03, marginalIncomeTaxRate = 0.3,
#'            inflation = 0.02,
#'            time_years = 20)
homeValue <- function(mortgageModel, 
                      propertyTaxRate,
                      maintenance,
                      housingAssociationDues,
                      insurance,
                      appreciationHome,
                      marginalIncomeTaxRate,
                      inflation,
                      time_years){
  
  # hard code
  # costs to purchase home
  #  includes: 
  #    $2100 Title Insurance fees; 
  #    $1400 Escrow company fees (buyer unusually pays \$700)
  #    $1,195 Mortgage lender underwriting fee
  purchase_transaction_costs <- 3995
  
  # set time to arbitrary number years (can be longer or shorter than mortgage)
  time_month <- 0:(12*time_years) 
  
  adjust_length <- function(x, length_new){
    additional_cells <- length_new - length(x)
    length(x) <- length_new
    if(additional_cells > 0) return(tidyr::replace_na(x, 0))
    x
  }
  length_new <- length(time_month)
  
  # Values in mortgage payment object
  principalAmortizaion <- mortgageModel$principalAmortizaion
  purchasePrice <- mortgageModel$purchasePrice
  downpayment <- mortgageModel$downpayment
  
  # modify vectors length to match length [time_month], 
  #  can be longer or shorter than than mortgage
  interestDebt <- adjust_length(mortgageModel$get_paymentsDF()$Interest_on_debt,
                                length_new)
  debt_t <- adjust_length(mortgageModel$get_paymentsDF()$Debt,
                          length_new)
  Paid_principal <- adjust_length(mortgageModel$get_paymentsDF()$Paid_principal,
                                  length_new)
  fixedMortgagePayment <- adjust_length(mortgageModel$fixedMortgagePayment,
                                        length_new)
  # stop("rename loan time to [time_month_loan] from [time_month]")
  # time_month_loan <- 0:(12*mortgageModel$principalAmortizaion)

  
  homeValue_t <- inflateValue(time_month,
                              purchasePrice, appreciationHome, 
                              adjustment = "monthly")

  equity_t <- cumsum(Paid_principal)
    #purchasePrice - (debt_t - interestDebt)  #homeValue_t - debt_t
 
  
  maintenance_t <- dplyr::lag( # no cost at time of purchase (t = 0)
    inflateValue(time_month,
                 maintenance/12, inflation, 
                 adjustment = "monthly"), 
    default = 0) 

 hoaDues_t <- dplyr::lag( # no cost at time of purchase (t = 0)
   inflateValue(time_month,
                housingAssociationDues, inflation, 
                adjustment = "yearly"),
   default = 0)
 
 insurance_t <- dplyr::lag( # no cost at time of purchase (t = 0)
   inflateValue(time_month,
                             insurance/12, inflation, 
                             adjustment = "yearly"),
   default = 0)

 propertyTax_t <- dplyr::lag(# no cost at time of purchase (t = 0)
   homeValue_t * propertyTaxRate / 12, default = 0)

 # Income tax savings from interest deduction
 interestDeduction <- (interestDebt + propertyTax_t) * marginalIncomeTaxRate
 
 cashOut <- dplyr::case_when(
   # closing costs at time of purchasing home (t = 0)
   time_month == 0 ~ purchase_transaction_costs +
     interestDebt + insurance_t + hoaDues_t + 
     maintenance_t + propertyTax_t - interestDeduction,
   time_month > 0 ~ interestDebt + insurance_t + hoaDues_t + 
     maintenance_t + propertyTax_t - interestDeduction,
   TRUE ~ NA_real_
 ) 
 
sales_transaction_cost_t <- sale_transaction_cost(homeValue_t)
 
 # Net cash for selling home
 sales_profit_t <- (homeValue_t - purchasePrice) + 
   0 - 
   (
     (cumsum(cashOut)) +
       sales_transaction_cost_t +
       capitol_gains_tax(basis = purchasePrice, 
                     adj_sales = homeValue_t - sales_transaction_cost_t, 
                     tax_rate = 0.20, 
                     tax_limit = 5*10^5) )
 

 structure(class = c("homeCosts", "kahnModel"), list(
   # attributes
   principalAmortizaion = mortgageModel$principalAmortizaion,
   purchasePrice = mortgageModel$purchasePrice,
   downpayment = mortgageModel$downpayment,
   fixedMortgagePayment = mortgageModel$fixedMortgagePayment,
   propertyTaxRate = propertyTaxRate,
   maintenance = maintenance,
   housingAssociationDues = housingAssociationDues,
   insurance = insurance,
   appreciationHome = appreciationHome,
   marginalIncomeTaxRate = marginalIncomeTaxRate,
   inflation = inflation,
   time_years = time_years,
   
   # methods
   get_valueDF = function() {
     data.frame(Time = time_month,
                equity = equity_t,
                propertyValue = homeValue_t,
                salesProfit = sales_profit_t,
                interestDeduction = interestDeduction,
                maintenance = maintenance_t, 
                hoaDues = hoaDues_t, 
                insurance = insurance_t, 
                propertyTax = propertyTax_t,
                housingCosts  = cashOut
     )
   }
 ))
}

#' Fixed transaction costs when selling home
#' @details 
#' Broker's fee of 6%
#' Real Estate Excise 1.28% of property sale
#' @param sales_price selling price
sale_transaction_cost <- function(sales_price){
  (sales_price * 0.06) + (sales_price * 0.0128) 
}

#' Capital Gains Tax
#' @param basis Original down payment plus improvements
#' @param adj_sales Selling price minus the transaction costs
#' @param tax_rate rate of taxation
#' @param tax_limit 
capitol_gains_tax <- function(basis, adj_sales, tax_rate, tax_limit){
  net_profit <- (basis - adj_sales)
  dplyr::case_when(
    tax_limit < net_profit ~ net_profit * tax_rate,
    tax_limit >= net_profit ~ 0,
    TRUE ~ NA_real_)
}

#' Value including interest accrued by compound
#' @param time vector of duration money is accruing interest
#' @param principal investment amount
#' @param interest_rate interest rate
#' @param compund_periods_per_time_unit number of times that interest 
#'  is compounded per unit  
#' @examples 
#'  # One year with 3% interest, compounded annually
#'  compoundInterest(c(0,1), 10, 0.03, 1)
#'  
#'  # 12 months with 3% annual interest, compounded annually
#'  compoundInterest(c(0,12), 10, 0.03/12, 1/12)
#' @references https://www.thecalculatorsite.com/articles/finance/compound-interest-formula.php
compoundInterest <- function(time,
                             principal, 
                             interest_rate,
                             compund_periods_per_time_unit){
  
  principal * (1 + (interest_rate / compund_periods_per_time_unit)) ^ (
    compund_periods_per_time_unit * time
  )
}

#' Time to break even.
#' The number of months to recoup the upfront cost to get discounted
#' rate.
#' @param rate_base monthly payment
#' @param rate_plus discount monthly payment with additional upfront cost
#' @param addtl_down additional upfront payment for discount
#' @examples 
#'   time_to_break_even(699.21, 690.68, 1000)
time_to_break_even <- function(rate_base, rate_plus, addtl_down){
  addtl_down/(x - y)
}

#' user helper constructor for renter model
#' Model for projecting the cost of renting over time.
#' @param rent_month monthly rental cost initially
#' @examples
#' renterModel(rent_month = 1200)
renterModel <- function(rent_month){
  x <- validate_renterModel(new_renterModel(rent_month))
  x
}

#' constructor function
#' Have one argument for the base object, and one for each attribute.
#' @examples 
#'  new_renterModel(rent_month = 1200)
new_renterModel <- function(rent_month){
  
  structure(class = "renterModel", list(
    # attributes
    rent_month = rent_month
  ))
}

#' Rather than encumbering the constructor with 
#' complicated checks, it’s better to put them 
#' in a separate function.
#' Check the type of the base object and the types of each attribute
#' @examples 
#' validate_renterModel( new_renterModel(rent_month = 1200))
validate_renterModel <- function(x){
  if(!is.numeric(x[[1]])) {
    stop(
      "The `rent_month` must be numeric",
      call. = FALSE
    )
    x
  }
  x
}

#' Costs associated with renting
#' @description 
#' Rent appreciation compounded annually
#' @param rentModel model of rental costs
#' @param appreciationRent average annual increase in the cost of rent
#' @param adjustment compound periods, either "monthly" or "yearly"
#' @param time_years number of years to model home value
#' @examples 
#' rent_Model <- renterModel(rent_month = 1200)
#' 
#'  rentValue(rent_Model,
#'            appreciationRent = 0.05, 
#'            adjustment = "yearly"
#'            time_years = 20)
rentValue <- function(rentModel, 
                      appreciationRent,
                      adjustment,
                      time_years){
  
  # set time to arbitrary number years (can be longer or shorter than mortgage)
  time_month <- 0:(12*time_years) 
  
  # Values in mortgage payment object
  rent_month <- rentModel$rent_month
  
  rentValue_t <- inflateValue(time_month,
                              rent_month, appreciationRent, 
                              adjustment = adjustment)
  
  structure(class = c("rentCosts", "renterModel"), list(
    # attributes
    rent_month = rentModel$rent_month,
    appreciationRent = appreciationRent,
    time_years = time_years,
    
    # methods
    get_valueDF = function() {
      data.frame(Time = time_month,
                 rent = rentValue_t
      )
    }
  ))
}

#' Value of investments and costs associated with owning or renting a home
#' @param value_model value model, either 'homeCosts' or 'rentCosts'
#' @param cash_on_hand cash available for non-home investment
#' @param investment_interest average annual appreciation from cash investments
#' @examples 
#' mortgageModel <- kahnModel(purchasePrice = 750000, downpayment = 280000,
#'                            #pctDown = 0.2,
#'                            interestRate = 0.06, principalAmortizaion = 15,
#'                            compound_periods = 12)
#'                            
#' homeValue_model <- homeValue(mortgageModel,
#'                              propertyTaxRate = 0.0125, maintenance = 1000,
#'                              housingAssociationDues = 0, insurance = 1500,
#'                              appreciationHome = 0.03, 
#'                              marginalIncomeTaxRate = 0.3, inflation = 0.02,
#'                              time_years = 20)
#'                              
#' investmentValue(homeValue_model, 100000, 0.05)
investmentValue <- function(value_model, cash_on_hand, investment_interest){
  UseMethod("investmentValue", x)
}

investmentValue.default <- function(...){
  stop("Class must be homeCosts or rentCosts", 
       call. = FALSE)
  }

investmentValue.homeCosts <- function(value_model, cash_on_hand, 
                                      investment_interest){
  
  time_month <- value_model$get_valueDF()$Time
  
  net_value <- value_model$get_valueDF()$salesProfit
  
  cashValue_t <- inflateValue(time_month,
                              cash_on_hand, investment_interest, 
                              adjustment = "monthly")
  
  data.frame(
    Time = time_month,
    net_value = net_value + cashValue_t
    )
  
  }

investmentValue.rentCosts <- function(value_model, cash_on_hand, 
                                      investment_interest){
  
  time_month <- value_model$get_valueDF()$Time
  
  # rent is a cost
  net_value <- -1 * value_model$get_valueDF()$rent
  
  cashValue_t <- inflateValue(time_month,
                              cash_on_hand, investment_interest, 
                              adjustment = "monthly")
  
  data.frame(
    Time = time_month,
    net_value = net_value + cashValue_t
  )
  }

