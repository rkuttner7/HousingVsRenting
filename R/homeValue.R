
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

#'
#' @param x initial monthly value
#' @param inflation percent change
#' @param compounding either "monthly" or "yearly"
#' @param years numeric scalar
#' @keywords internal
inflateValue <- function(x, inflation, 
                         compounding = c("yearly", "monthly"), years){
  
  k <- dplyr::case_when(compounding == "monthly" ~ 12L,
                        compounding == "yearly" ~ 1L,
                        TRUE ~ NA_integer_)
  
  time_vec <- if(compounding == "yearly"){
    0:(years - 1)
  } else {
    1:(years*12)
  }

  y <- x * (1 + inflation / k)^(time_vec)

  if(compounding == "yearly"){
         rep(y, each = 12)
    } else{
      y
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
  
  interestDebt <-  c(0, tail(debt_t, -1)) * interestRate / compound_periods
  paid_principal <- fixedMortgagePayment - interestDebt
  
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
      .call = FALSE
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
#' @examples 
#' mortgageModel <- kahnModel(purchasePrice = 750000, downpayment = 280000,
#'                            #pctDown = 0.2,
#'                            interestRate = 0.06, principalAmortizaion = 30,
#'                            compound_periods = 12)
#'  homeValue(mortgageModel,
#'            propertyTaxRate = 0.0125, maintenance = 1000,
#'            housingAssociationDues = 0, insurance = 1500,
#'            appreciationHome = 0.03, marginalIncomeTaxRate = 0.3,
#'            inflation = 0.02)
homeValue <- function(mortgageModel, 
                      propertyTaxRate,
                      maintenance,
                      housingAssociationDues,
                      insurance,
                      appreciationHome,
                      marginalIncomeTaxRate,
                      inflation){
  
  # Values in mortgage payment object
  principalAmortizaion <- mortgageModel$principalAmortizaion
  purchasePrice <- mortgageModel$purchasePrice
  interestDebt <- mortgageModel$get_paymentsDF()$Interest_on_debt
  fixedMortgagePayment <- mortgageModel$fixedMortgagePayment
  time_month <- 0:(12*mortgageModel$principalAmortizaion) 
  
  homeValue_t <- inflateValue(purchasePrice, appreciationHome, 
                              compounding = "yearly", 
                              years = principalAmortizaion)
 
 maintenance_t <- inflateValue(maintenance/12, inflation, 
                               compounding = "yearly", 
                               years = principalAmortizaion)
 
 hoaDues_t <- inflateValue(housingAssociationDues, 
                                          inflation, 
                           compounding = "yearly", 
                           years = principalAmortizaion)
 
 insurance_t <- inflateValue(insurance/12, inflation, 
                             compounding = "yearly", 
                             years = principalAmortizaion)

 propertyTax_t <- homeValue_t * propertyTaxRate / 12
 # Income tax savings from interest deduction
 interestDeduction <- (interestDebt + propertyTax_t) * marginalIncomeTaxRate
 
 cashOut <- fixedMortgagePayment + insurance_t + hoaDues_t + maintenance_t +
   propertyTax_t - interestDeduction
 
 structure(class = c("homeCosts", "kahnModel"), list(
   # attributes
   maintenance = maintenance_t, 
   hoaDues = hoaDues_t, 
   insurance = insurance_t, 
   homeValue = homeValue_t,
   propertyTax = propertyTax_t,
   interestDeduction = interestDeduction,
   cashOut = cashOut,
#   principalAmortizaion = principalAmortizaion,
#   compound_periods = compound_periods,
#   fixedMortgagePayment = fixedMortgagePayment,
   # methods
   get_vakueDF = function() {
     data.frame(Time = time_month,
                Debt = debt_t,
                Interest_on_debt = interestDebt,
                Paid_principal = paid_principal
     )
   }
 ))
}

#' calculate monlthy payments with interest compounded at yearly of monthly.
#' @param x0 numeric starting value
#' @param interest increase from the previous value
#' @param compoundPeriods number of times starting value is increased
#' by a factor of the interest rate per year
#' @param years number of years to calculate monthly payments
#' @examples 
#' # rent increased 10% annually for 30 years
#'   rentInitial <- compoundInterest(15000, 0.10, 1, 30)
compoundInterest <- function(x0, interest, compoundPeriods, years){
  time_intervals <- 0:(12 * principalAmortizaion)
  
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