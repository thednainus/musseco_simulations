#' Get the alpha and q10 values
#'
#' @param pa Proportion of ancestral in the population
#' @param mu Transition from Variant to Ancestral or molecular clock rate
#' @param gamma Recovery rate
#' @param omega Relative transmission fitness
#' @param tol Tolerance
#'
#' @returns
#' The valeu of alpha = q10/q01. q10 is the transition from ancestral to variant
#' and q01 is the transition from variant to ancestral or molecular clock rate.
#' It represents the inflation of the mutation rate from A to V.
#' @export
#'
#' @examples
#' alpha_q10_mutsel_balance( pa = 0.85, mu = 0.0016, gamma = 1/10.2, omega = 0.9, tol = 1e-3 )
alpha_q10_mutsel_balance <- function( pa, mu, gamma, omega, tol = 1e-3 ){
  alpha_root <- function( alpha ){#
    pa*(gamma*(1-pa)+mu*(1-pa)-mu*alpha*pa) - (omega*(1-pa))*(gamma*pa + mu*alpha*pa - mu*(1-pa))
  }
  alpha = uniroot( alpha_root, c(0,20), tol = tol )$root
  q10 = alpha * mu
  return( c( alpha = alpha, q10 = q10) )
}
