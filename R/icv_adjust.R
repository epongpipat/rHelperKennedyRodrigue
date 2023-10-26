#' icv_adjust
#' @description adjust volume by intracranial volume (ICV)
#' @details 
#' \enumerate{
#' \item GLM of volume as the DV and mean-centered ICV as the IV \cr 
#' \tab \eqn{volume_i = b_0 + b_1 * icv_{mc_i}} (Eq. 1) \cr
#' \item Adjust volume by subtracting the original volume by the product of the slope (\eqn{b_1}) from equation 1 and mean-centered ICV \cr
#' \tab \eqn{volume_{adj_i} = volume_i - (b_1 * icv_{mc_i})} (Eq. 2)
#' }
#' @concept data_wrangling
#' @param volume volume
#' @param icv intracranial volume
#' @return volume_adj volume adjusted for ICV
#' @export
#' @references Raz N, Rodrigue KM, Kennedy KM, Head D, Gunning-Dixon F, Acker JD. Differential aging of the human striatum: longitudinal evidence. AJNR Am J Neuroradiol. 2003 Oct;24(9):1849-56. PMID: 14561615; PMCID: PMC7976312.
icv_adjust <- function(volume, icv) {
  m <- lm(volume ~ scale(icv, scale = FALSE, center = TRUE))
  b1 <- coef(m)[2]
  volume_adj <- volume - (b1 * scale(icv, scale = FALSE, center = TRUE))
  return(volume_adj)
}