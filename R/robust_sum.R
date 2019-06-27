#' robust glm summary
#'
#' @param model glm object
#' @param vcov robust covariance matrix
#' @param level
#' @param transform logical
#'
#' @return data frame with robust summary
#' @export
#'
robust_sum <- function(model, vcov = sandwich::vcovHC(model, type = "HC3"), level = 0.95, transform = T) {
  link <- model$family$link

  transform <- transform & is.element(link, c("logit","log"))
  est_name <- ifelse(transform, "exp(Estimate)", "")


  # est_name <- switch(link,
  #                    "logit" = "Odds Ratio",
  #                    "log" = "Rate Ratio",
  #                    "")

  cbind(lmtest::coeftest(model, vcov. = vcov),
        lmtest::coefci(model, vcov. = vcov, level = level)) %>%
    as_tibble() %>%
    mutate(Coefficient = names(model$coefficients)) %>%
    select(Coefficient, Estimate, ends_with("%"), "Pr(>|z|)") %>%
    rename("p" = "Pr(>|z|)") %>%
    { if(transform) {
      mutate_at(., vars("Estimate", ends_with("%")), exp) %>%
        rename(!!est_name := "Estimate")
    }
      else .}
}
