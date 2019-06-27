#' Cook's Distance Plot
#'
#' @param model a glm or lm model
#' @param label text labels for plot
#'
#' @return a cook's distance plot
#' @export
#'
#' @examples
cooks_plot <- function(model, label) {
  ycutoff <- qf(.05,length(model$coef), model$df.residual)
  xcutoff <- 2*mean(hatvalues(model))

  tmp_dat <- broom::augment(model) %>%
    mutate(label_me = .hat > xcutoff | .cooksd > ycutoff) %>%
    mutate(label = label)

  ggplot(tmp_dat, aes(.hat, .cooksd, label = label)) +
    geom_vline(xintercept = xcutoff) +
    geom_hline(yintercept = ycutoff) +
    geom_point() +
    geom_text_repel(data = filter(tmp_dat, label_me), mapping = aes(.hat, .cooksd, label = label)) +
    labs(x = "Leverage", y = "Cook's Distance")
}
