#' dfbeta Plot
#'
#' @param model a glm or lm model
#' @param label text labels for plot
#'
#' @return a Dan-Gillen-Style dfbeta plot
#' @export
#'
#' @examples
dfbeta_plot <- function(model, label) {
  cutoff <- 2/sqrt(nrow(df))

  df <- dfbeta(modela) %>%
    as_tibble() %>%
    rowid_to_column(var = "obs") %>%
    gather("Parameter", "dfbeta", -obs) %>%
    mutate(label_me = abs(dfbeta) > cutoff)

  ggplot(df, aes(obs, dfbeta, color = Parameter)) +
    geom_hline(yintercept = -cutoff) +
    geom_hline(yintercept = cutoff) +
    geom_point() +
    geom_text_repel(aes(obs, dfbeta, color = Parameter, label = obs), data = filter(df, label_me)) +
    xlab("Observation")
}
