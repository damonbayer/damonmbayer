#' gg_lda_variogram
#'
#' Produce Dan Gillen style variograms with ggplot
#' @param x x vector
#' @param y y vector
#' @param id id vector
#' @param title title of plot
#'
#' @return Dan Gillen style variogram with ggplpot
#' @export

gg_lda_variogram <- function(x, y, id, title = NULL) {
  # https://www.ics.uci.edu/~dgillen/STAT212/Handouts/Stat212Functions.R
  lda.variogram <- function( id, y, x ){
    uid <- unique( id )
    m <- length( uid )
    delta.y <- NULL
    delta.x <- NULL
    did <- NULL
    for( i in 1:m ){
      yi <- y[ id==uid[i] ]
      xi <- x[ id==uid[i] ]
      n <- length(yi)
      expand.j <- rep( c(1:n), n )
      expand.k <- rep( c(1:n), rep(n,n) )
      keep <- expand.j > expand.k
      if( sum(keep)>0 ){
        expand.j <- expand.j[keep]
        expand.k <- expand.k[keep]
        delta.yi <- 0.5*( yi[expand.j] - yi[expand.k] )^2
        delta.xi <- abs( xi[expand.j] - xi[expand.k] )
        didi <- rep( uid[i], length(delta.yi) )
        delta.y <- c( delta.y, delta.yi )
        delta.x <- c( delta.x, delta.xi )
        did <- c( did, didi )
      }
    }
    out <- list( id = did, delta.y = delta.y, delta.x = delta.x )
    out
  }

  dat <- tibble(t = x, id)
  fit <- lm(y ~ ns(t), data = dat)
  resids <- fit$residuals
  vario <- lda.variogram(id, resids, dat$t)
  var.est <- var( resids )

  tibble(x = vario$delta.x, y = vario$delta.y) %>%
    ggplot(aes(x, y)) +
    geom_point(size = 0.5, alpha = 0.5) +
    stat_smooth(method = geom_smooth.spline, formula = y ~ x, color = "red") +
    coord_cartesian(ylim = c(0, 1.2 * var.est)) +
    geom_hline(yintercept = var.est, linetype = 2, color = "blue", size = 1) +
    xlab("Change in time") +
    ylab("Change in residual over time squared") +
    ggtitle(title)
}
