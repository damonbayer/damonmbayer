#' empirical_cov
#'
#' @param x x vector
#' @param y y vector
#' @param id id vector
#' @param m number of breaks
#'
#' @return list of rmat, cmat, cormat, nmat
#' @export

empirical_cov <- function(x, y, id, m) {
  # code adapted from
  # https://www.ics.uci.edu/~dgillen/STAT212/Handouts/lecture6code.R

  fit <- lm(y ~ ns(x) )
  resids <- y - fitted( fit )
  nobs <- length( y )
  nsubjects <- length( table( id ) )
  rmat <- matrix( NA, nsubjects, m )
  ycat <- seq(min(x), max(x), length.out = m)

  int_size <- (ycat[2] - ycat[1])/2

  nj <- unlist( lapply( split( id, id ), length ) )
  mymin <- function(x){ ifelse( sum( !is.na(x) )==0, NA, min(x, na.rm=TRUE ) ) }
  for( j in 1:m ){
    legal <- ( x >= ycat[j]- int_size)&( x < ycat[j]+int_size )
    jtime <- x + 0.01*rnorm(nobs)
    t0 <- unlist( lapply( split( abs(jtime - ycat[j]) , id ), min ) )
    tj <- rep( t0, nj )
    keep <- ( abs( jtime - ycat[j] )==tj ) & ( legal )
    yj <- rep( NA, nobs )
    yj[keep] <- resids[keep]
    yj <- unlist( lapply( split( yj, id ), mymin ) )
    rmat[ , j ] <- yj
  }

  cmat <- matrix( 0, m, m )
  nmat <- matrix( 0, m, m )

  for( j in 1:m ){
    for( k in j:m ){
      njk <- sum( !is.na( rmat[,j]*rmat[,k] ) )
      sjk <- sum( rmat[,j]*rmat[,k], na.rm=T )/njk
      cmat[j,k] <- sjk
      nmat[j,k] <- njk
    }
  }

  vvec <- diag(cmat)
  cormat <- cmat/( outer( sqrt(vvec), sqrt(vvec) ) )

  list(rmat = rmat, cmat = cmat, cormat = cormat, nmat = nmat)
}
