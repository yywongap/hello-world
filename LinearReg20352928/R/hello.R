# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


lm_20352928 <- function(Y,X){
  beta <- solve(t(X)%*%X)%*%t(X)%*%Y
  print('beta')
  prmatrix(beta)
  cat('beta',beta)

  sigma_square <- sum((y-x%*%beta)^2)/(nrow(X)-nrow(beta))

  print('SD')
  cov_beta = sigma_square * solve(t(X)%*%X)
  prmatrix(sqrt(diag(cov_beta)))


  print('t statistics')
  t <- beta/sqrt(diag(cov_beta))
  prmatrix(beta/sqrt(diag(cov_beta)))
  prob <- 2*(1- pt(t,((nrow(X)-nrow(beta)))))
  print('p-value')
  prmatrix(prob)
}

