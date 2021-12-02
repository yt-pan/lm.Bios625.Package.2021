#'lm_s
#'
#'The lm_s function will fit a linear model using given data set. It will automatically print summary of coefficients and anova table of the model.
#'The function can treat NA with different action.
#'The function may not treat interaction terms correctly and cannot fit a model without an intercept properly.
#'
#'@param formula The same object of class "formula" as the origin R function lm(): a symbolic description of the model to be fitted.
#'
#'@param data an optional data frame, list containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which lm_s is called.
#'
#'@return The function returns the Coefficients' matrix which contain the value, S.E, t-statistics and p-value of each coefficient. The function also will automatically print summary of coefficients and anova table of the model.
#'
#'@examples
#'y = rnorm(1000)
#'x = rnorm(1000)
#'z = rnorm(1000)
#'lm_s(y ~ x)
#'lm_s(y ~ x+z)
#'
#'@import stats
#'@export
#'lm_s


lm_s = function(formula, data) {
  ## handle formula input from R
  mf = match.call(expand.dots = FALSE)
  m = match(c("formula", "data"),
             names(mf), 0L)
  mf = mf[c(1, m)]
  mf$drop.unused.levels = TRUE

  mf[[1]] = quote(stats::model.frame)
  mf = eval(mf, parent.frame())


  xy = na.omit(mf)

  n = nrow(xy)
  Y = xy[,1]
  if (ncol(xy)==2){
    X = data.matrix(cbind(rep(1,n),xy[,2]))
  }
  else if (ncol(xy)>2){
    X = data.matrix(cbind(rep(1,n),xy[,2:ncol(xy)]))
  }
  else{
    print("Input should have at least two variables.")
    return(-1)
  }
  p = ncol(X)
  a = try(solve(t(X)%*%X),silent=T)
  if (!is.matrix(a)){
    print("Design matrix is not invertible. Check for strongly correlated variables.")
    return(-1)
  }
  if(n-p==0){
    print("Not enough data points for F-stat.")
    return(-1)
  }

  beta_hat = a %*% t(X) %*% Y
  H = X %*% a %*% t(X)
  Y_hat = H %*% Y
  re = Y-Y_hat
  sigma_hat2 = (t(re)%*%re)/(n-p)
  beta_var = diag(as.numeric(sigma_hat2) * as.matrix(a))
  beta_se = sqrt(beta_var)
  beta_t = beta_hat/beta_se
  beta_p = 2*pt(-abs(beta_t),df=n-1)

  RSE = sqrt(sigma_hat2)
  AR= H - matrix(1,n,n)/n
  AY= diag(n) - matrix(1,n,n)/n
  AE= diag(n) - H

  SSE = t(Y) %*% AE %*% Y
  SSY = t(Y) %*% AY %*% Y
  SSR = t(Y) %*% AR %*% Y
  R2 = 1-SSE/SSY
  adjR2 = 1- (SSE/(n-p))/(SSY/(n-1))

  Fstat = (SSR/(p-1))/(SSE/(n-p))
  Fstat_p = 1-pf(Fstat,df1=p-1,df2=n-p)

  if(ncol(X)>2){
    SStotal = rep(0, length(ncol(X)-1))
    for (i in 2:ncol(X)){
      SStotal[(i-1)] = lm_anova_getssr(X[,1:i],Y)
    }
    SSm = c(0,SStotal[1:(length(SStotal)-1)])
    SS = SStotal - SSm
  }
  else if(ncol(X)==2){
    SS = SSR
  }
  if(SSE<(1e-10)*SSY){
    warning("Essentially perfect fit: test-statistics may be unreliable.")
  }

  # print output
  if (is.null(colnames(X))){
    colnames(X) = as.character(0:(ncol(X)-1))
  }
  digits = max(3, getOption("digits") - 3)
  signif.stars = getOption("show.signif.stars")

  cat("\nCall:\n", # S has ' ' instead of '\n'
      paste(deparse(match.call()), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  cat("Residuals:\n", sep = "")


  rdf = n-p
  if (rdf > 5) {
    nam = c("Min", "1Q", "Median", "3Q", "Max")
    rq = if (length(dim(re)) == 2)
      structure(apply(t(re), 1L, quantile),
                dimnames = list(nam, dimnames(re)[[2]]))
    else  {
      zz = zapsmall(quantile(re), digits + 1)
      structure(zz, names = nam)
    }
    print(t(rq), digits = digits)
  }
  else if (rdf > 0) {
    print(re, digits = digits)
  }

  cat("\nCoefficients:\n")
  coefs = cbind(beta_hat,beta_se,beta_t,beta_p)
  dimnames(coefs) =
    list(c("(Intercept)",colnames(X)[-1]),
         c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
  printCoefmat(coefs, digits = digits, signif.stars = signif.stars,
               na.print = "NA")

  cat("\nResidual standard error:",
      format(signif(RSE, digits)), "on", rdf, "degrees of freedom\n")
  cat("Multiple R-squared: ", formatC(R2, digits = digits))
  cat(",\tAdjusted R-squared: ",formatC(adjR2, digits = digits),
      "\nF-statistic:", formatC(Fstat, digits = digits),
      "on", p-1, "and",
      rdf, "DF,  p-value:",
      format.pval(pf(Fstat, p-1, n-p, lower.tail = FALSE),
                  digits = digits))
  cat("\n\n")

  cat("Analysis of Variance Table\n\n")
  cat("Response: ",colnames(mf)[1])
  cat("\n")

  df = c(rep(1,p-1),n-p)
  sum_sq = c(SS,SSE)
  MS = sum_sq/df
  F_va = MS/c(SSE/(n-p))
  FPr = pf(F_va, df, n-p, lower.tail = FALSE)
  acoefs = cbind(df, sum_sq, MS, F_va, FPr)
  acoefs[nrow(acoefs),4:5] = NA
  dimnames(acoefs) =
    list(c(colnames(X)[-1],"Residuals"),
         c("Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)"))
  printCoefmat(acoefs, signif.stars = signif.stars, P.values=TRUE,has.Pvalue = TRUE,
               na.print = "", cs.ind=numeric(0))
  cat("\n")
  return(coefs[,1])
}

## helper function - simplfied regression only return ssr
lm_anova_getssr = function(X,Y){
  n = length(Y)
  a = try(solve(t(X)%*%X),silent=T)
  a = as.matrix(a)
  H = X %*% a %*% t(X)
  AR= H - matrix(1,n,n)/n
  SSR = t(Y) %*% AR %*% Y
  return(SSR)
}
