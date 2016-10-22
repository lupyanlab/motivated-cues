
reportF <- function(df, var) {
  f <- df[var, 'F']
  df.num <- df[var, 'Df']
  df.den <- df[var, 'Df.res']
  p <- df[var, 'Pr(>F)']
  sprintf("F(%i, %.2f) = %.2f, p = %.4f", df.num, df.den, f, p)
}

reportModCompF <- function(df) {
  f <- df$stats$Fstat
  df.num <- df$stats$ndf
  df.den <- df$stats$ddf
  p <- df$stats$p.value
  sprintf("F(%i, %.2f) = %.2f, p = %.4f", df.num, df.den, f, p)
}

reportModCompChiSqr <- function(big, small, factor) {
  coeff <- summary(big)$coefficients
  beta <- coeff[factor, 'Estimate']
  se <- coeff[factor, 'Std. Error']
  
  modcomp <- anova(big, small)
  
  chi_df <- modcomp['big', 'Chi Df']
  chi_sqr <- modcomp['big', 'Chisq']
  p <- modcomp['big', 'Pr(>Chisq)']
  
  text <- sprintf("Beta = %.2f(%.2f), Chi Sqr(%i) = %.2f, p = %.4f", beta, se, chi_df, chi_sqr, p)
  
  list(text = text,
       modcomp = modcomp)
}

reportNewStats <- function(mod, factor) {
  coeff <- summary(mod)$coefficients
  beta <- coeff[factor, 'Estimate']
  ci <- confint(mod, method='Wald', level=0.95)[factor, ]
  
  text <- sprintf("B = %.2f, 95%% CI [%.2f, %.2f]", beta, ci[1], ci[2])
  list(text = text,
       ci = ci,
       beta = beta)
}