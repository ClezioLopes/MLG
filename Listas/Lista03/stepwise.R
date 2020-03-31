stepwise <- function(full.model, initial.model, alpha.to.enter, alpha.to.leave) {
  full <- lm(full.model)
  msef <- (summary(full)$sigma)^2
  n <- length(full$residuals)
  allvars <- attr(full$terms, "predvars")
  current <- lm(initial.model)
  while (TRUE) {
    temp <- summary(current)
    rnames <- rownames(temp$coefficients)
    print(temp$coefficients)
    p <- dim(temp$coefficients)[1]
    mse <- (temp$sigma)^2
    cp <- (n - p) * mse/msef - (n - 2 * p)
    fit <- sprintf("\nS = %f, R-sq = %f, R-sq(adj) = %f, C-p = %f", temp$sigma,
                   temp$r.squared, temp$adj.r.squared, cp)
    write(fit, file = "")
    2
    write("=====", file = "")
    if (p > 1) {
      d <- drop1(current, test = "F")
      pmax <- max(d[-1, 6])
      if (pmax > alpha.to.leave) {
        var <- rownames(d)[d[, 6] == pmax]
        if (length(var) > 1) {
          var <- var[2]
        }
        write(paste("--- Dropping", var, "\n"), file = "")
        f <- formula(current)
        f <- as.formula(paste(f[2], "~", paste(f[3], var, sep = " - ")))
        current <- lm(f)
        next
      }
    }
    a <- tryCatch(add1(current, scope = full, test = "F"), error = function(e) NULL)
    if (is.null(a)) {
      break
    }
    pmin <- min(a[-1, 6])
    if (pmin < alpha.to.enter) {
      var <- rownames(a)[a[, 6] == pmin]
      if (length(var) > 1) {
        var <- var[2]
      }
      write(paste("+++ Adding", var, "\n"), file = "")
      f <- formula(current)
      f <- as.formula(paste(f[2], "~", paste(f[3], var, sep = " + ")))
      current <- lm(f)
      next
    }
    break
  }
}
