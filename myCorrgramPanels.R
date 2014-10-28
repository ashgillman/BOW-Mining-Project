library(scales)

# Like panel.conf, but white confidence text (Corrgram Panel)
panel.confWh <- function (x, y, corr = NULL, col.regions, cor.method,
                          digits = 2, cex.cor, ...) {
  auto <- missing(cex.cor)
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  if (!is.null(corr)) {
    est <- corr
    est <- formatC(est, digits = digits, format = "f")
    if (auto) 
      cex.cor <- 0.7/strwidth(est)
    text(0.5, 0.6, est, cex = cex.cor, col="white")
  }
  else {
    results <- cor.test(x, y, alternative = "two.sided")
    est <- results$estimate
    est <- formatC(est, digits = digits, format = "f")
    if (auto) 
      cex.cor <- 0.7/strwidth(est)
    text(0.5, 0.6, est, cex = cex.cor, col="white")
    ci <- results$conf.int
    ci <- formatC(ci, digits = 2, format = "f")
    ci <- paste("(", ci[1], ",", ci[2], ")", sep = "")
    if (auto) 
      cex.cor <- 0.8/strwidth(ci)
    text(0.5, 0.3, ci, cex = cex.cor, col="white")
  }
}

# combine shade and confidence interal text (Corrgram Panel)
panel.shadeConf <- function (x, y, corr = NULL, col.regions, cor.method, ...) {
  panel.shade(x, y, corr, col.regions, cor.method)
  if(sum(is.na(x) | is.na(y)) < length(x) - 1) {
    # get corr
    if (!is.null(corr)) {
      val <- corr
    } else {
      val <- cor.test(x, y, alternative = "two.sided")
      val <- val$estimate
    }
    # white or black?
    if (val<=0.4) {
      panel.conf(x, y, corr, col.regions, cor.method)
    } else {
      panel.confWh(x, y, corr, col.regions, cor.method)
    }
  }
}

# panel.pts but with alpha on points
panel.ptsAlpha <- function (x, y, corr = NULL, col.regions, cor.method, a=0.3, 
                            ...) 
{
  if (!is.null(corr)) 
    return()
  plot.xy(xy.coords(x, y), type = "p", col = alpha("black", a), ...)
  box(col = "lightgray")
}