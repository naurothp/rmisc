
# ----------------------------------------------------------
# -------------- Supplemantary Functions   ----------------
# ----------------------------------------------------------


EnsurePackage<-function(x)
{
  x <- as.character(x)
  if (!require(x,character.only=TRUE))
  {install.packages(pkgs=x,
                    repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
    
  }
}

panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y, use="pairwise.complete.obs", method="spearman")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}

scale.analysis <- function (df, sub) {
  sub.items <- c(names(sub))
  sub.scaleKey <- c(rep(1, length(sub.items)))
  sub.results <- scoreItems(keys = sub.scaleKey, items = df[sub.items])
  print(sub.results)
  return (sub.results)
}

factor.analysis <- function (fact, factors) {
  fit <- factanal(fact, factors, rotation="varimax")
  print(fit, digits=2, cutoff=.3, sort=TRUE)
  ev <- eigen(cor(fact)) # get eigenvalues
  ap <- parallel(subject=nrow(fact),var=ncol(fact),
                 rep=100,cent=.05)
  nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
  plotnScree(nS)
}

my.stat.desc <- function (x) {round(stat.desc(x), 2)}

my.mean <- function (x) mean(x, na.rm = TRUE)

count <- function (x, value) ifelse(is.na(value), length(which(is.na(x))), length(which(x==value)))


############ RESTEHALDE
# --> rel+ & rel*sci-(!) sagen meaning vorher

#plot cont. interaction:
# {z1 <- z2 <- c(-1,1)
# newdf <- expand.grid(sci.scaled=z1, rel.scaled=z2)
# p <- ggplot(data=transform(newdf, yp=predict(mea.lm, newdf)), 
#             aes(y=yp, x=sci.scaled, color=factor(rel.scaled))) + stat_smooth(method=lm)
# p + scale_colour_discrete(name="rel") + 
#   labs(x="sci", y="mea") + 
#   scale_x_continuous(breaks=c(-1,1)) + theme_bw()
# 
# mea.lm <- lm(mea ~ scale(rel) + epis, data=s0b)
# summary(mea.lm)}


