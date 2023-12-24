library(ISLR2)
library(glmnet)
library(ggplot2)
library(cowplot)
library(GGally)
library(glue)

data("Boston")
df = Boston
df$chas = as.factor(df$chas)
lapply(df, class)
attach(df)

overfitplots(rm, medv, 4, c(5, 10, 15, 20))

data("iris")
df = iris
attach(df)
ggpairs(df)

overfitplots(Petal.Width, Petal.Length, 4, c(0.2, 0.5, 0.7, 0))
overfitplots(Sepal.Width, Sepal.Length, 7, c(1, 3, 5, 0))
overfitplots(Petal.Width, Sepal.Length, 7, c(0.1, 0.2, 0.5, 0))
overfitplots(Sepal.Length, Petal.Width, 5, c(0.1, 0.3, 0.5, 1))

data('mtcars')
df = mtcars
attach(df)
ggpairs(df)

overfitplots = function(x, y, n, lam) {
  axis.names = c(deparse(substitute(x)), deparse(substitute(y)))
  title = ggdraw()+
    draw_label(glue("{n} degree polynomial regression"))
  
  p = overfit(x, y, n, lam[2], axis.names)
  leg = get_legend(p +
                     theme(legend.position = "bottom"))
  
  plot_row = plot_grid(overfit(x, y, n, lam[1], axis.names) + theme(legend.position = "none"), 
            overfit(x, y, n, lam[2], axis.names) + theme(legend.position = "none"), 
            overfit(x, y, n, lam[3], axis.names) + theme(legend.position = "none"), 
            overfit(x, y, n, lam[4], axis.names) + theme(legend.position = "none"))
  plot_leg = plot_grid(plot_row , leg, ncol=1, rel_heights = c(1, 0.1))
  
  plot_grid(title, plot_leg, ncol=1, rel_heights = c(0.1, 1))
  
}


 
overfit = function(x, y, n, lam, axis.names) {
  X = poly(x, n)
  
  lm.fit = lm(y ~ X)
  cv.ridge = cv.glmnet(X, y, alpha=0)
  if (lam==0) {
    lam = cv.ridge$lambda.min
    ridge.fit = glmnet(X, y, alpha=0, lambda=bestlam)
  } else {
    ridge.fit = glmnet(X, y, alpha=0, lambda=lam)
  }
  
  p = ggplot() +
    geom_point(aes(x, y = y)) +
    geom_line(aes(x, y = predict(lm.fit), color="OLS")) + 
    geom_line(aes(x, y = predict(ridge.fit, newx = X), color="Ridge")) +
    labs(subtitle = glue("lambda {lam}"), x=axis.names[1], y=axis.names[2]) 
    
  return(p)
}






coef(lm.fit)
coef(ridge.fit)








