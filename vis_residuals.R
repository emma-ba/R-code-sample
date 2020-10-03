# _ #################################
# ENVIRONMENT #######################
# _ #################################
if(!require('rstudioapi') ){ install.packages('rstudioapi') }
library(rstudioapi)
setwd(dirname(getSourceEditorContext()$path))

if(!require('tidyverse') ){ install.packages('tidyverse') }
library(tidyverse)

if(!require('grid')) install.packages('grid')
library(grid)


###### Multiplot (reused function) #####
# Adapted from:
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
# 
# Title can be passed as c("title1", "title2"). If only one title is
# provided it will be used as single title for all columns. If several titles
# are provided each column will then have it's own title.
#
# Title size, font and face can also be provided
multiplot <- function(..., plotlist = NULL, cols = 1, layout = NULL, title = NULL, 
                      fontsize = 14, fontfamily = "Helvetica", fontface = "bold") {
  require(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (length(title)>0){
    layout <- rbind(rep(0, ncol(layout)), layout)
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), 
                                               ncol(layout), 
                                               heights = if (length(title)>0) {unit(c(0.5, rep(5,nrow(layout)-1)), "null")}
                                               else {unit(c(rep(5, nrow(layout))), "null")})))
    if(length(title) > 1){
      ncols <- 1:ncol(layout)
      for(i in seq(ncols)){
        grid.text(title[i], 
                  vp = viewport(layout.pos.row = 1, layout.pos.col = i),
                  gp = gpar(fontsize = fontsize, fontfamily = fontfamily, fontface = fontface))
      }
    } else {
      grid.text(title, 
                vp = viewport(layout.pos.row = 1, layout.pos.col = 1:ncol(layout)),
                gp = gpar(fontsize = fontsize, fontfamily = fontfamily, fontface = fontface))
    }
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


###### Plot Residuals vs Fitted #####
plot_res_fit <- function(data, model, color_label=''){
  if(color_label=='') {
    color='black'
    show_legend = 'none'
  } else {
    color=data[color_label] %>% pull
    show_legend = "top"
  }
  
  data.frame(residuals=residuals(model), fitted=fitted(model)) %>%  ggplot() +
    geom_point(aes(x=fitted, y=residuals, colour=color), shape = 21) +
    geom_abline(intercept=0, slope=0, color='black') +
    labs(x='fitted', y='residuals', colour=color_label) +
    theme_minimal() +
    theme(legend.position='top')
}
# Example ####
model <- lm(mpg ~ poly(disp,3) + poly(wt,2) + poly(cyl, 2), data=mtcars)

mtcars %>% plot_res_fit(model)
mtcars %>% mutate(cyl=cyl%>%as.character) %>% plot_res_fit(model, 'cyl')

###### Plot Residuals vs Predictor #####
plot_res_pred <- function(data, model, x_label, color_label=''){
  if(color_label=='') {
    color='black'
    show_legend = 'none'
  } else {
    color=data[color_label] %>% pull
    show_legend = "top"
  }
  
data.frame(residuals=residuals(model), predictor=data[x_label]%>%pull, color=color) %>% 
  ggplot() +
    geom_point(aes(x=predictor, y=residuals, colour=color), shape = 21) +
    geom_abline(intercept=0, slope=0, color='black') +
    labs(x=x_label, y='residuals', colour=color_label) +
    theme_minimal() +
    theme(legend.position=show_legend)
}
# Example ####
model <- lm(mpg ~ poly(disp,3) + poly(wt,2) + poly(cyl, 2), data=mtcars)

mtcars %>% plot_res_pred(model, 'drat')
mtcars %>% mutate(cyl=cyl%>%as.character) %>% plot_res_pred(model, 'drat', 'cyl')



###### Plot Residuals in multivariate regression #####
plot_res <- function(data, model, x_label, y_label, color_label=''){
  x = data[x_label] %>% pull
  y = data[y_label] %>% pull
  y_hat=fitted(model) 
  if(color_label=='') {
    color='red'
    show_legend = 'none'
  } else {
    color=data[color_label] %>% pull
    show_legend = "top"
  }
  
  ggplot() +
    geom_point(aes(x=x, y=y, colour=color), shape = 21) +
    geom_point(aes(x=x, y=y_hat), size=0.2, alpha=0.8) +
    geom_segment(aes(x=x, y=y, xend=x, yend=y_hat, colour=color), 
                 alpha=0.5, size=0.5) +
    labs(x=x_label, y=y_label, colour=color_label) +
    theme_minimal() +
    theme(legend.position=show_legend,
          legend.text = element_text(size = 6)
          )
}
# Example ####
model <- lm(mpg ~ poly(disp,3) + poly(wt,2) + poly(cyl, 2), data=mtcars)

mtcars %>% plot_res(model, 'drat', 'mpg')
mtcars %>% mutate(cyl=cyl%>%as.character) %>% plot_res(model, 'drat', 'mpg', 'cyl')

###### Explore predictors  #####
plot_res_explo <- function(data, model, y_label, color_label=''){
  var <- colnames(data)
  list_vis <- list()
  for(i in 1:length(var)){
    list_vis[[length(list_vis)+1]] <- plot_res(data, model, var[i], y_label, color_label)
  }
  for(i in 1:length(var)){
    list_vis[[length(list_vis)+1]] <- plot_res_pred(data, model, var[i], color_label)
  }
  multiplot(plotlist=list_vis, cols=2)
}
# Example ####
model <- lm(mpg ~ poly(disp,3) + poly(wt,2) + poly(cyl, 2), data=mtcars)

mtcars %>% 
  select(mpg, cyl, disp, drat, wt, carb) %>%
  plot_res_explo(model, 'mpg')

mtcars %>% 
  select(mpg, cyl, disp, drat, wt, carb) %>%
  mutate(cyl=cyl%>%as.character) %>% 
  plot_res_explo(model, 'mpg', 'cyl')


###### Master Matrix plot of residuals #####
plot_dis <- function(data, model, x_label, y_label, color_label=''){
  x = data[x_label] %>% pull
  y = data[y_label] %>% pull
  y_hat=fitted(model) 
  if(color_label=='') {
    color='red'
  } else {
    color=data[color_label] %>% pull
  }
  
  ggplot() +
    geom_point(aes(x=x, y=y, colour=color), size=0.4, alpha=0.8) +
    labs(x=x_label, y=y_label, colour=color_label) +
    theme_minimal() +
    theme(legend.position='none')
}
master_res_matrix <- function(data, model, y_label, color_label=''){
  var <- colnames(data)
  list_vis <- list()
  for(i in 1:length(var)){
    for(k in 1:length(var)){
      if(var[k] == var[i]){
        # Residuals vs Predictor
        list_vis[[length(list_vis)+1]] <- plot_res_pred(data, model, var[i], color_label)
      } else if(var[k] == y_label){
        # Residuals in scatterplot
        list_vis[[length(list_vis)+1]] <- plot_res(data, model, var[i], var[k], color_label)
      } else {
        # Scatterplot
        list_vis[[length(list_vis)+1]] <- plot_dis(data, model, var[i], var[k], color_label)
      }
    }
  }
  multiplot(plotlist=list_vis, cols=length(var))
}
# Example ####
model <- lm(mpg ~ poly(disp,3) + poly(wt,2) + poly(cyl, 2), data=mtcars)

mtcars %>% 
  select(mpg, cyl, disp, drat, wt, carb) %>% 
  master_res_matrix(model, 'mpg')

mtcars %>% 
  select(mpg, cyl, disp, drat, wt, carb) %>%
  mutate(cyl=cyl%>%as.character) %>% 
  master_res_matrix(model, 'mpg', 'cyl')
