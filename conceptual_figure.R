#'################################'#
#'
#' Code to generate conceptual figure
#' looking at a new metric of multifunctionality
#'
#'
#'################################'#

# preamble ---------------------------------------------------------------
## load libraries ####
library(tidyverse)
library(ggplot2)
library(viridis)

# Functions ---------------------------------------------------------------

#' Function to make data of all combinations
#' of different levels of function, ranging from 
#' 0 to 1 with a user defined step size
make_data <- function(n=3, step_size = 0.1){
  amat <- as.tibble(matrix(rep(seq(0,1,step_size),n), ncol=n))
  amat %>% expand.grid %>% as.tibble
}

#' Get the effective number of a collection
#' of values as defined by Simpson's
#' Diversity following Jost 2006, 2010
eff_div <- function(df, type="Simpson"){
  #proportion of community
  p_df <- df/rowSums(df)
  ln_p_df <- log(p_df)
  ln_p_df[ln_p_df==-Inf] <- 0
  ln_p_df[ln_p_df==NaN] <- 0
  
  #effective number via Simpson's diversity'
  div <- 1/rowSums(p_df^2)
  if(type=="Shannon") div <-exp(-1*rowSums(p_df*ln_p_df))
  
  #NaN = 0
  div[is.nan(div)] <- 0
  
  div
}

#' Get the evenness factor
#' of a collection of values
#' as defined in Jost 2010
even_fact <- function(df, type="Simpson"){
  eff_div(df,type)/ncol(df)
}

# Try and plot ---------------------------------------------------------------
fun_df <- make_data(n = 4, step_size=0.05) %>%
  mutate(mf_a = rowMeans(.),
         mf_even = even_fact(.))


ggplot(fun_df) +
  aes(x=mf_a, y=mf_even, color=mf_a*mf_even) +
  geom_point() +
  geom_hline(yintercept=0.5, col="red", lty=2)+
  geom_vline(xintercept=0.5, col="red", lty=2) +
  scale_color_gradient(low="blue", high="red")

# conceptual figure ---------------------------------------------------------

## First attempt at a broad conceptual figure ####
pos <- as_tibble(matrix(c(
  1,1,
  0,1,
  0,1/10,
  1/10,1/10,
  1,1),
  byrow=TRUE,
  ncol=2)
) 
names(pos) <- c("x","y")

ggplot(pos, aes(x=x, y=y)) +
  geom_polygon(fill="grey") +
  scale_x_continuous(breaks = c(0, 1/10, 0.5, 1),
                     labels = c(0, "1/F", 0.5, 1),
                     lim=c(0,1), minor_breaks = NULL) +
  scale_y_continuous(breaks = c(0, 1/10, 0.5, 1),
                     labels = c(0, "1/F", 0.5, 1),
                     lim=c(0,1), minor_breaks = NULL) +
  geom_hline(yintercept=0.5, col="red", lty=2)+
  geom_vline(xintercept=0.5, col="red", lty=2) +
  theme_bw() +
  xlab("Average Level of Functioning") +
  ylab("Evenness of Functioning")

## Labeled conceptual figure ####
pos_labels <- as_tibble(matrix(c(
#  0.4, 0.4, "zMedo",
#  0.4, 1, "zMedo",
#  0.6, 1, "zMedo",
#  0.6, 0.6, "zMedo",
#  0.4, 0.4, "zMedo",
  1,1,"zHigh",
  0.5,1, "zHigh",
  0.5, 0.5, "zHigh",
  1,1, "zHigh",
  0.5,0.5, "Medium",
  0.5,1, "Medium",
  0,1, "Medium",
  0,0.5, "Medium",
  0.5, 0.5, "Medium",
  1/10, 1/10, "Low",
  0.5, 0.5, "Low",
  0, 0.5, "Low",
  0, 1/10, "Low",
  1/10, 1/10, "Low"
  ),
  byrow=TRUE,
  ncol=3)
) 
names(pos_labels) <- c("x","y", "type")

pos_labels <- pos_labels %>%
  mutate(x=as.numeric(x), y=as.numeric(y))


ggplot(pos_labels, aes(x=x, y=y, group = type, fill=type)) +
  geom_polygon() +
  scale_x_continuous(breaks = c(0, 1/10, 0.5, 1),
                     labels = c(0, "1/F", 0.5, 1),
                     lim=c(0,1), minor_breaks = NULL) +
  scale_y_continuous(breaks = c(0, 1/10, 0.5, 1),
                     labels = c(0, "1/F", 0.5, 1),
                     lim=c(0,1), minor_breaks = NULL) +
  geom_hline(yintercept=0.5, col="red", lty=2)+
  geom_vline(xintercept=0.5, col="red", lty=2) +
  theme_bw() +
  xlab("Average Level of Functioning") +
  ylab("Evenness of Functioning") +
  scale_fill_brewer(palette=7, guide="none") +
  annotate(geom="text", x=c(0.15, 0.15, 0.65),
           y = c(0.3, 0.8, 0.8),
           label = c("Uneven\nlow function",
                   "Even low\nfunction",
                   "Even high\nfunction"))

## Conceptual Gradient Figure ####
concep_df <- crossing(mf_a=seq(0,1,length.out=400), 
                      mf_e=seq(0,1,length.out=400)) %>%
  filter(mf_e>=mf_a) %>%
  filter(mf_e >=1/10)%>%
  mutate(mf = mf_e * mf_a)

ggplot(concep_df, aes(x=mf_a, y=mf_e, fill=mf)) +
  geom_raster(interpolate=TRUE) +
  scale_fill_viridis(begin=0.3, option="B",
                     guide=guide_colorbar("Multifunctionality"))+
  scale_x_continuous(breaks = c(0, 1/10, 0.5, 1),
                     labels = c(0, "1/F", 0.5, 1),
                     lim=c(0,1), minor_breaks = NULL) +
  scale_y_continuous(breaks = c(0, 1/10, 0.5, 1),
                     labels = c(0, "1/F", 0.5, 1),
                     lim=c(0,1), minor_breaks = NULL) +
  geom_hline(yintercept=0.5, col="black", lty=2)+
  geom_vline(xintercept=0.5, col="black", lty=2) +
  theme_bw() +
  xlab("Average Level of Functioning") +
  ylab("Evenness of Functioning")
