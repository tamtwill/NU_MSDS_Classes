######## NWU Capstone 498
########
######## Table for Four Project team
######## Advisor: Don Wedding 
########
######## 
######## Author: Tamara Williams

# For the sake of good programming hygiene, start with a clean workspace
#-------------------------------------------------------------------
# clear Workspace, then clear console
rm(list=ls())
cat("\014")

# Get location script, and set to working directory
#-------------------------------------------------------------------
working.dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(working.dir)
data.path = "/Users/tamtwill/gitRepo/capstone_raw_data/"



# include required packages
#-------------------------------------------------------------------
library(dplyr)
library(corrplot)



###############################################################################
######                       Read and prep data block                ##########
###############################################################################

# get the data for the master lists of reservations and visits
master.visit.df = read.csv(paste0(working.dir,"/all_visit_list.csv"),header=T)
master.res.df = read.csv(paste0(working.dir, "/all_reservation_list.csv"), header=T)

master.res.df$visit_date = as.Date(master.res.df$visit_datetime)
master.visit.df$visit_date = as.Date(master.visit.df$visit_date)

all.up = merge(master.visit.df, master.res.df, by = c('air_store_id', 'visit_date'), suffixes = c('','.res'))
all.up$visit_date = as.numeric(all.up$visit_date)

# convert factors to numerics
indx = sapply(all.up, is.factor)
tmp = all.up
tmp[indx] = lapply(all.up[indx], function(x) as.numeric(x))
tmp[is.na(tmp)] = 0


#-------------------------------------------------------------------
# Make Corrplots
#-------------------------------------------------------------------

cor.tmp = cor(tmp)
corrplot(cor.tmp)
corrplot(cor.tmp,type="square", order="hclust")
corrplot.mixed(cor.tmp,order = "AOE", upper = "ellipse", lower = "number", upper.col = col4, 
               lower.col = col4,tl.cex = 1.5, cl.cex = 1.5)


# with significance testing
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

cor.sig <- cor.mtest(tmp)
corrplot(cor.tmp, type="lower", order="hclust", p.mat = cor.sig, sig.level = 0.01, tl.cex = .75, cl.cex = .75)

# just against visitors
tmp2 = cor(tmp$visitors,tmp)

