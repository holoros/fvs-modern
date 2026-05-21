#!/usr/bin/env Rscript
# =============================================================================
# benchmark_sf_vs_legA.R
#
# Operational benchmark: pure species-free vs hybrid (per-species where Leg A
# is reliable) vs Leg-A-style coverage, on a held-out test set. Answers what
# the hybrid default buys over pure species-free, especially where Leg A covers
# only a handful of species.
#
# DESIGN (valid across components and despite any base-predictor approximation):
#   All arms share ONE consistent base linear predictor from the species-free
#   bundle:  base_eta = intercept + trait_effect[sp] + z_L1+z_L2+z_L3+z_FT
#            + sum(covariate fixed effects).
#   On the link scale g(): the per-species empirical intercept is
#       delta[sp] = mean_train( g(resp) - (base_eta - trait_effect[sp]) ).
#   Arms differ only in the species term; prediction is h(eta) (inverse link):
#       pure_sf    : species term = trait_effect[sp]
#       hybrid     : delta[sp] if sp Leg-A-reliable, else trait_effect[sp]
#       legA_style : delta[sp] if sp Leg-A-reliable, else 0
#       global_only: 0 for all (reference floor)
#   The comparison is internally consistent and robust to the base predictor's
#   absolute accuracy (e.g., HG's BGI site-slope omitted from the base).
#
# Per-component link / response / covariates are registered in PREP below:
#   hg     log link, resp = annualized height increment
#   cr     identity link, resp = annualized crown-ratio change
#   hcb    logit/beta link, resp = HCB ratio (1 - CR)
#   htdbh  Wykoff log link, resp = total height (m)
#
# DEV/TEST ONLY. Reads bundles + variant JSON; writes metrics CSVs. Never
# writes production configs.
#
# Author: A. Weiskittel + Claude
# Date: 2026-05-21
# =============================================================================

suppressPackageStartupMessages({ library(data.table); library(jsonlite) })

args <- commandArgs(trailingOnly = TRUE)
ga <- function(n,d=NULL){ m<-grep(paste0("^--",n,"="),args,value=TRUE)
  if(length(m)) sub(paste0("^--",n,"="),"",m[1]) else d }
COMPONENT <- ga("component","hg")
BUNDLE_DIR<- ga("bundle_dir","calibration/output/conus/sf_integration")
PREFIX    <- ga("bundle_prefix","hg_v5_prod")
DATA      <- ga("data","calibration/data/conus_remeasurement_pairs_metric_cond_v2.rds")
TRAITS    <- ga("traits","calibration/traits/species_traits.rds")
VJSON     <- ga("variant_json","config/calibrated/ne.json")
SPLIT     <- toupper(ga("variety_split","FALSE")) %in% c("TRUE","T","1")
TEST_N    <- as.integer(ga("test_n","200000"))
OUTDIR    <- ga("outdir","calibration/output/conus/sf_integration/benchmark")
dir.create(OUTDIR, showWarnings=FALSE, recursive=TRUE)
CKEY <- c(hg="height_growth",dg="diameter_growth",hcb="height_crown_base",
          cr="crown_recession",htdbh="height_diameter",mort="mortality")[[COMPONENT]]
cat("== benchmark_sf_vs_legA.R ==\n component:",COMPONENT," bundle:",PREFIX,
    " variety_split:",SPLIT,"\n\n")

inv_logit <- function(x) 1/(1+exp(-x)); logit <- function(p) log(p/(1-p))
ln_csi_of <- function(dat){
  if("climate_si" %in% names(dat)){ med<-median(dat$climate_si,na.rm=TRUE)
    dat[!is.finite(climate_si), climate_si:=med]; log(pmax(dat$climate_si,0.1))
  } else if("CSPI" %in% names(dat)){ med<-median(dat$CSPI,na.rm=TRUE)
    dat[!is.finite(CSPI), CSPI:=med]; log(pmax(dat$CSPI,0.1)) } else rep(0,nrow(dat)) }

# ---- per-component registry: response, covariates, link, filter ------------
prep_hg <- function(dat){
  dat[, resp:=(HT2-HT1)/YEARS]; dat[, ln_dbh:=log(DBH1)]
  dat[, ln_ht:=log(pmax(HT1,1.5))]; dat[, ln_cr_adj:=log((CR1+0.2)/1.2)]
  dat[, bal_log:=log((BAL_SW1+BAL_HW1)+5)]
  if(!"SLOPE"%in%names(dat))dat[,SLOPE:=0]; if(!"ASPECT"%in%names(dat))dat[,ASPECT:=0]
  dat[!is.finite(SLOPE),SLOPE:=0]; dat[!is.finite(ASPECT),ASPECT:=0]
  dat[, slope_pct:=as.numeric(SLOPE)]; dat[, cos_aspect:=cos(as.numeric(ASPECT)*pi/180)]
  dat[!is.finite(slope_pct),slope_pct:=0]; dat[!is.finite(cos_aspect),cos_aspect:=0]
  dat[, ba_metric:=BA1*0.2296]
  dat <- dat[is.finite(DBH1)&DBH1>=2.54 & is.finite(HT1)&HT1>1.5 & is.finite(HT2)&HT2>1.5 &
    is.finite(YEARS)&YEARS>=1&YEARS<=20 & is.finite(bgi) & is.finite(BA1)&BA1>=0 &
    is.finite(BAL_SW1)&is.finite(BAL_HW1) & is.finite(CR1)&CR1>0&CR1<=1 &
    !is.na(EPA_L1_CODE)&!is.na(EPA_L2_CODE)&!is.na(EPA_L3_CODE) &
    EPA_L1_CODE!=""&EPA_L2_CODE!=""&EPA_L3_CODE!="" & !is.na(FORTYPCD_cond1)&FORTYPCD_cond1>0 &
    resp>-0.5 & resp<5.0]
  list(dat=dat, gfun=function(y)log(y), hfun=function(e)exp(pmin(e,20)),
       valid=function(y)y>0.001,
       cov=function(d,f) f[["a1"]]*d$ln_dbh+f[["a2"]]*d$ln_ht+f[["a3"]]*d$ln_cr_adj+
         f[["a4"]]*d$bgi+f[["a5"]]*d$bal_log+f[["a6"]]*d$ba_metric+
         f[["a7"]]*d$slope_pct+f[["a8"]]*d$cos_aspect)
}
prep_cr <- function(dat){
  dat[, resp:=(CR2-CR1)/YEARS]; dat[, dbh:=DBH1]; dat[, dbh_sq:=DBH1^2]
  dat[, ba_metric:=BA1*0.2296]; dat[, bal_metric:=BAL_SW1+BAL_HW1]; dat[, cr_init:=CR1]
  dat[, ln_csi:=ln_csi_of(dat)]; dat[!is.finite(ln_csi),ln_csi:=0]
  dat <- dat[is.finite(DBH1)&DBH1>=2.54 & is.finite(CR1)&CR1>0&CR1<=1 &
    is.finite(CR2)&CR2>0&CR2<=1 & is.finite(YEARS)&YEARS>=1&YEARS<=20 &
    is.finite(BA1)&BA1>=0 & is.finite(BAL_SW1)&is.finite(BAL_HW1) &
    !is.na(EPA_L1_CODE)&!is.na(EPA_L2_CODE)&!is.na(EPA_L3_CODE) &
    EPA_L1_CODE!=""&EPA_L2_CODE!=""&EPA_L3_CODE!="" & !is.na(FORTYPCD_cond1)&FORTYPCD_cond1>0 &
    resp>-0.5 & resp<0.5]
  list(dat=dat, gfun=function(y)y, hfun=function(e)e, valid=function(y)rep(TRUE,length(y)),
       cov=function(d,f) f[["b1"]]*d$dbh+f[["b2"]]*d$dbh_sq+f[["b3"]]*d$ba_metric+
         f[["b4"]]*d$bal_metric+f[["b5"]]*d$cr_init+f[["b6"]]*d$ln_csi)
}
prep_hcb <- function(dat){
  dat[, resp:=1-CR1]; dat[, ln_ht:=log(pmax(HT1,1.5))]; dat[, ln_dbh:=log(DBH1)]
  dat[, bal_over_ht:=(BAL_SW1+BAL_HW1)/(HT1+1)]; dat[, sqrt_ba:=sqrt(BA1*0.2296)]
  dat[, ln_cspi_shift:=ln_csi_of(dat)]; dat[!is.finite(ln_cspi_shift),ln_cspi_shift:=0]
  dat <- dat[is.finite(DBH1)&DBH1>=2.54 & is.finite(HT1)&HT1>1 & is.finite(CR1)&CR1>0&CR1<1 &
    is.finite(BA1)&BA1>=0 & is.finite(BAL_SW1)&is.finite(BAL_HW1) &
    !is.na(EPA_L1_CODE)&!is.na(EPA_L2_CODE)&!is.na(EPA_L3_CODE) &
    EPA_L1_CODE!=""&EPA_L2_CODE!=""&EPA_L3_CODE!="" & !is.na(FORTYPCD_cond1)&FORTYPCD_cond1>0 &
    resp>0.01 & resp<0.99]
  list(dat=dat,
       gfun=function(y) logit(pmin(pmax((y-0.001)/0.998,1e-6),1-1e-6)),
       hfun=function(e) 0.001+0.998*inv_logit(e),
       valid=function(y) y>0.01 & y<0.99,
       cov=function(d,f) f[["h1"]]*d$ln_ht+f[["h2"]]*d$ln_dbh+
         f[["h3"]]*d$bal_over_ht+f[["h4"]]*d$sqrt_ba+f[["h5"]]*d$ln_cspi_shift)
}
prep_htdbh <- function(dat){
  dat[, resp:=HT1]; dat[, bal:=BAL_SW1+BAL_HW1]; dat[, sqrt_ba:=sqrt(BA1*0.2296)]
  dat[, rd_ratio:=sdi_additive1/SDImax_brms]; dat[, ba_metric:=BA1*0.2296]
  dat[, ba_x_rd:=ba_metric*rd_ratio]; dat[, bal_x_rd:=bal*rd_ratio]; dat[, dbh:=DBH1]
  dat[, ln_cspi_shift:=ln_csi_of(dat)]; dat[!is.finite(ln_cspi_shift),ln_cspi_shift:=0]
  dat <- dat[is.finite(DBH1)&DBH1>=2.54&DBH1<250 & is.finite(HT1)&HT1>=1.37&HT1<85 &
    is.finite(BA1)&BA1>0 & is.finite(BAL_SW1)&is.finite(BAL_HW1) &
    is.finite(rd_ratio)&rd_ratio>0&rd_ratio<2 &
    !is.na(EPA_L1_CODE)&!is.na(EPA_L2_CODE)&!is.na(EPA_L3_CODE) &
    EPA_L1_CODE!=""&EPA_L2_CODE!=""&EPA_L3_CODE!="" & !is.na(FORTYPCD_cond1)&FORTYPCD_cond1>0]
  list(dat=dat, gfun=function(y)log(pmax(y-1.37,0.01)), hfun=function(e)1.37+exp(pmin(e,20)),
       valid=function(y)y>1.37,
       cov=function(d,f) f[["a_bal"]]*d$bal+f[["a_ba"]]*d$sqrt_ba+f[["a_cspi"]]*d$ln_cspi_shift+
         f[["a_bard"]]*d$ba_x_rd+f[["a_blrd"]]*d$bal_x_rd+f[["b1"]]*(1/(d$dbh+1)))
}
PREP <- list(hg=prep_hg, cr=prep_cr, hcb=prep_hcb, htdbh=prep_htdbh)
if(is.null(PREP[[COMPONENT]])) stop("No predictor registered for '",COMPONENT,"'")

# ---- load + prep ------------------------------------------------------------
cat("Loading data ...\n"); flush.console()
dat <- as.data.table(readRDS(DATA)); traits <- as.data.table(readRDS(TRAITS))
pr <- PREP[[COMPONENT]](dat); dat <- pr$dat
cat("After filters:", nrow(dat), "rows\n")
if(SPLIT){
  if(any(traits$SPCD==2020L)&&any(traits$SPCD==2021L)){
    dat[SPCD==202L & as.character(EPA_L1_CODE)=="7", SPCD:=2020L]; dat[SPCD==202L, SPCD:=2021L] }
  if(any(traits$SPCD==1080L)&&any(traits$SPCD==1081L)){
    dat[SPCD==108L & as.character(EPA_L1_CODE)=="7", SPCD:=1080L]; dat[SPCD==108L, SPCD:=1081L] }
}
sp_counts <- dat[, .N, by=SPCD][N>=5000]; dat <- dat[SPCD %in% sp_counts$SPCD]
cat("After species filter:", nrow(dat), "rows;", nrow(sp_counts), "species\n")
set.seed(42); tr_idx <- sort(sample.int(nrow(dat), min(100000L,nrow(dat))))
itr <- rep(FALSE,nrow(dat)); itr[tr_idx]<-TRUE
train <- dat[itr]; test <- dat[!itr]
if(nrow(test)>TEST_N){ set.seed(7); test <- test[sort(sample.int(nrow(test),TEST_N))] }
cat("train:",nrow(train)," test:",nrow(test),"\n\n")

# ---- read bundle ------------------------------------------------------------
bp <- function(s) file.path(BUNDLE_DIR, paste0(PREFIX,s))
fixed <- fread(bp("_sf_fixed.csv")); fx <- setNames(fixed$mean, fixed$variable)
sppt  <- fread(bp("_sf_species.csv")); te <- setNames(sppt$trait_effect_mean, as.integer(sppt$SPCD))
re <- function(t){ f<-bp(paste0("_sf_re_",t,".csv"))
  if(file.exists(f)){x<-fread(f); setNames(x$mean,as.character(x$level))} else setNames(numeric(0),character(0)) }
reL1<-re("L1");reL2<-re("L2");reL3<-re("L3");reFT<-re("FT")
icpt <- if("a0"%in%names(fx))fx[["a0"]] else if("b0"%in%names(fx))fx[["b0"]] else fx[["h0"]]
re_lk <- function(tab,code) ifelse(as.character(code)%in%names(tab), tab[as.character(code)], 0)
add_eta <- function(d){
  te_sp <- ifelse(as.integer(d$SPCD)%in%names(te), te[as.character(d$SPCD)], 0)
  rp <- re_lk(reL1,d$EPA_L1_CODE)+re_lk(reL2,d$EPA_L2_CODE)+re_lk(reL3,d$EPA_L3_CODE)+
        re_lk(reFT,as.integer(d$FORTYPCD_cond1))
  d[, te_sp:=as.numeric(te_sp)]; d[, non_sp_eta:=icpt+as.numeric(rp)+pr$cov(d,fx)]
  d[, base_eta:=non_sp_eta+te_sp]; d }
train<-add_eta(train); test<-add_eta(test)

# ---- Leg-A reliable species + per-species delta -----------------------------
legA <- integer(0)
if(file.exists(VJSON)){ vj<-fromJSON(VJSON,simplifyVector=TRUE)
  si<-tryCatch(vj$categories_conus[[CKEY]]$species_intercepts$SPCD, error=function(e)NULL)
  if(!is.null(si)) legA<-as.integer(si) }
cat("Leg-A reliable species (",length(legA),"):", paste(head(legA,12),collapse=","),"\n")
trv <- train[pr$valid(resp)]; trv[, spr:=pr$gfun(resp)-non_sp_eta]
dl <- trv[, .(delta=mean(spr)), by=SPCD]; dmap<-setNames(dl$delta, as.integer(dl$SPCD))
dget <- function(s) ifelse(as.integer(s)%in%names(dmap), dmap[as.character(s)], 0)

# ---- predict + metrics ------------------------------------------------------
ev <- test[pr$valid(resp)]; spc<-as.integer(ev$SPCD); isA<-spc%in%legA; ds<-as.numeric(dget(spc))
arms <- list(pure_sf=pr$hfun(ev$base_eta),
             hybrid=pr$hfun(ev$non_sp_eta+ifelse(isA,ds,ev$te_sp)),
             legA_style=pr$hfun(ev$non_sp_eta+ifelse(isA,ds,0)),
             global_only=pr$hfun(ev$non_sp_eta))
obs <- ev$resp
met <- function(p,idx){o<-obs[idx];q<-p[idx];e<-q-o
  data.frame(n=length(o),rmse=sqrt(mean(e^2)),bias=mean(e),r2=1-sum(e^2)/sum((o-mean(o))^2))}
east <- ev$EPA_L1_CODE %in% c("5","8")
subs <- list(all=rep(TRUE,nrow(ev)),east=east,legA_species=isA,legA_species_east=isA&east)
rows<-list()
for(s in names(subs)) for(a in names(arms)){ idx<-subs[[s]]; if(sum(idx)<5)next
  m<-met(arms[[a]],idx); m$subset<-s; m$arm<-a; rows[[paste(s,a)]]<-m }
res <- rbindlist(rows)[, .(component=COMPONENT,subset,arm,n,rmse,bias,r2)]
fwrite(res, file.path(OUTDIR, paste0(PREFIX,"_benchmark.csv")))
lev <- data.table(scope=c("all","east"), n=c(nrow(ev),sum(east)),
  legA_species_share=c(mean(isA), if(sum(east))mean(isA[east]) else NA))
fwrite(lev, file.path(OUTDIR, paste0(PREFIX,"_leverage.csv")))

cat("\n=== LEVERAGE (share of test trees in the",length(legA),"Leg-A species) ===\n"); print(lev)
cat("\n=== THREE-WAY METRICS (lower RMSE better) ===\n"); print(res[order(subset,arm)])
hb<-res[subset=="all"&arm=="hybrid"]; sf<-res[subset=="all"&arm=="pure_sf"]
if(nrow(hb)&&nrow(sf)) cat(sprintf("\n[%s] hybrid vs pure_sf (all): RMSE %.4f vs %.4f (%+.2f%%)\n",
  COMPONENT, hb$rmse, sf$rmse, 100*(hb$rmse-sf$rmse)/sf$rmse))
cat("\nDone. Output:", OUTDIR, "\n"); quit(save="no", status=0)
