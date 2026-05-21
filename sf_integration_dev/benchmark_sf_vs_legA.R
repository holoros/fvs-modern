#!/usr/bin/env Rscript
# =============================================================================
# benchmark_sf_vs_legA.R
#
# Operational benchmark: pure species-free vs hybrid (per-species where Leg A
# is reliable) vs Leg-A-style coverage, on a held-out test set. Answers:
# what does the hybrid default buy over pure species-free, especially where
# Leg A covers only a handful of species (the Northeast case).
#
# DESIGN (why this is valid even with HG's BGI approximation):
#   All arms share ONE consistent base linear predictor built from the
#   species-free bundle:
#       base_eta = intercept + trait_effect[sp]
#                + z_L1 + z_L2 + z_L3 + z_FT + sum(covariate fixed effects)
#   The per-species empirical intercept delta[sp] is the mean training value of
#       log(response) - (base_eta - trait_effect[sp])
#   so it is defined RELATIVE to the same base. The three arms differ only in
#   the species-level term, so the comparison is internally consistent and
#   robust to the base predictor's absolute accuracy (HG's BGI site-slope
#   refinement is omitted from the base; it cancels in the comparison).
#
#   pure_sf    : species term = trait_effect[sp]            (Leg B for all)
#   hybrid     : delta[sp] if sp is Leg-A-reliable, else trait_effect[sp]
#   legA_style : delta[sp] if sp is Leg-A-reliable, else 0  (Leg A sparse cover)
#   global_only: 0 for all species (reference floor)
#
# Reliable (Leg A) species are read from the variant's existing
# categories_conus.{component}.species_intercepts (the per-species fits).
#
# DEV/TEST ONLY. Reads bundles + ne.sf_preview/ne.json; writes a metrics CSV.
# Never writes the production configs.
#
# Auto-fold: with --scan, runs every bundle in --bundle_dir that has a
# registered component predictor (HG now; CR/HCB/HT-DBH are simple standard-B1
# additions). v2split bundles drop in automatically once extracted by 61b.
#
# Usage (single component):
#   Rscript benchmark_sf_vs_legA.R --component hg --bundle_dir DIR \
#     --bundle_prefix hg_v5_prod --data DATA.rds --traits species_traits.rds \
#     --variant_json ne.json --variety_split FALSE --test_n 200000 --outdir OUT
#
# Author: A. Weiskittel + Claude
# Date: 2026-05-21
# =============================================================================

suppressPackageStartupMessages({ library(data.table); library(jsonlite) })

args <- commandArgs(trailingOnly = TRUE)
ga <- function(n, d=NULL){ m<-grep(paste0("^--",n,"="),args,value=TRUE)
  if(length(m)) sub(paste0("^--",n,"="),"",m[1]) else d }
COMPONENT  <- ga("component","hg")
BUNDLE_DIR <- ga("bundle_dir","calibration/output/conus/sf_integration")
PREFIX     <- ga("bundle_prefix","hg_v5_prod")
DATA       <- ga("data","calibration/data/conus_remeasurement_pairs_metric_cond_v2.rds")
TRAITS     <- ga("traits","calibration/traits/species_traits.rds")
VJSON      <- ga("variant_json","config/calibrated/ne.json")
SPLIT      <- toupper(ga("variety_split","FALSE")) %in% c("TRUE","T","1")
TEST_N     <- as.integer(ga("test_n","200000"))
OUTDIR     <- ga("outdir","calibration/output/conus/sf_integration/benchmark")
dir.create(OUTDIR, showWarnings=FALSE, recursive=TRUE)
CKEY <- c(hg="height_growth",dg="diameter_growth",hcb="height_crown_base",
          cr="crown_recession",htdbh="height_diameter",mort="mortality")[[COMPONENT]]

cat("== benchmark_sf_vs_legA.R ==\n component:",COMPONENT," bundle:",PREFIX,
    " variety_split:",SPLIT,"\n\n")

# ---- component predictor registry: response + covariate construction --------
# Each returns dat with columns: resp (response), and the covariate columns,
# plus a function eta_cov(dat, fixed) giving the covariate part of eta.
prep_hg <- function(dat){
  dat[, resp := (HT2 - HT1)/YEARS]
  dat[, ln_dbh := log(DBH1)]
  dat[, ln_ht := log(pmax(HT1,1.5))]
  dat[, ln_cr_adj := log((CR1+0.2)/1.2)]
  dat[, bal_log := log((BAL_SW1+BAL_HW1)+5)]
  if(!"SLOPE" %in% names(dat)) dat[,SLOPE:=0]
  if(!"ASPECT" %in% names(dat)) dat[,ASPECT:=0]
  dat[!is.finite(SLOPE),SLOPE:=0]; dat[!is.finite(ASPECT),ASPECT:=0]
  dat[, slope_pct := as.numeric(SLOPE)]
  dat[, cos_aspect := cos(as.numeric(ASPECT)*pi/180)]
  dat[!is.finite(slope_pct),slope_pct:=0]; dat[!is.finite(cos_aspect),cos_aspect:=0]
  dat[, ba_metric := BA1*0.2296]
  dat <- dat[is.finite(DBH1)&DBH1>=2.54 & is.finite(HT1)&HT1>1.5 &
             is.finite(HT2)&HT2>1.5 & is.finite(YEARS)&YEARS>=1&YEARS<=20 &
             is.finite(bgi) & is.finite(BAL_SW1)&is.finite(BAL_HW1) &
             is.finite(BA1)&BA1>=0 & is.finite(CR1)&CR1>0&CR1<=1 &
             !is.na(EPA_L1_CODE)&!is.na(EPA_L2_CODE)&!is.na(EPA_L3_CODE) &
             EPA_L1_CODE!=""&EPA_L2_CODE!=""&EPA_L3_CODE!="" &
             !is.na(FORTYPCD_cond1)&FORTYPCD_cond1>0 &
             resp > -0.5 & resp < 5.0]
  list(dat=dat, link="log",
       cov=function(d,f) f[["a1"]]*d$ln_dbh + f[["a2"]]*d$ln_ht +
         f[["a3"]]*d$ln_cr_adj + f[["a4"]]*d$bgi + f[["a5"]]*d$bal_log +
         f[["a6"]]*d$ba_metric + f[["a7"]]*d$slope_pct + f[["a8"]]*d$cos_aspect)
}
PREP <- list(hg=prep_hg)
if(is.null(PREP[[COMPONENT]])) stop("No predictor registered for component '",
  COMPONENT,"'. Registered: ", paste(names(PREP),collapse=","),
  ". (Standard-B1 components CR/HCB/HT-DBH are a small addition.)")

# ---- load + prep data -------------------------------------------------------
cat("Loading data ...\n"); flush.console()
dat <- as.data.table(readRDS(DATA))
traits <- as.data.table(readRDS(TRAITS))
pr <- PREP[[COMPONENT]](dat); dat <- pr$dat
cat("After filters:", nrow(dat), "rows\n")
if(SPLIT){
  if(any(traits$SPCD==2020L)&&any(traits$SPCD==2021L)){
    dat[SPCD==202L & as.character(EPA_L1_CODE)=="7", SPCD:=2020L]; dat[SPCD==202L, SPCD:=2021L] }
  if(any(traits$SPCD==1080L)&&any(traits$SPCD==1081L)){
    dat[SPCD==108L & as.character(EPA_L1_CODE)=="7", SPCD:=1080L]; dat[SPCD==108L, SPCD:=1081L] }
}
sp_counts <- dat[, .N, by=SPCD][N>=5000]; dat <- dat[SPCD %in% sp_counts$SPCD]
cat("After species filter (>=5000):", nrow(dat), "rows;", nrow(sp_counts), "species\n")

# train/test reproducing the fit's seed-42 100K subsample
set.seed(42); train_idx <- sort(sample.int(nrow(dat), min(100000L,nrow(dat))))
is_train <- rep(FALSE,nrow(dat)); is_train[train_idx]<-TRUE
train <- dat[is_train]; test <- dat[!is_train]
if(nrow(test) > TEST_N){ set.seed(7); test <- test[sort(sample.int(nrow(test),TEST_N))] }
cat("train:",nrow(train)," test:",nrow(test),"\n\n")

# ---- read the species-free bundle ------------------------------------------
bp <- function(s) file.path(BUNDLE_DIR, paste0(PREFIX,s))
fixed <- fread(bp("_sf_fixed.csv")); fx <- setNames(fixed$mean, fixed$variable)
sppt  <- fread(bp("_sf_species.csv")); te <- setNames(sppt$trait_effect_mean, as.integer(sppt$SPCD))
re <- function(tag){ f<-bp(paste0("_sf_re_",tag,".csv"))
  if(file.exists(f)){ x<-fread(f); setNames(x$mean, as.character(x$level)) } else setNames(numeric(0),character(0)) }
reL1<-re("L1"); reL2<-re("L2"); reL3<-re("L3"); reFT<-re("FT")
icpt <- if("a0"%in%names(fx)) fx[["a0"]] else if("b0"%in%names(fx)) fx[["b0"]] else fx[["h0"]]

re_lk <- function(tab,code) ifelse(as.character(code) %in% names(tab), tab[as.character(code)], 0)
add_eta <- function(d){
  te_sp <- ifelse(as.integer(d$SPCD) %in% names(te), te[as.character(d$SPCD)], 0)
  re_part <- re_lk(reL1,d$EPA_L1_CODE)+re_lk(reL2,d$EPA_L2_CODE)+
             re_lk(reL3,d$EPA_L3_CODE)+re_lk(reFT,as.integer(d$FORTYPCD_cond1))
  d[, te_sp := as.numeric(te_sp)]
  d[, non_sp_eta := icpt + as.numeric(re_part) + pr$cov(d, fx)]
  d[, base_eta := non_sp_eta + te_sp]
  d
}
train <- add_eta(train); test <- add_eta(test)

# ---- Leg-A reliable species from the variant's per-species intercepts -------
legA <- integer(0)
if(file.exists(VJSON)){
  vj <- fromJSON(VJSON, simplifyVector=TRUE)
  si <- tryCatch(vj$categories_conus[[CKEY]]$species_intercepts$SPCD, error=function(e) NULL)
  if(!is.null(si)) legA <- as.integer(si)
}
cat("Leg-A reliable species (",length(legA),"):", paste(head(legA,12),collapse=","),"\n")

# ---- per-species empirical intercept delta[sp] from TRAIN (resp>0.001) ------
tr_pos <- train[resp > 0.001]
tr_pos[, sp_resid := log(resp) - non_sp_eta]
delta <- tr_pos[, .(delta=mean(sp_resid)), by=SPCD]
dmap <- setNames(delta$delta, as.integer(delta$SPCD))
dget <- function(spcd) ifelse(as.integer(spcd) %in% names(dmap), dmap[as.character(spcd)], 0)

# ---- predict on TEST (resp>0.001 for log link) + metrics -------------------
ev <- test[resp > 0.001]
spc <- as.integer(ev$SPCD); is_legA <- spc %in% legA
d_sp <- as.numeric(dget(spc))
eta_pure  <- ev$base_eta
eta_hyb   <- ev$non_sp_eta + ifelse(is_legA, d_sp, ev$te_sp)
eta_legA  <- ev$non_sp_eta + ifelse(is_legA, d_sp, 0)
eta_glob  <- ev$non_sp_eta
back <- function(eta) exp(pmin(eta,20))
obs <- ev$resp
metrics <- function(pred, idx){
  o<-obs[idx]; p<-pred[idx]; e<-p-o
  data.frame(n=length(o), rmse=sqrt(mean(e^2)), bias=mean(e),
             r2=1 - sum(e^2)/sum((o-mean(o))^2))
}
east <- ev$EPA_L1_CODE %in% c("5","8")     # Northern + Eastern Temperate forests
subsets <- list(all=rep(TRUE,nrow(ev)), east=east,
                legA_species=is_legA, legA_species_east=is_legA & east)
arms <- list(pure_sf=back(eta_pure), hybrid=back(eta_hyb),
             legA_style=back(eta_legA), global_only=back(eta_glob))
rows <- list()
for(s in names(subsets)) for(a in names(arms)){
  idx<-subsets[[s]]; if(sum(idx)<5) next
  m<-metrics(arms[[a]], idx); m$subset<-s; m$arm<-a; rows[[paste(s,a)]]<-m }
res <- rbindlist(rows)[, .(component=COMPONENT,subset,arm,n,rmse,bias,r2)]
fwrite(res, file.path(OUTDIR, paste0(PREFIX,"_benchmark.csv")))

# ---- leverage: share of test obs in Leg-A-reliable species -----------------
lev <- data.table(
  scope = c("all","east"),
  n = c(nrow(ev), sum(east)),
  legA_species_share = c(mean(is_legA), if(sum(east)) mean(is_legA[east]) else NA))
fwrite(lev, file.path(OUTDIR, paste0(PREFIX,"_leverage.csv")))

# ---- console rollup --------------------------------------------------------
cat("\n=== LEVERAGE (share of test trees in the",length(legA),"Leg-A species) ===\n")
print(lev)
cat("\n=== THREE-WAY METRICS (response: annualized increment; lower RMSE better) ===\n")
print(res[order(subset,arm)])
hb <- res[subset=="all"&arm=="hybrid"]; sf <- res[subset=="all"&arm=="pure_sf"]
if(nrow(hb)&&nrow(sf))
  cat(sprintf("\nHybrid vs pure species-free (all): RMSE %.4f vs %.4f (%.2f%% change)\n",
      hb$rmse, sf$rmse, 100*(hb$rmse-sf$rmse)/sf$rmse))
hbe<-res[subset=="legA_species"&arm=="hybrid"]; sfe<-res[subset=="legA_species"&arm=="pure_sf"]
if(nrow(hbe)&&nrow(sfe))
  cat(sprintf("Hybrid vs pure SF on the Leg-A species only: RMSE %.4f vs %.4f (%.2f%% change)\n",
      hbe$rmse, sfe$rmse, 100*(hbe$rmse-sfe$rmse)/sfe$rmse))
cat("\nDone. Output:", OUTDIR, "\n")
quit(save="no", status=0)
