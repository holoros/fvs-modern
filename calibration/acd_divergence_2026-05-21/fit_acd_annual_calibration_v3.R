## fit_acd_annual_calibration_v3.R
##
## Corrected annual Acadian calibration. Fixes a units bug present in v1/v2:
## the recorded BAL/BA columns are in ft2/acre, but the Acadian dDBH_fun expects
## BAL in m2/ha. v3:
##   - converts recorded BAL to m2/ha (x 0.2295684), the correct scale
##   - splits it into BAL.SW and BAL.HW using a per-tree larger-tree softwood
##     fraction (softwood = SPCD < 300), reconstructed from the records
##   - fits annual diameter multipliers by iterating the real dDBH_fun
##   - fits annual height multipliers from HT_annual (ft/yr) against dHT_fun;
##     CCFL is held at a constant (its coefficient is negligible and any constant
##     is absorbed by the per species multiplier)
##
## Output: acd_annual_calibration.csv (supersedes v1).
## Run: module load gcc/12.3.0 R/4.4.0; Rscript fit_acd_annual_calibration_v3.R
suppressWarnings(suppressMessages({
  library(data.table); library(plyr); library(dplyr); library(purrr); library(tibble)
}))

ACD <- Sys.getenv("ACD_PATH",
  "~/upstream_fvs_check/ForestVegetationSimulator-Interface/fvsOL/inst/extdata/AcadianGY.R")
source(ACD)
cat("sourced", AcadianVersionTag, "\n")

DG    <- Sys.getenv("ACD_DG",
  "/users/PUOM0008/crsfaaron/fvs-conus/data/processed/acd/diameter_growth.csv")
CAP   <- as.integer(Sys.getenv("FIT_CAP", "6000"))
FT2AC_to_M2HA <- 0.2295684

xwalk <- c(
  "12"="BF","91"="NS","94"="WS","95"="BS","97"="RS","71"="TA","105"="JP",
  "125"="RN","130"="SC","129"="WP","241"="WC","261"="EH",
  "315"="ST","316"="RM","317"="SV","318"="SM","319"="MM",
  "371"="YB","375"="PB","379"="GB","531"="AB","541"="WA","543"="BA","544"="GA",
  "601"="BN","741"="BP","743"="BT","746"="QA","762"="BC",
  "802"="WO","833"="RO","837"="BO","951"="BW","972"="AE")

si_to_csi <- function(si_ft) {
  csi <- approxfun(c(0,8,14,20), c(0,8,12,14), rule = 2)(si_ft * 0.3048)
  ifelse(is.na(csi) | csi <= 0, 12, csi)
}

cat("reading", DG, "\n")
d <- fread(DG, select = c("PLT_CN","DIA_t1","DIA_t2","CR_t1","HT_annual",
                          "BAL","SI","years_interval","SPCD","TPA"))
setnames(d, c("DIA_t1","DIA_t2","CR_t1","HT_annual","years_interval","SPCD","TPA"),
            c("d1","d2","cr","ht_ann","yi","spcd","tpa"))
d <- d[is.finite(d1) & d1 > 0 & is.finite(d2) & yi >= 1 & yi <= 15 &
       is.finite(BAL) & is.finite(tpa) & tpa > 0]
d[, `:=`(
  alpha   = xwalk[as.character(spcd)],
  DBHcm   = d1 * 2.54,
  isSW    = as.integer(spcd < 300),
  CSI     = si_to_csi(SI),
  CRp     = pmin(0.95, pmax(0.05, cr / 100)),
  BALm2ha = BAL * FT2AC_to_M2HA,            # recorded BAL ft2/acre -> m2/ha
  yi      = as.integer(round(yi)))]
d[, w := DBHcm^2 * tpa]                     # relative larger-tree weight (units cancel)

## per plot larger-tree softwood fraction (ordered by DBH desc)
setorder(d, PLT_CN, -DBHcm)
d[, tot_larger := cumsum(w)      - w,        by = PLT_CN]
d[, sw_larger  := cumsum(w*isSW) - w*isSW,   by = PLT_CN]
d[, plot_swfrac := sum(w*isSW)/sum(w),       by = PLT_CN]
d[, swfrac := fifelse(tot_larger > 0, sw_larger / tot_larger, plot_swfrac)]
d[, `:=`(BALSW = BALm2ha * swfrac, BALHW = BALm2ha * (1 - swfrac))]

cat("records:", nrow(d), " plots:", uniqueN(d$PLT_CN),
    " median BAL m2/ha:", round(median(d$BALm2ha),1), "\n")

df  <- d[!is.na(alpha)]
re  <- as.data.frame(ddbh.fun.spp); reh <- as.data.frame(dht.fun.spp)
get_re  <- function(a){ r<-re[re$Spp==a,];  if(!nrow(r)) c(0,0,0,0) else as.numeric(r[1,c("ddbh.b0.spp","ddbh.b2.spp","ddbh.b3.spp","ddbh.b4.spp")]) }
get_reh <- function(a){ r<-reh[reh$Spp==a,]; if(!nrow(r)) c(0,0)     else as.numeric(r[1,c("dht.b0.spp","dht.b2.spp")]) }
CCFL_CONST <- 100

pred_dia <- function(s, b, c) {
  DBH <- s$DBHcm; start <- DBH; maxy <- max(s$yi)
  for (yr in seq_len(maxy)) {
    inc <- c * dDBH_fun("X", pmax(DBH,1), s$CRp, s$BALSW, s$BALHW, s$CSI, b[1],b[2],b[3],b[4])
    inc[!is.finite(inc) | inc < 0] <- 0
    a <- s$yi >= yr; DBH[a] <- DBH[a] + inc[a]
  }
  DBH - start
}

set.seed(1)
species <- names(sort(table(df$alpha), decreasing = TRUE))
out <- vector("list", length(species))
for (i in seq_along(species)) {
  a <- species[i]; sub <- df[alpha == a]
  if (nrow(sub) > CAP) sub <- sub[sample(.N, CAP)]
  ## diameter
  bd <- get_re(a); obsd <- (sub$d2 - sub$d1) * 2.54
  kd <- is.finite(obsd) & obsd > -1; sd_ <- sub[kd]; obsd <- obsd[kd]
  cdia <- optimize(function(c) sum((pred_dia(sd_, bd, c) - obsd)^2), c(0.2, 4.0))$minimum
  rd1 <- sqrt(mean((pred_dia(sd_, bd, 1)    - obsd)^2))
  rdc <- sqrt(mean((pred_dia(sd_, bd, cdia) - obsd)^2))
  meanpred_def <- mean(pred_dia(sd_, bd, 1))
  ## height from annual growth (no iteration; CCFL constant absorbed by multiplier)
  sh <- sub[is.finite(ht_ann) & ht_ann > 0]
  if (nrow(sh) >= 30) {
    bh <- get_reh(a); obsh <- sh$ht_ann * 0.3048           # ft/yr -> m/yr
    ## height proxy from dbh since measured HT not loaded: HT ~ 1.37+0.6*DBHcm^0.8 (m)
    HTm <- 1.37 + 0.6 * sh$DBHcm^0.8
    predh <- dHT_fun("X", pmax(HTm,1.4), sh$CRp, CCFL_CONST, sh$CSI, bh[1], bh[2])
    cht <- optimize(function(c) sum((predh*c - obsh)^2), c(0.2, 3.0))$minimum
    nh <- nrow(sh)
  } else { cht <- 1; nh <- nrow(sh) }
  ## NOTE: dHt.mult shipped as 1.0. The height diagnostic (cht) is printed for
  ## transparency but NOT shipped: HT_annual carries implausible outliers and the
  ## height proxy is crude, so the fit saturates and is not trustworthy. Height
  ## calibration needs measured HT_t1/HT_t2 with outlier handling (future work).
  out[[i]] <- data.frame(SP=a, n=nrow(sd_), n_ht=nh,
    dDBH.mult=round(cdia,4), dHt.mult=1, mort.mult=1, dHt_diag=round(cht,4),
    mean_obs_cm=round(mean(obsd),3), mean_def_cm=round(meanpred_def,3),
    rmse_def_cm=round(rd1,3), rmse_cal_cm=round(rdc,3), stringsAsFactors=FALSE)
  cat(sprintf("  %-3s nD=%5d dDBH=%.3f (obs %.2f def %.2f) rmse %.2f->%.2f | dHt(diag,not shipped)=%.2f\n",
              a, nrow(sd_), cdia, mean(obsd), meanpred_def, rd1, rdc, cht))
}
tab <- do.call(rbind, out); tab <- tab[order(-tab$n), ]
miss <- setdiff(sort(unique(as.character(ddbh.fun.spp$Spp))), tab$SP)
if (length(miss)) tab <- rbind(tab, data.frame(SP=miss, n=0, n_ht=0, dDBH.mult=1,
  dHt.mult=1, mort.mult=1, dHt_diag=NA, mean_obs_cm=NA, mean_def_cm=NA,
  rmse_def_cm=NA, rmse_cal_cm=NA))

outfile <- Sys.getenv("OUT_CSV", "acd_annual_calibration.csv")
write.csv(tab[,c("SP","dDBH.mult","dHt.mult","mort.mult","n","n_ht",
                 "mean_obs_cm","mean_def_cm","rmse_def_cm","rmse_cal_cm","dHt_diag")],
          outfile, row.names=FALSE)
fr <- tab[tab$n>0,]
cat(sprintf("\nwrote %s : %d fitted species\n", outfile, nrow(fr)))
cat(sprintf("diameter RMSE default=%.3f cal=%.3f (%.1f%% reduction)\n",
            mean(fr$rmse_def_cm), mean(fr$rmse_cal_cm),
            100*(mean(fr$rmse_def_cm)-mean(fr$rmse_cal_cm))/mean(fr$rmse_def_cm)))
cat("dDBH.mult range:", round(range(fr$dDBH.mult),3),
    " dHt.mult range:", round(range(fr$dHt.mult),3), "\n")
cat("DONE\n")
