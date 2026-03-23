!ODE SEGMENT CVCOM
!----------
! COMMON $Id$
!----------
!
LOGICAL LBROW,LCAL1,LCAL2,LCALIB,LCNOP,LCOV,LCOVER, &
           LCVNOH,LCVSUM,LSAGE,LSHOW,LSHRUB
!
LOGICAL LTHIND(MAXCY1)
!
INTEGER COVOPT,ICEHAB,ICVBGN,IDIST,IGFHAB,IHTYPE,INF,IOV,IPHYS, &
           ITUN,IUN,JCVNOH,JOSHRB,NKLASS,NSHOW
!
INTEGER ICVAGE(MAXCY1),ILAYR(31),INDSP(MAXCY1,2,6),ISHAPE(MAXTRE), &
           ISHOW(6),ISSP(MAXCY1,2,12),ISTAGE(MAXCY1,2)
!
REAL    CRAREA,HTMAX,HTMIN,SAGE,SUMCVR,TALLSH,TCOV
!
REAL    ASHT(MAXCY1,2),AVGBHT(3),AVGBPC(3),BHTCF(31),BPCCF(31), &
           CCON(31),CFBXHT(MAXCY1,2,16),CIND(MAXCY1,2,6), &
           CLOW(MAXCY1,2),CMED(MAXCY1,2),CRXHT(16),CTALL(MAXCY1,2), &
           CVAVG(3),CVFRAC(3),HCON(31),HIND(MAXCY1,2,6),HTAVG(3), &
           HTFRAC(3),PB(31),PBCV(31),PCON(31),PCXHT(MAXCY1,2,16), &
           PGT0(MAXCY1,2),PIND(MAXCY1,2,6),PROXHT(MAXCY1,2,16), &
           SBMASS(MAXCY1,2),SCOV(MAXCY1,2,11),SCV(MAXCY1,2,12), &
           SD2XHT(16),SDIAM(MAXCY1,2),SH(31),SHRBHT(31),SHRBPC(31), &
           SHT(MAXCY1,2,12),SPB(MAXCY1,2,12),STDHT(MAXCY1,2),TCON(2), &
           TCVOL(MAXCY1,2),TIMESD(MAXCY1,2),TOTBMS(MAXCY1,2), &
           TOTLCV(MAXCY1,2),TPCTCV(MAXCY1,2),TPROAR(MAXCY1,2), &
           TRECW(MAXTRE),TRETOT(MAXCY1,2),TRFBMS(MAXTRE), &
           TRSH(MAXCY1,2,11),TWIGS(MAXCY1,2),TXHT(MAXCY1,2,16), &
           VOLXHT(MAXCY1,2,16),XCV(31),XPB(31),XSH(31)
!
COMMON /CVCOM/ ASHT,AVGBHT,AVGBPC,BHTCF,BPCCF,CCON,CFBXHT,CIND, &
                  CLOW,CMED,COVOPT,CRAREA,CRXHT,CTALL,CVAVG,CVFRAC, &
                  HCON,HIND,HTAVG,HTFRAC,HTMAX,HTMIN,ICEHAB,ICVAGE, &
                  ICVBGN,IDIST,IGFHAB,IHTYPE,ILAYR,INDSP,INF,IOV, &
                  IPHYS,ISHAPE,ISHOW,ISSP,ISTAGE,ITUN,IUN,JCVNOH, &
                  JOSHRB,LBROW,LCAL1,LCAL2,LCALIB,LCNOP,LCOV,LCOVER, &
                  LCVNOH,LCVSUM,LSAGE,LSHOW,LSHRUB,LTHIND,NKLASS, &
                  NSHOW,PB,PBCV,PCON,PCXHT,PGT0,PIND,PROXHT,SAGE, &
                  SBMASS,SCOV,SCV,SD2XHT,SDIAM,SH,SHRBHT,SHRBPC,SHT, &
                  SPB,STDHT,SUMCVR,TALLSH,TCON,TCOV,TCVOL,TIMESD, &
                  TOTBMS,TOTLCV,TPCTCV,TPROAR,TRECW,TRETOT,TRFBMS, &
                  TRSH,TWIGS,TXHT,VOLXHT,XCV,XPB,XSH
!
!----------
!  DEFINITIONS OF VARIABLES IN 'CVCOM' COMMON BLOCK:
!----------
!  ASHT     --
!  AVGBHT   --
!  AVGBPC   --
!  BHTCF    --
!  BPCCF    --
!  CCON     --
!  CFBXHT   --
!  CIND     --
!  CLOW     --
!  CMED     --
!  COVOPT   --
!  CRAREA   --
!  CRXHT    --
!  CTALL    --
!  CVAVG    --
!  CVFRAC   --
!  HCON     --
!  HIND     --
!  HTAVG    --
!  HTFRAC   --
!  HTMAX    --
!  HTMIN    --
!  ICEHAB   --
!  ICVAGE   --
!  ICVBGN   --
!  IDIST    --
!  IGFHAB   --
!  IHTYPE   --
!  ILAYR    --
!  INDSP    --
!  INF      --
!  IOV      --
!  IPHYS    --
!  ISHAPE   --
!  ISHOW    --
!  ISSP     --
!  ISTAGE   --
!  ITUN     --
!  IUN      --
!  JCVNOH   --
!  JOSHRB   --
!  LBROW    --
!  LCAL1    --
!  LCAL2    --
!  LCALIB   --
!  LCNOP    --
!  LCOV     --
!  LCOVER   --
!  LCVNOH   --
!  LCVSUM   --
!  LSAGE    --
!  LSHOW    --
!  LSHRUB   --
!  LTHIND   --
!  NKLASS   --
!  NSHOW    --
!  PB       --
!  PBCV     --
!  PCON     --
!  PCXHT    --
!  PGT0     --
!  PIND     --
!  PROXHT   --
!  SAGE     --
!  SBMASS   --
!  SCOV     -- SCOV(I,J,K), I-CYCLE No., J=1 NO THIN =2 THIN,
!              K= TOTAL COVER OF SHRUBS GREATER THAN HEIGHT(in SHTRHT)
!  SCV      --
!  SD2XHT   --
!  SDIAM    --
!  SH       --
!  SHRBHT   --
!  SHRBPC   --
!  SHT      --
!  SPB      --
!  STDHT    --
!  SUMCVR   --
!  TALLSH   --
!  TCON     --
!  TCOV     --
!  TCVOL    --
!  TIMESD   --
!  TOTBMS   --
!  TOTLCV   --
!  TPCTCV   --
!  TPROAR   --
!  TRECW    --
!  TRETOT   --
!  TRFBMS   --
!  TRSH     --TRSH(I,J,K), I-CYCLE No., J=1 NO THIN =2 THIN,
!             K=TOTAL NUMBER OF TREES/ACRE GREATER THAN HEIGHT (SHTRHT)
!  TWIGS    --
!  TXHT     --
!  VOLXHT   --
!  XCV      --
!  XPB      --
!  XSH      --
!
!-----END SEGMENT











