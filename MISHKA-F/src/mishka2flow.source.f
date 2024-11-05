*COMDECK COMVER
      CHARACTER  VERSION*(*),   DD*(*)
      PARAMETER (VERSION = '2', DD = '24 Oct 1996')
*COMDECK COMMAX
      PARAMETER (LANZ=3, MANZ=LANZ, MDIF=1, LMAX=128)
      PARAMETER (LVANZ=3, MVANZ=3, LVMAX=LMAX)
      PARAMETER (NGMAX=801, MXNINT=NGMAX-1, NDEQ=4*MXNINT)
      PARAMETER (NPSIMAX=201, NCHIMAX=257)
      PARAMETER (NPNC=NPSIMAX*NCHIMAX, NP4=4*NPSIMAX)
      PARAMETER (NVPSIMX=201, NVCHIMX=257)
      PARAMETER (NVPNVC=NVPSIMX*NVCHIMX,NVP4=4*NVPSIMX)
*COMDECK COMPAR
      PARAMETER (NGL=7, NBG=2*NGL*MANZ, NZMA=2*NBG, NB3=3*NBG)
c      PARAMETER (KILWOR=15000000, NDIM1=1500/NBG*NBG)
      PARAMETER (KILWOR=30000000, NDIM1=1000/NBG*NBG)
      PARAMETER (KPRGR=604999+11*NDEQ+NGMAX)
      PARAMETER (KPMAX=KILWOR-KPRGR, KPMEX=KPMAX-8*NBG*NBG)
*IF CRAY
      PARAMETER (NREST1=KPMEX-4*NDIM1**2-4*NDIM1)
      PARAMETER (NREST1D=NREST1)
*ELSE
      PARAMETER (NREST1=KPMEX-4*NDIM1**2-3*NDIM1-NDIM1/2)
      PARAMETER (NREST1D=KPMEX-4*NDIM1**2-4*NDIM1)
*ENDIF
*COMDECK COMPAR2
      PARAMETER (ML=2*NBG-1)
      PARAMETER (MU=ML)
      PARAMETER (LDA=3*(2*NBG-1)+1,LDB=LDA-ML)
      PARAMETER (NDIM2=(KPMEX/(2*LDA+9))/NBG*NBG)
*IF CRAY
      PARAMETER (NREST2=KPMEX-2*LDA*NDIM2-9*NDIM2)
*ELSE
      PARAMETER (NREST2=KPMEX-2*LDA*NDIM2-8*NDIM2-NDIM2/2)
*ENDIF
*COMDECK COMPAR3
      PARAMETER (NCV=(KPMEX-2*NBG*(2*NGMAX+NB3+6))/(4*NBG*NB3+14*NBG))
      PARAMETER (NDIM3=NBG*NB3*NCV*2)
      PARAMETER (NDIM3H=NDIM3/2)
*IF CRAY
      PARAMETER (N3=KPMEX-2*NBG*(2*NCV*(NB3+3)+2*NGMAX+NB3+6)-2*NBG*NCV)
*ELSE
      PARAMETER (N3=KPMEX-2*NBG*(2*NCV*(NB3+3)+2*NGMAX+NB3+6)-NBG*NCV)
*ENDIF
      PARAMETER (NREST3=N3)
*COMDECK COMPAR4
      PARAMETER (NCVIC=(KPMEX-NB3*(4+2*NBG))/(2*NBG*NB3+9*NBG))
      PARAMETER (NDIM4=NBG*NB3*NCVIC)
*IF CRAY
      PARAMETER (NREST4=KPMEX-9*NBG*NCVIC-2*NB3*(NBG*NCVIC+NBG+2))
*ELSE
      PARAMETER (NSP4=8*NBG*NCVIC+2*NB3*(NBG*NCVIC+NBG+2)+NBG*NCVIC/2)
      PARAMETER (NREST4=KPMEX-NSP4)
*ENDIF
*COMDECK COMP234
      PARAMETER (NCVD=(KPMEX-NBG*(12+6*NBG)-3*NGL*NGL)/(4*MANZ+2*NBG+2))
      PARAMETER (NPDIM=2*NCVD-1)
      PARAMETER (ND=2*NBG*NCVD+2*NB3*(NBG+2)+3*NGL**2+NPDIM*(2*MANZ+1))
      PARAMETER (NR234=KPMEX-ND)
*COMDECK COMPAR5
      PARAMETER (LDAA=3*(2*NBG-1)+1)
      PARAMETER (MXAB=51, MXLP=12)
      PARAMETER (NDIM5=(KPMEX/(3*LDAA+30))/NBG*NBG)
      PARAMETER (MLLZ=2*NBG-1)
      PARAMETER (MULZ=MLLZ)
      PARAMETER (LDAL=LDAA-MULZ)
      PARAMETER (LDBL=MULZ+1)
*IF CRAY
      PARAMETER (NREST5=KPMEX-NDIM5*(2*LDAA+16+LDAL+LDBL+6+7))
*ELSE
      PARAMETER (NREST5=KPMEX-NDIM5*(2*LDAA+16+LDAL+LDBL+6)-7*NDIM5/2)
*ENDIF
*COMDECK COMPARV
      PARAMETER (NGLV=1, NBGV=2*NGLV*MVANZ, NZMAV=2*NBGV, NB3V=3*NBGV)
      PARAMETER (NGVMAX=101, NPDIMV=2*NGVMAX)
      PARAMETER (MLV=2*NBGV-1)
      PARAMETER (MUV=MLV)
      PARAMETER (LDAV=3*(2*NBGV-1)+1)
*COMDECK COMPIO
*IF IPP
      PARAMETER (NIN=5, NIN2=2, NOUT=6, NOUTI=8, NOUT2=1, NOUT3=10)
      PARAMETER (NOUTP=11, NOUTV=20, NOUTE=21, NOUTVB=22, NOUTB=23)
C *ELSEIF JET
C       PARAMETER (NIN=5, NIN2=2, NOUT=6, NOUTI=8, NOUT2=1, NOUT3=10)
C       PARAMETER (NOUTP=11, NOUTV=21, NOUTE=22, NOUTVB=23, NOUTB=24)
*ELSE
      PARAMETER (NIN=10, NIN2=11, NOUT=20, NOUTI=21, NOUT2=21, NOUT3=23)
      PARAMETER (NOUTP=24, NOUTV=25, NOUTE=26, NOUTVB = 27, NOUTB=28)
*ENDIF
      PARAMETER (NMAP=12)
      PARAMETER (ND3=15, ND4=16, ND5=17, ND6=18)
*COMDECK COMPARP
      PARAMETER (IF1 =0)
      PARAMETER (IXC1=2)
      PARAMETER (IXC2=7)
      PARAMETER (IXC3=8)
      PARAMETER (IXQ1=1)
      PARAMETER (IXQ2=3)
      PARAMETER (IXQ3=4)
      PARAMETER (IXQ4=5)
      PARAMETER (IXQ5=6)
*COMDECK COMPCON
      REAL       PI, ZERO, ONE
      COMPLEX    ZEROC, ONEC, CHALF, CTWO
      PARAMETER (NMAX=201)
      PARAMETER (PI=3.141592653589793)
      PARAMETER (ZERO=0.E0, ONE=1.E0)
      PARAMETER (ZEROC=(0.E0,0.E0), ONEC=(1.E0,0.E0))
      PARAMETER (CHALF=(.5E0,0.E0), CTWO=(2.E0,0.E0))
C-----------------------------------------------------------------------
*COMDECK CORE
      COMMON / CORE /
     C         ZMA(NZMA,NZMA),
     R         AP(KPMEX)
C
      COMPLEX  ZMA
      REAL     AP
C-----------------------------------------------------------------------
*COMDECK CORE1
      COMMON / CORE /
     C         ZMA(NZMA,NZMA),
     C         AMAT(NDIM1,NDIM1), BMAT(NDIM1,NDIM1),
C     R         WR(NDIM1), WI(NDIM1),
C     R         EVMAG(1,NDIM1),
C     R         HCOR(NREST1),
C     I         INDEX(NDIM1)
     C         WRI(NDIM1), VRI(1,1)
 
      COMPLEX  ZMA, AMAT, BMAT, WRI, VRI
C      REAL     WR, WI, EVMAG, HCOR
C      INTEGER  INDEX
C-----------------------------------------------------------------------
*COMDECK CORE2
      COMMON / CORE /
     C         ZMA(NZMA,NZMA),
     C         X0(NDIM2), X1(NDIM2), Y0(NDIM2), Y1(NDIM2),
     C         AMAT(LDA,NDIM2), BMAT(LDB,NDIM2),
     R         HCOR(NREST2),
     I         IPVT(NDIM2)
 
      COMPLEX  ZMA, X0, X1, Y0, Y1, AMAT, BMAT
      REAL     HCOR
      INTEGER  IPVT
C-----------------------------------------------------------------------
*COMDECK CORE3
      COMMON / CORE /
     C         ZMA(NZMA,NZMA), EV(NBG,NGMAX,2),
     C         APR(NBG,NB3,NCV,2), X(NBG,NCV,2,3),
     C         BUFF(NBG,NB3), HVX(NBG,3), HVX2(NBG,3),
     R         HCOR(NREST3),
     I         IPVT(NBG,NCV,2)
 
      COMPLEX  ZMA, EV, APR, X, BUFF, HVX, HVX2
      REAL     HCOR
      INTEGER  IPVT
C-----------------------------------------------------------------------
*COMDECK CORE4
      COMMON / CORE /
     C         ZMA(NZMA,NZMA),
     C         EV(NBG,NCVIC,2),
     C         X(NBG,NCVIC,2), APR(NBG,NB3,NCVIC),
     C         BUFF(NBG,NB3), HVX(NB3), HVX2(NB3),
     R         HCOR(NREST4),
     I         IPVT(NBG,NCVIC)
 
      COMPLEX  ZMA, EV, X, APR, BUFF, HVX, HVX2
      REAL     HCOR
      INTEGER  IPVT
C-----------------------------------------------------------------------
*COMDECK CORE5
      COMMON / CORE /
     C         ZMA(NZMA,NZMA),
     C         AA(LDAA,NDIM5),
     C         VS(NDIM5), WS(NDIM5),
     C         V1(NDIM5), V2(NDIM5), W1(NDIM5), W2(NDIM5),
     C         CONEV(NDIM5), CONEVB(NDIM5),
     R         AORIG(LDAL,NDIM5), BB(LDBL,NDIM5),
     R         ERREV(NDIM5), ERREVB(NDIM5),
     R         GR(NDIM5), GC(NDIM5), G(NDIM5), GG(NDIM5),
     R         HCOR(NREST5),
     I         IPVT(NDIM5), MP(NDIM5), MP2(NDIM5),
     I         MULEV(NDIM5), MULEVB(NDIM5), ISIGMA(NDIM5), ILOOP(NDIM5)
 
      COMPLEX  ZMA, AA, VS, WS, V1, V2, W1, W2, CONEV, CONEVB
      REAL     AORIG, BB, ERREV, ERREVB, GR, GC, G, GG, HCOR
      INTEGER  IPVT, MP, MP2, MULEV, MULEVB, ISIGMA, ILOOP
C-----------------------------------------------------------------------
*COMDECK CORE1D
      COMMON / CORE /
     C         ZMA(NZMA,NZMA),
     C         AMAT(NDIM1,NDIM1), BMAT(NDIM1,NDIM1),
     R         WR(NDIM1), WI(NDIM1),
     R         X(NDIM1), Y(NDIM1),
     R         HCOR(NREST1D)
C
      COMPLEX  ZMA, AMAT, BMAT
      REAL     WR, WI, X, Y, HCOR
C-----------------------------------------------------------------------
*COMDECK CORE234D
      COMMON / CORE /
     C         ZMA(NZMA,NZMA),
     C         EV(NBG,NCVD),
     C         BUFF(NBG,NB3), HVX(NB3), HVX2(NB3),
     C         XTAX(NGL*NGL),
     R         YP(NPDIM,2,MANZ), XP(NPDIM), PROZ(NGL*NGL),
     R         HCOR(NR234)
C
      COMPLEX  ZMA, EV, BUFF, HVX, HVX2, XTAX
      REAL     YP, XP, PROZ, HCOR
C-----------------------------------------------------------------------
*COMDECK COMMOD
      COMMON / COMMOD  / MODE
      INTEGER            MODE
C-----------------------------------------------------------------------
*COMDECK COMDIM
      COMMON / COMDIM  / NDIM
      INTEGER            NDIM
C-----------------------------------------------------------------------
*COMDECK COMLAB
      COMMON / COMLAB   / LABEL,    EQNAME
      CHARACTER          LABEL*34, EQNAME*10
C-----------------------------------------------------------------------
*COMDECK COMDIM5
      COMMON / COMDIM5 / MUL, MLL, NCONV
      INTEGER            MUL, MLL, NCONV
C-----------------------------------------------------------------------
*COMDECK COMIT
      COMMON / COMIT /
     C         EWSHIFT, EW, EWTEST,
     R         EPS,
     I         IT, ITER
C
      COMPLEX  EWSHIFT, EW, EWTEST
      REAL     EPS
      INTEGER  IT, ITER
C-----------------------------------------------------------------------
*COMDECK COMINT
      COMMON / COMINT /
     I         NREC,
     I         CPLG, CPLGA, CPLGX, CPLGXA, CPLGXS,
     I         LREAD, LWRIT
C
      INTEGER  NREC, CPLG, CPLGA, CPLGX, CPLGXA, CPLGXS, LREAD, LWRIT
C-----------------------------------------------------------------------
*COMDECK COMBX
      COMMON / COMBX   / NSHIFT
      INTEGER            NSHIFT
C-----------------------------------------------------------------------
*COMDECK COMDIAG
      COMMON / COMDIAG / NDIAGFK,IBVAC
      INTEGER            NDIAGFK,IBVAC
C-----------------------------------------------------------------------
*COMDECK COMPLOT
      COMMON / COMPLOT /
     R         XMINQR(5), XMAXQR(5), YMINQR(5), YMAXQR(5),
     I         NPLOT
 
      REAL     XMINQR, XMAXQR, YMINQR, YMAXQR
      INTEGER  NPLOT
C-----------------------------------------------------------------------
*COMDECK COMGRID
      COMMON / COMGRID / SGRID(NGMAX), NG, NGINT
      REAL               SGRID
      INTEGER            NG, NGINT
C-----------------------------------------------------------------------
*COMDECK COMWEL
      COMMON / COMWEL  / RFOUR(MANZ), VFOUR(MVANZ),
     >                   MSTART(NGMAX), ZNKWEL, NTOR
      REAL               RFOUR, VFOUR, ZNKWEL, MSTART
      INTEGER            NTOR
C-----------------------------------------------------------------------
*COMDECK COMGEW
      COMMON / COMGEW  / GEWI(4)
      REAL               GEWI
C-----------------------------------------------------------------------
*COMDECK COMGEO
      COMMON / COMGEO  / ASPECT
      REAL               ASPECT
C-----------------------------------------------------------------------
*COMDECK COMEQUI
      COMMON / COMEQUI /
     R         GAMMA, ETA, PSIS, Q0ZYL, DSURF, DSURF1, ALPHIN,
     R         DSURF2, DSURF3, ZMU, ZTE, ZTI, CWW,
     R         VEL3, VSURF, VSURF1, VSURF2, VSURF3, VSURF4,
     I         IDPOW, IEQ, IAS
C
      REAL     GAMMA, PSIS, Q0ZYL, DSURF, DSURF1, ALPHIN
      REAL     DSURF2, DSURF3, ZMU, ZTE, ZTI, CWW
      INTEGER  IDPOW, IEQ, IAS
      COMPLEX  ETA
C-----------------------------------------------------------------------
*COMDECK COMEQV
      COMMON / COMEQV /
     R         SGI(NDEQ), Q(NDEQ), DQ(NDEQ), T(NDEQ), DT(NDEQ),
     R         ETAV(NDEQ), DETA(NDEQ), RHO(NDEQ), DRHO(NDEQ),
     R         ZT0(NDEQ), ZDT0(NDEQ),ZDDT0(NDEQ),
     R         FLOW3(NDEQ), DFLOW3(NDEQ)
C
      REAL     SGI, Q, DQ, T, DT, RHO, DRHO, ZT0, ZDT0, ZDDT0
      REAL     FLOW3,DFLOW3
      COMPLEX  ETAV,DETA
C-----------------------------------------------------------------------
*COMDECK COMGEM
      COMMON / COMGEM / GEM11(NPNC), GEM12(NPNC), GEM33(NPNC)
      REAL              GEM11, GEM12, GEM33
C-----------------------------------------------------------------------
*COMDECK COMIOD
      COMMON / COMIOD /
     R         CPSURF, CS(NPSIMAX), QS(NPSIMAX), DQS(NPSIMAX),
     R         CURJ(NPSIMAX), CHI(NCHIMAX), P0(NPSIMAX), RBPHI(NPSIMAX),
     R         OMEGAS(NPSIMAX),OMEGA2(NPSIMAX),
     I         NPSI, NCHI,
     L         NLTORE
C
      REAL     CPSURF, CS, QS, DQS, CURJ, CHI,P0, RBPHI
      INTEGER  NPSI, NCHI
      LOGICAL  NLTORE
C-----------------------------------------------------------------------
*COMDECK COMMEW
      COMMON / COMMEW /
     C         VSHIFT(100),
     R         DRS, DIS,
     I         NRS, NIS
C
      COMPLEX  VSHIFT
      REAL     DRS, DIS
      INTEGER  NRS, NIS
C-----------------------------------------------------------------------
*COMDECK COMSPL
      COMMON / COMSPL /
     R         Q1(NPSIMAX), Q2(NPSIMAX), Q3(NPSIMAX), Q4(NPSIMAX),
     R         C1(NPSIMAX), C2(NPSIMAX), C3(NPSIMAX), C4(NPSIMAX),
     R         P1(NPSIMAX), P2(NPSIMAX), P3(NPSIMAX), P4(NPSIMAX),
     R         RBP1(NPSIMAX), RBP2(NPSIMAX), RBP3(NPSIMAX),
     R         RBP4(NPSIMAX)
C
      REAL     Q1, Q2, Q3, Q4, C1, C2, C3, C4,
     >         P1, P2, P3, P4, RBP1, RBP2, RBP3, RBP4
C-----------------------------------------------------------------------
*COMDECK COMFFT
      COMMON / COMFFT /
     R       RR2(NP4,MANZ+11),RR4(NP4,MANZ+11),
     R       RGPSI(NP4,MANZ+11),RSGGG(NP4,MANZ+11),
     R       RSOGPSI(NP4,MANZ+11),RGPGT(NP4,MANZ+11),
     R       RSR2GGG(NP4,MANZ+11),RR2GPSI(NP4,MANZ+11),
     R       RSOR2GP(NP4,MANZ+11),RR4GPSI(NP4,MANZ+11),
     R       RSR2OGP(NP4,MANZ+11),RR2GPGT(NP4,MANZ+11),
     R       RR4GPGT(NP4,MANZ+11),RDSR2(NP4,MANZ+11),
     R       RDSGPSI(NP4,MANZ+11),ROOR2(NP4,MANZ+11),
     R       RSR4GGG(NP4,MANZ+11),RR4GPT2(NP4,MANZ+11),
     R       RGPSI2(NP4,MANZ+11),RGPGPGT(NP4,MANZ+11),
     R       RGPDSGP(NP4,MANZ+11),RR2DSGP(NP4,MANZ+11),
     R       RGPSIOR(NP4,MANZ+11),RGPGT2(NP4,MANZ+11),
     R       RGPTDSG(NP4,MANZ+11),RFR2GT2(NP4,MANZ+11),
     R       RDSGPGT(NP4,MANZ+11),RFR2(NP4,MANZ+11),
     R       RFR2GPT(NP4,MANZ+11),RFR2GPS(NP4,MANZ+11),
     R       RFR4GPT(NP4,MANZ+11),RFR4GPS(NP4,MANZ+11),
     R       RGPSI2DSR2(NP4,MANZ+11),RGPSIDSR2(NP4,MANZ+11),
     R       RGPGT2DSR2(NP4,MANZ+11),RGPGTGPDSR2(NP4,MANZ+11),
     R       RPTOR2DSR2(NP4,MANZ+11),RR2GPTGPDSR2(NP4,MANZ+11),
     R       RDTR2(NP4,MANZ+11), RDTR2OR2(NP4,MANZ+11),
     R       RDTR2OR4(NP4,MANZ+11),RR2GPGT2DTR2(NP4,MANZ+11),
     R       RGPGPGTDTR2(NP4,MANZ+11),RPTOR2DTR2(NP4,MANZ+11),
     R       RGPSIOR2DSR2(NP4,MANZ+11),RDSR2OR2(NP4,MANZ+11),
     R       RGPGTDSR2(NP4,MANZ+11),RGPGT2DTR2(NP4,MANZ+11),
     R       RR4GPTOGP(NP4,MANZ+11), RR2GPTDSR2(NP4,MANZ+11),
     R       RR2GPDSR2(NP4,MANZ+11),RGPTDTR2(NP4,MANZ+11),
     R       RR2GGG2DSR2(NP4,MANZ+11),RR2GPGT3DTR2(NP4,MANZ+11),
     R       RGPGPT2DSR2(NP4,MANZ+11),RGPGT3DTR2(NP4,MANZ+11),
     R       RR2GP2GPTDSR2(NP4,MANZ+11),RR2GGG2DTR2(NP4,MANZ+11),
     R       RR2GPGTDTR2(NP4,MANZ+11),RR2GPDTR2(NP4,MANZ+11),
     R       RGPSIDTR2(NP4,MANZ+11),RFDTR2(NP4,MANZ+11),
     R       RB02(NP4,MANZ+11), IB02(NP4,MANZ+11),
     R       IR2(NP4,MANZ+11),IR4(NP4,MANZ+11),
     R       IGPSI(NP4,MANZ+11),ISGGG(NP4,MANZ+11),
     R       ISOGPSI(NP4,MANZ+11),IGPGT(NP4,MANZ+11),
     R       ISR2GGG(NP4,MANZ+11),IR2GPSI(NP4,MANZ+11),
     R       ISOR2GP(NP4,MANZ+11),IR4GPSI(NP4,MANZ+11),
     R       ISR2OGP(NP4,MANZ+11),IR2GPGT(NP4,MANZ+11),
     R       IR4GPGT(NP4,MANZ+11),IDSR2(NP4,MANZ+11),
     R       IDSGPSI(NP4,MANZ+11),IOOR2(NP4,MANZ+11),
     R       ISR4GGG(NP4,MANZ+11),IR4GPT2(NP4,MANZ+11),
     R       IGPSI2(NP4,MANZ+11),IGPGPGT(NP4,MANZ+11),
     R       IGPDSGP(NP4,MANZ+11),IR2DSGP(NP4,MANZ+11),
     R       IGPSIOR(NP4,MANZ+11),IGPGT2(NP4,MANZ+11),
     R       IGPTDSG(NP4,MANZ+11),IFR2GT2(NP4,MANZ+11),
     R       IDSGPGT(NP4,MANZ+11),IFR2(NP4,MANZ+11),
     R       IFR2GPT(NP4,MANZ+11),IFR2GPS(NP4,MANZ+11),
     R       IFR4GPT(NP4,MANZ+11),IFR4GPS(NP4,MANZ+11),
     R       IGPSI2DSR2(NP4,MANZ+11),IGPSIDSR2(NP4,MANZ+11),
     R       IGPGT2DSR2(NP4,MANZ+11),IGPGTGPDSR2(NP4,MANZ+11),
     R       IPTOR2DSR2(NP4,MANZ+11), IR2GPTGPDSR2(NP4,MANZ+11), 
     R       IDTR2(NP4,MANZ+11),IDTR2OR2(NP4,MANZ+11),
     R       IDTR2OR4(NP4,MANZ+11),IR2GPGT2DTR2(NP4,MANZ+11),
     R       IGPGPGTDTR2(NP4,MANZ+11),IPTOR2DTR2(NP4,MANZ+11),
     R       IGPSIOR2DSR2(NP4,MANZ+11),IDSR2OR2(NP4,MANZ+11),
     R       IGPGTDSR2(NP4,MANZ+11),IGPGT2DTR2(NP4,MANZ+11),
     R       RR2GPSI2DSR2(NP4,MANZ+11), IR2GPSI2DSR2(NP4,MANZ+11),
     R       RR2GPGT2DSR2(NP4,MANZ+11), IR2GPGT2DSR2(NP4,MANZ+11),
     R       IR4GPTOGP(NP4,MANZ+11), IR2GPTDSR2(NP4,MANZ+11),
     R       IR2GPDSR2(NP4,MANZ+11),IGPTDTR2(NP4,MANZ+11),
     R       IR2GGG2DSR2(NP4,MANZ+11),IR2GPGT3DTR2(NP4,MANZ+11),
     R       IGPGPT2DSR2(NP4,MANZ+11),IGPGT3DTR2(NP4,MANZ+11),
     R       IR2GP2GPTDSR2(NP4,MANZ+11),IR2GGG2DTR2(NP4,MANZ+11),
     R       IR2GPGTDTR2(NP4,MANZ+11),IR2GPDTR2(NP4,MANZ+11),
     R       IGPSIDTR2(NP4,MANZ+11),IFDTR2(NP4,MANZ+11),
     I       NP1, N2P1, N3P1
C
      REAL     RR2, RR4, RGPSI, RSOGPSI, RGPGT, RSGGG, RSR2GGG,
     >         RR2GPSI, RSOR2GP, RSR2OGP, RR2GPGT, RR4GPSI, RR4GPGT,
     >         RDSR2, RDSGPSI, RSR4GGG, IR2, IR4, IGPSI, ISOGPSI,
     >         IGPGT, ISGGG, ISR2GGG, IR2GPSI, ISOR2GP, ISR2OGP,
     >         IR2GPGT, IR4GPSI, IR4GPGT, IDSR2, IDSGPSI, ISR4GGG,
     >         RR4GPT2, IR4GPT2, ROOR2, RDSGPGT, IOOR2,
     >         RGPSI2, IGPSI2, RGPGPGT, IGPGPGT, RGPDSGP,IGPDSGP,
     >         RGPSIOR,IGPSIOR, RGPGT2, IGPGT2, RFR4GPS, IFR4GPS,
     >         RR2DSGP,IR2DSGP, RGPTDSG, IGPTDSG, RFR4GPT,
     >         RFR2GPS,IFR2GPS, RFR2GPT, IFR2GPT, RFR2, IFR2,
     >         RFR2GT2, IFR2GT2, IDSGPGT, IFR4GPT,RGPGT2DTR2,
     >         RGPSI2DSR2, IGPSI2DSR2, RGPSIDSR2, IGPSIDSR2,
     >         RGPGT2DSR2,IGPGT2DSR2, RGPGTGPDSR2, IGPGTGPDSR2,
     >         RPTOR2DSR2, IPTOR2DSR2, RR2GPTGPDSR2, IR2GPTGPDSR2,
     >         RDTR2,IDTR2, RDTR2OR2, IDTR2OR2, RDTR2OR4, IDTR2OR4,
     >         RR2GPGT2DTR2,IR2GPGT2DTR2, RGPGPGTDTR2,IGPGPGTDTR2,
     >         RPTOR2DTR2,IPTOR2DTR2, RGPSIOR2DSR2,IGPSIOR2DSR2,
     >         RDSR2OR2, IDSR2OR2, RGPGTDSR2,IGPGTDSR2,IGPGT2DTR2,
     >         RR2GPSI2DSR2,IR2GPSI2DSR2,RR2GPGT2DSR2,IR2GPGT2DSR2,
     >         RR4GPTOGP, IR4GPTOGP,RR2GPTDSR2,IR2GPTDSR2,
     >         RR2GPDSR2, IR2GPDSR2, RGPTDTR2, IGPTDTR2,
     >         RR2GGG2DSR2, IR2GGG2DSR2,RR2GPGT3DTR2,IR2GPGT3DTR2,
     >         RGPGPT2DSR2, IGPGPT2DSR2,RGPGT3DTR2,IGPGT3DTR2,
     >         RR2GP2GPTDSR2,IR2GP2GPTDSR2,RR2GGG2DTR2,
     >         IR2GGG2DTR2, RR2GPGTDTR2, IR2GPGTDTR2,
     >         RR2GPDTR2, IR2GPDTR2, RGPSIDTR2, IGPSIDTR2,
     >         RFDTR2,IFDTR2, RB02,IB02
      INTEGER  NP1, N2P1, N3P1
C-----------------------------------------------------------------------
*COMDECK COMESH
      COMMON / COMESH  / R0, RA, PALAC, PS0AC, PSIGAC
      REAL               R0, RA, PALAC, PS0AC, PSIGAC
C-----------------------------------------------------------------------
*COMDECK COMVGRD
      COMMON / COMVGRD /
     R         SVGRID(NGVMAX), CSV(NGVMAX), SVGI(4*(NGVMAX-1)),
     R         SIGV, NGV
      REAL     SVGRID, CSV, SVGI, SIGV
C-----------------------------------------------------------------------
*COMDECK COMVAC
      COMMON / COMVAC /
     R         RWALL,           FD,              RMIN,
     R         TC(NVCHIMX),     RW(NVCHIMX),     RP(NVCHIMX),
     R         FTC(2*NVCHIMX),  FRW(2*NVCHIMX),  FRP(2*NVCHIMX),
     R         DTC(NVCHIMX),    DRW(NVCHIMX),    DRP(NVCHIMX),
     R         VJ(NVPNVC),      VG11(NVPNVC),    VG12(NVPNVC),
     R         VG22(NVPNVC),    VG33(NVPNVC),    FW(NVCHIMX),
     I         NVPSI, NVCHI, NFW, IVAC
C
      REAL     RWALL, FD, RMIN, TC, RW, RP, FTC, FRW, FRP, DTC,
     >         DRW, DRP, VJ, VG11, VG12, VG22, VG33, FW
      INTEGER  NVPSI, NVCHI, NFW, IVAC
C-----------------------------------------------------------------------
*COMDECK COMJET                                                         
      COMMON / COMJET /                                                 
     R         RZV(2,73),THTV(73),
     R         RV1(73),RV2(73),RV3(73),RV4(73),                         
     R         RCNTR,ZCNTR,
     I         NV                                                   
C                                                                       
      REAL     RZV,THTV,RV1,RV2,RV3,RV4,RCNTR,ZCNTR
      INTEGER  NV 
C-----------------------------------------------------------------------
*COMDECK COMVFT
      COMMON / COMVFT /
     R         VR11(NVP4,LVANZ), VR12(NVP4,LVANZ), VR22(NVP4,LVANZ),
     R         VR33(NVP4,LVANZ), VI11(NVP4,LVANZ), VI12(NVP4,LVANZ),
     R         VI22(NVP4,LVANZ), VI33(NVP4,LVANZ),
     I         NVP1,NV2P1,NV3P1
C
      REAL     VR11, VR12, VR22, VR33, VI11, VI12, VI22, VI33
      INTEGER  NVP1,NV2P1,NV3P1
C-----------------------------------------------------------------------
*COMDECK COMBND
      COMMON / COMBND  /
     R         ASPI, RADIUS, VX(NVCHIMX), VY(NVCHIMX), VC(NVCHIMX)
      REAL     ASPI, RADIUS, VX, VY, VC
C-----------------------------------------------------------------------
*COMDECK COMVRSP
      COMMON / COMVRSP / B3B1(MANZ,MANZ)
      COMPLEX            B3B1
C-----------------------------------------------------------------------
*COMDECK COMVMAT
      COMMON / CORE /
     C         VMAT(LDAV,NBGV*NGVMAX), B(NBGV*NGVMAX), Q(NBGV*NGVMAX)
C
      COMPLEX  VMAT, B, Q
C-----------------------------------------------------------------------
*COMDECK COMBDIA
      COMMON / COMBDIA /
     R         RPSOR2(NVP4,LVANZ), IPSOR2(NVP4,LVANZ),
     R         RPTOR2(NVP4,LVANZ), IPTOR2(NVP4,LVANZ)
C
      REAL     RPSOR2, IPSOR2, RPTOR2, IPTOR2
C-----------------------------------------------------------------------
*COMDECK COMESH2
      COMMON / COMESH2 / RS0, RSA, BGF, XR1, XR2, SIG1,SIG2,FACT,
     >                   SBEGIN,SEND,IMESHAC
      REAL               RS0, RSA, BGF, XR1, XR2, SIG1,SIG2,FACT
      INTEGER            IMESHAC
C---------------------------------------------------------------------
*COMDECK COMSTVR
      COMMON / COMSTVR / SX, SY
*IF CRAY
      REAL               SX, SY
*ELSE
      REAL               SX(NGMAX*NBG), SY(NGMAX*NBG)
*ENDIF
C-----------------------------------------------------------------------
*COMDECK ISEED
      COMMON ISEED
C-----------------------------------------------------------------------
*COMDECK SHIFT
      COMMON / SHIFT   / SIGMA
      COMPLEX            SIGMA
C-----------------------------------------------------------------------
*COMDECK MACHT
      COMMON / MACHT   / MACHEP
      REAL               MACHEP
C-----------------------------------------------------------------------
*COMDECK LANTOL
      COMMON / LANTOL  / LTOL
      REAL               LTOL
C-----------------------------------------------------------------------
*COMDECK LANCZOS
      COMMON / LANCZOS /
     C         OWS(100),
     R         XLIML, XLIMR, YLIMB, YLIMT,
     R         XHOLEL, XHOLER, YHOLEB, YHOLET,
     I         NUS, ISHIFT, ISTART, ISTOP, KMAX, MXLOOP,
     L         IHOLE
C
      COMPLEX  OWS
      REAL     XLIML, XLIMR, YLIMB, YLIMT,
     >         XHOLEL, XHOLER, YHOLEB, YHOLET
      INTEGER  NUS, ISHIFT, ISTART, ISTOP, KMAX, MXLOOP
      LOGICAL  IHOLE
C-----------------------------------------------------------------------
*COMDECK CONLAN
      COMMON / CONLAN  /
     R         RELTOL,
     I         SVSEED, SAVTEV, MXINIT
C
      REAL     RELTOL
      INTEGER  SVSEED, SAVTEV, MXINIT
C-----------------------------------------------------------------------
*DECK MISHKA
      PROGRAM MISHKA
C
************************************************************************
************************************************************************
**                                                                    **
**  M A S T E R F I L E  :  MISHKA                                    **
**  ------------------------------                                    **
**                                                                    **
**  AUTHORS :         G. HUYSMANS, A. MIKHAILOVSKII,                  **
**                    W. KERNER, S. SHARAPOV                          **
**                                                                    **
**  VERSION :     2                                                   **
**           - seven variables resistive MHD version                  **
*CALL COMVER
**                                                                    **
************************************************************************
************************************************************************
**                                                                    **
**  INPUT :                                                           **
**  -----                                                             **
**                                                                    **
**  EQUILIBRIUM : READ FROM DISK (UNIT NMAP)                          **
**                                                                    **
**  NAMELIST NEWRUN :  ON UNIT NIN (DEFINED IN COMPIO = 5)            **
**                                                                    **
**    MODE     - CONTROL PARAMETER;                                   **
**                VALUES:                                              **
**                0 - TERMINATION OF EXECUTION                        **
**                1 - QR ALGORITHM                                    **
**                2 - VECTOR ITERATION, IN-CORE (STEUERWALD)           **
**                3 - VECTOR ITERATION, OUT-OF-CORE (SCHWARZ)         **
**                4 - VECTOR ITERATION, IC VERSION OF OOC SOLVER      **
**                5 - LANCZOS ALGORITHM                               **
**               11 - TESTCASE SOLOVEV (ASPECT=3,ELLIPT=2), SOLVER 1  **
**               12 - TESTCASE SOLOVEV (ASPECT=3,ELLIPT=2), SOLVER 2  **
**               13 - TESTCASE SOLOVEV (ASPECT=3,ELLIPT=2), SOLVER 3  **
**               14 - TESTCASE SOLOVEV (ASPECT=3,ELLIPT=2), SOLVER 4  **
**               15 - TESTCASE SOLOVEV (ASPECT=3,ELLIPT=2), SOLVER 5  **
**                                                                    **
**    EQNAME   - NAME OF THE EQUILIBRIUM                              **
**    NLTORE   - TOROIDAL EQUILIBRIUM (.T. OR .F.)                    **
**    NG       - NUMBER OF GRID POINTS                                **
**    RFOUR(1) - LOWEST POLOIDAL MODE NUMBER                          **
**    NTOR     - TOROIDAL MODE NUMBER                                 **
**    ASPECT   - ASPECT RATIO (ONLY IF NLTORE=.F.)                    **
**    Q0ZYL    - Q ON AXIS                                            **
**    SIG1     - SIGMA OF FIRST MESHACC. POINT                        **
**    SIG2     - SIGMA OF SECOND MESHACC. POINT                       **
**    XR1      - POSITION OF FIRST MESHACC. POINT                     **
**    XR2      - POSITION OF SECOND MESHACC. POINT                    **
**    RWALL    - POSITION OF THE WALL                                 **
**    NVPSI    - NUMBER OF RADIAL POINTS IN VACUUM METRIC             **
**    NGV      - NUMBER OF RADIAL POINTS IN VAC. POTENTIAL SOL.       **
**    SIGV     - SIGMA OF MESHACC. IN VACUUM (XR = 0.)                **
**    DSURF    - PARAMETER FOR DENSITY PROFILE :                      **
**    IDPOW    - PARAMETER FOR DENSITY PROFILE :                      **
**               RHO = ( 1 - (1 - DSURF) * S**2 )**IDPOW              **
**    NDIAGFK  - PRINT SWITCH FOR FOURIER COMPONENTS OF THE METRIC    **
**                                                                    **
**    VSHIFT(I)- I-TH ESTIMATE OF EIGENVALUE FOR VECTOR ITERATION     **
**               (I.LE.100); IF AUTOMATIC :                           **
**    NRS      - NUMBER OF AUTOMATIC REAL      SHIFTS                 **
**    NIS      - NUMBER OF AUTOMATIC IMAGINARY SHIFTS                 **
**    DRS      - WIDTH  OF AUTOMATIC REAL      SHIFTS                 **
**    DIS      - WIDTH  OF AUTOMATIC IMAGINARY SHIFTS                 **
**                                                                    **
**    EPS      - RELATIVE ACCURACY OF EIGENVALUE (VECTOR ITERATION)   **
**    NPLOT    - PLOT SWITCH FOR SOLVERS 1 - 4,                       **
**               NUMBER OF PLOTS FOR QR-SOLVER                        **
**    XMINQR(I)- LOWER LIMIT X-AXIS FOR I-TH QR-PLOT                  **
**    XMAXQR(I)- UPPER LIMIT X-AXIS FOR I-TH QR-PLOT                  **
**    YMINQR(I)- LOWER LIMIT Y-AXIS FOR I-TH QR-PLOT                  **
**    YMAXQR(I)- UPPER LIMIT Y-AXIS FOR I-TH QR-PLOT                  **
**    DSURF1   - JET-EQUIL.                                           **
**    ALPHIN   - JET-EQUIL.                                           **
**    IEQ      - =1                                                   **
**               =2 --> JET-EQUIL.                                    **
**    IAS      - =0 SYMMETRIC EQUILIBRIUM                             **
**               =1 ASYMMETRIC EQUILIBRIUM                             **
**                                                                    **
**  NAMELIST NEWLAN :  ON UNIT NIN (DEFINED IN COMPIO = 5)            **
**                                                                    **
**    ISTART  - IF 0 : START WITH SHIFT (EWSHIFT) AND COMPUTATION      **
**                     OF THE T-MATRIX                                **
**              ELSE : READ T-MATRIX FROM DISK                        **
**    ISTOP   - IF < 0 : WRITE T-MATRIX ON DISK + COMPUTATION         **
**              ELSE   : WRITE T-MATRIX ON DISK AND STOP              **
**    KMAX    - DIMENSION OF THE T-MATRIX                             **
**    MXLOOP  - MAXIMUM NUMBER OF SHIFTS                              **
**    ISHIFT  - IF 1 : MAKE SHIFTS FOR EWSHIFT AND OWS(1:NUS)         **
**              ELSE : MAKE SHIFTS FOR EWSHIFT AND REGION DETERINED   **
**                     BY XLIML,XLIMR,YLIMB,YLIMT                     **
**    FOR ISHIFT.EQ.1 :                                               **
**    NUS     - NUMBER OF GIVEN SHIFTS IN ADDITION TO EWSHIFT         **
**    OWS     - VECTOR FOR EIGENVALUE SHIFTS                          **
**    FOR ISHIFT.NE.1 :                                               **
**    XLIML   - MINIMUM REAL PART OF INVESTIGATED REGION              **
**    XLIMR   - MAXIMUM REAL PART OF INVESTIGATED REGION              **
**    YLIMB   - MINIMUM IMAGINARY PART OF INVESTIGATED REGION         **
**    YLIMT   - MAXIMUM IMAGINARY PART OF INVESTIGATED REGION         **
**    IHOLE   - .TRUE. : THERE IS A REGION XHOLEL,XHOLER,YHOLEB,      **
**                       YHOLET, PART OF THE REGION XLIML,XLIMR,      **
**                       YLIMB,YLIMT, WITHIN WHICH NO EIGENVALUES     **
**                       ARE TO BE SEARCHED FOR                       **
**    XHOLEL  - MINIMUM REAL PART                                     **
**    XHOLER  - MAXIMUM REAL PART                                     **
**    YHOLEB  - MINIMUM IMAGINARY PART                                **
**    YHOLET  - MAXIMUM IMAGINARY PART                                **
**                                                                    **
**                                                                    **
**  AND FOR SOLVER 5 : READ ON UNIT NIN2 (DEFINED IN COMPIO = 2)      **
**                                                                    **
**                                                                    **
**  OUTPUT :                                                          **
**  ------                                                            **
**                                                                    **
**    WRITTEN ON UNIT NOUT  (DEFINED IN COMPIO = 6)                   **
**            ON UNIT NOUTI (AT IPP, DEFINED IN COMPIO = 8)           **
**            ON UNIT NOUTP (TEXT FOR FIRST PLOT,                     **
**                           DEFINED IN COMPIO = 11)                  **
**            AND FOR SOLVER 3 :                                      **
**            ON UNIT ND3 (DEFINED IN COMPIO = 15)                    **
**            ON UNIT ND4 (DEFINED IN COMPIO = 16)                    **
**            ON UNIT ND5 (DEFINED IN COMPIO = 17)                    **
**            ON UNIT ND6 (DEFINED IN COMPIO = 18)                    **
**            AND FOR SOLVER 5 :                                      **
**            ON UNIT NOUT2 (DEFINED IN COMPIO = 1)                   **
**            ON UNIT NOUT3 (DEFINED IN COMPIO = 10)                  **
**                                                                    **
************************************************************************
************************************************************************
**                                                                    **
**   MODULAR STRUCTURE :                                               **
**  -----------------                                                 **
**    CASTOR - PRESET                                                 **
**             TESTS                                                   **
**             EQUIL                                                  **
**                (MODULE SPECIFYING THE EQUILIBRIUM)                 **
**             VACUUM                                                 **
**                (MODULE COMPUTING VACUUM PERTURBATION)              **
**             MAT1-5                                                 **
**                (MODULE COMPUTING THE MATRICES A AND B)             **
**             SOLV1-5                                                **
**                (MODULES FOR THE DIFFERENT EIGENVALUE SOLVERS)      **
**             DIAG1-5                                                **
**                (MODULE FOR THE DIAGNOSTICS)                        **
**                                                                    **
**  EXTERNAL SUBROUTINES :                                            **
**  --------------------                                              **
**    PPPLIB  : BEGPLT, LBLTOP, LPLOT, DLCH, NFRAME, FINPLT           **
**    BLAS    :  CXCOPY  ,  CXDOTC  ,  CXSCAL  ,  CXAXPY  ,  ICXAMAX  **
**              =C(Z)COPY, =C(Z)DOTC, =C(Z)SCAL, =C(Z)AXPY, =IC(Z)AMAX**
**               CXDOTU  ,  SGSCAL  ,  SGCOPY  ,  CX(SG)SCAL          **
**              =C(Z)DOTU, =S(D)SCAL, =S(D)COPY, =C(Z)S(D)SCAL        **
**    LINPACK : CPOCO, CPOSL, S(D)GTSL                                **
**    EISPACK : CBAL, CORTH, COMQR                                    **
**    CRAY    : RANSET, RCFFT2, ORDERS                                **
**    HGOLIB  : RFT2 (NOT AT IPP)                                     **
**                                                                    **
************************************************************************
C
*CALL COMMAX
*CALL COMPAR
*CALL COMPAR2
*CALL COMPAR3
*CALL COMPAR4
*CALL COMPAR5
*CALL COMPARP
*CALL COMP234
*CALL COMPARV
*CALL COMPIO
*CALL COMMOD
*CALL COMDIM
*CALL COMLAB
*CALL COMDIM5
*CALL COMIT
*CALL COMINT
*CALL COMBX
*CALL COMDIAG
*CALL COMPLOT
*CALL COMGRID
*CALL COMWEL
*CALL COMGEO
*CALL COMEQUI
*CALL COMEQV
*CALL COMGEM
*CALL COMIOD
*CALL COMMEW
*CALL COMSPL
*CALL COMFFT
*CALL COMESH
*CALL COMESH2
*CALL SHIFT
*CALL LANCZOS
*CALL CONLAN
*CALL CORE
*CALL COMVAC
*CALL COMVGRD
*CALL COMVFT
*CALL COMBND
C
      REAL DUMMY(3)
      CHARACTER*11  TXTPL(5)
      COMPLEX EWOUT
C
      DATA TXTPL /'QR         ','STEUERWALD ','OUT-OF-CORE',
     >            'IN-CORE-OOC','LANCZOS    '/
C
      NAMELIST / NEWRUN / MODE, EQNAME, NLTORE, NG, CWW,IFAST,
     >                    RFOUR, VFOUR, NTOR, ETA, ZMU, ZTE, ZTI,
     >                    ASPECT, Q0ZYL, SIG1, SIG2, XR1, XR2,
     >                    RWALL, SBEGIN, SEND,
     >                    NVPSI, NGV, SIGV, DSURF, IDPOW, NDIAGFK,
     >                    VSHIFT, NRS, NIS, DRS, DIS, EPS,ITER,
     >                    NPLOT, XMINQR, YMINQR, XMAXQR, YMAXQR,
     >                    DSURF1,ALPHIN,IEQ,IAS,IBVAC,GAMMA,
     >                    VSURF, VSURF1, VSURF2, VSURF3, VSURF4, VEL3,
     >                    DSURF2, DSURF3, IVAC, FW, NFW, RMIN, ZCNTR
C
      NAMELIST / NEWLAN / ISTART, ISTOP, KMAX, MXLOOP,
     >                    ISHIFT, NUS, OWS,
     >                    XLIML, XLIMR, YLIMB, YLIMT,
     >                    IHOLE, XHOLEL, XHOLER, YHOLEB, YHOLET
CC
      IF(LANZ.GT.MANZ) STOP '**ERROR: LANZ > MANZ'
      CALL PRESET
C
      WRITE(EQNAME,'(A10)') ' '
      LABEL(1:3) = VERSION
C
C     OPENING PLOT FILE :
C     =================
*IF KUL
      IPLOT = 4
      CALL BEGPLT(IPLOT)
*ELSE
      CALL BEGPLT('CASPLOT')
*ENDIF
C
      IBEGIN = 0
   10 READ(NIN,NEWRUN)
      IF(IBEGIN.NE.0) WRITE(NOUT,'(''1'')')
      IBEGIN = 1
      IF(MODE.EQ.0) GOTO 1000
      IF(NG.GT.NGMAX) STOP 'NG > NGMAX'
      MODSOL=MOD(MODE,10)
      NRTEST=MODE/10
      IF(MODSOL.LT.1.OR.MODSOL.GT.5) THEN
         WRITE(NOUT,*) 'WRONG MODE'
         GOTO 1000
      ENDIF
      IF(MODSOL.EQ.5) THEN
         IF(KMAX.GT.MXAB) STOP 'MXAB'
         IF(MXLOOP.GT.MXLP) STOP 'MXLP'
      ENDIF
 
      IF(MODE.GT.10) CALL TESTS
      READ(NIN,NEWLAN)
C
      RFOUR1 = RFOUR(1)
      DO 20 JJ=1,MANZ
         RFOUR(JJ) = RFOUR1 + FLOAT((JJ-1))
   20 CONTINUE
C
      EWSHIFT = VSHIFT(1)
      SIGMA   = VSHIFT(1)
      DO 40 I=0,NRS-1
         SR = REAL(EWSHIFT) + DRS*I
         DO 30 J=0,NIS-1
            SI = AIMAG(EWSHIFT) + DIS*J
            VSHIFT(I*NIS+J+1) = CMPLX(SR,SI)
   30    CONTINUE
   40 CONTINUE
C
      REWIND NOUTP
      WRITE(NOUT,41) VERSION,EQNAME
      WRITE(NOUTP,41) VERSION,EQNAME
      IF(MODE.GT.10) THEN
         WRITE(NOUT,42) NRTEST,TXTPL(MODSOL)
         WRITE(NOUTP,42) NRTEST,TXTPL(MODSOL)
         IF(MODSOL.GE.2.AND.MODSOL.LE.4) THEN
            WRITE(NOUT,43) EWTEST
            WRITE(NOUTP,43) EWTEST
         ENDIF
      ELSE
         WRITE(NOUT,44) TXTPL(MODSOL)
         WRITE(NOUTP,44) TXTPL(MODSOL)
      ENDIF
      WRITE(NOUT,45)  LANZ, NG, ASPECT, ETA, Q0ZYL, NTOR, NLTORE,
     >                DSURF, IDPOW, RWALL, EPS
      WRITE(NOUTP,45) LANZ, NG, ASPECT, ETA, Q0ZYL, NTOR, NLTORE,
     >                DSURF, IDPOW, RWALL, EPS
      WRITE(NOUT,61)  ZTE,ZTI
      WRITE(NOUTP,61) ZTE,ZTI
      WRITE(NOUT,62)  ZMU,GAMMA
      WRITE(NOUTP,62) ZMU,GAMMA
      WRITE(NOUT,46)  (RFOUR(II),II=1,MANZ)
      WRITE(NOUTP,46) (RFOUR(II),II=1,MANZ)
      WRITE(NOUT,47)  SIG1, SIG2, XR1, XR2
      IF(SIG1.LE.99.) THEN
         WRITE(NOUTP,47) SIG1, SIG2, XR1, XR2
      ENDIF
      WRITE(NOUT,48)  NVPSI, NGV, SIGV
      IF(RWALL.GT.1.) THEN
         WRITE(NOUTP,48) NVPSI, NGV, SIGV
      ENDIF
      WRITE(NOUT,49)  (VSHIFT(II),II=1,NRS*NIS)
      WRITE(NOUTP,49) (VSHIFT(II),II=1,NRS*NIS)
      IF(MODSOL.EQ.1) WRITE(NOUT,50) NPLOT,
     >                (XMINQR(J),YMINQR(J),XMAXQR(J),YMAXQR(J),
     >                J=1,NPLOT)
C
      CALL WRTEXT(NOUTP)
C
      NDIM   = NG * NBG
      NGINT  = NG - 1
      ZNKWEL = FLOAT(NTOR)
      IF(.NOT.NLTORE) ZNKWEL = ZNKWEL / ASPECT
C
      WRITE(NOUT,53) NDIM
      WRITE(NOUT,55) NGINT
      WRITE(NOUT,57) NZMA
      WRITE(NOUT,59) ZNKWEL
C------------------------------------------------------------------------
C READ EQUILIBRIUM  FROM DISK
C------------------------------------------------------------------------
      CALL EQUIL
      CALL BUSSAC
C------------------------------------------------------------------------
C SET POLOIDAL MODE NUMBERS AS FUNCTION OF RADIUS
C------------------------------------------------------------------------
      DO IG=1,NG
        QI = SPWERT(NPSI,SGRID(IG),Q1,Q2,Q3,Q4,CS,DUMMY)
		 IF (IFAST .EQ. 1) THEN
          MSTART(IG) = 1.+ FLOAT(INT(-ZNKWEL * QI - FLOAT((MANZ-1)/2)))
        ELSE
		   MSTART(IG) = RFOUR(1)
		 ENDIF
      ENDDO
      IF (IFAST .EQ. 1) VFOUR(1) = MSTART(NG) - FLOAT((MVANZ - MANZ)/2)
      WRITE(*,'(A,i5)') ' IFAST : ',IFAST
      WRITE(*,'(A,2f6.0)') ' MSTART(1),MSTART(NG):',MSTART(1),MSTART(NG)

C------------------------------------------------------------------------
C VACUUM RESPONSE 
C------------------------------------------------------------------------
      IF(RWALL.GT.1.) CALL VACUUM
C------------------------------------------------------------------------
C SOLVERS 
C------------------------------------------------------------------------
      NREC = (NG+NCV-1)/NCV
      REWIND(NOUTE)
C
      GOTO ( 100 , 200 , 300 , 400 , 500 ) MODSOL
C
  100 CONTINUE
C-----------------------------------------------------------------------
C QR ALGORITHM
C-----------------------------------------------------------------------
      WRITE(NOUT,101)
C
      IF(NDIM.GT.NDIM1) THEN
         WRITE(NOUT,102) NDIM,NDIM1
*IF CRAY
         MNDIM1 = ((-4.+SQRT(16.+16.*KPMEX))/8.)/NBG*NBG
*ELSE
         MNDIM1 = ((-3.5+SQRT(3.5**2+16.*KPMEX))/8.)/NBG*NBG
*ENDIF
         NGT = MNDIM1/NBG
         IF(MNDIM1.NE.NDIM1) WRITE(NOUT,103) KILWOR,NGMAX,MANZ,MNDIM1,
     >                                    NGT
         WRITE(NOUT,104)
         CALL FINPLT
         STOP
      ENDIF
C
      CALL MAT1
      CALL SOLV1
      CALL DIAG1
      GOTO 10
  200 CONTINUE
C-------------------------------------------------------------------------
C INVERSE VECTOR ITERATION, IN-CORE (STEUERWALD)
C-------------------------------------------------------------------------
      WRITE(NOUT,201) EWSHIFT
C
      IF(NDIM.GT.NDIM2) THEN
         WRITE(NOUT,202) NDIM,NDIM2
         STOP
      ENDIF
C
      CALL MAT2
      CALL SOLV2
      CALL DIAG234
      GOTO 10
  300 CONTINUE
C-------------------------------------------------------------------------
C     INVERSE VECTOR ITERATION, OUT-OF-CORE (SCHWARZ)
C-------------------------------------------------------------------------
      IF(NCV.LT.1) THEN
         WRITE(NOUT,301)
         STOP
      ENDIF
C
      DO 310  JJ = 1, NRS*NIS
C
         NSHIFT = JJ
C
         CALL MAT3
C
         EWSHIFT = VSHIFT(JJ)
         WRITE(NOUT,302) EWSHIFT
         CALL SOLV3
         VSHIFT(JJ) = EW
C
         IF(NPLOT.NE.0) CALL DIAG234
C
         REWIND ND3
         REWIND ND4
         REWIND ND5
         REWIND ND6
C
  310 CONTINUE
      GOTO 10
  400 CONTINUE
C-----------------------------------------------------------------------
C INVERSE VECTOR ITERATION, IN-CORE VERSION OF OUT-OF-CORE SOLVER
C-----------------------------------------------------------------------
      WRITE(NOUT,401) EWSHIFT
C
      IF(NCVIC.LT.NG) THEN
         WRITE(NOUT,402)
         STOP
      ENDIF
C
      CALL MAT4
      CALL SOLV4
      CALL DIAG234
      GOTO 10
  500 CONTINUE
C------------------------------------------------------------------------
C LANCZOS ALGORITHM
C------------------------------------------------------------------------
      WRITE(NOUT,501)
C
      IF(NDIM.GT.NDIM5) THEN
         WRITE(NOUT,502) NDIM,NDIM5
         STOP
      ENDIF
C
      CALL MAT5
      CALL SOLV5
      IF(ISHIFT.NE.1) CALL DIAG5
      GOTO 10
C------------------------------------------------------------------------
 1000 CALL FINPLT
C
      STOP
C
   41 FORMAT(1X,34('*')/1X,'***',28X,'***'/
     >       1X,'***  MISHKA1 ,  VERSION ',A3,'    ***'/
     >       1X,'***  EQUILIBRIUM : ',A10,'  ***'/
     >       1X,'***',28X,'***'/1X,34('*')/)
   42 FORMAT(' TEST CASE ',I2,' : ',A20)
   43 FORMAT(' EIGENVALUE SHOULD BE : ',1P,2E15.5,0P/)
   44 FORMAT(' ',A20,/)
   45 FORMAT(' INPUT DATA :',//,
     >       '    LANZ    = ',I5,        12X,               //
     >       ' NEWRUN :',/,
     >       '    NG      = ',I5,        12X,'ASPECT  = ',1P,E12.4,0P,/
     >       '    ETA     = ',1P,2E12.4,0P,5X,'Q0ZYL   = ',1P,E12.4,0P,/
     >       '    NTOR    = ',I5,        12X,'NLTORE  = ',L7         ,/
     >       '    DSURF   = ',1P,E12.4,0P,5X,'IDPOW   = ',I5         ,/
     >       '    RWALL   = ',1P,E12.4,0P,5X,'EPS     = ',1P,E12.4,0P )
   46 FORMAT('    M       = ',F7.2/(14X,F7.2))
   47 FORMAT('    SIG1    = ',1P,E12.4,0P,5X,'SIG2    = ',1P,E12.4,0P,/
     >       '    XR1     = ',1P,E12.4,0P,5X,'XR2     = ',1P,E12.4,0P )
   48 FORMAT('    NVPSI   = ',I5,        12X,'NGV     = ',I5         ,/
     >       '    SIGV    = ',1P,E12.4,0P,5X )
   49 FORMAT('    VSHIFT  = ',1P,2E12.4,0P/(14X,1P,E12.4,0P))
   50 FORMAT('    NPLOT   = ',I5,' PLOTS IN RANGE :'    /
     >           (4X,4E12.4))
   51 FORMAT(/,' NEWLAN :',/,
     >       '    ISTART  = ',I3,        14X,'ISTOP   = ',I3/
     >       '    KMAX    = ',I3,        14X,'MXLOOP  = ',I3/
     >       '    ISHIFT  = ',I3,        14X,'NUS     = ',I3/
     >       '    XLIML   = ',1P,E12.4,0P,5X,'XLIMR   = ',1P,E12.4,0P,/
     >       '    YLIMB   = ',1P,E12.4,0P,5X,'YLIMT   = ',1P,E12.4,0P,/
     >       '    IHOLE   = ',L7)
   52 FORMAT('    XHOLEL  = ',1P,E12.4,0P,5X,'XHOLER  = ',1P,E12.4,0P,/
     >       '    YHOLEB  = ',1P,E12.4,0P,5X,'YHOLET  = ',1P,E12.4,0P)
   53 FORMAT(/5X,'DIMENSION OF THE A-MATRIX :     NDIM = ',I8)
   55 FORMAT(5X,'NUMBER OF FINITE ELEMENTS :    NGINT = ',I8)
   57 FORMAT(5X,'DIMENSION OF ZMA SUBBLOCK :     NZMA = ',I4/)
   59 FORMAT(/' ZNKWEL =',F7.3)
   61 FORMAT('    TAUE    = ',1P,E12.4,5X,'TAUI    = ',E12.4)
   62 FORMAT('    ZMU     = ',1P,E12.4,5x,'GAMMA   = ',E12.4)
  101 FORMAT(///1X,80('*')/1X,'*',33X,'QR-ALGORITHM',
     >       33X,'*'/1X,80('*'))
  102 FORMAT(///1X,50('*')/1X,'***',44X,'***'/
     >       1X,'***',20X,'STOP',20X,'***'/
     >       1X,'***',10X,'NDIM=',I5,' > NDIM1=',I5,10X,'***'/
     >       1X,'***',44X,'***')
  103 FORMAT(' ***',44X,'***'/
     >       ' ***',5X,'FOR:',34X,'***'/
     >       ' ***',5X,'KILWOR=',I9,23X,'***'/
     >       ' ***',5X,'NGMAX=',I5,28X,'***'/
     >       ' ***',5X,'MANZ=',I3,31X,'***'/
     >       ' ***',5X,'IS MAXIMUM NDIM1=',I6,' POSSIBLE',6X,'***'/
     >       ' ***',5X,'I.E. : NG=',I5,19X,'***'/' ***',44X,'***')
  104 FORMAT(1X,50('*'))
  201 FORMAT(//1X,80('*')/1X,'*',25X,'VECTOR ITERATION (STEUERWALD)',
     >       25X,'*'/1X,80('*')/
     >       ' INITIAL GUESS EIGENVALUE :',1P,2E12.4,0P)
  202 FORMAT(///' NDIM=',I5,' > NDIM2=',I5)
  301 FORMAT(///' BLOCKS TOO LARGE - NCV < 1 ')
  302 FORMAT(//1X,80('*')/1X,'*',26X,'VECTOR ITERATION (SCHWARZ)',
     >       27X,'*'/1X,80('*')//
     >       ' INITIAL GUESS EIGENVALUE :',1P,2E12.4,0P,/)
  401 FORMAT(//1X,80('*')/1X,'*',26X,'VECTOR ITERATION INCORE-OOC',
     >       25X,'*'/1X,80('*')//
     >       ' INITIAL GUESS EIGENVALUE :',1P,2E12.4,/)
  402 FORMAT(///' BLOCKS TOO LARGE - NCV < NG')
  501 FORMAT(///1X,80('*')/1X,'*',29X,'LANCZOS ',
     >       32X,'*'/1X,80('*'))
  502 FORMAT(///' NDIM=',I5,' > NDIM5=',I5)
      END
************************************************************************
*DECK PRESET
      SUBROUTINE PRESET
C-----------------------------------------------------------------------
C INITIALIZE ALL NAMELIST VARIABLES
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPARV
*CALL COMDIAG
*CALL COMPLOT
*CALL COMGRID
*CALL COMWEL
*CALL COMGEO
*CALL COMEQUI
*CALL COMGEW
*CALL COMMEW
*CALL COMIT
*CALL COMINT
*CALL LANCZOS
*CALL CONLAN
*CALL COMIOD
*CALL COMVGRD
*CALL COMVAC
*CALL COMESH2
*CALL ISEED
C-----------------------------------------------------------------------
C VARIABLES FOR VSHIFT
C-----------------------------------------------------------------------
      NRS       =  1
      NIS       =  1
      DRS       =  1.0
      DIS       =  1.0
      VSHIFT(1) = (0.,33.)
      DO 10 I=2,100
   10 VSHIFT(I) = (0.,0.)
C----------------------------------------------------------------------
C VARIABLES FOR PLOTTING 
C----------------------------------------------------------------------
      NPLOT     =    3
      XMINQR(1) =   -1.
      XMINQR(2) =  -10.
      XMINQR(3) =   -1.
      XMINQR(4) =  -10.
      XMINQR(5) =  -10.
      XMAXQR(1) =    1.
      XMAXQR(2) =   10.
      XMAXQR(3) =    1.
      XMAXQR(4) =   10.
      XMAXQR(5) =   10.
      YMINQR(1) =   -0.1
      YMINQR(2) =    0.
      YMINQR(3) =  1.4
      YMINQR(4) =    0.
      YMINQR(5) =    0.
      YMAXQR(1) =    2.
      YMAXQR(2) =  2000.
      YMAXQR(3) =  1.6
      YMAXQR(4) =   10.
      YMAXQR(5) =   10.
C-----------------------------------------------------------------------
C PHYSICAL VARIABLES 
C-----------------------------------------------------------------------
      RFOUR(1)  =  0.
      DO 20 I= 2,MANZ
   20 RFOUR(I)  =  0.
      NLTORE    = .TRUE.
      NTOR      = -1
      GAMMA     =  5. / 3.
      Q0ZYL     =  0.3
      ASPECT    =  1.
      DSURF     =  0.
      DSURF1    =  0.
      DSURF2    =  0.
      DSURF3    =  0.
      VEL3      =  0.
      VSURF     =  0.
      VSURF1    =  0.
      VSURF2    =  0.
      VSURF3    =  0.
      VSURF4    =  0.
      ALPHIN    =  1.
      ETA       =  (0.,0.)
      ZMU       =  0.
      ZTI       =  0.
      ZTE       =  0.
      IEQ       =  1
      IAS       =  0
      IDPOW     =  1
      SIG1      = 9999.
      SIG2      = 9999.
      XR1       = 99999.
      XR2       = 99999.
      RWALL     = -1.
      FW(1)     = 10.
      NFW       = NVCHIMX
      DO 25 I = 1, NFW
         FW(I)  = 0.
 25   CONTINUE
      RMIN      = 1.
      NVPSI     = 42
      NGV       = 51
      SIGV      = 9999.
      IVAC      = 1

      SBEGIN    = 0.0
      SEND      = 1.0
C--------------------------------------------------------------------------
C VARIABLES FOR DIAGNOSTICS 
C--------------------------------------------------------------------------
      NDIAGFK   = 0
      IBVAC     = 0
C--------------------------------------------------------------------------
C NUMERICAL VARIABLES 
C--------------------------------------------------------------------------
      NG        =  11
      ITER      =  50
      EPS       = 1.E-6
C
      GEWI(1)   = .17392742256872693
      GEWI(2)   = .17392742256872693
      GEWI(3)   = .32607257743127307
      GEWI(4)   = .32607257743127307
C-------------------------------------------------------------------------
C VARIABLES FOR LANCZOS 
C-------------------------------------------------------------------------
      SVSEED    = 7892713
C     ISEED     = 123456789
      MXINIT    = 5
      SAVTEV    = 1
      RELTOL    = 1.E-8
C
      ISHIFT    = 0
      ISTART    = 0
      ISTOP     = 1
      IHOLE     = .FALSE.
      KMAX      = 51
      MXLOOP    = 10
      NUS       =  0
      XLIML     = 0.
      XLIMR     = 1.30
      YLIMB     = 0.
      YLIMT     = 10.
      XHOLEL    =  0.0
      XHOLER    =  0.0
      YHOLEB    =  0.0
      YHOLET    =  0.0
      DO 30 I=1,100
   30 OWS(I)    = (0.,0.)
C
      RETURN
      END
************************************************************************
*DECK TESTS
      SUBROUTINE TESTS
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPIO
*CALL COMMOD
*CALL COMDIAG
*CALL COMPLOT
*CALL COMGRID
*CALL COMWEL
*CALL COMGEO
*CALL COMEQUI
*CALL COMGEW
*CALL COMMEW
*CALL COMIT
*CALL COMINT
*CALL LANCZOS
*CALL CONLAN
*CALL COMIOD
*CALL ISEED
C
      IF(MODE.GE.11.AND.MODE.LE.15) THEN
C
         VSHIFT(1) = (1.25,0.0)
C
C ...... PHYSICAL VARIABLES ...
C
C        RFOUR(1)  =  -1.
         RFOUR(1)  =  1.-FLOAT(MANZ/2)
         NLTORE    = .TRUE.
         NTOR      = -2
         ETA       = (0.,0.)
         Q0ZYL     = 0.3
         ASPECT    = 1.
         DSURF     = 1.
         IDPOW     = 1
C
C ...... NUMERICAL VARIABLES ...
C
         IF(MODE.EQ.11) THEN
            NG     = 7
            ETA    = (1.E-4,0.)
         ENDIF
C
C ...... RESULT ...
C
         EWTEST = (1.24153,0.0)
C
C ...... VARIABLES FOR LANCZOS ...
C
C
         ISHIFT    = 0
          IHOLE     = .FALSE.
         KMAX      = 51
         MXLOOP    = 10
         NUS       = 0
         XLIML     = 0.0
         XLIMR     = 1.3
         YLIMB     = -0.01
         YLIMT     = 10.
         XHOLEL    = 0.0
         XHOLER    = 0.0
         YHOLEB    = 0.0
         YHOLET    = 0.0
         DO 10 I=1,100
   10    OWS(I)    = (0.0,0.0)
      ELSE
         WRITE(NOUT,*) 'THIS TEST CASE DOES NOT EXIST'
         STOP
      ENDIF
C
      RETURN
      END
************************************************************************
*DECK EQUIL
      SUBROUTINE EQUIL
C
************************************************************************
************************************************************************
**                                                                    **
**    BEGIN : MODULE EQUIL  (SPECIFICATION OF THE EQUILIBRIUM)        **
**    --------------------                                            **
**                                                                    **
**    STRUCTURE :                                                     **
**                 EQUIL                                              **
**                   IODSK                                            **
**                   GRID                                             **
**                   MESHAC                                           **
**                     FGAUS                                          **
**                   EQUILV                                           **
**                   FKEQ                                             **
**                     FFTRAN                                         **
**                     SPLFK                                          **
**                     SPLINE                                         **
**                     SPWERT                                         **
**                   EQVAC                                            **
**                   VFKEQ                                            **
**                     FFTRAN                                         **
**                     SPLFK                                          **
**                                                                    **
************************************************************************
************************************************************************
C
*CALL COMMAX
*CALL COMPAR
*CALL COMPARV
*CALL COMPIO
*CALL COMGRID
*CALL COMESH2
*CALL COMIOD
*CALL COMVGRD
*CALL COMVAC
C-------------------------------------------- READ EQUILIBRIUM FROM DISK 
      CALL IODSK
C------------------------------------------------- GRID (S - COORDINATES) 
      XWALL=1.0
      IF ((RWALL.GT.0.).AND.(RWALL.LT.1.)) XWALL = RWALL
      CALL GRID(SBEGIN,SEND,XWALL)
C----------------------------------------------------- MESH ACCUMULATION 
      RS0 = SGRID(1)
      RSA = SGRID(NG)
      BGF  = 0.3
      FACT = 1.
      IMESHAC = 1
      IF(SIG1.GT.99.) IMESHAC=0
C
      IF(IMESHAC.NE.0) THEN
         WRITE(NOUT,*)
         WRITE(NOUT,*) ' MESH ACCUMULATION AT XR1, XR2 : ',XR1,XR2
         WRITE(NOUT,*)
         CALL MESHAC(XR1,XR2,BGF,SIG1,SIG2,FACT,RS0,RSA,
     >               SGRID,NG,NG-1)
      ENDIF
C--------------------------------------- MESH ACCUMULATION IN THE VACUUM 
      RS0 = SVGRID(1)
      RSA = SVGRID(NGV)
      BGF = 0.3
      XR1 = 1.E-12
      XR2 = 999999.
      SIG1 = SIGV
      SIG2 = 999999.
      FACT = 1.
      IMESHAC = 1
      IF(SIGV.GT.99.) IMESHAC = 0
C
      IF(IMESHAC.NE.0) CALL MESHAC(XR1,XR2,BGF,SIG1,SIG2,FACT,RS0,RSA,
     >                             SVGRID,NGV,NGV-1)
C
C-------------------------------------------- DEFINE PLASMA COEFFICIENTS
      CALL EQUILV
      CALL FKEQ
C------------------------------------------------ DEFINE GEOMETRY VACUUM 
      IF(RWALL.GT.1.) THEN
         CALL EQVAC
         CALL VFKEQ
      ENDIF
C
      RETURN
C
    1 FORMAT('1',61('*'),/' *',24X,'GRID POINTS',24X,'*',/1X,61('*'))
    2 FORMAT(10F8.4)
    3 FORMAT(' ',61('*'),
     >       /' *',20X,'VACUUM GRID POINTS',21X,'*',
     >       /' ',61('*'))
      END
************************************************************************
*DECK IODSK
      SUBROUTINE IODSK
C-----------------------------------------------------------------------
C     PERFORMS INPUT FROM DISK
C-----------------------------------------------------------------------
*CALL COMMAX
*CALL COMPIO
*CALL COMGEM
*CALL COMEQUI
*CALL COMIOD
*CALL COMSPL
*CALL COMBND
*CALL COMVAC
*CALL COMWEL
C
      REAL     SCALEQ, RBPHI02, DJ0, DJE, DP0, DPE, DRBPHI0, DRBPHIE,
     >         WURZEL, DQEC, RAXIS
      REAL     PLOTS(100),PLOTQ(101),DUMMY(3)
      REAL     G22AV(NPSIMAX),B0AV(NPSIMAX)
      INTEGER  JS0
C
      REWIND(NMAP)
C------------------------------------- READ MAPPED EQUILIBRIUM FROM DISK
C
      READ(NMAP,*) JS0,(CS(JS),JS=1,JS0+1),
     >             (QS(JS),JS=1,JS0+1),DQS(1),DQEC,(DQS(JS),JS=2,JS0+1),
     >             (CURJ(JS),JS=1,JS0+1),DJ0,DJE,
     >             NCHI,(CHI(JC),JC=1,NCHI),
     >             (GEM11(J),J=NCHI+1,(JS0+1)*NCHI),
     >             (GEM12(J),J=NCHI+1,(JS0+1)*NCHI),
     >             CPSURF,RADIUS
C
      NVCHI = NCHI
      NPSI = JS0 + 1
      NG = NPSI*NCHI
C
       DO 10 J=1,NG
         GEM33(J) = 1.0
   10 CONTINUE
C------------------------------------------------------------------------
C FOR TOROIDAL GEOMETRY ADDITIONAL INPUT : GEM33(J) = R**2
C------------------------------------------------------------------------
      IF(NLTORE) READ(NMAP,*) (GEM33(J),J=NCHI+1,NG),RAXIS
      READ(NMAP,*) (P0(JS),JS=1,NPSI),DP0,DPE,
     >             (RBPHI(JS),JS=1,NPSI),DRBPHI0,DRBPHIE
C
C------------------------------------------ READ ADDITIONAL BOUNDARY DATA 
      IF(RWALL.GT.1.) THEN
         READ(NMAP,*) (VX(JS),JS=1,NCHI)
         READ(NMAP,*) (VY(JS),JS=1,NCHI)
         READ(NMAP,*) ASPI
      ENDIF
C
      DO 20 JC = 1,NCHI
         GEM11(JC) = 0.0
   20 CONTINUE
      IF(NLTORE) THEN
         DO 30 JC=1,NCHI
            GEM33(JC) = RAXIS**2
   30    CONTINUE
      ENDIF
C
      DO 40 JC=1,NCHI
         CALL SGCOPY(NPSI-1,GEM12(NCHI+JC),NCHI,C1,1)
         CALL SPLINE(NPSI-1,CS(2),C1,0.0,0.0,3,Q1,Q2,Q3,Q4)
         GEM12(JC) = SPWERT(NPSI-1,0.0,Q1,Q2,Q3,Q4,CS(2),DUMMY)
   40 CONTINUE
C
C------------------------------------------------  SCALE TO Q ON AXIS
      IF (Q0ZYL.GT.0.0) THEN
      SCALEQ = QS(1)/Q0ZYL
C
      CPSURF = CPSURF*SCALEQ
      CALL SGSCAL(NPSI,SCALEQ,CURJ,1)
      CALL SGSCAL(NPSI,SCALEQ**2,P0,1)
      CALL SGSCAL(NPSI*NCHI,-SCALEQ,GEM12,1)
      CALL SGSCAL(NPSI*NCHI,SCALEQ**2,GEM11,1)
C
      RBPHI02 = RBPHI(1)**2
C
      DJ0     = DJ0*SCALEQ
      DJE     = DJE*SCALEQ
      DP0     = DP0*SCALEQ**2
      DPE     = DPE*SCALEQ**2
      DRBPHI0 = DRBPHI0*SCALEQ**2
      DRBPHIE = DRBPHIE*SCALEQ/SQRT(1.+RBPHI02/RBPHI(NPSI)**2*
     >          (1./SCALEQ**2-1.))
C
      DO 50 J=1,NPSI
         WURZEL   = SQRT(1.+RBPHI02/RBPHI(J)**2*(1./SCALEQ**2-1.))
         QS(J)    = WURZEL * QS(J)
         RBPHI(J) = WURZEL * SCALEQ * RBPHI(J)
   50 CONTINUE
      ELSE
        CALL DSCAL(NPSI*NCHI,-1.,GEM12,1)
      ENDIF
C
C
      WRITE(NOUT,51) SCALEQ
      WRITE(NOUT,52) CPSURF
      WRITE(NOUT,53) (QS(JJ),JJ=1,JS0+1)
      WRITE(NOUT,54) (P0(JJ),JJ=1,NPSI)
      WRITE(NOUT,55) (RBPHI(JJ),JJ=1,NPSI)
      WRITE(NOUT,56) DJ0,DJE,(CURJ(JJ),JJ=1,JS0+1)
C
C------------------------------------------------------------- SPLINES
      DQ1 = (QS(NPSI)-QS(NPSI-1))/(CS(NPSI)-CS(NPSI-1))
      DQ0 = (QS(2)-QS(1))/(CS(2)-CS(1))
      CALL SPLINE(NPSI,CS,QS,DQ0,DQ1,1,Q1,Q2,Q3,Q4)
      CALL SPLINE(NPSI,CS,CURJ,DJ0,DJE,1,C1,C2,C3,C4)
      CALL SPLINE(NPSI,CS,P0,DP0,DPE,1,P1,P2,P3,P4)
      CALL SPLINE(NPSI,CS,RBPHI,DRBPHI0,DRBPHIE,1,RBP1,RBP2,RBP3,RBP4)
C
      CALL SGCOPY(NPSI,Q2,1,DQS,1)
      RETURN
c---------------------------------- omega* calculation
c------------ flux surface everages of some quantities
      AREA = 0.
      DO 200 I=2,NPSI
        AG22 = 0.
        AB0  = 0.
        ADL  = 0.
        RMA  = -1.
        RMI  = +2.
        SPS2 = 2. * CPSURF * CS(I)
        SPSM = 2. * CPSURF * CS(I-1)
        DO 210 J=1,NCHI
          INDEX = (I-1)*NCHI + J
          INDM  = (I-2)*NCHI + J
          FACT=1.
          IF ((IAS.EQ.0).AND.((J.EQ.1).OR.(J.EQ.NCHI))) FACT=0.5
c----------------------------------- carefull : this is a 2D jacobian
          ZJACM= SPSM*QS(I-1)*SQRT(GEM33(INDM))/RBPHI(I-1) 
          ZJAC = SPS2*QS(I)*SQRT(GEM33(INDEX))/RBPHI(I)
          G22L = QS(I)**2 * GEM11(INDEX)*GEM33(INDEX)/RBPHI(I)**2
          AG22 = AG22 + FACT * SQRT(G22L*GEM11(INDEX)) 
          ADL  = ADL  + FACT * SQRT(G22L)
          AREA = AREA + FACT * (ZJAC+ZJACM)*(CS(I)-CS(I-1))/2.
          AB0  = AB0 + SQRT((RBPHI(I)**2+GEM11(INDEX))/GEM33(INDEX))
     >         * SQRT(G22L) * FACT
          RM = SQRT(GEM33(INDEX))
          IF (RM.GT.RMA) RMA = RM
          IF (RM.LT.RMI) RMI = RM
  210   CONTINUE
        G22AV(I) = AG22 / ADL 
        B0AV(I) = AB0 / ADL
        RAV = (RMA-RMI)/(RMA+RMI) 
        RAV2= 2.*AREA / ADL
        SPS2 = 2. * CPSURF * CS(I)
        OMEGAS(I) = - CWW/(SPS2 * RAV * B0AV(I)) *
     >            G22AV(I) * P2(I) * ABS(ZNKWEL) * QS(I)
        OMEGA2(I) = - CWW/(SPS2 * RAV2 * B0AV(I)) *
     >            G22AV(I) * P2(I) * ABS(ZNKWEL) * QS(I)
        OMOLD = - CWW/(ASPI**2 * CS(I) *RBPHI(I)) * P2(I)
     >        * ZNKWEL * QS(I) 
c        WRITE(20,201) I,CS(I),QS(I),OMEGAS(I),OMOLD,B0AV(I),
c     >                G22AV(I)*ASPI/(2*CPSURF*CS(I)),
c     >                RAV/(ASPI*CS(I)),
c     >                RAV2/(ASPI*CS(I)),
c     >                AREA*3.14159/(NCHI-1)/ASPI**2,
c     >                ADL*3.14159/(NCHI-1)/ASPI
  200 CONTINUE
  201 FORMAT(I3,2F7.3,10F9.5)
      CALL SPLINE(NPSI,CS,OMEGA2,0.,0.,2,OM1,OM2,OM3,OM4)
      RETURN
C
   51 FORMAT(//' AFTER Q ON AXIS SCALING : SCALE FACTOR =',1P,E12.4,0P)
   52 FORMAT(/' CPSURF = ',1P,E12.4,0P)
   53 FORMAT(/' QS'/(1X,1P,6E12.4,0P))
   54 FORMAT(/' P0'/(1X,1P,6E12.4,0P))
   55 FORMAT(/' RBPHI'/(1X,1P,6E12.4,0P))
   56 FORMAT(/' DJ0,DJE',1P,2E12.4,0P/' CURJ'/(1X,1P,5E12.4,0P))
      END
**********************************************************************
*DECK DERIV
      SUBROUTINE DERIV(ARRIN,DARR,NCHI,IAS)
C---------------------------------------------------------------------
C SUBROUTINE TO CALCULATE THE THETA DERIVATE USING FFT.
C ARRIN : THE INPUT ARRAY
C DARR  : THE RESULTING THETA DERIVATIVE 
C NCHI  : THE NUMBER OF POLOIDAL POINTS
C IAS   : 0 FOR UP/DOWN SYMMETRIC EQUILIBRIA, 1 FOR ASYMMETRIC EQUIL.
C---------------------------------------------------------------------
*CALL COMMAX 
      REAL       ARRIN(*),DARR(*),FF(2*NCHIMAX+2),DF(2*NCHIMAX+2)
      INTEGER    INDEX(2*NCHIMAX)

      PI = 2.* ASIN(1.)

      DO 10 J=1,NCHI
         INDEX(J) = J
   10 CONTINUE
   
      IF (IAS.EQ.0) THEN
        DO 20 J=NCHI+1,2*NCHI-2
          INDEX(J) = 2*NCHI-J
   20   CONTINUE
      ENDIF

      IF (IAS.EQ.0) THEN
         N=2*(NCHI-1)
      ELSE
         N=NCHI
      ENDIF

      DO 40 J=1,N       
        FF(J) = ARRIN(INDEX(J))
   40 CONTINUE 
                        
      CALL RFT2(FF,N,1)
      
      DO 50 J = 1,N/2
        DF(2*J-1) = - FLOAT(J-1) * FF(2*J)   
        DF(2*J)   =   FLOAT(J-1) * FF(2*J-1) 
   50 CONTINUE
   
      DF(2) = 0.
      DF(N+2) = 0.

      CALL RFI2(DF,N,1)

      DO 60 J=1,NCHI
        DARR(J) = DF(J)
   60 CONTINUE
      END
************************************************************************
*DECK GRID
      SUBROUTINE GRID(SBEGIN,SEND,XWALL)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPARV
*CALL COMGRID
*CALL COMVAC
*CALL COMVGRD
*CALL COMPIO
C
      WRITE(NOUT,'(A,2f10.4)')' SBEGIN,SEND : ',SBEGIN,SEND
      WRITE(NOUT,'(A,f10.4)') ' XWALL : ',XWALL

      DELS  = (SEND-SBEGIN) / FLOAT(NG - 1)
      DO N = 1, NG
         SGRID(N)  = SBEGIN+(N - 1) * DELS
      ENDDO
C
      DELSV = XWALL/(NGV - 1)
      DO N = 1, NGV
        SVGRID(N) = FLOAT(N - 1) * DELSV
      ENDDO
C
      RETURN
      END     
************************************************************************
C*DECK GRID
C      SUBROUTINE GRID(XWALL)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
C*CALL COMMAX
C*CALL COMPARV
C*CALL COMGRID
C*CALL COMVAC
C*CALL COMVGRD
C
C      DELS  = XWALL / (NG - 1)
C      DO 10 N = 1, NG
C         SGRID(N)  = (N - 1) * DELS
C   10 CONTINUE
C
C      DELSV = XWALL/(NGV - 1)
C      DO 20 N = 1, NGV
C        SVGRID(N) = (N - 1) * DELSV
C   20 CONTINUE
C
C      RETURN
C      END
************************************************************************
*DECK MESHAC
      SUBROUTINE MESHAC(XR1,XR2,BGF,SIG1,SIG2,FACT,RS0,RSA,
     >                  SGRID,NG,NGINT)
C-----------------------------------------------------------------------
C MESH ACCUMULATION IN XR1 AND XR2
C-----------------------------------------------------------------------
*CALL COMPIO
C
      REAL SGRID(*)
C
C     CONDITIONS
C
      IF(BGF .EQ.0.) GO TO 60
      IF(XR1 .EQ.0.) GO TO 60
      IF(XR2 .EQ.0.) GO TO 60
      IF(SIG1.EQ.0.) GO TO 60
      IF(SIG2.EQ.0.) GO TO 60
      IF(FACT.EQ.0.) GO TO 60
C------------------------------------------- EVALUATION OF NORM
      JINT = 100*NGINT+1
      ZS   = RS0
      ZSUM = 0.0
      ZDS  = (RSA - RS0) / FLOAT(JINT-1)
      DO 10 J=1,JINT
         ZF   = FGAUS(ZS,BGF,XR1,XR2,SIG1,SIG2,FACT)
         ZSUM = ZSUM + ZF * ZDS
         ZS   = ZS + ZDS
   10 CONTINUE
      ZNORM   = (RSA-RS0) / (ZSUM)
C
      J1INT = 100
      JINT  = J1INT * NGINT
      ZFD   = (RSA - RS0) / FLOAT(NGINT)
      ZS    = RS0
      ZSUM  = 0.0
      ZF    = FGAUS(ZS,BGF,XR1,XR2,SIG1,SIG2,FACT)
      ZF    = ZF * ZNORM
      ZDS0  = (RSA - RS0) * ZF / FLOAT(JINT)
      I = 2
   20 CONTINUE
      ZI    = FLOAT(I-1) * ZFD 
      ZDS   = ZDS0 / ZF
      ZS    = ZS + ZDS
      ZSUM1 = ZSUM
      ZF    = FGAUS(ZS,BGF,XR1,XR2,SIG1,SIG2,FACT)
      ZF    = ZF * ZNORM
      ZSUM  = ZSUM + ZF * ZDS
      IF(ZI.GT.ZSUM) GO TO 20
      ZWL   = (ZI-ZSUM1)/(ZSUM - ZSUM1)
      SGRID(I) = ZS - ZDS * (1.0 - ZWL)
      IF(SGRID(I).LT.SGRID(I-1)) GOTO 40
      I = I + 1
      IF(I.GT.NGINT) GO TO 30
      GO TO 20
   30 CONTINUE
      SGRID(1) = RS0
      SGRID(NGINT+1) = RSA
      SGRID(NGINT) = 0.5 * (SGRID(NGINT-1) + SGRID(NGINT+1))
      IF(SGRID(NGINT-1).LT.RSA) GO TO 50
      WRITE(NOUT,31)
   40 WRITE(NOUT,41)
      STOP
   50 CONTINUE
   60 CONTINUE
C
      RETURN
C
   31 FORMAT('0',' ERR. IN S.R. MESHAC2 : SGRID(NGINT) GT. RSA ')
   41 FORMAT('0',' ERR. IN S.R. MESHAC2 : SGRID(I) .GT. SGRID(I+1) ')
   51 FORMAT('1',61('*'),
     >       /' *',20X,'MESHAC GRID POINTS',21X,'*',
     >       /' ',61('*'))
   53 FORMAT(15F8.4)
      END
************************************************************************
*DECK FGAUS
      FUNCTION FGAUS(ZS,BGF,XR1,XR2,SIG1,SIG2,FACT)
C-----------------------------------------------------------------------
C     BGF + (1 - BGF) * (GAUSS1 + FACT * GAUSS2) / FACT
C-----------------------------------------------------------------------
      ZNORM1 = 0.39894 / SIG1
      ZNORM2 = 0.39894 / SIG2
      ZEX1   = -0.5 * (ZS - XR1)**2 / SIG1**2
      ZEX2   = -0.5 * (ZS - XR2)**2 / SIG2**2
      F1     = ZNORM1 * EXP(ZEX1)
      F2     = ZNORM2 * EXP(ZEX2)
      FGAUS  = BGF + (1.0 - BGF) * (F1 + FACT * F2) / FACT
      RETURN
      END
************************************************************************
*DECK EQUILV
      SUBROUTINE EQUILV
C-----------------------------------------------------------------------
C CALCULATE EQUILIBRIUM QUANTITIES ON GAUSSIAN POINTS
C-----------------------------------------------------------------------
*CALL COMMAX
*CALL COMPIO
*CALL COMGRID
*CALL COMEQUI
*CALL COMEQV
*CALL COMIOD
*CALL COMSPL
      REAL ZS(4), ABLTG(3), ZA, ZB, ZC, ZDIF,
     >     OMEGA(4*NGMAX), RLARM(4*NGMAX)
C
      DO 20  NI = 1 , NGINT
         SL = SGRID(NI)
         SU = SGRID(NI+1)
C-------------------------------------------------- GAUSSIAN POINTS
         ZDIF  = SU - SL
         ZA    = .5 * ( SU + SL )
         ZB    = .43056815579702629 * ZDIF
         ZC    = .16999052179242813 * ZDIF
         ZS(1) = ZA + ZB
         ZS(2) = ZA - ZB
         ZS(3) = ZA + ZC
         ZS(4) = ZA - ZC
C
         DO 10 I=1,4
            J       = (NI-1)*4+I
            SGI(J)  = ZS(I)
            Q(J)    = SPWERT(NPSI,ZS(I),Q1,Q2,Q3,Q4,CS,ABLTG)
            DQ(J)   = ABLTG(1)
            T(J)    = SPWERT(NPSI,ZS(I),RBP1,RBP2,RBP3,RBP4,CS,ABLTG)
            DT(J)   = ABLTG(1)
            ZP      = SPWERT(NPSI,ZS(I),P1,P2,P3,P4,CS,ABLTG)
            ZDP     = ABLTG(1)
            ZDDP    = ABLTG(2)
            ETAV(J) = ETA
            IF(IEQ.EQ.2) THEN
               RHO(J) = (1.-DSURF1*SGI(J)**2)*(1.+DSURF*SGI(J)**2)
               DRHO(J) = -2.*DSURF1*SGI(J)*(1.+DSURF*SGI(J)**2)
     >                   +2.*DSURF*SGI(J)*(1.-DSURF1*SGI(J)**2)
            ELSEIF(IEQ.EQ.3) THEN
               RHO(J) = 1.+DSURF*SGI(J)**2+DSURF1*SGI(J)**4
     >                    +DSURF2*SGI(J)**6+DSURF3*SGI(J)**8
               DRHO(J) = 2.*DSURF*SGI(J)+4.*DSURF1*SGI(J)**3
     >                 +6.*DSURF2*SGI(J)**5+8.*DSURF3*SGI(J)**7
           ELSEIF(IEQ.EQ.4) THEN
               RHO(J) = 1.+DSURF*SGI(J)     + DSURF1*SGI(J)**2
     >                    +DSURF2*SGI(J)**3 + DSURF3*SGI(J)**4
               DRHO(J) =   DSURF            + 2*DSURF1*SGI(J)
     >                 + 3*DSURF2*SGI(J)**2 + 4*DSURF3*SGI(J)**3
            ELSE
               RHO(J)  = 1
               DRHO(J) = IDPOW*0*(IDPOW-1)*(-2*(1.-DSURF)*SGI(J))
            ENDIF
            FLOW3(J) = VEL3*(1.+VSURF*SGI(J)+VSURF1*SGI(J)**2+
     >                       VSURF2*SGI(J)**3+VSURF3*SGI(J)**8)
            DFLOW3(J)= VEL3*( -2.*VSURF1*SGI(J)*(1.+VSURF*SGI(J)**2)
     >                   +2.*VSURF*SGI(J)*(1.-VSURF1*SGI(J)**2) )
            ZT0(J)  = ZP
            ZDT0(J) = ZDP
            ZDDT0(J) = ZDDP
C------------------------------------ omega*
            OMEGA(J) = SPWERT(NPSI,ZS(I),OM1,OM2,OM3,OM4,CS,ABLTG) /
     >                 RHO(J)
C------------------------------------ LArmor radius (normalised)
            RLARM(J) = CWW * SQRT(ABS(2.*ZP/RHO(J)))
C
   10    CONTINUE
   20 CONTINUE

c      WRITE(NOUT,21)
c      DO 30 J=1,4*NGINT
c        WRITE(NOUT,22) SGI(J),Q(J),DQ(J),T(J),DT(J),ETAV(J),DETA(J),
c     >               RHO(J),DRHO(J),ZT0(J),ZDT0(J),FLOW3(J)
c   30 CONTINUE   
   
      QWALL   = SPWERT(NPSI,SGRID(NI),Q1,Q2,Q3,Q4,CS,ABLTG)
      WRITE(NOUT,31) QWALL
C
      DMNQ1 = 1e12
      DMNQ2 = 1e12      
      IQ1 = 0
      IQ2 = 0
      QMIN = 1e12
      QMAX = -1e12
      DO J=1,4*NGINT
        IF (ABS(Q(J)-1.).LT.DMNQ1) THEN
		   DMNQ1 = ABS(Q(J)-1.)
		   IQ1 = J
		 ENDIF
        IF (ABS(Q(J)-2.).LT.DMNQ2) THEN
		   DMNQ2 = ABS(Q(J)-2.)
		   IQ2 = J
		 ENDIF
        IF (QMIN.GT.Q(J)) QMIN=Q(J)
        IF (QMAX.LT.Q(J)) QMAX=Q(J)
      ENDDO
      IF ((QMIN.LE.1.).AND.(QMAX.GE.1.)) THEN
        WRITE(20,32) SGI(IQ1),Q(IQ1),DQ(IQ1),OMEGA(IQ1)
      ENDIF
      IF ((QMIN.LE.2.).AND.(QMAX.GE.2.)) THEN
        WRITE(20,33) SGI(IQ2),Q(IQ2),DQ(IQ2),OMEGA(IQ2)
      ENDIF
C
      RETURN
   21 FORMAT(///7X,'S',11X,'Q',10X,'DQ',11X,'T',10X,'DT',10X,'ETA',
     >       8X,'DETA',9X,'RHO',8X,'DRHO',9X,'T0',10X,'DT0'/1X,132('-'))
   22 FORMAT(1X,1P,11E12.4)
   31 FORMAT(/' Q AT BOUNDARY :',E12.4)
   32 FORMAT(//' S = ',F6.3,'  Q = ',F6.3,'  DQ = ',E12.4,
     >       '  OM*(Q=1) = ',E12.4)
   33 FORMAT(' S = ',F6.3,'  Q = ',F6.3,'  DQ = ',E12.4,
     >       '  OM*(Q=2) = ',E12.4)
      END

*************************************************************************
*DECK FKEQ
      SUBROUTINE FKEQ
C-----------------------------------------------------------------------
C
C     COMPUTATION OF THE SPLINE COEFFICIENTS OF THE FOURIER COEFFICIENTS
C
C     DIMENSIONS:
C
C     INDEX,FWT(2*(NCHI-1)),
C     WORK(3*NCHI-1), ZFK(NCHI,NPSI)
C     HV,GEM11,GEM12,GEM33 (NCHI*NPSI)
C     ALLE FOURIERKOEFFIZIENTEN  (4*NPSI,L)
C-----------------------------------------------------------------------
*CALL COMMAX
*CALL COMPIO
*CALL COMPCON
*CALL COMGEM
*CALL COMIOD
*CALL COMEQUI
*CALL COMDIAG
*CALL COMFFT
*CALL COMBDIA
 
      INTEGER  INDEX(2*(NCHIMAX-1))
      REAL     HV(NPNC), FWT(2*(NCHIMAX-1))
      REAL     H1(NPSIMAX), H2(NPSIMAX), H3(NPSIMAX), H4(NPSIMAX)
      REAL     DUMMY(3), DTGEM33(NPNC)
      COMPLEX  ZFK(NCHIMAX,NPSIMAX)
*IF CRAY
      COMPLEX  WORK(3*NCHIMAX)
*ELSE
      REAL     WORK(6*NCHIMAX)
*ENDIF
C
      INTEGER  NGES
      REAL     ABLTG(3)
C
      NGES = NPSI*NCHI
      NP1  = NPSI+1
      N2P1 = 2*NPSI+1
      N3P1 = 3*NPSI+1
C
      DO 10 I=1,NCHI
         INDEX(I) = I
   10 CONTINUE
      IF (IAS.EQ.0) THEN
         DO 20 I=NCHI+1,2*NCHI-2
            INDEX(I) = 2*NCHI-I
 20      CONTINUE
      ENDIF
C-------------------------- INITIALIZATION OF THE SINE AND COSINE TABLES
      IF (IAS.EQ.0) THEN
         N = 2*(NCHI-1)
      ELSE
         N = NCHI
      ENDIF
C-------------------------- CALCULATE DR2/D(CHI) -----------------------
      DO I=1,NPSI 
        CALL DERIV(GEM33((I-1)*NCHI+1),DTGEM33((I-1)*NCHI+1),NCHI,IAS) 
      ENDDO
*IF CRAY
         CALL RCFFT2(1,IDUMMY,N,DUMMY,WORK,DUMMY)
*ELSE
         DO 30 I=1,N/2
            WORK(2*N+4+I) = COS(FLOAT(I-1)*2.*PI/FLOAT(N))
 30      CONTINUE
*ENDIF
C-----------------------------------------------------------------------
C FOURIER ANALYSIS AND SPLINE FOR EVERY FOURIER COEFFICIENT
C-----------------------------------------------------------------------
C                        R**2  -->  RR2,IR2
C-----------------------------------------------------------------------
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,GEM33,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RR2,IR2)
C-----------------------------------------------------------------------
C                        R**4  -->  RR4,IR4
C-----------------------------------------------------------------------
      DO 40 I=1,NGES
         HV(I) = GEM33(I)**2
   40 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RR4,IR4)
C-----------------------------------------------------------------------
C                        1/R**2  -->  ROOR2,IOOR2
C-----------------------------------------------------------------------
      DO 45 I=1,NGES
         HV(I) = 1./GEM33(I)
   45 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,ROOR2,IOOR2)
C-----------------------------------------------------------------------
C                        GRAD.PSI**2  -->  RGPSI,IGPSI
C-----------------------------------------------------------------------
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,GEM11,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RGPSI,IGPSI)
C-----------------------------------------------------------------------
C                        GRAD.PSI**4  -->  RGPSI2,IGPSI2
C-----------------------------------------------------------------------
      DO 55 I=1,NGES
         HV(I) = GEM11(I)**2
   55 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RGPSI2,IGPSI2)
C-----------------------------------------------------------------------
C                        S**2/GRAD.PSI**2  -->  RSOGPSI,ISOGPSI
C-----------------------------------------------------------------------
      DO 50 I=NCHI+1,NGES
         HV(I) = CS(1+(I-1)/NCHI)**2 / GEM11(I)
   50 CONTINUE
      DO 60 J=1,NCHI
         CALL SGCOPY(NPSI-1,HV(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HV(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
   60 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RSOGPSI,ISOGPSI)
C-----------------------------------------------------------------------
C        GRAD.PSI * GRAD.THETA  -->  RGPGT,IGPGT
C-----------------------------------------------------------------------
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,GEM12,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RGPGT,IGPGT)
C-----------------------------------------------------------------------
C       (GRAD.PSI * GRAD.THETA)**2  -->  RGPGT2,IGPGT2
C-----------------------------------------------------------------------
      DO 65 I=1,NGES
         HV(I) = GEM12(I)**2
   65 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RGPGT2,IGPGT2)
C-----------------------------------------------------------------------
C        GRAD.PSI**2 . GRAD.PSI * GRAD.THETA  -->  RGPGPGT,IGPGPGT
C-----------------------------------------------------------------------
      DO 75 I=1,NGES
         HV(I) = GEM11(I)* GEM12(I)
   75 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RGPGPGT,IGPGPGT)
C-----------------------------------------------------------------------
C        S**2*(GRAD.PSI*GRAD.THETA)**2/GRAD.PSI**2  -->  RSGGG,ISGGG
C-----------------------------------------------------------------------
      DO 70 I=NCHI+1,NGES
         HV(I) = CS(1+(I-1)/NCHI)**2 * GEM12(I)**2 / GEM11(I)
   70 CONTINUE
      DO 80 J=1,NCHI
         CALL SGCOPY(NPSI-1,HV(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HV(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
   80 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RSGGG,ISGGG)
C-----------------------------------------------------------------------
C        S**2 * R**2 * (GRAD.PSI*GRAD.THETA)**2/GRAD.PSI**2  -->
C                                                     RSR2GGG,ISR2GGG
C-----------------------------------------------------------------------
      DO 90 I=NCHI+1,NGES
         HV(I) = CS(1+(I-1)/NCHI)**2 * GEM33(I)*GEM12(I)**2 / GEM11(I)
   90 CONTINUE
      DO 100 J=1,NCHI
         CALL SGCOPY(NPSI-1,HV(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HV(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
  100 CONTINUE
C
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RSR2GGG,ISR2GGG)
C-----------------------------------------------------------------------
C        S**2 * R**4 * (GRAD.PSI*GRAD.THETA)**2/GRAD.PSI**2  -->
C                                                  RSR4GGG,ISR4GGG
C-----------------------------------------------------------------------
      DO 110 I=NCHI+1,NGES
         HV(I) = CS(1+(I-1)/NCHI)**2 * GEM33(I)**2 * GEM12(I)**2
     >           / GEM11(I)
  110 CONTINUE
      DO 120 J=1,NCHI
         CALL SGCOPY(NPSI-1,HV(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HV(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
  120 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RSR4GGG,ISR4GGG)
C-----------------------------------------------------------------------
C        R**2 * GRAD.PSI**2  -->  RR2GPSI,IR2GPSI
C-----------------------------------------------------------------------
      DO 130 I=1,NGES
         HV(I) = GEM33(I)*GEM11(I)
  130 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RR2GPSI,IR2GPSI)
C-----------------------------------------------------------------------
C        GRAD.PSI**2 / R**2 -->  RGPSIOR,IGPSIOR
C-----------------------------------------------------------------------
      DO 135 I=1,NGES
         HV(I) = GEM11(I)/GEM33(I)
  135 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RGPSIOR,IGPSIOR)
C-----------------------------------------------------------------------
C        S**2 / (R**2*GRAD.PSI**2)  -->  RSOR2GP,ISOR2GP
C-----------------------------------------------------------------------
      DO 140 I=NCHI+1,NGES
         HV(I) = CS(1+(I-1)/NCHI)**2 / (GEM33(I) * GEM11(I))
  140 CONTINUE
      DO 150 J=1,NCHI
         CALL SGCOPY(NPSI-1,HV(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HV(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
  150 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RSOR2GP,ISOR2GP)
C-----------------------------------------------------------------------
C        S**2 * R**2 / GRAD.PSI**2  -->  RSR2OGP,ISR2OGP
C-----------------------------------------------------------------------
      DO 160 I=NCHI+1,NGES
         HV(I) = CS(1+(I-1)/NCHI)**2 * GEM33(I) / GEM11(I)
  160 CONTINUE
      DO 170 J=1,NCHI
         CALL SGCOPY(NPSI-1,HV(NCHI+J),NCHI,FWT,1)
         CALL SPLINE(NPSI-1,CS(2),FWT,0.0,0.0,2,H1,H2,H3,H4)
         HV(J) = SPWERT(NPSI-1,0.0,H1,H2,H3,H4,CS(2),DUMMY)
  170 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RSR2OGP,ISR2OGP)
C-----------------------------------------------------------------------
C        R**2*GRAD.PSI*GRAD.THETA  -->  RR2GPGT,IR2GPGT
C-----------------------------------------------------------------------
      DO 180 I=1,NGES
         HV(I) = GEM33(I)*GEM12(I)
  180 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RR2GPGT,IR2GPGT)
C-----------------------------------------------------------------------
C        R**4*GRAD.PSI**2  -->  RR4GPSI,IR4GPSI
C-----------------------------------------------------------------------
      DO 190 I=1,NGES
         HV(I) = GEM33(I)**2*GEM11(I) 
     >    / (1. + GEM11(I) / RBPHI(1+(I-1)/NCHI)**2  )    
  190 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RR4GPSI,IR4GPSI)
C-----------------------------------------------------------------------
C        B_0^2  -->  RB02,IB02
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = (1. + GEM11(I) / RBPHI(1+(I-1)/NCHI)**2  )    
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RB02,IB02)
C-----------------------------------------------------------------------
C        R**4*GRAD.PSI*GRAD.THETA  -->  RR4GPGT,IR4GPGT
C-----------------------------------------------------------------------
      DO 200 I=1,NGES
         HV(I) = GEM33(I)**2*GEM12(I)
     >    / (1. + GEM11(I) / RBPHI(1+(I-1)/NCHI)**2  )         
  200 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RR4GPGT,IR4GPGT)
C-----------------------------------------------------------------------
C   R**4*(GRAD.PSI*GRAD.THETA)**2 / GRAD.PSI**2 -->  RR4GPTOGP, IR4GPTOGP
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = GEM33(I)**2 * GEM12(I)**2 / GEM11(I)         
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                            RR4GPTOGP,IR4GPTOGP)
C-----------------------------------------------------------------------
C        R**2*GRAD.PSI**2/(1 + GRAD.PSI**2/F**2)  -->  RFR2GPS,IFR2GPS
C-----------------------------------------------------------------------
      DO 210 I=1,NGES
         HV(I) = GEM33(I)*GEM11(I) 
     >    / (1. + GEM11(I) / RBPHI(1+(I-1)/NCHI)**2  )    
  210 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RFR2GPS,IFR2GPS)
C-----------------------------------------------------------------------
C   R**2*GRAD.PSI*GRAD.THETA /(1 + GRAD.PSI**2/F**2) --> RFR2GPT,IFR2GPT
C-----------------------------------------------------------------------
      DO 220 I=1,NGES
         HV(I) = GEM33(I)*GEM12(I)
     >    / (1. + GEM11(I) / RBPHI(1+(I-1)/NCHI)**2  )         
  220 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RFR2GPT,IFR2GPT)
C-----------------------------------------------------------------------
C   R**2*GRAD.PSI*GRAD.THETA**2 /(1+GRAD.PSI**2/F**2) --> RFR2GT2,IFR2GT2
C-----------------------------------------------------------------------
      DO 225 I=1,NGES
         HV(I) = GEM33(I)*GEM12(I)**2
     >    / (1. + GEM11(I) / RBPHI(1+(I-1)/NCHI)**2  )         
  225 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RFR2GT2,IFR2GT2)
C-----------------------------------------------------------------------
C         R**2/(1 + GRAD.PSI**2/F**2) --> RFR2,IFR2
C-----------------------------------------------------------------------
      DO 230 I=1,NGES
         HV(I) = GEM33(I)
     >    / (1. + GEM11(I) / RBPHI(1+(I-1)/NCHI)**2  )         
  230 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RFR2,IFR2)
C-----------------------------------------------------------------------
C        R**4*GRAD.PSI*GRAD.THETA**2 -->  RR4GPGT,IR4GPGT
C-----------------------------------------------------------------------
      DO 240 I=1,NGES
         HV(I) = GEM33(I)**2*GEM12(I)**2
     >    / (1. + GEM11(I) / RBPHI(1+(I-1)/NCHI)**2  )         
  240 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RR4GPT2,IR4GPT2)
C-----------------------------------------------------------------------
C        DS R**2  -->  RDSR2,IDSR2
C-----------------------------------------------------------------------
      DO 250 J=1,NCHI
         CALL SGCOPY(NPSI,GEM33(J),NCHI,FWT,1)
         CALL SPLINE(NPSI,CS,FWT,0.0,0.0,3,H1,H2,H3,H4)
         CALL SGCOPY(NPSI,H2,1,HV(J),NCHI)
  250 CONTINUE
C
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RDSR2,IDSR2)
C-----------------------------------------------------------------------
C       GRAD.PSI**4 *  DS R**2  -->  RGPSI2DSR2,IGPSI2DSR2
C-----------------------------------------------------------------------
      DO J=1,NCHI
         CALL SGCOPY(NPSI,GEM33(J),NCHI,FWT,1)
         CALL SPLINE(NPSI,CS,FWT,0.0,0.0,3,H1,H2,H3,H4)
         CALL SGCOPY(NPSI,H2,1,HV(J),NCHI)
      ENDDO

      DO I=1,NGES
        HV(I) = HV(I) * GEM11(I)**2
      ENDDO      
C
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                           RGPSI2DSR2,IGPSI2DSR2)
C-----------------------------------------------------------------------
C   R**2*    GRAD.PSI**4 *  DS R**2  -->  RR2GPSI2DSR2,IR2GPSI2DSR2
C-----------------------------------------------------------------------
      DO J=1,NCHI
         CALL SGCOPY(NPSI,GEM33(J),NCHI,FWT,1)
         CALL SPLINE(NPSI,CS,FWT,0.0,0.0,3,H1,H2,H3,H4)
         CALL SGCOPY(NPSI,H2,1,HV(J),NCHI)
      ENDDO

      DO I=1,NGES
        HV(I) = HV(I) * GEM33(I) * GEM11(I)**2
      ENDDO      
C
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                       RR2GPSI2DSR2,IR2GPSI2DSR2)
C-----------------------------------------------------------------------
C   R**2*GRAD.PSI**4*(G.PSI*G.THETA) *  DS R**2  --> RR2GP2GPTDSR2,IR2GP2GPTDSR2
C-----------------------------------------------------------------------
      DO J=1,NCHI
         CALL SGCOPY(NPSI,GEM33(J),NCHI,FWT,1)
         CALL SPLINE(NPSI,CS,FWT,0.0,0.0,3,H1,H2,H3,H4)
         CALL SGCOPY(NPSI,H2,1,HV(J),NCHI)
      ENDDO

      DO I=1,NGES
        HV(I) = HV(I) * GEM33(I) * GEM11(I)**2 * GEM12(I)
     >    / (1. + GEM11(I) / RBPHI(1+(I-1)/NCHI)**2  )        
      ENDDO      
C
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                   RR2GP2GPTDSR2,IR2GP2GPTDSR2)
C-----------------------------------------------------------------------
C   R**2*    GRAD.PSI**2 *  DS R**2  -->  RR2GPDSR2,IR2GPDSR2
C-----------------------------------------------------------------------
      DO J=1,NCHI
         CALL SGCOPY(NPSI,GEM33(J),NCHI,FWT,1)
         CALL SPLINE(NPSI,CS,FWT,0.0,0.0,3,H1,H2,H3,H4)
         CALL SGCOPY(NPSI,H2,1,HV(J),NCHI)
      ENDDO

      DO I=1,NGES
        HV(I) = HV(I) * GEM33(I) * GEM11(I)
     >    / (1. + GEM11(I) / RBPHI(1+(I-1)/NCHI)**2  )        
      ENDDO      
C
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                       RR2GPDSR2,IR2GPDSR2)
C-----------------------------------------------------------------------
C R**2*GRAD.PSI**2*GRAD.PSI*(GRAD.THETA)**2 *DS R**2 --> RR2GGG2DSR2,IR2GGG2DSR2
C-----------------------------------------------------------------------
      DO J=1,NCHI
         CALL SGCOPY(NPSI,GEM33(J),NCHI,FWT,1)
         CALL SPLINE(NPSI,CS,FWT,0.0,0.0,3,H1,H2,H3,H4)
         CALL SGCOPY(NPSI,H2,1,HV(J),NCHI)
      ENDDO

      DO I=1,NGES
        HV(I) = HV(I) * GEM33(I) * GEM11(I) * GEM12(I)**2
     >    / (1. + GEM11(I) / RBPHI(1+(I-1)/NCHI)**2  )        
      ENDDO      
C
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                     RR2GGG2DSR2,IR2GGG2DSR2)
C-----------------------------------------------------------------------
C   GRAD.PSI**2*(GRAD.PSI*GRAD.THETA)**2 *DS R**2 --> RGPGPT2DSR2,IGPGPT2DSR2
C-----------------------------------------------------------------------
      DO J=1,NCHI
         CALL SGCOPY(NPSI,GEM33(J),NCHI,FWT,1)
         CALL SPLINE(NPSI,CS,FWT,0.0,0.0,3,H1,H2,H3,H4)
         CALL SGCOPY(NPSI,H2,1,HV(J),NCHI)
      ENDDO

      DO I=1,NGES
        HV(I) = HV(I) * GEM11(I) * GEM12(I)**2
     >    / (1. + GEM11(I) / RBPHI(1+(I-1)/NCHI)**2  )        
      ENDDO      
C
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                     RGPGPT2DSR2,IGPGPT2DSR2)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     1/R**2 *  DS R**2  -->  RDSR2OR2,IDSR2OR2
C-----------------------------------------------------------------------
      DO J=1,NCHI
         CALL SGCOPY(NPSI,GEM33(J),NCHI,FWT,1)
         CALL SPLINE(NPSI,CS,FWT,0.0,0.0,3,H1,H2,H3,H4)
         CALL SGCOPY(NPSI,H2,1,HV(J),NCHI)
      ENDDO

      DO I=1,NGES
        HV(I) = HV(I) / GEM33(I)
      ENDDO      
C
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                           RDSR2OR2,IDSR2OR2)
C-----------------------------------------------------------------------
C  R**2*GRAD.PSI*GRAD.THETA*GRAD.PSI**2 *DS R**2 -->RR2GPTGPDSR2,IR2GPTGPDSR2
C-----------------------------------------------------------------------
      DO J=1,NCHI
         CALL SGCOPY(NPSI,GEM33(J),NCHI,FWT,1)
         CALL SPLINE(NPSI,CS,FWT,0.0,0.0,3,H1,H2,H3,H4)
         CALL SGCOPY(NPSI,H2,1,HV(J),NCHI)
      ENDDO

      DO I=1,NGES
        HV(I) = HV(I) * GEM11(I) * GEM12(I) * GEM33(I)
      ENDDO      
C
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                      RR2GPTGPDSR2,IR2GPTGPDSR2)
C-----------------------------------------------------------------------
C  R**2 *  GRAD.PSI*GRAD.THETA * DS R**2 -->RR2GPTDSR2,IR2GPTDSR2
C-----------------------------------------------------------------------
      DO J=1,NCHI
         CALL SGCOPY(NPSI,GEM33(J),NCHI,FWT,1)
         CALL SPLINE(NPSI,CS,FWT,0.0,0.0,3,H1,H2,H3,H4)
         CALL SGCOPY(NPSI,H2,1,HV(J),NCHI)
      ENDDO

      DO I=1,NGES
        HV(I) = HV(I) * GEM12(I) * GEM33(I)
     >    / (1. + GEM11(I) / RBPHI(1+(I-1)/NCHI)**2  )        
      ENDDO      

      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                      RR2GPTDSR2,IR2GPTDSR2)
C-----------------------------------------------------------------------
C       GRAD.PSI**2 *  DS R**2  -->  RGPSIDSR2,IGPSIDSR2
C-----------------------------------------------------------------------
      DO J=1,NCHI
         CALL SGCOPY(NPSI,GEM33(J),NCHI,FWT,1)
         CALL SPLINE(NPSI,CS,FWT,0.0,0.0,3,H1,H2,H3,H4)
         CALL SGCOPY(NPSI,H2,1,HV(J),NCHI)
      ENDDO

      DO I=1,NGES
        HV(I) = HV(I) * GEM11(I)
      ENDDO      
C
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                           RGPSIDSR2,IGPSIDSR2)
C-----------------------------------------------------------------------
C       GRAD.PSI*GRAD.THETA**2 * DS R**2  -->  RGPGT2DSR2,IGPGT2DSR2
C-----------------------------------------------------------------------
      DO J=1,NCHI
         CALL SGCOPY(NPSI,GEM33(J),NCHI,FWT,1)
         CALL SPLINE(NPSI,CS,FWT,0.0,0.0,3,H1,H2,H3,H4)
         CALL SGCOPY(NPSI,H2,1,HV(J),NCHI)
      ENDDO

      DO I=1,NGES
        HV(I) = HV(I) * GEM12(I)**2
      ENDDO      
C
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                         RGPGT2DSR2,IGPGT2DSR2)
C-----------------------------------------------------------------------
C R**2 * GRAD.PSI*GRAD.THETA**2 * DS R**2  -->  RR2GPGT2DSR2,IR2GPGT2DSR2
C-----------------------------------------------------------------------
      DO J=1,NCHI
         CALL SGCOPY(NPSI,GEM33(J),NCHI,FWT,1)
         CALL SPLINE(NPSI,CS,FWT,0.0,0.0,3,H1,H2,H3,H4)
         CALL SGCOPY(NPSI,H2,1,HV(J),NCHI)
      ENDDO

      DO I=1,NGES
        HV(I) = HV(I) * GEM33(I) * GEM12(I)**2 
      ENDDO      
C
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                      RR2GPGT2DSR2,IR2GPGT2DSR2)
C-----------------------------------------------------------------------
C       GRAD.PSI*GRAD.THETA * DS R**2  -->  RGPGTDSR2,IGPGTDSR2
C-----------------------------------------------------------------------
      DO J=1,NCHI
         CALL SGCOPY(NPSI,GEM33(J),NCHI,FWT,1)
         CALL SPLINE(NPSI,CS,FWT,0.0,0.0,3,H1,H2,H3,H4)
         CALL SGCOPY(NPSI,H2,1,HV(J),NCHI)
      ENDDO

      DO I=1,NGES
        HV(I) = HV(I) * GEM12(I)
      ENDDO      
C
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                         RGPGTDSR2,IGPGTDSR2)
C-----------------------------------------------------------------------
C       GRAD.PSI**2/R**2 * DS R**2  -->  RGPSIOR2DSR2,IGPSIOR2DSR2
C-----------------------------------------------------------------------
      DO J=1,NCHI
         CALL SGCOPY(NPSI,GEM33(J),NCHI,FWT,1)
         CALL SPLINE(NPSI,CS,FWT,0.0,0.0,3,H1,H2,H3,H4)
         CALL SGCOPY(NPSI,H2,1,HV(J),NCHI)
      ENDDO

      DO I=1,NGES
        HV(I) = HV(I) * GEM11(I)/GEM33(I)
      ENDDO      
C
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                   RGPSIOR2DSR2,IGPSIOR2DSR2)
C-----------------------------------------------------------------------
C  GRAD.PSI*GRAD.THETA * GRAD.PSI**2*DS R**2 -->RGPGTGPDSR2,IGPGTGPDSR2
C-----------------------------------------------------------------------
      DO J=1,NCHI
         CALL SGCOPY(NPSI,GEM33(J),NCHI,FWT,1)
         CALL SPLINE(NPSI,CS,FWT,0.0,0.0,3,H1,H2,H3,H4)
         CALL SGCOPY(NPSI,H2,1,HV(J),NCHI)
      ENDDO

      DO I=1,NGES
        HV(I) = HV(I) * GEM12(I)* GEM11(I)
      ENDDO      
C
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                       RGPGTGPDSR2,IGPGTGPDSR2)
C-----------------------------------------------------------------------
C  GRAD.PSI*GRAD.THETA / R**2 *DS R**2 --> RPTOR2DSR2,IPTOR2DSR2
C-----------------------------------------------------------------------
      DO J=1,NCHI
         CALL SGCOPY(NPSI,GEM33(J),NCHI,FWT,1)
         CALL SPLINE(NPSI,CS,FWT,0.0,0.0,3,H1,H2,H3,H4)
         CALL SGCOPY(NPSI,H2,1,HV(J),NCHI)
      ENDDO

      DO I=1,NGES
        HV(I) = HV(I) * GEM12(I) / GEM33(I)
      ENDDO      
C
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                       RPTOR2DSR2,IPTOR2DSR2)
C-----------------------------------------------------------------------
C   D(R2)/D(THETA)  --> RDTR2, IDTR2 
C-----------------------------------------------------------------------
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,DTGEM33,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RDTR2,IDTR2)
C-----------------------------------------------------------------------
C F  D(R2)/D(THETA)  --> RFDTR2, IFDTR2 
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = DTGEM33(I)
     >    / (1. + GEM11(I) / RBPHI(1+(I-1)/NCHI)**2  )        
      ENDDO

      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RFDTR2,IFDTR2)
C-----------------------------------------------------------------------
C   D(R2)/D(THETA)/ R2  --> RDTR2OR2, IDTR2OR2 
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = DTGEM33(I)/GEM33(I)
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                           RDTR2OR2,IDTR2OR2)
C-----------------------------------------------------------------------
C   D(R2)/D(THETA)/ R4  --> RDTR2OR4, IDTR2OR4 
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = DTGEM33(I)/GEM33(I)**2
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                           RDTR2OR4,IDTR2OR4)
C-----------------------------------------------------------------------
C  R**2*(GRAD.PSI*GRAD.THETA)**2*D(R2)/D(THETA)--> RR2GPGT2DTR2,IR2GPGT2DTR2
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = DTGEM33(I)*GEM33(I)*GEM12(I)**2
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                      RR2GPGT2DTR2,IR2GPGT2DTR2)
C-----------------------------------------------------------------------
C  R**2*(GRAD.PSI*GRAD.THETA)*D(R2)/D(THETA)--> RR2GPGTDTR2,IR2GPGTDTR2
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = DTGEM33(I)*GEM33(I)*GEM12(I)
     >    / (1. + GEM11(I) / RBPHI(1+(I-1)/NCHI)**2  )        
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                      RR2GPGTDTR2,IR2GPGTDTR2)
C-----------------------------------------------------------------------
C  R**2*(GRAD.PSI*GRAD.THETA)**3*D(R2)/D(THETA)--> RR2GPGT3DTR2,IR2GPGT3DTR2
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = DTGEM33(I)*GEM33(I)*GEM12(I)**3
     >    / (1. + GEM11(I) / RBPHI(1+(I-1)/NCHI)**2  )        
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                      RR2GPGT3DTR2,IR2GPGT3DTR2)
C-----------------------------------------------------------------------
C  R**2*(G.PSI*G.THETA)**2*GP**2*D(R2)/D(THETA)--> RR2GGG2DTR2,IR2GGG2DTR2
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = DTGEM33(I)*GEM33(I)*GEM12(I)**2 * GEM11(I)
     >    / (1. + GEM11(I) / RBPHI(1+(I-1)/NCHI)**2  )        
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                      RR2GGG2DTR2,IR2GGG2DTR2)
C-----------------------------------------------------------------------
C  R**2* *GRAD PSI**2 *D(R2)/D(THETA)--> RR2GPDTR2,IR2GPDTR2
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = DTGEM33(I) * GEM33(I) * GEM11(I)
     >    / (1. + GEM11(I) / RBPHI(1+(I-1)/NCHI)**2  )        
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                      RR2GPDTR2,IR2GPDTR2)
C-----------------------------------------------------------------------
C    (GRAD.PSI*GRAD.THETA)**3*D(R2)/D(THETA)--> RGPGT3DTR2,IGPGT3DTR2
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = DTGEM33(I)*GEM12(I)**3
     >    / (1. + GEM11(I) / RBPHI(1+(I-1)/NCHI)**2  )        
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                      RGPGT3DTR2,IGPGT3DTR2)
C-----------------------------------------------------------------------
C    (GRAD.PSI*GRAD.THETA)**2*D(R2)/D(THETA)--> RGPGT2DTR2,IGPGT2DTR2
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = DTGEM33(I)*GEM12(I)**2
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                      RGPGT2DTR2,IGPGT2DTR2)
C-----------------------------------------------------------------------
C GRAD.PSI**2*(GRAD.PSI*GRAD.THETA)*D(R2)/D(THETA)--> RGPGPGTDTR2,IGPGPGTDTR2
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = DTGEM33(I)*GEM11(I)*GEM12(I)
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                      RGPGPGTDTR2,IGPGPGTDTR2)
C-----------------------------------------------------------------------
C         GRAD.PSI**2 *D(R2)/D(THETA)--> RGPSIDTR2,IGPSIDTR2
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = DTGEM33(I)*GEM11(I)
     >    / (1. + GEM11(I) / RBPHI(1+(I-1)/NCHI)**2  )        
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                      RGPSIDTR2,IGPSIDTR2)
C-----------------------------------------------------------------------
C (GRAD.PSI*GRAD.THETA)/R**2 * *D(R2)/D(THETA)--> RPTOR2DTR2,IPTOR2DTR2
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = DTGEM33(I)*GEM12(I)/GEM33(I)
     >    / (1. + GEM11(I) / RBPHI(1+(I-1)/NCHI)**2  )        
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                      RPTOR2DTR2,IPTOR2DTR2)
C-----------------------------------------------------------------------
C (GRAD.PSI*GRAD.THETA) *D(R2)/D(THETA)--> RGPTDTR2,IGPTDTR2
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = DTGEM33(I)*GEM12(I)
     >    / (1. + GEM11(I) / RBPHI(1+(I-1)/NCHI)**2  )        
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,
     >                                      RGPTDTR2,IGPTDTR2)
C-----------------------------------------------------------------------
C        DS GRAD.PSI**2  -->  RDSGPSI,IDSGPSI
C-----------------------------------------------------------------------
      DO 260 J=1,NCHI
         CALL SGCOPY(NPSI,GEM11(J),NCHI,FWT,1)
         CALL SPLINE(NPSI,CS,FWT,0.0,0.0,3,H1,H2,H3,H4)
         CALL SGCOPY(NPSI,H2,1,HV(J),NCHI)
  260 CONTINUE
C
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RDSGPSI,IDSGPSI)
C-----------------------------------------------------------------------
C     DS GRAD.PSI*GRAD.THETA  -->  RDSGPGT,IDSGPGT
C-----------------------------------------------------------------------
      DO 265 J=1,NCHI
         CALL SGCOPY(NPSI,GEM12(J),NCHI,FWT,1)
         CALL SPLINE(NPSI,CS,FWT,0.0,0.0,3,H1,H2,H3,H4)
         CALL SGCOPY(NPSI,H2,1,HV(J),NCHI)
  265 CONTINUE

      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RDSGPGT,IDSGPGT)
C-----------------------------------------------------------------------
C       R**2 * DS GRAD.PSI**2  -->  RR2DSGP,IR2DSGP
C-----------------------------------------------------------------------
      DO 270 J=1,NCHI
         CALL SGCOPY(NPSI,GEM11(J),NCHI,FWT,1)
         CALL SPLINE(NPSI,CS,FWT,0.0,0.0,3,H1,H2,H3,H4)
         CALL SGCOPY(NPSI,H2,1,HV(J),NCHI)
  270 CONTINUE
      DO 280 I=1,NGES
        HV(I) = HV(I) * GEM33(I)
  280 CONTINUE      
C
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RR2DSGP,IR2DSGP)
C-----------------------------------------------------------------------
C       GRAD.PSI**2 * DS GRAD.PSI**2  -->  RGPDSGP,IR2DSGP
C-----------------------------------------------------------------------
      DO 290 J=1,NCHI
         CALL SGCOPY(NPSI,GEM11(J),NCHI,FWT,1)
         CALL SPLINE(NPSI,CS,FWT,0.0,0.0,3,H1,H2,H3,H4)
         CALL SGCOPY(NPSI,H2,1,HV(J),NCHI)
  290 CONTINUE
      DO 300 I=1,NGES
        HV(I) = HV(I) * GEM11(I)
  300 CONTINUE      
C
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RGPDSGP,IGPDSGP)
C-----------------------------------------------------------------------
C       (GRAD.PSI.GRAD.THETA) * DS GRAD.PSI**2  -->  RGPTDSG,IGPTDSG
C-----------------------------------------------------------------------
      DO 310 J=1,NCHI
         CALL SGCOPY(NPSI,GEM11(J),NCHI,FWT,1)
         CALL SPLINE(NPSI,CS,FWT,0.0,0.0,3,H1,H2,H3,H4)
         CALL SGCOPY(NPSI,H2,1,HV(J),NCHI)
  310 CONTINUE
      DO 320 I=1,NGES
        HV(I) = HV(I) * GEM12(I)
  320 CONTINUE      
C
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RGPTDSG,IGPTDSG)
C-----------------------------------------------------------------------
C ADDITIONAL FOURIER COEFF. FOR DIAGNOSTIC BOUND. COND. 
C-----------------------------------------------------------------------
C        GRAD.PSI**2 / R**2   -->  RPSOR2,IPSOR2
C-----------------------------------------------------------------------
      DO 330 I=1,NGES
         HV(I) = GEM11(I) / GEM33(I)
  330 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RPSOR2,IPSOR2)
C-----------------------------------------------------------------------
C    (F)    R**4*GRAD.PSI**2  -->  RFR4GPS,IFR4GPS
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = GEM33(I)**2*GEM11(I)
     >    / (1. + GEM11(I) / RBPHI(1+(I-1)/NCHI)**2  )         
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RFR4GPS,IFR4GPS)
C-----------------------------------------------------------------------
C    (F)    R**4*GRAD.PSI*GRAD.THETA  -->  RFR4GPT,IFR4GPT
C-----------------------------------------------------------------------
      DO I=1,NGES
         HV(I) = GEM33(I)**2*GEM12(I)
     >    / (1. + GEM11(I) / RBPHI(1+(I-1)/NCHI)**2  )         
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RFR4GPT,IFR4GPT)
C-----------------------------------------------------------------------
C        GRAD.PSI*GRAD.THETA / R**2  -->  RPTOR2,IPTOR2
C-----------------------------------------------------------------------
      DO 340 I=1,NGES
         HV(I) = GEM12(I) / GEM33(I)
  340 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RPTOR2,IPTOR2)
C
      RETURN
      END
************************************************************************
*DECK FFTRAN
      SUBROUTINE FFTRAN(L,NCHIMAX,NPSI,NCHI,FW,WORK,FWT,ZFK,INDEX,IS)
C-----------------------------------------------------------------------
C     FOURIER ANALYSIS
C-----------------------------------------------------------------------
*CALL COMPIO
*CALL COMPCON
*CALL COMDIAG
*CALL COMEQUI
 
      INTEGER  L, IS, N, NCHI, NPSI, INDEX(*)
      REAL     FW(NCHI,*), FWT(*), WORK(*), ONETON
      COMPLEX  ZFK(NCHIMAX,*)
C
      IF (IAS.EQ.0) THEN
          N=2*(NCHI-1)
      ELSE
         N=NCHI
      ENDIF
      ONETON = 1./(2.*N)
C
      DO 100 J=1,NPSI
*IF CRAY
         CALL GATHER(N,FWT,FW(1,J),INDEX)
*ELSE
         DO 10 IG = 1,N
            FWT(IG) = FW(INDEX(IG),J)
   10    CONTINUE
*ENDIF
         IF((IAS.EQ.0).AND.(IS.EQ.0))
     >        CALL SGSCAL(NCHI-2,-1.,FWT(NCHI+1),1)
*IF CRAY
         CALL RCFFT2(0,1,N,FWT,WORK,ZFK(1,J))
*ELSE
         CALL RFT2  (FWT,N,1)
         DO 30 I = 1,L
   30        ZFK(I,J) = CMPLX(FWT(2*I-1),-FWT(2*I)) / FLOAT(N)
*ENDIF
*IF CRAY
         CALL CSSCAL(L,ONETON,ZFK(1,J),1)
*ENDIF
         DO 40 I=2,L
            ZFK(I,J)=(N/(PI*(I-1)))**2*0.5*(1.-WORK((N+2)*2+I))*ZFK(I,J)
   40    CONTINUE
         IF(NDIAGFK.NE.0) WRITE(NOUT,41) J,(ZFK(II,J),II=1,L)
  100 CONTINUE
C
      RETURN
C
   41 FORMAT(' J=',I2,2X,5(1X,1P,2E12.4,0P)/(7X,5(1X,1P,2E12.4,0P)))
      END
************************************************************************
*DECK SPLFK
      SUBROUTINE SPLFK(NCHI,NPSI,NCHIMAX,NP4,L,CS,HV,ZFK,RV,AIV)
C-----------------------------------------------------------------------
C L SPLINES ON FOURIER COEFF(1:NPSI)
C-----------------------------------------------------------------------
      INTEGER  NPSI, NCHI, L, NP1, N2P1, N3P1, NP4, NCHIMAX
      REAL     HV(*), CS(*), RV(NP4,*), AIV(NP4,*)
      COMPLEX  ZFK(NCHIMAX,*)
C
      NP1  = NPSI+1
      N2P1 = 2*NPSI+1
      N3P1 = 3*NPSI+1
      DO 30 J = 1 , L
C
         DO 10 JH=1,NPSI
            HV(JH)      = REAL (ZFK(J,JH))
   10    CONTINUE
         DO 20 JH=1,NPSI
            HV(NPSI+JH) = AIMAG(ZFK(J,JH))
   20    CONTINUE
C
         CALL SPLINE(NPSI,CS,HV,0.0,0.0,3,RV(1,J),RV(NP1,J),RV(N2P1,J),
     >               RV(N3P1,J))
         CALL SPLINE(NPSI,CS,HV(NPSI+1),0.0,0.0,3,AIV(1,J),AIV(NP1,J),
     >               AIV(N2P1,J),AIV(N3P1,J))
C
   30 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK SPLINE
      SUBROUTINE SPLINE(N,X,Y,ALFA,BETA,TYP,A,B,C,D)
C-----------------------------------------------------------------------
C     INPUT:
C
C     N     ANZAHL DER KNOTEN
C     X     ARRAY DER X-WERTE
C     Y     ARRAY DER Y-WERTE
C     ALFA  RANDBEDINGUNG IN X(1)
C     BETA        "       IN X(N)
C     TYP   =  0  NOT-A-KNOT SPLINE
C              1  ALFA, BETA 1. ABLEITUNGEN VORGEGEBEN
C              2    "    "   2.     "           "
C              3    "    "   3.     "           "
C
C     BEMERKUNG: MIT TYP = 2 UND ALFA = BETA = 0 ERHAELT MAN
C           EINEN NATUERLICHEN SPLINE
C
C     OUTPUT:
C
C     A, B, C, D     ARRAYS DER SPLINEKOEFFIZIENTEN
C       S = A(I) + B(I)*(X-X(I)) + C(I)*(X-X(I))**2+ D(I)*(X-X(I))**3
C
C     BEI ANWENDUNGSFEHLERN WIRD DAS PROGRAMM MIT ENTSPRECHENDER
C     FEHLERMELDUNG ABGEBROCHEN
C-----------------------------------------------------------------------
C
*CALL COMPIO
*CALL COMPCON
C
      INTEGER  N, TYP
      REAL     X(N), Y(N), ALFA, BETA, A(N), B(N), C(N), D(N)
      INTEGER  I, IERR
      REAL     H(NMAX)
C
      IF((TYP.LT.0).OR.(TYP.GT.3)) THEN
         WRITE(NOUT,*) 'FEHLER IN ROUTINE SPLINE: FALSCHER TYP'
         STOP
      ENDIF
C
      IF((N.LT.3).OR.(N.GT.NMAX)) THEN
         WRITE(NOUT,*) 'FEHLER IN ROUTINE  SPLINE: N < 3 ODER N > NMAX'
         STOP
      ENDIF
C
C
C     BERECHNE DIFFERENZ AUFEINENDERFOLGENDER X-WERTE UND
C     UNTERSUCHE MONOTONIE
C
      DO 10 I = 1, N-1
         H(I) = X(I+1)- X(I)
         IF(H(I).LE.0.0) THEN
            WRITE(NOUT,*) 'MONOTONIEFEHLER IN SPLINE: X(I-1) >= X(I)'
            STOP
         ENDIF
   10 CONTINUE
C
C     AUFSTELLEN DES GLEICHUNGSSYSTEMS
C
      DO 20 I = 1, N-2
         A(I) = 3.0 * ((Y(I+2)-Y(I+1)) / H(I+1) - (Y(I+1)-Y(I)) / H(I))
         B(I) = H(I)
         C(I) = H(I+1)
         D(I) = 2.0 * (H(I) + H(I+1))
   20 CONTINUE
C
C     BERUECKSICHTIGEN DER RANDBEDINGUNGEN
C
C     NOT-A-KNOT
C
      IF(TYP.EQ.0) THEN
         A(1)   = A(1) * H(2) / (H(1) + H(2))
         A(N-2) = A(N-2) * H(N-2) / (H(N-1) + H(N-2))
         D(1)   = D(1) - H(1)
         D(N-2) = D(N-2) - H(N-1)
         C(1)   = C(1) - H(1)
         B(N-2) = B(N-2) - H(N-1)
      ENDIF
C
C     1. ABLEITUNG VORGEGEBEN
C
      IF(TYP.EQ.1) THEN
         A(1)   = A(1) - 1.5 * ((Y(2)-Y(1)) / H(1) - ALFA)
         A(N-2) = A(N-2) - 1.5 * (BETA - (Y(N)-Y(N-1)) / H(N-1))
         D(1)   = D(1) - 0.5 * H(1)
         D(N-2) = D(N-2) - 0.5 * H(N-1)
      ENDIF
C
C     2. ABLEITUNG VORGEGEBEN
C
      IF(TYP.EQ.2) THEN
         A(1)   = A(1) - 0.5 * ALFA * H(1)
         A(N-2) = A(N-2) - 0.5 * BETA * H(N-1)
      ENDIF
C
C     3. ABLEITUNG VORGEGEBEN
C
      IF(TYP.EQ.3 ) THEN
         A(1)   = A(1) + 0.5 * ALFA * H(1) * H(1)
         A(N-2) = A(N-2) - 0.5 * BETA * H(N-1)* H(N-1)
         D(1)   = D(1) + H(1)
         D(N-2) = D(N-2) + H(N-1)
      ENDIF
C
C     BERECHNUNG DER KOEFFIZIENTEN
C
*IF CRAY,JET
      CALL SGTSL(N-2,B,D,C,A,IERR)
*ELSE
      CALL SGTSL(N-2,B,D,C,A,IERR)
*ENDIF
      IF(IERR.NE.0) THEN
         WRITE(NOUT,21)
         STOP
      ENDIF
C
C     UEBERSCHREIBEN DES LOESUNGSVEKTORS
C
      CALL SGCOPY(N-2,A,1,C(2),1)
C
C     IN ABHAENGIGKEIT VON DEN RANDBEDINGUNGEN WIRD DER 1. UND
C     DER LETZTE WERT VON C KORRIGIERT
C
      IF(TYP.EQ.0) THEN
         C(1) = C(2) + H(1) * (C(2)-C(3)) / H(2)
         C(N) = C(N-1) + H(N-1) * (C(N-1)-C(N-2)) / H(N-2)
      ENDIF
C
      IF(TYP.EQ.1) THEN
         C(1) = 1.5*((Y(2)-Y(1)) / H(1) - ALFA) / H(1) - 0.5 * C(2)
         C(N) = -1.5*((Y(N)-Y(N-1)) / H(N-1)-BETA) / H(N-1)-0.5*C(N-1)
      ENDIF
C
      IF(TYP.EQ.2) THEN
         C(1) = 0.5 * ALFA
         C(N) = 0.5 * BETA
      ENDIF
C
      IF(TYP.EQ.3) THEN
         C(1) = C(2) - 0.5 * ALFA * H(1)
         C(N) = C(N-1) + 0.5 * BETA * H(N-1)
      ENDIF
C
      CALL SGCOPY(N,Y,1,A,1)
C
      DO 30 I = 1, N-1
         B(I) = (A(I+1)-A(I)) / H(I) - H(I) * (C(I+1)+2.0 * C(I)) / 3.0
         D(I) = (C(I+1)-C(I)) / (3.0 * H(I))
   30 CONTINUE
C
      B(N) = (3.0 * D(N-1) * H(N-1) + 2.0 * C(N-1)) * H(N-1) + B(N-1)
C
      RETURN
C
   21 FORMAT(1X,'ERROR IN SGTSL: MATRIX SINGULAR')
      END
************************************************************************
*DECK SPWERT
      REAL FUNCTION SPWERT(N,XWERT,A,B,C,D,X,ABLTG)
C-----------------------------------------------------------------------
C     INPUT:
C
C     N           ANZAHL DER KNOTENPUNKTE
C     XWERT       STELLE AN DER FUNKTIONSWERTE BERECHNET WERDEN
C     A, B, C, D  ARRAYS DER SPLINEKOEFFIZIENTEN (AUS SPLINE)
C     X           ARRAY DER KNOTENPUNKTE
C
C     OUTPUT:
C
C     SPWERT   FUNKTIONSWERT AN DER STELLE XWERT
C     ABLTG(I) WERT DER I-TEN ABLEITUNG BEI XWERT
C-----------------------------------------------------------------------
C
      INTEGER  N
      REAL     XWERT, A(*), B(*), C(*), D(*), X(N), ABLTG(3)
      INTEGER  I, K, M
C
C     SUCHE PASSENDES INTERVALL (BINAERE SUCHE)
C
      I = 1
      K = N
C
   10 M = (I+K) / 2
C
      IF(M.NE.I) THEN
         IF(XWERT.LT.X(M)) THEN
            K = M
         ELSE
            I = M
         ENDIF
         GOTO 10
      ENDIF
C
      XX = XWERT - X(I)
C
      ABLTG(1) = (3.0 * D(I) * XX + 2.0 * C(I)) * XX + B(I)
      ABLTG(2) = 6.0 * D(I) * XX + 2.0 * C(I)
      ABLTG(3) = 6.0 * D(I)
C
      SPWERT = ((D(I)*XX + C(I))*XX + B(I))*XX + A(I)
C
      RETURN
      END
************************************************************************
*DECK BUSSAC
      SUBROUTINE BUSSAC
C***********************************************************************
C calculates the value of the bussac poloidal beta for the q=1 and
C q=2 surface. Result should be identical to HELENA output.
C***********************************************************************
*CALL COMMAX
*CALL COMPIO
*CALL COMPCON
*CALL COMGEM
*CALL COMIOD
*CALL COMEQUI
*CALL COMDIAG
*CALL COMFFT
*CALL COMBND
      INTEGER  INDEX(2*(NCHIMAX-1)),NGES
      REAL     HV(NPNC), FWT(2*(NCHIMAX-1))
      REAL     RAV(NPSIMAX),R2AV(NPSIMAX),R3AV(NPSIMAX)
      REAL     WORK(6*NCHIMAX)
      COMPLEX  ZFK(NCHIMAX,NPSIMAX)
      REAL     ZJ0(NPSIMAX),ZPAR(NPSIMAX),ZJAR(NPSIMAX),
     >         ZAR(NPSIMAX),XL(NPSIMAX),BPBUS(NPSIMAX),
     >         DP0(NPSIMAX),DRBPHI(NPSIMAX),RGSAV(NPSIMAX),
     >         P1(NPSIMAX),P2(NPSIMAX),P3(NPSIMAX),P4(NPSIMAX),
     >         RB1(NPSIMAX),RB2(NPSIMAX),RB3(NPSIMAX),RB4(NPSIMAX),
     >         PARINT(NPSIMAX),ZJINT(NPSIMAX),ARINT(NPSIMAX)
C
      NGES = NPSI*NCHI
C
      DO I=1,NCHI
         INDEX(I) = I
      ENDDO
      IF (IAS.EQ.0) THEN
         DO I=NCHI+1,2*NCHI-2
            INDEX(I) = 2*NCHI-I
         ENDDO
      ENDIF
      IF (IAS.EQ.0) THEN
         N = 2*(NCHI-1)
      ELSE
         N = NCHI
      ENDIF
      DO I=1,N/2
         WORK(2*N+4+I) = COS(FLOAT(I-1)*2.*PI/FLOAT(N))
      ENDDO
C
C-----------------first calculate the poloidal average of R,R**2,R**3
C
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,GEM33,WORK,FWT,ZFK,INDEX,1)
      DO I=1,NPSI
        R2AV(I) = REAL(ZFK(1,I))
      ENDDO   
      DO I=1,NGES
         HV(I) = GEM33(I)**0.5
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      DO I=1,NPSI
        RAV(I) = REAL(ZFK(1,I))
      ENDDO 
      DO I=1,NGES
         HV(I) = GEM33(I)**1.5
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      DO I=1,NPSI
        R3AV(I) = REAL(ZFK(1,I))
      ENDDO 
      DO I=1,NGES
         HV(I) = (GEM33(I)*GEM11(I))**0.5
      ENDDO
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      DO I=1,NPSI
        RGSAV(I) = REAL(ZFK(1,I))
      ENDDO
      DP1 = (P0(NPSI)-P0(NPSI-1))/(CS(NPSI)-CS(NPSI-1)) 
      CALL SPLINE(NPSI,CS,P0,0.,DP1,1,P1,P2,P3,P4)
      DO I=2,NPSI
        DP0(I) = P2(I)/(2*CPSURF*CS(I))
      ENDDO
      DRB1 = (RBPHI(NPSI)-RBPHI(NPSI-1))/(CS(NPSI)-CS(NPSI-1)) 
      CALL SPLINE(NPSI,CS,RBPHI,0.,DRB1,1,RB1,RB2,RB3,RB4)
      DO I=2,NPSI
        DRBPHI(I) = RB2(I)/(2*CPSURF*CS(I))
      ENDDO
      DO I=2,NPSI
        SPS2 = 2.*CPSURF*CS(I)
        PARINT(I)=P0(I)*RAV(I)*SPS2*QS(I)/RBPHI(I)     * 2.*PI
        ZJINT(I)= SPS2*QS(I) * DRBPHI(I)               * 2.*PI
     >          + SPS2*QS(I)/RBPHI(I) *R2AV(I) * DP0(I)* 2.*PI
        ARINT(I)= SPS2*QS(I)/RBPHI(I) * RAV(I)         * 2.*PI
      ENDDO
      PARINT(1) = 0. 
      ARINT(1)  = 0. 
      ZJINT(1)  = 0. 
      DO I=2, NPSI
        XL(I) = 2.*PI*QS(I)/RBPHI(I) * RGSAV(I)
      ENDDO
      CALL QINT(CS,PARINT,NPSI,ZPAR)
      CALL QINT(CS,ZJINT,NPSI,ZJAR)
      CALL QINT(CS,ARINT,NPSI,ZAR)
      DO I=2,NPSI
        BPBUS(I) =-2.*(P0(I)-ZPAR(I)/ZAR(I))/(ZJAR(I)/XL(I))**2
      ENDDO 
C      WRITE(20,3) (I,RAV(I),R2AV(I),RGSAV(I),ARINT(I),I=1,NPSI)
    3 FORMAT(I4,3F10.5,E12.4)
C      WRITE(20,4) (I,XL(I),ZAR(I),ZPAR(I),P0(I),I=1,NPSI)
C      WRITE(20,*)
C      WRITE(20,4) (I,ZJAR(I),BPBUS(I),P0(I),DP0(I),I=1,NPSI)
    4 FORMAT(I3,4e14.6)
      
      IQ1 = 0
      IQ2 = 0
      DO I=1,NPSI
        IF (QS(I).LT.1.) IQ1=I
        IF (QS(I).LT.2.) IQ2=I
      ENDDO
      IF ((IQ1.GT.0).AND.(IQ1.LT.NPSI)) THEN
         XR1 = CS(IQ1) + (1.-QS(IQ1))/(QS(IQ1+1)-QS(IQ1)) 
     >        * (CS(IQ1+1)-CS(IQ1))
         BB1 = BPBUS(IQ1) + (1.-QS(IQ1))/(QS(IQ1+1)-QS(IQ1)) 
     >        * (BPBUS(IQ1+1)-BPBUS(IQ1))
      ENDIF
      IF ((IQ2.GT.0).AND.(IQ2.LT.NPSI)) THEN
         XR2 = CS(IQ2) + (2.-QS(IQ2))/(QS(IQ2+1)-QS(IQ2)) 
     >        * (CS(IQ2+1)-CS(IQ2))
         BB2 = BPBUS(IQ2) + (1.-QS(IQ2))/(QS(IQ2+1)-QS(IQ2)) 
     >        * (BPBUS(IQ2+1)-BPBUS(IQ2))
      ENDIF
      IF (IQ1.LT.NPSI) THEN
        WRITE(20,6) XR1,BB1
      ENDIF
      IF (IQ2.LT.NPSI) THEN
        WRITE(20,7) XR2,BB2
      ENDIF
    6 FORMAT(' BUSSAC BETA AT Q=1 : S=',f6.3,' BETA(B) = ',e12.4)
    7 FORMAT(' BUSSAC BETA AT Q=2 : S=',f6.3,' BETA(B) = ',e12.4)
      END
********************************************************************
*DECK QINT     
      subroutine qint(s,p,N,pint)
C******************************************************************
C integrates a array of non-equidistant points using quadratic 
C interpolation
C******************************************************************
      real s(*),p(*),pint(*)
      
      pint(1) = 0.
      is=1
      ie=2
      i =1
      sumj = 0.
      do j=0,2
        j1=j+i
        j2=mod(j+1,3)+i
        j3=mod(j+2,3)+i
        sumj = sumj + p(j1)/((s(j1)-s(j2))*(s(j1)-s(j3))) *
     >           (   (s(ie)**3-s(is)**3)/3. 
     >              - (s(j2)+s(j3))*(s(ie)**2-s(is)**2)/2.
     >              + s(j2)*s(j3)*(s(ie)-s(is))  )
      enddo
      pint(2)=sumj
      do i=1,N-2 
        sumj=0.
        is=i
        ie=i+2
        do j=0,2
          j1=j+i
          j2=mod(j+1,3)+i
          j3=mod(j+2,3)+i
          sumj = sumj + p(j1)/((s(j1)-s(j2))*(s(j1)-s(j3))) *
     >             (   (s(ie)**3-s(is)**3)/3. 
     >                - (s(j2)+s(j3))*(s(ie)**2-s(is)**2)/2.
     >                + s(j2)*s(j3)*(s(ie)-s(is))  )
        enddo
        pint(i+2)= pint(i)+sumj
      enddo
      return
      end   
************************************************************************
*DECK EQVAC
      SUBROUTINE EQVAC
C-----------------------------------------------------------------------
C     CALCULATES THE GEOMETRIC QUANTITIES OF THE COORDINATE SYSTEM IN
C     THE VACUUM.
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPARV
*CALL COMPIO
*CALL COMDIAG
*CALL COMEQUI
*CALL COMVAC
*CALL COMJET
*CALL COMVGRD
*CALL COMBND
C
      REAL     ZS(4),RPP(NVCHIMX),ZPP(NVCHIMX),
     >         RWP(NVCHIMX),ZWP(NVCHIMX),DUMMY(3)
C
      PI = 2.*ASIN(1.)
      RGEO = RADIUS / ASPI
C
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'EPS      : ',ASPI
      WRITE(NOUT,*) 'RADIUS   : ',RADIUS
      WRITE(NOUT,*) 'RGEO     : ',RGEO
C
C     IMPLEMENTED JET GEOMETRY JUST FOR IAS = 1 !!!                     
C                                                                       
      IF (IVAC.EQ.4) CALL JETGEO                                        
C                                                                       
      DO 5 J=1,NVCHI
         RP(J) = RADIUS * SQRT(VX(J)**2 + VY(J)**2)
         TC(J) = ATAN2(VY(J),VX(J))
         IF (IAS.EQ.0) THEN
            VC(J) = PI * REAL(J-1)/REAL(NVCHI-1)
         ELSE
            IF((J.GT.1).AND.(TC(J).LT.TC(J-1)))
     >           TC(J) = TC(J) + 2.*PI
            VC(J) = 2. * PI * REAL(J-1)/REAL(NVCHI)
         ENDIF
                                                                        
         TCPOS = TC(J)                                                  
         IF (TCPOS.LT.0.) TCPOS = TCPOS + 2.*PI                         
         IF (TCPOS.GE.2.*PI) TCPOS = TCPOS - 2.*PI                      
                                                                        
         IF (IVAC.EQ.1) THEN
            RW(J) = RWALL * RP(J)
         ELSEIF (IVAC.EQ.4) THEN                                        
            RW(J) = SPWERT(NV,TCPOS,RV1,RV2,RV3,RV4,THTV,DUMMY)         
         ELSE
            RW(J) = RADIUS*RBOUND(FW,NFW,TC(J))/RMIN
         ENDIF
    5 CONTINUE
      DO 7 J = 1, NVCHI
         RPP(J) = RP(J) * COS(TC(J))
         ZPP(J) = RP(J) * SIN(TC(J))
         RWP(J) = RW(J) * COS(TC(J))
         ZWP(J) = RW(J) * SIN(TC(J))
 7    CONTINUE
      IF (IAS.EQ.0) THEN
         CALL NFRAME(1,1,1,-0.5,0.5,0.,1.5,
     >        'GEOMETRY',8,'R',1,'Z',1)
      ELSE
         CALL NFRAME(1,1,1,-0.5,0.5,-1.,1.,
     >        'GEOMETRY',8,'R',1,'Z',1)
      ENDIF
      CALL LPLOT(1,1,1,RPP,ZPP,-NVCHI,1,' ',1,' ',1,' ',1)
      CALL LPLOT(1,1,1,RWP,ZWP,-NVCHI,1,' ',1,' ',1,' ',1)
         

C
C ... FOURIER TRANSFORM RW AND RP IN THE ANGLE CHI!! ...
C
      DO 10 J=1,NVCHI
         FRW(J) = RW(J)
         FRP(J) = RP(J)
         FTC(J) = TC(J) - VC(J)
   10 CONTINUE
      IF (IAS.EQ.0) THEN
         DO 20 J=1,NVCHI-2
            FRW(NVCHI+J) =   FRW(NVCHI-J)
            FRP(NVCHI+J) =   FRP(NVCHI-J)
            FTC(NVCHI+J) = - FTC(NVCHI-J)
 20      CONTINUE
C
         CALL RFT2(FRW,2*(NVCHI-1),1)
         CALL RFT2(FRP,2*(NVCHI-1),1)
         CALL RFT2(FTC,2*(NVCHI-1),1)
C
C ... KEEP ONLY COSINE OR SINE PART ...
C
         DO 30 M=1,NVCHI-1
            FRW(M) = FRW(2*(M-1)+1) / REAL(NVCHI-1)
            FRP(M) = FRP(2*(M-1)+1) / REAL(NVCHI-1)
            FTC(M) = FTC(2*(M-1)+2) / REAL(NVCHI-1)
 30      CONTINUE
      ELSE
         CALL RFT2(FRW,NVCHI,1)
         CALL RFT2(FRP,NVCHI,1)
         CALL RFT2(FTC,NVCHI,1)
      ENDIF
C
C ... DERIVATIVES OF RW, RP AND TC ...
C
      DO 50 J=1,NVCHI
         ANGLE = VC(J)
         SUMW = 0.
         SUMP = 0.
         IF (IAS.EQ.0) THEN
            SUMT = 1.
         ELSE
            SUMT = 0.
         ENDIF
         SUMC = 0.
         DSUMW = 0.
         DSUMP = 0.
         DSUMT = 0.
         IF (IAS.EQ.0) THEN
            DO 40 M=2,NVCHI-1
               SUMW = SUMW - FRW(M) * (M-1) * SIN((M-1)*ANGLE)
               SUMP = SUMP - FRP(M) * (M-1) * SIN((M-1)*ANGLE)
               SUMT = SUMT - FTC(M) * (M-1) * COS((M-1)*ANGLE)
 40         CONTINUE
            DRW(J) = SUMW
            DRP(J) = SUMP
            DTC(J) = SUMT
         ELSE
            DO 45 M=2,NVCHI/2
               SUMW = SUMW - FRW(2*M-1) * (M-1) * SIN((M-1)*ANGLE)
     >                     - FRW(2*M)   * (M-1) * COS((M-1)*ANGLE)
               SUMP = SUMP - FRP(2*M-1) * (M-1) * SIN((M-1)*ANGLE)
     >                     - FRP(2*M)   * (M-1) * COS((M-1)*ANGLE)
               SUMT = SUMT - FTC(2*M-1) * (M-1) * SIN((M-1)*ANGLE)
     >                     - FTC(2*M)   * (M-1) * COS((M-1)*ANGLE)
 45         CONTINUE
            DRW(J) = 2. * SUMW / REAL(NVCHI)
            DRP(J) = 2. * SUMP / REAL(NVCHI)
            DTC(J) = 2. * SUMT / REAL(NVCHI) + 1.
         ENDIF
   50 CONTINUE
C
C     WRITE DATA FOR VACUUM MAGNETIC FIELD RECONSTRUCTION:
C
      IF (IBVAC.NE.0) WRITE(NOUTVB,*) NVPSI, NVCHI, ASPI, RADIUS
C
C ... GEOMETRIC QUANTITIES ...
C
      DO 70 I=1,NVPSI
         DO 60 J=1,NVCHI
            INDEX = (I-1)*NVCHI + J
            S = REAL(I-1)/REAL(NVPSI-1)
            CSV(I) = S
             RAD = S * (RW(J) - RP(J)) + RP(J)
            BIGR = RGEO + RAD * COS(TC(J))
            DSDR = 1./(RW(J) - RP(J))
             DCDT = 1./DTC(J)
            DSDC = -DRP(J)/(RW(J)-RP(J)) - (RAD-RP(J))/(RW(J)-RP(J))**2
     >                                                 *(DRW(J)-DRP(J))
            DSDT = DSDC * DCDT
C
            VJ(INDEX) = RAD * BIGR / (DSDR * DCDT)
            VG11(INDEX) = DSDR**2 + (DSDT/RAD)**2
            VG22(INDEX) = (DCDT / RAD)**2
            VG12(INDEX) = (DSDT * DCDT) / RAD**2
            VG33(INDEX) = 1. / BIGR**2
C
C     CALCULATE DATA FOR VACUUM MAGNETIC FIELD RECONSTRUCTION:
C
            IF (IBVAC.NE.0) THEN
               X = RAD*COS(TC(J))/RADIUS
               Y = RAD*SIN(TC(J))/RADIUS
               DSDX = RADIUS*(DSDR*COS(TC(J))-DSDT*SIN(TC(J))/RAD)
               DSDY = RADIUS*(DSDR*SIN(TC(J))+DSDT*COS(TC(J))/RAD)
               DCDX = -RADIUS*SIN(TC(J))*DCDT/RAD
               DCDY = RADIUS*COS(TC(J))*DCDT/RAD
C
C     WRITE DATA:
C
               WRITE(NOUTVB,*) S, VC(J), X, Y
               WRITE(NOUTVB,*) DSDX, DSDY, DCDX, DCDY
            ENDIF
C ......................................................................
C           IF(I.EQ.1) THEN
C              XJAC = BIGR/SQRT(VG11(INDEX)*VG22(INDEX)-VG12(INDEX)**2)
C              WRITE(NOUTV,*) VJ(INDEX),XJAC,VJ(INDEX)-XJAC
C           ENDIF
C ......................................................................
C
   60    CONTINUE
   70 CONTINUE
C
      DO 90 NI = 1, NGV-1
         SL = SVGRID(NI)
         SU = SVGRID(NI+1)
C
C ...... BERECHNUNG DER STUETZSTELLEN ...
C
         ZDIF = SU - SL
         ZA   = .5 * (SU + SL)
         ZB   = .43056815579702629 * ZDIF
         ZC   = .16999052179242813 * ZDIF
         ZS(1) = ZA + ZB
         ZS(2) = ZA - ZB
         ZS(3) = ZA + ZC
         ZS(4) = ZA - ZC
         DO 80 I=1,4
            J = (NI-1)*4+I
            SVGI(J) = ZS(I)
   80    CONTINUE
   90 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK JETGEO                                                            
      SUBROUTINE JETGEO                                                 
C-----------------------------------------------------------------------
C THE DESCRIPTION OF R AND Z COORDINATES OF THE JET VESSEL              
C-----------------------------------------------------------------------
*CALL COMMAX                                                            
*CALL COMPARV                                                           
*CALL COMVAC                                                            
*CALL COMBND                                                            
*CALL COMJET                                                            
*CALL COMEQUI                                                           
                                                                        
      REAL RZV2(2,37),RV(73)
                                                                        
      DATA   ((RZV2(J,I),J=1,2),I=1,37) /                               
     >          4.350,    0.000,                                        
     >          4.346,    0.118,                                        
     >          4.336,    0.236,                                        
     >          4.319,    0.353,                                        
     >          4.294,    0.471,                                        
     >          4.263,    0.589,                                        
     >          4.224,    0.707,                                        
     >          4.177,    0.824,                                        
     >          4.123,    0.942,                                        
     >          4.060,    1.060,                                        
     >          3.988,    1.178,                                        
     >          3.907,    1.295,                                        
     >          3.815,    1.412,                                        
     >          3.712,    1.527,                                        
     >          3.597,    1.640,                                        
     >          3.469,    1.750,                                        
     >          3.327,    1.853,                                        
     >          3.170,    1.945,                                        
     >          3.000,    2.022,                                        
     >          2.819,    2.074,                                        
     >          2.631,    2.092,                                        
     >          2.446,    2.067,                                        
     >          2.275,    1.993,                                        
     >          2.127,    1.873,                                        
     >          2.006,    1.721,                                        
     >          1.913,    1.552,                                        
     >          1.843,    1.379,                                        
     >          1.791,    1.210,                                        
     >          1.751,    1.048,                                        
     >          1.721,    0.895,                                        
     >          1.699,    0.751,                                        
     >          1.682,    0.615,                                        
     >          1.670,    0.484,                                        
     >          1.661,    0.359,                                        
     >          1.655,    0.237,                                        
     >          1.651,    0.118,                                        
     >          1.650,    0.000 /                                       
      DATA   NV2 / 37 /                                          
                                                                        
      PI = 2.*ASIN(1.)                                                  
                                                                        
      DO 10 J=1,NV2                                                     
         RZV(1,J) = RZV2(1,J)                                           
         RZV(2,J) = RZV2(2,J)                                           
         RZV(1,2*NV2-J) = RZV2(1,J)                                     
         RZV(2,2*NV2-J) = -RZV2(2,J)                                    
   10 CONTINUE                                                          

      NV = 2*NV2-1                                                      
                                                                        
      RCNTR = RMIN/ASPI                                                 
      RMAG  = RMIN/RADIUS       
      RMIN = 0.90     
      ZCNTR = 0.32
      
      WRITE(20,11) RCNTR,ZCNTR
   11 FORMAT(' PLASMA CENTRE, R, Z : ',2f9.3)                                     
                                                                        
      DO 100 J=1,NV                                                     
        THTV(J) = ATAN2(RZV(2,J)-ZCNTR,RZV(1,J)-RCNTR)                        
        IF (J.GT.1) THEN                                                
           IF (THTV(J).LT.THTV(J-1)) THTV(J) = THTV(J) + 2.*PI          
        ENDIF                                                           
        RV(J) = SQRT((RZV(1,J)-RCNTR)**2
     >                +(RZV(2,J)-ZCNTR)**2) / RMAG            
  100 CONTINUE                                                          
                                                                        
      CALL SPLINE(NV,THTV,RV,0.,0.,2,RV1,RV2,RV3,RV4)                   
                                                                        
      RETURN                                                            
      END                                                               
************************************************************************
*DECK RBOUND
      REAL FUNCTION RBOUND(FR,NFR,THETA)
C-----------------------------------------------------------------------
C     CALCULATE RADIUS AS A FUNCTION OF THETA
C-----------------------------------------------------------------------

*CALL COMEQUI
      REAL FR(*), THETA
      INTEGER NFR

      R = FR(1)
      IF (IAS.EQ.0) THEN
         DO 10 M=2, NFR
            R = R + FR(M)*COS(FLOAT(M-1)*THETA)
 10      CONTINUE
      ELSE
         DO 20 M=2, NFR/2
            R = R + FR(M*2-1)*COS(FLOAT(M-1)*THETA)
     >            + FR(M*2)*SIN(FLOAT(M-1)*THETA)
 20      CONTINUE
      ENDIF
      RBOUND = R
      END
************************************************************************
*DECK VFKEQ
      SUBROUTINE VFKEQ
C-----------------------------------------------------------------------
C     SPLINE COEFFICIENTS OF THE  FOURIER COEFFICIENTS FOR THE VACUUM
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPIO
*CALL COMPCON
*CALL COMPARV
*CALL COMEQUI
*CALL COMVAC
*CALL COMVFT
*CALL COMVGRD
C
      INTEGER  INDEX(2*(NVCHIMX-1))
      REAL     HV(NVPNVC), FWT(2*(NVCHIMX-1))
      REAL     H1(NVPSIMX), H2(NVPSIMX), H3(NVPSIMX), H4(NVPSIMX)
      REAL     DUMMY(3)
      COMPLEX  ZFK(NVCHIMX,NVPSIMX)
*IF CRAY
      COMPLEX  WORK(3*NVCHIMX)
*ELSE
      REAL     WORK(6*NVCHIMX)
*ENDIF
C
      INTEGER  L, NVGES
      REAL     ABLTG(3)
C
      NVGES = NVPSI*NVCHI
      NVP1  = NVPSI+1
      NV2P1 = 2*NVPSI+1
      NV3P1 = 3*NVPSI+1
C
      NPR = 1
C
      DO 10 I=1,NVCHI
         INDEX(I) = I
   10 CONTINUE
      IF (IAS.EQ.0) THEN
         DO 20 I=NVCHI+1,2*NVCHI-2
            INDEX(I) = 2*NVCHI-I
 20      CONTINUE
      ENDIF
C
C     INITIALIZATION OF THE SINE AND COSINE TABLES
C     --------------------------------------------
C
      IF (IAS.EQ.0) THEN
         N = 2*(NVCHI-1)
      ELSE
         N = NVCHI
      ENDIF
*IF CRAY
      CALL RCFFT2(1,IDUMMY,N,DUMMY,WORK,DUMMY)
*ELSE
      DO 30 I=1,N/2
         WORK(2*N+4+I) = COS(FLOAT(I-1)*2.*PI/FLOAT(N))
   30 CONTINUE
*ENDIF
C
C     FOURIER ANALYSIS AND SPLINE FOR EVERY FOURIER COEFFICIENT
C     ---------------------------------------------------------
C
C        JAC * G11  -->  VR11, VI11
C
      DO 50 I=1,NVGES
         HV(I)= VJ(I) * VG11(I)
   50 CONTINUE
      CALL FFTRAN(LVMAX,NVCHIMX,NVPSI,NVCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NVCHI,NVPSI,NVCHIMX,NVP4,LVANZ,CSV,HV,ZFK,VR11,VI11)
C
C        JAC * G12 --->  VR12,VI12
C
      DO 70 I=1,NVGES
         HV(I) = VJ(I) * VG12(I)
   70 CONTINUE
      CALL FFTRAN(LVMAX,NVCHIMX,NVPSI,NVCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NVCHI,NVPSI,NVCHIMX,NVP4,LVANZ,CSV,HV,ZFK,VR12,VI12)
C
C        JAC * G22 -->  VR22,VI22
C
      DO 90 I=1,NVGES
         HV(I) = VJ(I) * VG22(I)
   90 CONTINUE
      CALL FFTRAN(LVMAX,NVCHIMX,NVPSI,NVCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NVCHI,NVPSI,NVCHIMX,NVP4,LVANZ,CSV,HV,ZFK,VR22,VI22)
C
C        JAC * G33 --->  VR33,VI33
C
      DO 110 I=1,NVGES
         HV(I) = VJ(I) * VG33(I)
  110 CONTINUE
      CALL FFTRAN(LVMAX,NVCHIMX,NVPSI,NVCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NVCHI,NVPSI,NVCHIMX,NVP4,LVANZ,CSV,HV,ZFK,VR33,VI33)
      RETURN
C
      END

************************************************************************
*DECK VACUUM
      SUBROUTINE VACUUM
************************************************************************
************************************************************************
**                                                                    **
**    BEGIN : MODULE VACUUM  (VACUUM RESPONSE TO UNIT PERTURBATION)   **
**    ---------------------                                           **
**                                                                    **
**    STRUCTURE :                                                     **
**                 VACUUM                                             **
**                   CONVMAT                                          **
**                     CUBFCT                                         **
**                     DCUBF                                           **
**                     SPWERT                                         **
**                     FKUBL                                          **
**                   CGBFA                                            **
**                   CGBSL                                            **
**                                                                    **
************************************************************************
C
*CALL COMMAX
*CALL COMPARV
*CALL COMPIO
*CALL COMDIAG
*CALL COMWEL
*CALL COMVGRD
*CALL COMGRID
*CALL COMVRSP
*CALL COMVMAT
*CALL COMBND
C
      COMPLEX  ZMA(NZMAV*NZMAV)
      COMPLEX  ZB1, ZB2, ZB3
      INTEGER  IPVT(NBGV*NGVMAX)
C
      WRITE(NOUT,1)

      NDIMV = NGV * NBGV
C
      VFOUR1 = VFOUR(1)
      DO 50 M=1,LVMAX
C         VFOUR(M) = REAL(- LVMAX/2 + M - 1)
          VFOUR(M) = VFOUR1 + FLOAT(M) - 1.
   50 CONTINUE
C      
      DO M=1,MANZ
        RFOUR(M) = MSTART(NG) + FLOAT(M-1)
      ENDDO
C
C     WRITE DATA FOR VACUUM MAGNETIC FIELD RECONSTRUCTION:
C
      IF (IBVAC.NE.0) THEN
         WRITE(NOUTVB,*) NGV, MVANZ
         DO 51, M = 1, MVANZ
            WRITE(NOUTVB,*) VFOUR(M)
 51      CONTINUE
         WRITE(NOUTVB,*) MANZ
      ENDIF
C
C
      DO 70 I=1,LDAV
         DO 60 J=1,NBGV*NGVMAX
            VMAT(I,J) = (0.,0.)
   60    CONTINUE
   70 CONTINUE
      DO 120 NI=1,NGV-1
         CALL CONVMAT(NI,NZMAV,ZMA)
         NB = (NI-1)*NZMAV/2
         DO 90 L=1,NZMAV
            JZ = NB + L - 1
            IZ = 2*NZMAV
            DO 80 K=L,NZMAV
               JZ = JZ + 1
               IZ = IZ - 1
               VMAT(IZ,JZ) = VMAT(IZ,JZ) + ZMA(L+(K-1)*NZMAV)
   80       CONTINUE
   90    CONTINUE
         DO 110 L=2,NZMAV
            IZ = 2*NZMAV + L - 1
            JZ = NB
            DO 100 K=1,L-1
               IZ = IZ - 1
               JZ = JZ + 1
               VMAT(IZ,JZ) = VMAT(IZ,JZ) + ZMA(L+(K-1)*NZMAV)
  100       CONTINUE
  110    CONTINUE
  120 CONTINUE
C
      CALL CGBFA(VMAT,LDAV,NDIMV,MLV,MUV,IPVT,INFO)
      IF(INFO.NE.0) THEN
         WRITE(NOUT,*) 'ERROR IN VACUUM --> CGBFA INFO : ',INFO
         STOP
      ENDIF
C
      DO 170 MF=1,MANZ
C
         MOFF = INT(RFOUR(MF) - VFOUR(1)) - MF + 1
         ZMPERT = RFOUR(MF)
C
         DO 130 I=1,NBGV * NGVMAX
            B(I) = (0.,0.)
  130    CONTINUE
C
C ...... FILL RIGHT HAND SIDE ...
C
         B(2*(MF+MOFF-1)+1) = (1.,0.)
C
         CALL CGBSL(VMAT,LDAV,NDIMV,MLV,MUV,IPVT,B,0,Q)
C
         DO 140 MRESP = 1,MANZ
            ZMFRES = RFOUR(MRESP)
            ZB1 = - B(2*(MRESP+MOFF-1)+2)
            ZB2 = - (0.,1.) * ZMFRES * B(2*(MRESP+MOFF-1)+1)
            ZB3 = - (0.,1.) * ZNKWEL * B(2*(MRESP+MOFF-1)+1)
            B3B1(MF,MRESP) = ZB3
            WRITE(NOUT,131) INT(ZMPERT),INT(ZMFRES),ZB1,ZB2,ZB3
  140    CONTINUE
C
C     WRITE DATA FOR VACUUM MAGNETIC FIELD RECONSTRUCTION:
C
         IF (IBVAC.NE.0) THEN
            DO 160 MRESP = 1,MVANZ
               DO 150 I=1,NGV
                  WRITE(NOUTVB,*) SVGRID(I),
     >                 REAL(B(2*(MRESP-1)+2*(I-1)*MVANZ+1)),
     >                 AIMAG(B(2*(MRESP-1)+2*(I-1)*MVANZ+1))
 150           CONTINUE
 160        CONTINUE
            DO 165 MRESP = 1,MVANZ
               DO 155 I=1,NGV
                  WRITE(NOUTVB,*) SVGRID(I),
     >                 REAL(B(2*(MRESP-1)+2*(I-1)*MVANZ+2)),
     >                 AIMAG(B(2*(MRESP-1)+2*(I-1)*MVANZ+2))
 155           CONTINUE
 165        CONTINUE
         ENDIF
  170 CONTINUE
      RETURN
C
C  21 FORMAT(1X,2I3,2E12.4)
  131 FORMAT(1X,2i3,6E12.4)
    1 FORMAT(1X,/,' VACUUM PERTURBATION MATRIX : ',/)
      END
************************************************************************
*DECK CONVMAT
      SUBROUTINE CONVMAT(NI,NZMA,ZMA)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPARV
*CALL COMWEL
*CALL COMGEW
*CALL COMVAC
*CALL COMVFT
*CALL COMVGRD
C
      INTEGER  NI, NBG, NZMA,  MS, MZ, I, K, INDCC(1)
      REAL     SL, SU, ZDIF, ZSR, MSNQ, MZNQ, FKDUMMY,
     >         ZBIG, HC(4), DHC(4), DUMMY(3) 
      COMPLEX  G11, G12, G22, G33, ZMA(*), FACT(4,1)
C
      DATA INDCC / 1 /
C
C ... NULLSCHREIBEN DER MATRIX ZMA ...
C
      DO 10  I = 1, NZMA*NZMA
         ZMA(I) = (0.0,0.0)
   10 CONTINUE
      ZBIG = 1.E+22
      NBG = NZMA/2
C
C ... INTEGRATION IM INTERVALL  SVGRID(N) - SVGRID(N+1) ...
C
      SL = SVGRID(NI)
      SU = SVGRID(NI+1)
C
C ... BERECHNUNG DER STUETZSTELLEN ...
C
      ZDIF = (SU - SL)
C
      DO 200  I = 1 , 4
C
         ZSR     = SVGI((NI-1)*4+I)
         FKDUMMY = 1.0
C
      CALL CUBFCT(ZSR,SL,SU,HC)
      CALL DCUBF (ZSR,SL,SU,DHC)
C
      DO 200   KF = 1 , MVANZ
C
        K = (KF-1) + 1
C
        G11 = CMPLX(SPWERT(NVPSI,ZSR,VR11(1,K),VR11(NVP1,K),
     >                     VR11(NV2P1,K),VR11(NV3P1,K),CSV,DUMMY),
     >              SPWERT(NVPSI,ZSR,VI11(1,K),VI11(NVP1,K),
     >                     VI11(NV2P1,K),VI11(NV3P1,K),CSV,DUMMY))
C
        G12 = - CMPLX(SPWERT(NVPSI,ZSR,VR12(1,K),VR12(NVP1,K),
     >                     VR12(NV2P1,K),VR12(NV3P1,K),CSV,DUMMY),
     >              SPWERT(NVPSI,ZSR,VI12(1,K),VI12(NVP1,K),
     >                     VI12(NV2P1,K),VI12(NV3P1,K),CSV,DUMMY))
C
        G22 = CMPLX(SPWERT(NVPSI,ZSR,VR22(1,K),VR22(NVP1,K),
     >                      VR22(NV2P1,K),VR22(NV3P1,K),CSV,DUMMY),
     >                 SPWERT(NVPSI,ZSR,VI22(1,K),VI22(NVP1,K),
     >                      VI22(NV2P1,K),VI22(NV3P1,K),CSV,DUMMY))
C
        G33 = CMPLX(SPWERT(NVPSI,ZSR,VR33(1,K),VR33(NVP1,K),
     >                      VR33(NV2P1,K),VR33(NV3P1,K),CSV,DUMMY),
     >                 SPWERT(NVPSI,ZSR,VI33(1,K),VI33(NVP1,K),
     >                      VI33(NV2P1,K),VI33(NV3P1,K),CSV,DUMMY))
C
C
      DO 100  MS = 1 , MVANZ - KF + 1
C
      MZ = MS + KF - 1
C
C
      SMZ = VFOUR(MZ)
      SMS = VFOUR(MS)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(CUB)   FUER DIE GLEICHUNG  P(1,1)
C     ----------------------
C
         FACT(1:4,1) =   SMS*SMZ * G22 + ZNKWEL**2 * G33
C
      CALL FKUBL(MZ,MS,MVANZ,1,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),HC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H'(CUB)   FUER DIE GLEICHUNG  P(1,1)
C     ------------------------
C
         FACT(1:4,1) =   G11
C
      CALL FKUBL(MZ,MS,MVANZ,1,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),DHC,DHC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H(CUB)   FUER DIE GLEICHUNG  P(1,1)
C     -----------------------
C
         FACT(1:4,1) = + (0.,1.) * SMS * G12
C
      CALL FKUBL(MZ,MS,MVANZ,1,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),DHC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H'(CUB)   FUER DIE GLEICHUNG  P(1,1)
C     -----------------------
C
         FACT(1:4,1) = - (0.,1.) * SMZ * G12
C
      CALL FKUBL(MZ,MS,MVANZ,1,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),HC,DHC)
C
C
      IF(MS.EQ.MZ) GOTO 100
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(CUB)   FUER DIE GLEICHUNGEN  P(1,1)
C     ----------------------
C
         FACT(1:4,1) =   SMS*SMZ * CONJG(G22) + ZNKWEL**2 * CONJG(G33)
C
      CALL FKUBL(MS,MZ,MVANZ,1,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),HC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H'(CUB)   FUER DIE GLEICHUNG   P(1,10
C     ------------------------
C
         FACT(1:4,1) = CONJG(G11)
C
      CALL FKUBL(MS,MZ,MVANZ,1,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),DHC,DHC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H(CUB)   FUER DIE GLEICHUNG  P(1,1)
C     -----------------------
C
         FACT(1:4,1) = + (0.,1.) * SMZ * CONJG(G12)
C
      CALL FKUBL(MS,MZ,MVANZ,1,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),DHC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H'(CUB)   FUER DIE GLEICHUNG  P(1,1)
C     -----------------------
C
         FACT(1:4,1) = - (0.,1.) * SMS * CONJG(G12)
C
      CALL FKUBL(MS,MZ,MVANZ,1,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),HC,DHC)
C
C
  100 CONTINUE
C
         FKDUMMY = 0.0
C
  200 CONTINUE
C
      DO 210  I = 1,NZMA* NZMA
         ZMA(I) = ZDIF * ZMA(I)
  210 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK MAT1
      SUBROUTINE MAT1
C
************************************************************************
************************************************************************
**                                                                    **
**    BEGIN : MODULE MAT  (COMPUTATION OF MATRICES AMAT AND BMAT)     **
**    ------------------                                              **
**                                                                    **
**    STRUCTURE :                                                     **
**                                                                    **
**    MAT1         MAT2         MAT3         MAT4         MAT5        **
**      CONBMAT      (RANSET)     (RANSET)     (RANSET)     (CXSCAL)  **
**        CUBFCT     STVAL        STVAL        (STVAL)      CONBMAT   **
**        QUAFCT     CONAMAT      CONAMAT      CONAMAT        <--     **
**        SPWERT       <--          <--          <--        (SGSCAL)  **
**        FKUBL                   (CXCOPY)     CONBMAT      CONAMAT   **
**      CONAMAT                   CONBMAT        <--          <--     **
**        CUBFCT                    <--                               **
**        QUAFCT                  (CXDOTU)                            **
**        DCUBF                   CGESLP                              **
**        DQUAF                   CGEFAP                              **
**        SPWERT                     ICMAXP                           **
**        FKUBL                                                       **
**        ADDBOUND                                                     **
**          FBOUND                                                    **
**                                                                    **
************************************************************************
************************************************************************
C
C-----------------------------------------------------------------------
C     COMPUTATION MATRICES FOR QR
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPAR
*CALL CORE1
*CALL COMGRID
C
      DO 10 J=1,NDIM1
          DO 10 I=1,NDIM1
             AMAT(I,J) = (0.0,0.0)
             BMAT(I,J) = (0.0,0.0)
   10 CONTINUE
C
      CALL CONBMAT(1,NZMA,ZMA)
C
      DO 20 J=1,NZMA
          DO 20 I=1,NZMA
             BMAT(I,J) = ZMA(I,J)
   20 CONTINUE
C
      CALL CONAMAT(1,NZMA,ZMA)
C
      DO 30 J=1,NZMA
         DO 30 I=1,NZMA
            AMAT(I,J) = ZMA(I,J)
   30 CONTINUE
C
      DO 60 NI=2,NGINT
         CALL CONBMAT(NI,NZMA,ZMA)
         IA = (NI-1)*NBG
C
         DO 40 J=1,NZMA
            DO 40 I=1,NZMA
               BMAT(IA+I,IA+J) = BMAT(IA+I,IA+J)+ZMA(I,J)
   40    CONTINUE
C
         CALL CONAMAT(NI,NZMA,ZMA)
C
         DO 50 J=1,NZMA
            DO 50 I=1,NZMA
               AMAT(IA+I,IA+J) = AMAT(IA+I,IA+J) + ZMA(I,J)
   50    CONTINUE
   60 CONTINUE
C
      RETURN
      END
C***********************************************************************
*DECK MAT2
      SUBROUTINE MAT2
C-----------------------------------------------------------------------
C     COMPUTATION MATRICES FOR INVERSE VECTOR ITERATION (IN-CORE)
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPAR
*CALL COMPAR2
*CALL CORE2
*CALL COMGRID
*CALL COMDIM
*CALL ISEED
C
      COMPLEX  CNUL
C
      DATA CNUL /(0.0,0.0)/
C
*IF CRAY
      CALL RANSET(3141593)
*ENDIF
      DO 10 II=1,NDIM2
         X0(II) = (0.0,0.0)
         X1(II) = (0.0,0.0)
         Y0(II) = (0.0,0.0)
         Y1(II) = (0.0,0.0)
   10 CONTINUE
      CALL STVAL(NDIM,X0)
      CALL STVAL(NDIM,Y0)
C
      DO 20 I=1,LDA
      DO 20 J=1,NDIM
         AMAT(I,J) = CNUL
   20 CONTINUE
C
      DO 50 NI=1,NGINT
         CALL CONAMAT(NI,NZMA,ZMA)
         NB = (NI-1)*NZMA/2
         DO 30 L=1,NZMA
            JZ = NB+L-1
            IZ = 2*NZMA
            DO 30 K=L,NZMA
               JZ = JZ+1
               IZ = IZ-1
               AMAT(IZ,JZ) = AMAT(IZ,JZ)+ZMA(L,K)
   30    CONTINUE
         DO 40 L=2,NZMA
            IZ = 2*NZMA+L-1
            JZ = NB
            DO 40 K=1,L-1
               IZ = IZ-1
               JZ = JZ+1
               AMAT(IZ,JZ) = AMAT(IZ,JZ)+ZMA(L,K)
   40    CONTINUE
   50 CONTINUE
C
      RETURN
      END
C***********************************************************************
*DECK MAT3
      SUBROUTINE MAT3
C-----------------------------------------------------------------------
C     COMPUTATION MATRICES FOR INVERSE VECTOR ITERATION (OUT-OF-CORE)
C     VERS: 11.9.92 (CRAY-YMP)
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPAR
*CALL COMPAR3
*CALL COMPIO
*CALL CORE3
*CALL COMINT
*CALL COMIT
*CALL COMGRID
C
      COMPLEX  CXDOTU, SUM, ZZ
C
      REWIND ND3
      REWIND ND4
      REWIND ND6
C
      NBG1 = NBG+1
      NBG2 = 2*NBG+1
      NREC = (NG+NCV-1)/NCV
C
*IF CRAY
      CALL RANSET(3141593)
*ENDIF
C
      DO 10 I=1,NG
      DO 10 K=1,NBG
         EV(K,I,1) = (0.0,0.0)
         EV(K,I,2) = (0.0,0.0)
   10 CONTINUE
C
      CALL STVAL(NBG*NG,EV(1,1,1))
      CALL STVAL(NBG*NG,EV(1,1,2))
C
C
      DO 20 J=1,NB3
      DO 20 I=1,NBG
         BUFF(I,J) = (0.0,0.0)
   20 CONTINUE
C
      DO 30 I=1,NBG
         HVX(I,1) = (0.0,0.0)
         HVX(I,2) = (0.0,0.0)
   30 CONTINUE
C
      IZAS = 0
      LWRIT = 0
      CPLG = 1
      IND3 = 1
C
C ... SCHLEIFE UEBER  N INTERVALLE ...
C
      DO 200 NI=1,NG
C     --------------
      JZAS = IZAS
      IND3A = IND3
      IZAS = MOD(NI-1,NCV)+1
      IF(IZAS.EQ.1) THEN
         CPLGA = CPLG
         CPLG = 3-CPLG
         IND3 = CPLGA
         IF(NI.GT.NCV) THEN
            LWRIT = LWRIT+1
*IF KUL
            IF(LWRIT.NE.1) THEN
               WAIT(ND3,ID=LWRIT-1,COND=I1)
               IF(I1.NE.1) STOP 'ND3/MAT3'
               WAIT(ND4,ID=LWRIT-1,COND=I1)
               IF(I1.NE.1) STOP 'ND4/MAT3'
               WAIT(ND6,ID=LWRIT-1,COND=I1)
               IF(I1.NE.1) STOP 'ND6/MAT3'
            ENDIF
            WRITE(ND3,ID=LWRIT) APR(1,1,1,CPLG)...APR(NBG,NB3,NCV,CPLG)
            WRITE(ND4,ID=LWRIT) X(1,1,1,CPLG)...X(NBG,NCV,2,CPLG)
            WRITE(ND6,ID=LWRIT) IPVT(1,1,CPLG)...IPVT(NBG,NCV,CPLG)
*ELSEIF IBM
            WRITE(ND3) (((APR(II,IJ,IK,CPLG),
     >                 II=1,NBG),IJ=1,NB3),IK=1,NCV)
            WRITE(ND4) (((X(II,IJ,IK,CPLG),II=1,NBG),IJ=1,NCV),IK=1,2)
            WRITE(ND6) ((IPVT(II,IK,CPLG),II=1,NBG),IK=1,NCV)
*ENDIF
*IF CRAY
            IF(LWRIT.NE.1) THEN
               IF(UNIT(ND3).GE.0.0) STOP 'ND3/MAT3'
               IF(UNIT(ND4).GE.0.0) STOP 'ND4/MAT3'
               IF(UNIT(ND6).GE.0.0) STOP 'ND6/MAT3'
            ENDIF
            BUFFER OUT(ND3,0) (APR(1,1,1,CPLG),APR(NBG,NB3,NCV,CPLG))
            BUFFER OUT(ND4,0) (X(1,1,1,CPLG),X(NBG,NCV,2,CPLG))
             BUFFER OUT(ND6,0) (IPVT(1,1,CPLG),IPVT(NBG,NCV,CPLG))
*ENDIF
         ENDIF
      ENDIF
C
      IF(NI.LT.NG) THEN
         CALL CONAMAT(NI,NZMA,ZMA)
C
         DO 40 L=1,NZMA
            M = NBG + L
            DO 40 K=1,NBG
               BUFF(K,M) = BUFF(K,M) + ZMA(K,L)
   40    CONTINUE
      ENDIF
C
      CALL CXCOPY(NBG*NB3,BUFF(1,1),1,APR(1,1,IZAS,CPLGA),1)
      CALL CXCOPY(NBG,HVX(1,1),1,X(1,IZAS,1,CPLGA),1)
      CALL CXCOPY(NBG,HVX(1,2),1,X(1,IZAS,2,CPLGA),1)
      IF(NI.EQ.NG) GOTO 110
C
      DO 50 J=1,NB3
      DO 50 I=1,NBG
         BUFF(I,J) = (0.0,0.0)
   50 CONTINUE
      DO 60 L=1,NZMA
         CALL CXCOPY(NBG,ZMA(NBG+1,L),1,BUFF(1,L),1)
   60 CONTINUE
C
C ... A-EWSHIFT*B  UND B*X ...
C
C
      CALL CONBMAT(NI,NZMA,ZMA)
C
      DO 70 J=NBG+1,NB3
         M = J-NBG
         DO 70 I=1,NBG
            APR(I,J,IZAS,CPLGA) = APR(I,J,IZAS,CPLGA)-EWSHIFT*ZMA(I,M)
   70 CONTINUE
C
      DO 80 J=1,NZMA
      DO 80 I=1,NBG
         M = I+NBG
         BUFF(I,J) = BUFF(I,J)-EWSHIFT*ZMA(M,J)
   80 CONTINUE
C
      DO 90 K=1,NBG
         X(K,IZAS,1,CPLGA) = X(K,IZAS,1,CPLGA)
     >                      + CXDOTU(NZMA,ZMA(K,1),NZMA,EV(1,NI,1),1)
         HVX(K,1) = CXDOTU(NZMA,ZMA(NBG+K,1),NZMA,EV(1,NI,1),1)
         X(K,IZAS,2,CPLGA) = X(K,IZAS,2,CPLGA)
     >                      + CXDOTU(NZMA,ZMA(K,1),NZMA,EV(1,NI,2),1)
         HVX(K,2) = CXDOTU(NZMA,ZMA(NBG+K,1),NZMA,EV(1,NI,2),1)
   90 CONTINUE
C
C
C ... A' = L*U ...
C ... L-U-ZERLEGUNG VON APR ...
C
  100 IF(NI.EQ.1) GOTO 150
C
  110 CALL CGESLP(APR(1,NBG1,JZAS,IND3A),NBG,NBG,APR(1,1,IZAS,IND3),
     >            IPVT(1,JZAS,IND3A),HVX2,1)
C
      DO 140 K2=1,NBG
         K2A = 2*NBG+K2
         DO 120 KUS=1,NBG
            HVX2(KUS,1) = APR(IPVT(KUS,JZAS,IND3A),K2A,JZAS,IND3A)
  120    CONTINUE
         DO 130 K1=1,NBG
            SUM = CXDOTU(NBG,APR(K1,1,IZAS,IND3),NBG,HVX2(1,1),1)
            ZZ = APR(K1,NBG+K2,IZAS,IND3)-SUM
            APR(K1,NBG+K2,IZAS,IND3) = ZZ
  130    CONTINUE
  140 CONTINUE
C
  150 CALL CGEFAP(APR(1,NBG1,IZAS,IND3),NBG,NBG,IPVT(1,IZAS,IND3),IER)
      IF(NI.EQ.NG) GOTO 200
      CALL CGESLP(APR(1,NBG1,IZAS,IND3),NBG,NBG,APR(1,NBG2,IZAS,IND3),
     >            IPVT(1,IZAS,IND3),HVX2,0)
C
C
  200 CONTINUE
C     --------
      LWRIT = LWRIT+1
*IF KUL
      IF(LWRIT.GT.1)
         WAIT(ND3,ID=LWRIT-1,COND=I1)
         IF(I1.NE.1) STOP 'ND3/MAT3'
         WAIT(ND4,ID=LWRIT-1,COND=I1)
         IF(I1.NE.1) STOP 'ND4/MAT3'
         WAIT(ND6,ID=LWRIT-1,COND=I1)
         IF(I1.NE.1) STOP 'ND6/MAT3'
      ENDIF
      WRITE(ND3,ID=LWRIT) APR(1,1,1,CPLGA)...APR(NBG,NB3,NCV,CPLGA)
      WRITE(ND4,ID=LWRIT) X(1,1,1,CPLGA)...X(NBG,NCV,2,CPLGA)
      WRITE(ND6,ID=LWRIT) IPVT(1,1,CPLGA)...IPVT(NBG,NCV,CPLGA)
*ELSEIF IBM
      WRITE(ND3) (((APR(II,IJ,IK,CPLGA),II=1,NBG),IJ=1,NB3),IK=1,NCV)
      WRITE(ND4) (((X(II,IJ,IK,CPLGA),II=1,NBG),IJ=1,NCV),IK=1,2)
      WRITE(ND6) ((IPVT(II,IK,CPLGA),II=1,NBG),IK=1,NCV)
*ENDIF
*IF CRAY
      IF(LWRIT.GT.1) THEN
         IF(UNIT(ND3).GE.0.0) STOP 'ND3/MAT3'
         IF(UNIT(ND4).GE.0.0) STOP 'ND4/MAT3'
         IF(UNIT(ND6).GE.0.0) STOP 'ND6/MAT3'
      ENDIF
      BUFFER OUT(ND3,0) (APR(1,1,1,CPLGA),APR(NBG,NB3,NCV,CPLGA))
      BUFFER OUT(ND4,0) (X(1,1,1,CPLGA),X(NBG,NCV,2,CPLGA))
      BUFFER OUT(ND6,0) (IPVT(1,1,CPLGA),IPVT(NBG,NCV,CPLGA))
*ENDIF
      RETURN
      END
C***********************************************************************
*DECK MAT4
      SUBROUTINE MAT4
C-----------------------------------------------------------------------
C     COMPUTATION MATRICES FOR INVERSE VECTOR ITERATION
C     (IN-CORE VERSION OF THE OUT-OF-CORE SOLVER)
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPAR
*CALL COMPAR4
*CALL CORE4
*CALL COMIT
*CALL COMDIAG
*CALL COMGRID
*CALL ISEED
C
      COMPLEX   CXDOTU
C
*IF CRAY
      CALL RANSET(3141593)
*ENDIF
C
      DO 10 I = 1 , NCVIC
      DO 10 K = 1 , NBG
         EV(K,I,1) = (0.0,0.0)
         EV(K,I,2) = (0.0,0.0)
   10 CONTINUE
C
      CALL STVAL(NBG*NG,EV(1,1,1))
      CALL STVAL(NBG*NG,EV(1,1,2))
C
      DO 20 NI = 1 , NCVIC
      DO 20 J  = 1 , NB3
      DO 20 I  = 1 , NBG
         APR(I,J,NI) = (0.0,0.0)
   20 CONTINUE
C
C ... SCHLEIFE UEBER  N INTERVALLE ...
C
      DO 100  NI = 1 , NGINT
C
         CALL CONAMAT(NI,NZMA,ZMA)
C
         DO 30  L = 1 , NZMA
            M = NBG + L
            DO 30  K = 1 , NBG
               APR(K,M,NI) = APR(K,M,NI) + ZMA(K,L)
   30    CONTINUE
C
         NIP1 = NI + 1
C
         DO 40  L = 1     , NZMA
         DO 40  K = NBG+1 , NZMA
            M = K - NBG
            APR(M,L,NIP1) = APR(M,L,NIP1) + ZMA(K,L)
   40    CONTINUE
  100 CONTINUE
C
C ... A-EWSHIFT*B  UND B*X ...
C
      NBG2 = 2*NBG
C
      DO 110 K = 1 , NBG
         X(K,1,1) = (0.0,0.0)
         X(K,1,2) = (0.0,0.0)
  110 CONTINUE
C
      CALL CONBMAT(1,NZMA,ZMA)
C
      DO 120 J = NBG+1,NB3
         M = J-NBG
         DO 120 I = 1, NBG
            APR(I,J,1) = APR(I,J,1) - EWSHIFT * ZMA(I,M)
  120 CONTINUE
C
      DO 200  NI = 2 , NG
C
         DO 130 K = 1 , NBG
            X(K,NI-1,1) = X(K,NI-1,1)
     >                    +CXDOTU(NBG2,ZMA(K,1),NZMA,EV(1,NI-1,1),1)
            X(K,NI,1)   =  CXDOTU(NBG2,ZMA(NBG+K,1),NZMA,EV(1,NI-1,1),1)
            X(K,NI-1,2) = X(K,NI-1,2)
     >                    +CXDOTU(NBG2,ZMA(K,1),NZMA,EV(1,NI-1,2),1)
            X(K,NI,2)   =  CXDOTU(NBG2,ZMA(NBG+K,1),NZMA,EV(1,NI-1,2),1)
  130    CONTINUE
C
         DO 140 J = 1 , NZMA
         DO 140 I = 1 , NBG
            M = I + NBG
            APR(I,J,NI) = APR(I,J,NI) - EWSHIFT*ZMA(M,J)
  140    CONTINUE
C
         IF(NI.LT.NG) THEN
            CALL CONBMAT(NI,NZMA,ZMA)
            DO 150  J = NBG+1 , NB3
               M = J - NBG
               DO 150  I = 1 , NBG
                  APR(I,J,NI) = APR(I,J,NI) - EWSHIFT * ZMA(I,M)
  150       CONTINUE
         ENDIF
C
  200 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK STVAL
      SUBROUTINE STVAL(N,X)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
 
*CALL COMMAX
*CALL COMPAR
*CALL ISEED
*CALL COMSTVR
 
      INTEGER  N, I, ISEED
      COMPLEX  X(*)
 
*IF IBM
C
C     TEST HORUS
C
      RSEED= 1.0
      CALL DURAND(RSEED,N,SX)
      CALL DURAND(RSEED,N,SY)
C      ISEED= 1
C      CALL RNSET(ISEED)
C      CALL DRNUN(N,SX)
C      CALL DRNUN(N,SY)
*ENDIF
      DO 10 I=1,N
*IF CRAY
         SX = RANF( )
         SY = RANF( )
         X(I) = CMPLX(SX,SY)
*ELSE
         X(I) = CMPLX(SX(I),SY(I))
*ENDIF
   10 CONTINUE
 
      RETURN
      END
************************************************************************
*DECK DURAND
      SUBROUTINE DURAND(RSEED,NR,AR)
      REAL AR(*)
      DO I=1,NR
        AR(I) = RAND()
      ENDDO
      RETURN
      END
************************************************************************
*DECK MAT5
      SUBROUTINE MAT5
C-----------------------------------------------------------------------
C     COMPUTATION OF THE MATRICES FOR LANCZOS SOLVER
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPAR
*CALL COMPAR5
*CALL CORE5
*CALL COMGRID
*CALL COMDIM
*CALL COMDIM5
C
      COMPLEX  CNUL
 
      DATA CNUL /(0.0,0.0)/
 
      MUL = MULZ
      MLL = MLLZ
C
      CALL CXSCAL(LDAA*NDIM,CNUL,AA,1)
C
      DO 100 NI=1,NGINT
         CALL CONBMAT(NI,NZMA,ZMA)
         NB = (NI-1)*NZMA/2
         DO 10 L=1,NZMA
            JZ = NB+L-1
            IZ = 2*NZMA
            DO 10 K=L,NZMA
               JZ = JZ+1
               IZ = IZ-1
               AA(IZ,JZ) = AA(IZ,JZ)+ZMA(L,K)
   10    CONTINUE
         DO 20 L=2,NZMA
            IZ = 2*NZMA+L-1
            JZ = NB
            DO 20 K=1,L-1
               IZ = IZ-1
               JZ = JZ+1
               AA(IZ,JZ) = AA(IZ,JZ)+ZMA(L,K)
   20    CONTINUE
  100 CONTINUE
C
      CALL SGSCAL(LDBL*NDIM5,0.0,BB,1)
C
      DO 120 J=1,NDIM
         DO 110 K=1,LDBL
            BB(K,J) = REAL(AA(K+MLL,J))
  110    CONTINUE
  120 CONTINUE
C
      CALL CXSCAL(LDAA*NDIM,CNUL,AA,1)
C
      DO 200 NI=1,NGINT
         CALL CONAMAT(NI,NZMA,ZMA)
         NB = (NI-1)*NZMA/2
         DO 130 L=1,NZMA
            JZ = NB+L-1
            IZ = 2*NZMA
            DO 130 K=L,NZMA
               JZ = JZ+1
               IZ = IZ-1
               AA(IZ,JZ) = AA(IZ,JZ)+ZMA(L,K)
  130    CONTINUE
         DO 140 L=2,NZMA
            IZ = 2*NZMA+L-1
            JZ = NB
            DO 140 K=1,L-1
               IZ = IZ-1
               JZ = JZ+1
               AA(IZ,JZ) = AA(IZ,JZ)+ZMA(L,K)
  140    CONTINUE
  200 CONTINUE
 
      CALL SGSCAL(LDAL*NDIM5,0.0,AORIG,1)
C
      DO 220 J=1,NDIM
         DO 210 K=1,LDAL
            AORIG(K,J) = REAL(AA(K+MLL,J))
  210    CONTINUE
  220 CONTINUE
C
      RETURN
      END
C***********************************************************************
*DECK CONBMAT
      SUBROUTINE CONBMAT(NI,NZMA,ZMA)
C-----------------------------------------------------------------------
C     COMPUTATION OF THE MATRIX BMAT (STORED IN ZMA)
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMGRID
*CALL COMEQUI
*CALL COMEQV
*CALL COMWEL
*CALL COMGEW
*CALL COMIOD
*CALL COMFFT
*CALL COMVAC
C
      INTEGER  NI,NBG,NZMA,MS,MZ,I,K,
     >         INDQQ(6),INDCC(5),INDCQ(3),INDQC(2)
      REAL     SL,SU,ZDIF,ZA,ZB,ZC,ZSR,ZQ,ZT,QOT,TOQ,
     >         DZQ,DZT,ZRHO,DZRHO,T0,DT0,FKDUMMY(4),
     >         ZS(4),HC(4),HQ(4),DUMMY(3),SMZ(4),SMS(4)
      COMPLEX  R2_K(MANZ+11),R4_K(MANZ+11),R2GPSI_K(MANZ+11),
     >         R4GPSI_K(MANZ+11),R2GPGT_K(MANZ+11),
     >         R4GPGT_K(MANZ+11),SOGPSI_K(MANZ+11),GPGT_K(MANZ+11),
     >         SR2OGP_K(MANZ+11),SR2GGG_K(MANZ+11),SR4GGG_K(MANZ+11),
     >         R4GPT2_K(MANZ+11),GPSI_K(MANZ+11),OOR2_K(MANZ+11),
     >         FR2GPT_K(MANZ+11),FR2GT2_K(MANZ+11),
     >         SOR2GP_K(MANZ+11),SGGG_K(MANZ+11),ZETA,DZETA,
     >         R2(4),R4(4),R2GPSI(4),
     >         R4GPSI(4),R2GPGT(4),
     >         R4GPGT(4),SOGPSI(4),GPGT(4),
     >         SR2OGP(4),SR2GGG(4),SR4GGG(4),
     >         R4GPT2(4),GPSI(4),OOR2(4),
     >         FR2GPT(4),FR2GT2(4),
     >         SOR2GP(4),SGGG(4),
     >         ZMA(NZMA*NZMA),FACT(4,7)
C
C--------------------------------------------------------------------
C B(i,j) has the index value (j-1)*ngl + i
C--------------------------------------------------------------------
      DATA INDQQ / 9, 17, 10, 41, 37, 38/
      DATA INDCC / 1, 25, 33, 49, 4 /
      DATA INDCQ / 8, 11, 36    /
      DATA INDQC / 2,  3 /
      
C
C ... NULLSCHREIBEN DER MATRIX ZMA ...
C
      NBG = NZMA/2
      DO 10 I = 1, NZMA*NZMA
         ZMA(I) = (0.0,0.0)
   10 CONTINUE
C
C ... INTEGRATION IM INTERVALL  SGRID(N) - SGRID(N+1) ...
C
      SL = SGRID(NI)
      SU = SGRID(NI+1)
C
C ... BERECHNUNG DER STUETZSTELLEN ...
C
      ZDIF = SU - SL
C
      DO 200  I = 1 , 4
C     -----------------
C
         ZSR     = SGI((NI-1)*4+I)
         ZQ      = Q((NI-1)*4+I)
         DZQ     = DQ((NI-1)*4+I)
         ZT      = T((NI-1)*4+I)
         DZT     = DT((NI-1)*4+I)
         ZETA    = ETAV((NI-1)*4+I)
         DZETA   = DETA((NI-1)*4+I)

         ZRHO    = RHO((NI-1)*4+I) 
         DZRHO   = DRHO((NI-1)*4+I)
         T0      = ZT0((NI-1)*4+I)
         DT0     = ZDT0((NI-1)*4+I)
C
         QOT     = ZQ/ZT
         TOQ     = ZT/ZQ
         SPS2    = 2.*ZSR*CPSURF
         FKDUMMY =  1.0
         CW      = CWW
C
         CALL CUBFCT(ZSR,SL,SU,HC)
         CALL QUAFCT(ZSR,SL,SU,HQ)
C
C
      DO  K = 1 , MANZ+5
C     ------------------------
C
        R2_K(K) = CMPLX(SPWERT(NPSI,ZSR,RR2(1,K),RR2(NP1,K),
     >                      RR2(N2P1,K),RR2(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IR2(1,K),IR2(NP1,K),
     >                      IR2(N2P1,K),IR2(N3P1,K),CS,DUMMY))
C     
        OOR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,ROOR2(1,K),ROOR2(NP1,K),
     >                      ROOR2(N2P1,K),ROOR2(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IOOR2(1,K),IOOR2(NP1,K),
     >                      IOOR2(N2P1,K),IOOR2(N3P1,K),CS,DUMMY))
C
        GPSI_K(K) = CMPLX(SPWERT(NPSI,ZSR,RGPSI(1,K),RGPSI(NP1,K),
     >                       RGPSI(N2P1,K),RGPSI(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IGPSI(1,K),IGPSI(NP1,K),
     >                       IGPSI(N2P1,K),IGPSI(N3P1,K),CS,DUMMY))
C
        SOGPSI_K(K)= CMPLX(SPWERT(NPSI,ZSR,RSOGPSI(1,K),RSOGPSI(NP1,K),
     >                       RSOGPSI(N2P1,K),RSOGPSI(N3P1,K),CS,DUMMY),
     >                SPWERT(NPSI,ZSR,ISOGPSI(1,K),ISOGPSI(NP1,K),
     >                      ISOGPSI(N2P1,K),ISOGPSI(N3P1,K),CS,DUMMY))
C
        R2GPSI_K(K)=CMPLX(SPWERT(NPSI,ZSR,RR2GPSI(1,K),RR2GPSI(NP1,K),
     >                      RR2GPSI(N2P1,K),RR2GPSI(N3P1,K),CS,DUMMY),
     >                SPWERT(NPSI,ZSR,IR2GPSI(1,K),IR2GPSI(NP1,K),
     >                      IR2GPSI(N2P1,K),IR2GPSI(N3P1,K),CS,DUMMY))
C     
        R4GPSI_K(K)=CMPLX(SPWERT(NPSI,ZSR,RR4GPSI(1,K),RR4GPSI(NP1,K),
     >                      RR4GPSI(N2P1,K),RR4GPSI(N3P1,K),CS,DUMMY),
     >               SPWERT(NPSI,ZSR,IR4GPSI(1,K),IR4GPSI(NP1,K),
     >                     IR4GPSI(N2P1,K),IR4GPSI(N3P1,K),CS,DUMMY))
C
        GPGT_K(K)=CMPLX(SPWERT(NPSI,ZSR,RGPGT(1,K),RGPGT(NP1,K),
     >                       RGPGT(N2P1,K),RGPGT(N3P1,K),CS,DUMMY),
     >               SPWERT(NPSI,ZSR,IGPGT(1,K),IGPGT(NP1,K),
     >                      IGPGT(N2P1,K),IGPGT(N3P1,K),CS,DUMMY))
C     
        R2GPGT_K(K)=CMPLX(SPWERT(NPSI,ZSR,RR2GPGT(1,K),RR2GPGT(NP1,K),
     >                      RR2GPGT(N2P1,K),RR2GPGT(N3P1,K),CS,DUMMY),
     >               SPWERT(NPSI,ZSR,IR2GPGT(1,K),IR2GPGT(NP1,K),
     >                     IR2GPGT(N2P1,K),IR2GPGT(N3P1,K),CS,DUMMY))
C     
        R4GPGT_K(K)=CMPLX(SPWERT(NPSI,ZSR,RR4GPGT(1,K),RR4GPGT(NP1,K),
     >                      RR4GPGT(N2P1,K),RR4GPGT(N3P1,K),CS,DUMMY),
     >               SPWERT(NPSI,ZSR,IR4GPGT(1,K),IR4GPGT(NP1,K),
     >                     IR4GPGT(N2P1,K),IR4GPGT(N3P1,K),CS,DUMMY))
C
        R4GPT2_K(K)=CMPLX(SPWERT(NPSI,ZSR,RR4GPT2(1,K),RR4GPT2(NP1,K),
     >                      RR4GPT2(N2P1,K),RR4GPT2(N3P1,K),CS,DUMMY),
     >                SPWERT(NPSI,ZSR,IR4GPT2(1,K),IR4GPT2(NP1,K),
     >                     IR4GPT2(N2P1,K),IR4GPT2(N3P1,K),CS,DUMMY))
C
        FR2GPT_K(K)=CMPLX(SPWERT(NPSI,ZSR,RFR2GPT(1,K),RFR2GPT(NP1,K),
     >                      RFR2GPT(N2P1,K),RFR2GPT(N3P1,K),CS,DUMMY),
     >               SPWERT(NPSI,ZSR,IFR2GPT(1,K),IFR2GPT(NP1,K),
     >                     IFR2GPT(N2P1,K),IFR2GPT(N3P1,K),CS,DUMMY))
C     
        FR2GT2_K(K)=CMPLX(SPWERT(NPSI,ZSR,RFR2GT2(1,K),RFR2GT2(NP1,K),
     >                     RFR2GT2(N2P1,K),RFR2GT2(N3P1,K),CS,DUMMY),
     >                SPWERT(NPSI,ZSR,IFR2GT2(1,K),IFR2GT2(NP1,K),
     >                     IFR2GT2(N2P1,K),IFR2GT2(N3P1,K),CS,DUMMY))
C     
        SR2OGP_K(K)=CMPLX(SPWERT(NPSI,ZSR,RSR2OGP(1,K),RSR2OGP(NP1,K),
     >                     RSR2OGP(N2P1,K),RSR2OGP(N3P1,K),CS,DUMMY),
     >               SPWERT(NPSI,ZSR,ISR2OGP(1,K),ISR2OGP(NP1,K),
     >                     ISR2OGP(N2P1,K),ISR2OGP(N3P1,K),CS,DUMMY))
C
        SR2GGG_K(K)=CMPLX(SPWERT(NPSI,ZSR,RSR2GGG(1,K),RSR2GGG(NP1,K),
     >                      RSR2GGG(N2P1,K),RSR2GGG(N3P1,K),CS,DUMMY),
     >                SPWERT(NPSI,ZSR,ISR2GGG(1,K),ISR2GGG(NP1,K),
     >                     ISR2GGG(N2P1,K),ISR2GGG(N3P1,K),CS,DUMMY))
C
        SR4GGG_K(K)=CMPLX(SPWERT(NPSI,ZSR,RSR4GGG(1,K),RSR4GGG(NP1,K),
     >                     RSR4GGG(N2P1,K),RSR4GGG(N3P1,K),CS,DUMMY),
     >                SPWERT(NPSI,ZSR,ISR4GGG(1,K),ISR4GGG(NP1,K),
     >                     ISR4GGG(N2P1,K),ISR4GGG(N3P1,K),CS,DUMMY))
C
        SOR2GP_K(K)=CMPLX(SPWERT(NPSI,ZSR,RSOR2GP(1,K),RSOR2GP(NP1,K),
     >                     RSOR2GP(N2P1,K),RSOR2GP(N3P1,K),CS,DUMMY),
     >                SPWERT(NPSI,ZSR,ISOR2GP(1,K),ISOR2GP(NP1,K),
     >                     ISOR2GP(N2P1,K),ISOR2GP(N3P1,K),CS,DUMMY))
C
        SGGG_K(K)= CMPLX(SPWERT(NPSI,ZSR,RSGGG(1,K),RSGGG(NP1,K),
     >                       RSGGG(N2P1,K),RSGGG(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,ISGGG(1,K),ISGGG(NP1,K),
     >                      ISGGG(N2P1,K),ISGGG(N3P1,K),CS,DUMMY))

      ENDDO
C

C
      DO 100  MS = 1 , MANZ 
C     ------------------------------
C
      SMS(1) = MSTART(NI)   + FLOAT(MS-1)
      SMS(2) = MSTART(NI+1) + FLOAT(MS-1)
      SMS(3) = MSTART(NI)   + FLOAT(MS-1)
      SMS(4) = MSTART(NI+1) + FLOAT(MS-1)

      DO 100  MZ = 1 , MANZ
C     ------------------------------
C
      SMZ(1) = MSTART(NI)   + FLOAT(MZ-1)
      SMZ(2) = MSTART(NI)   + FLOAT(MZ-1)
      SMZ(3) = MSTART(NI+1) + FLOAT(MZ-1)
      SMZ(4) = MSTART(NI+1) + FLOAT(MZ-1)
            
      DO I4=1,4
      
        FKDUMMY(I4) = 0.
        IF (SMZ(I4) .EQ. SMS(I4)) FKDUMMY(I4) = 1. 
        KI = INT(ABS(INT(SMS(I4)) - INT(SMZ(I4))))+ 1
        IF ( (SMZ(I4) - SMS(I4)) .GE. 0) THEN
          R2(I4)     = R2_K(KI)
          GPSI(I4)   = GPSI_K(KI)
          FR2GPT(I4) = FR2GPT_K(KI)
          R4GPT2(I4) = R4GPT2_K(KI)
          R4GPGT(I4) = R4GPGT_K(KI)
          R4GPSI(I4) = R4GPSI_K(KI)
          SR2OGP(I4) = SR2OGP_K(KI)
          SR4GGG(I4) = SR4GGG_K(KI)
          R2GPSI(I4) = R2GPSI_K(KI)
          GPGT(I4)   = GPGT_K(KI)
          R2GPGT(I4) = R2GPGT_K(KI)
          SOGPSI(I4) = SOGPSI_K(KI)
          SR2GGG(I4) = SR2GGG_K(KI)
          FR2GT2(I4) = FR2GT2_K(KI)
        ELSE
          R2(I4)     = CONJG(R2_K(KI))
          GPSI(I4)   = CONJG(GPSI_K(KI))
          FR2GPT(I4) = CONJG(FR2GPT_K(KI))
          R4GPT2(I4) = CONJG(R4GPT2_K(KI))
          R4GPSI(I4) = CONJG(R4GPSI_K(KI))
          R4GPGT(I4) = CONJG(R4GPGT_K(KI))
          SR2OGP(I4) = CONJG(SR2OGP_K(KI))
          SR4GGG(I4) = CONJG(SR4GGG_K(KI)) 
          R2GPSI(I4) = CONJG(R2GPSI_K(KI))
          GPGT(I4)   = CONJG(GPGT_K(KI))
          R2GPGT(I4) = CONJG(R2GPGT_K(KI))
          SOGPSI(I4) = CONJG(SOGPSI_K(KI))
          SR2GGG(I4) = CONJG(SR2GGG_K(KI)) 
          FR2GT2(I4) = CONJG(FR2GT2_K(KI))
        ENDIF
      ENDDO


C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(QUA)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(2,2), B(3,3), B(3,2), B(6,6), B(2,6), B(3,6)
C
      FACT(1:4,1) =  ZRHO * R4GPSI * ZQ / (SPS2*ZT**3)
      FACT(1:4,2) =  FKDUMMY 
      FACT(1:4,3) = - ZMU * ZQ/ZT**2 * FR2GPT
      FACT(1:4,4) = ZRHO * ZQ * ZT * FKDUMMY 
     >        + ZRHO * ZQ / ZT * GPSI   
      FACT(1:4,5) = ZRHO * ZQ / (SPS2*ZT**2) * R2GPSI  
      FACT(1:4,6) = - ZMU * SPS2*ZQ/(ZT) * GPGT 
    
C
      CALL FKUBL(MZ,MS,MANZ,6,INDQQ,NBG,NZMA,ZMA,FACT,GEWI(I),HQ,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(CUB)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(1,1), B(4,4), B(5,5),  B(7,7),  B(4,1)
C
      FACT(1:4,1) = ZRHO *SPS2/(ZSR**2 *ZQ*ZT)*(SR2OGP+QOT**2*SR4GGG)
     >        - ZRHO * R4GPT2 * SPS2 * ZQ / ZT**5 
      FACT(1:4,2) = ZT*FKDUMMY + GPSI/ZT 
      FACT(1:4,3) = SPS2 * ZQ / ZT   * FKDUMMY
     >        + SPS2 * ZQ / ZT**3 * GPSI 
      FACT(1:4,4) = ZQ * R2 / ZT      
      FACT(1:4,5) = - ZMU * ZQ/ZT * R2GPGT 
C
      CALL FKUBL(MZ,MS,MANZ,5,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),HC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(QUA)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(1,2), B(4,2), B(1,6)
C
      FACT(1:4,1)  = (0.,1.) * ZRHO * ZQ / ZT**3 * R4GPGT
      FACT(1:4,2)  = - (0.,1.)* ZMU * ZQ/(SPS2*ZT) * R2GPSI
      FACT(1:4,3)  = (0.,1.) * ZRHO * ZQ / ZT**2 * R2GPGT *SPS2     
C
      CALL FKUBL(MZ,MS,MANZ,3,INDCQ,NBG,NZMA,ZMA,FACT,GEWI(I),HC,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(CUB)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(2,1), B(3,1)
C
      FACT(1:4,1)  = - (0.,1.) * ZRHO * ZQ / ZT**3 * R4GPGT 
      FACT(1:4,2)  = (0.,1.)*ZMU * SPS2/(ZQ*ZSR*ZSR) 
     >                     * (SOGPSI + (ZQ/ZT)**2 * SR2GGG)
     >         - (0.,1.)*ZMU * SPS2 * ZQ/ZT**4 * FR2GT2
C
      CALL FKUBL(MZ,MS,MANZ,2,INDQC,NBG,NZMA,ZMA,FACT,GEWI(I),HQ,HC)
C
      IF(MS.EQ.MZ)  GOTO 100
C     ----------------------
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(QUA)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(2,2), B(3,3), B(3,2), B(6,6), B(2,6), B(3,6)
C
C      FACT(1) = ZRHO * CONJG(R4GPSI) * ZQ / (SPS2*ZT**3)
C      FACT(2) = FKDUMMY 
C      FACT(3) = - ZMU * ZQ/ZT**2 * CONJG(FR2GPT)
C      FACT(4) = ZRHO * ZQ * ZT * FKDUMMY
C     >        + ZRHO * ZQ / ZT * CONJG(GPSI) 
C      FACT(5)  = ZRHO * ZQ / (SPS2*ZT**2) * CONJG(R2GPSI)
C      FACT(6)  = - ZMU * ZQ*SPS2/(ZT) * CONJG(GPGT) 
C
C      CALL FKUBL(MS,MZ,MANZ,6,INDQQ,NBG,NZMA,ZMA,FACT,GEWI(I),HQ,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(CUB)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(1,1), B(4,4), B(5,5),  B(7,7),  B(4,1)
C
C      FACT(1) = ZRHO * SPS2/(ZSR**2 *ZQ*ZT) * 
C     >                (CONJG(SR2OGP) + QOT**2 * CONJG(SR4GGG))
C     >        - ZRHO * CONJG(R4GPT2) * SPS2 * ZQ / ZT**5 
C      FACT(2) = ZT*FKDUMMY + CONJG(GPSI)/ZT 
C      FACT(3) = SPS2 * ZQ / ZT * FKDUMMY
C     >        + SPS2 * ZQ / ZT**3 * CONJG(GPSI)
C      FACT(4) = ZQ * CONJG(R2) / ZT
C      FACT(5) = - ZMU * ZQ/ZT * CONJG(R2GPGT) 
C
C      CALL FKUBL(MS,MZ,MANZ,5,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),HC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(QUA)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(1,2), B(4,2), B(1,6)
C
C      FACT(1)  = + (0.,1.) * ZRHO * ZQ / ZT**3 * CONJG(R4GPGT)
C      FACT(2)  = - (0.,1.) * ZMU * ZQ/(SPS2*ZT) * CONJG(R2GPSI)
C      FACT(3) =    (0.,1.) * ZRHO * ZQ / ZT**2 * CONJG(R2GPGT)      
C
C      CALL FKUBL(MS,MZ,MANZ,3,INDCQ,NBG,NZMA,ZMA,FACT,GEWI(I),HC,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(CUB)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(2,1), B(3,1)
C
C      FACT(1)  = - (0.,1.) * ZRHO * ZQ / ZT**3 * CONJG(R4GPGT)
C      FACT(2)  = (0.,1.)*ZMU * SPS2/(ZQ*ZSR*ZSR) 
C     >                     * CONJG(SOGPSI + (ZQ/ZT)**2 * SR2GGG)
C     >         - (0.,1.)*ZMU * SPS2 * ZQ/ZT**4 * CONJG(FR2GT2)
C
C      CALL FKUBL(MS,MZ,MANZ,2,INDQC,NBG,NZMA,ZMA,FACT,GEWI(I),HQ,HC)
C
  100 CONTINUE
C     --------
C
      FKDUMMY =  0.0
C
  200 CONTINUE
C     --------
C
      DO 210 I = 1,NZMA*NZMA
         ZMA(I) = ZDIF * ZMA(I)
  210 CONTINUE
C
C
C ... REGULARITAETSBEDINGUNG FUER S = 0 ...
C
      IF(NI.EQ.1) THEN
        DO 230 I=1,2*MANZ,2
        DO 220 J=1,NZMA
           ZMA((I-1)*NZMA+J) = (0.0,0.0)
           ZMA(I+(J-1)*NZMA) = (0.0,0.0)
  220   CONTINUE
        ZMA((I-1)*NZMA+I) = (1.,0.)
  230   CONTINUE
        DO 232 I=2*MANZ+1,12*MANZ,2
        DO 234 J=1,NZMA
           ZMA((I-1)*NZMA+J) = (0.0,0.0)
           ZMA(I+(J-1)*NZMA) = (0.0,0.0)
  234   CONTINUE
        ZMA((I-1)*NZMA+I) = (1.,0.)
  232   CONTINUE
        DO 236 I=12*MANZ+1,14*MANZ,2
        DO 238 J=1,NZMA
           ZMA((I-1)*NZMA+J) = (0.0,0.0)
           ZMA(I+(J-1)*NZMA) = (0.0,0.0)
  238   CONTINUE
        ZMA((I-1)*NZMA+I) = (1.,0.)
  236   CONTINUE
      ENDIF
C
C ... RANDBEDINGUNG FUER  S = 1 ...
C
      IF(RWALL.LE.1.AND.NI.EQ.NGINT) THEN
C
C V1 AT PLASMA BOUNDARY SET TO ZERO
C
         DO 250 I=NBG+1,NBG+2*MANZ,2
            DO 240 J=1,NZMA
               ZMA((I-1)*NZMA+J) = (0.0,0.0)
               ZMA(I+(J-1)*NZMA) = (0.0,0.0)
  240       CONTINUE
            ZMA((I-1)*NZMA+I) = (1.0,0.0)
  250    CONTINUE
C
C A2, A3  AT PLASMA BOUNDARY SET TO ZERO
C 
         DO 270 I=NBG+6*MANZ+1,NBG+10*MANZ,2
            DO 260 J=1,NZMA
               ZMA((I-1)*NZMA+J) = (0.0,0.0)
               ZMA(I+(J-1)*NZMA) = (0.0,0.0)
  260       CONTINUE
            ZMA((I-1)*NZMA+I) = (1.0,0.0)
  270    CONTINUE
      ENDIF
C
      RETURN
      END
************************************************************************
*DECK CONAMAT
      SUBROUTINE CONAMAT(NI,NZMA,ZMA)
C-----------------------------------------------------------------------
C     COMPUTATION OF THE MATRIX AMAT (STORED IN ZMA)
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMGRID
*CALL COMWEL
*CALL COMGEW
*CALL COMEQUI
*CALL COMEQV
*CALL COMIOD
*CALL COMFFT
*CALL COMVAC
C
      INTEGER  NI,NBG,NZMA,MS,MZ,I,K,
     >         INDCC(13),INDCQ(8),INDQC(10),INDQQ(7),IDCDC(4),
     >         IDCQ(3),IQDC(6),IDCC(8),ICDC(6),IDQDC(1),IDQQ(1)
      REAL     SL,SU,ZDIF,ZSR,ZRHO,DZRHO,ZQ,DZQ,
     >         ZT,DZT,T0,DT0,DDT0,QOT,TOQ,T2OQ,DOQDOT,SPS2,
     >         ZBIG, HC(4),HQ(4),DHC(4),DHQ(4),DUMMY(3),
     >         SMS(4),SMZ(4),MSNQ(4),MZNQ(4),FKDUMMY(4)
      COMPLEX  R2_K(MANZ+11),R4_K(MANZ+11),GPSI_K(MANZ+11),
     >         GPGT_K(MANZ+11),R2GPSI_K(MANZ+11),R2GPGT_K(MANZ+11),
     >         SOGPSI_K(MANZ+11),OOR2_K(MANZ+11),GPSI2_K(MANZ+11),
     >         GPGPGT_K(MANZ+11),DSR2GPSI_K(MANZ+11),
     >         SOR2GP_K(MANZ+11),SGGG_K(MANZ+11),SR2GGG_K(MANZ+11),
     >         DSR2_K(MANZ+11),DSGPSI_K(MANZ+11),B02_K(MANZ+11),
     >         GPDSGP_K(MANZ+11),GPSIOR_K(MANZ+11),
     >         GPGT2_K(MANZ+11),R2DSGP_K(MANZ+11),FDTR2_K(MANZ+11), 
     >         GPTDSG_K(MANZ+11), FR2_K(MANZ+11), FR2GPS_K(MANZ+11), 
     >         FR2GPT_K(MANZ+11),DSGPGT_K(MANZ+11),
     >         R4GPT2_K(MANZ+11),SR2OGP_K(MANZ+11), 
     >         SR4GGG_K(MANZ+11),R4GPGT_K(MANZ+11),R4GPSI_K(MANZ+11),
     >         FR4GPS_K(MANZ+11),FR4GPT_K(MANZ+11),
     >         R2GPTGPDSR2_K(MANZ+11), GPSI2DSR2_K(MANZ+11),
     >         GPSIDSR2_K(MANZ+11), GPGT2DSR2_K(MANZ+11),
     >         DSR2OR2_K(MANZ+11), GPGTGPDSR2_K(MANZ+11),
     >         PTOR2DSR2_K(MANZ+11), GPGTDSR2_K(MANZ+11), 
     >         R2GPGT2DTR2_K(MANZ+11), GPGPGTDTR2_K(MANZ+11),
     >         PTOR2DTR2_K(MANZ+11), GPGTGTDTR2_K(MANZ+11),
     >         GP2GT2DTR2_K(MANZ+11), TOR2DTR2_K(MANZ+11),
     >         DTR2_K(MANZ+11), DTR2OR2_K(MANZ+11), 
     >         DTR2OR4_K(MANZ+11), GPSIOR2DSR2_K(MANZ+11),  
     >         GPGT2DTR2_K(MANZ+11), R2GPSI2DSR2_K(MANZ+11),
     >         R2GPGT2DSR2_K(MANZ+11), R4GPTOGP_K(MANZ+11),
     >         R2GPTDSR2_K(MANZ+11),R2GPDSR2_K(MANZ+11),
     >         GPTDTR2_K(MANZ+11), R2GGG2DSR2_K(MANZ+11),
     >         R2GPGT3DTR2_K(MANZ+11),GPGPT2DSR2_K(MANZ+11),
     >         GPGT3DTR2_K(MANZ+11),R2GP2GPTDSR2_K(MANZ+11),
     >         R2GGG2DTR2_K(MANZ+11),R2GPGTDTR2_K(MANZ+11),
     >         R2GPDTR2_K(MANZ+11),GPSIDTR2_K(MANZ+11), 
     >         R2(4),R4(4),GPSI(4), R4GPTOGP(4),
     >         GPGT(4),R2GPSI(4),R2GPGT(4),
     >         SOGPSI(4),OOR2(4),GPSI2(4),
     >         GPGPGT(4),DSR2GPSI(4),R2GPGTDTR2(4),
     >         SOR2GP(4),SGGG(4),SR2GGG(4),
     >         DSR2(4),DSGPSI(4), R2GPDTR2(4),
     >         GPDSGP(4),GPSIOR(4),B02(4),
     >         GPGT2(4),R2DSGP(4), FDTR2(4),
     >         GPTDSG(4), FR2(4), FR2GPS(4), 
     >         FR2GPT(4),DSGPGT(4),
     >         R4GPT2(4),SR2OGP(4),R2GPGT2DSR2(4), 
     >         SR4GGG(4),R4GPGT(4),R4GPSI(4),
     >         FR4GPS(4),FR4GPT(4), R2GPSI2DSR2(4),  
     >         R2GPTGPDSR2(4), GPSI2DSR2(4),
     >         GPSIDSR2(4), GPGT2DSR2(4),
     >         DSR2OR2(4), GPGTGPDSR2(4),
     >         PTOR2DSR2(4), GPGTDSR2(4),
     >         R2GPGT2DTR2(4), GPGPGTDTR2(4),
     >         PTOR2DTR2(4), GPGTGTDTR2(4),
     >         GP2GT2DTR2(4), TOR2DTR2(4),
     >         DTR2(4), DTR2OR2(4), GPGT2DTR2(4),
     >         DTR2OR4(4), GPSIOR2DSR2(4),GPSIDTR2(4),
     >         R2GPTDSR2(4),R2GPDSR2(4),GPTDTR2(4),
     >         R2GGG2DSR2(4), R2GPGT3DTR2(4),GPGPT2DSR2(4),
     >         GPGT3DTR2(4),R2GP2GPTDSR2(4),R2GGG2DTR2(4),
     >         ZETA,DZETA,ZETA1,
     >         ZMA(NZMA*NZMA),FACT(4,14)
C--------------------------------------------------------------------
C A(i,j) has the index value (j-1)*ngl + i
C--------------------------------------------------------------------
      DATA INDCC / 22, 4,  7, 43, 26, 25, 32, 29, 33, 49, 47, 46, 1 /
      DATA INDCQ / 15, 14, 19, 18, 42, 8, 36, 6 /
      DATA INDQC / 23, 44, 24, 31, 30, 34, 48, 27, 45, 2 /
      DATA INDQQ / 16, 10, 17, 9, 41, 37, 13 /
      DATA IDCDC / 22, 29, 33, 25 /
      DATA IDCQ  / 15, 19, 18 /
      DATA IQDC  / 23, 30, 24, 31, 45, 2 /
      DATA IDCC  / 22, 26, 43,  7, 33, 29, 25, 32 /
      DATA ICDC  / 22, 29, 33, 26, 25, 1/
      DATA IDQDC / 24 /
      DATA IDQQ  / 17 /
C
C ... NULLSCHREIBEN DER MATRIX ZMA ...
C
      ZBIG = 1.E+20
      NBG  = NZMA/2
C
      DO 10 I = 1, NZMA*NZMA
         ZMA(I) = (0.0,0.0)
   10 CONTINUE
C
C ... INTEGRATION IM INTERVALL  SGRID(N) - SGRID(N+1)...
C
      SL = SGRID(NI)
      SU = SGRID(NI+1)
C
C ... BERECHNUNG DER STUETZSTELLEN ...
C
      ZDIF = SU - SL
C
C----------------------------------- switch eta on/off in A1 equation
      ETA1 = ETA
C--------------------------------------------------------------------      
      
      DO 200 I=1,4
C     ------------
C
      ZSR     = SGI((NI-1)*4+I)
      ZQ      = Q((NI-1)*4+I)
      DZQ     = DQ((NI-1)*4+I)
      ZT      = T((NI-1)*4+I)
      DZT     = DT((NI-1)*4+I)
      
      ZETA    = ETAV((NI-1)*4+I)
      DZETA   = DETA((NI-1)*4+I)
      ZETA1   = ZETA
      CW      = CWW
      
      ZRHO    = RHO((NI-1)*4+I) 
      DZRHO   = DRHO((NI-1)*4+I)
      T0      = ZT0((NI-1)*4+I)
      DT0     = ZDT0((NI-1)*4+I)
      DDT0    = ZDDT0((NI-1)*4+I)
      V03     = FLOW3((NI-1)*4+I)
      DV03    = DFLOW3((NI-1)*4+I)

      QOT     = ZQ/ZT
      TOQ     = ZT/ZQ
      T2OQ    = ZT**2/ZQ
      DOQDOT  = DZQ/ZQ-DZT/ZT
      SPS2    = 2.*ZSR*CPSURF
      DSPS    = 2.*CPSURF
      
      FKDUMMY =  1.0
C
      CALL CUBFCT(ZSR,SL,SU,HC)
      CALL QUAFCT(ZSR,SL,SU,HQ)
      CALL DCUBF (ZSR,SL,SU,DHC)
      CALL DQUAF (ZSR,SL,SU,DHQ)
C
      DO K = 1, MANZ+11
C     --------------------
C
C
        R2_K(K)    = CMPLX(SPWERT(NPSI,ZSR,RR2(1,K),RR2(NP1,K),
     >                        RR2(N2P1,K),RR2(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IR2(1,K),IR2(NP1,K),
     >                        IR2(N2P1,K),IR2(N3P1,K),CS,DUMMY))
C
        B02_K(K)    = CMPLX(SPWERT(NPSI,ZSR,RB02(1,K),RB02(NP1,K),
     >                        RB02(N2P1,K),RB02(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IB02(1,K),IB02(NP1,K),
     >                        IB02(N2P1,K),IB02(N3P1,K),CS,DUMMY))
C
        R4_K(K)    = CMPLX(SPWERT(NPSI,ZSR,RR4(1,K),RR4(NP1,K),
     >                        RR4(N2P1,K),RR4(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IR4(1,K),IR4(NP1,K),
     >                        IR4(N2P1,K),IR4(N3P1,K),CS,DUMMY))
C     
        OOR2_K(K)  = CMPLX(SPWERT(NPSI,ZSR,ROOR2(1,K),ROOR2(NP1,K),
     >                        ROOR2(N2P1,K),ROOR2(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IOOR2(1,K),IOOR2(NP1,K),
     >                        IOOR2(N2P1,K),IOOR2(N3P1,K),CS,DUMMY))
C
        FR2_K(K)   = CMPLX(SPWERT(NPSI,ZSR,RFR2(1,K),RFR2(NP1,K),
     >                        RFR2(N2P1,K),RFR2(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IFR2(1,K),IFR2(NP1,K),
     >                        IFR2(N2P1,K),IFR2(N3P1,K),CS,DUMMY))

        GPSI_K(K)  = CMPLX(SPWERT(NPSI,ZSR,RGPSI(1,K),RGPSI(NP1,K),
     >                        RGPSI(N2P1,K),RGPSI(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IGPSI(1,K),IGPSI(NP1,K),
     >                        IGPSI(N2P1,K),IGPSI(N3P1,K),CS,DUMMY))
C
        R2GPSI_K(K)= CMPLX(SPWERT(NPSI,ZSR,RR2GPSI(1,K),RR2GPSI(NP1,K),
     >                      RR2GPSI(N2P1,K),RR2GPSI(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IR2GPSI(1,K),IR2GPSI(NP1,K),
     >                     IR2GPSI(N2P1,K),IR2GPSI(N3P1,K),CS,DUMMY))
C
        FR2GPS_K(K)= CMPLX(SPWERT(NPSI,ZSR,RFR2GPS(1,K),RFR2GPS(NP1,K),
     >                      RFR2GPS(N2P1,K),RFR2GPS(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IFR2GPS(1,K),IFR2GPS(NP1,K),
     >                      IFR2GPS(N2P1,K),IFR2GPS(N3P1,K),CS,DUMMY))
C
        GPSI2_K(K) = CMPLX(SPWERT(NPSI,ZSR,RGPSI2(1,K),RGPSI2(NP1,K),
     >                        RGPSI2(N2P1,K),RGPSI2(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IGPSI2(1,K),IGPSI2(NP1,K),
     >                        IGPSI2(N2P1,K),IGPSI2(N3P1,K),CS,DUMMY))
C
        GPSIOR_K(K)= CMPLX(SPWERT(NPSI,ZSR,RGPSIOR(1,K),RGPSIOR(NP1,K),
     >                      RGPSIOR(N2P1,K),RGPSIOR(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IGPSIOR(1,K),IGPSIOR(NP1,K),
     >                      IGPSIOR(N2P1,K),IGPSIOR(N3P1,K),CS,DUMMY))
C
        GPGT_K(K)  = CMPLX(SPWERT(NPSI,ZSR,RGPGT(1,K),RGPGT(NP1,K),
     >                        RGPGT(N2P1,K),RGPGT(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IGPGT(1,K),IGPGT(NP1,K),
     >                        IGPGT(N2P1,K),IGPGT(N3P1,K),CS,DUMMY))
C
        DSGPGT_K(K)= CMPLX(SPWERT(NPSI,ZSR,RDSGPGT(1,K),RDSGPGT(NP1,K),
     >                       RDSGPGT(N2P1,K),RDSGPGT(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IDSGPGT(1,K),IDSGPGT(NP1,K),
     >                      IDSGPGT(N2P1,K),IDSGPGT(N3P1,K),CS,DUMMY))
C
        GPGT2_K(K) = CMPLX(SPWERT(NPSI,ZSR,RGPGT2(1,K),RGPGT2(NP1,K),
     >                        RGPGT2(N2P1,K),RGPGT2(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IGPGT2(1,K),IGPGT2(NP1,K),
     >                        IGPGT2(N2P1,K),IGPGT2(N3P1,K),CS,DUMMY))
C
        R2GPGT_K(K)= CMPLX(SPWERT(NPSI,ZSR,RR2GPGT(1,K),RR2GPGT(NP1,K),
     >                       RR2GPGT(N2P1,K),RR2GPGT(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IR2GPGT(1,K),IR2GPGT(NP1,K),
     >                       IR2GPGT(N2P1,K),IR2GPGT(N3P1,K),CS,DUMMY))
C
        FR2GPT_K(K)= CMPLX(SPWERT(NPSI,ZSR,RFR2GPT(1,K),RFR2GPT(NP1,K),
     >                       RFR2GPT(N2P1,K),RFR2GPT(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IFR2GPT(1,K),IFR2GPT(NP1,K),
     >                       IFR2GPT(N2P1,K),IFR2GPT(N3P1,K),CS,DUMMY))
C
        GPGPGT_K(K)= CMPLX(SPWERT(NPSI,ZSR,RGPGPGT(1,K),RGPGPGT(NP1,K),
     >                       RGPGPGT(N2P1,K),RGPGPGT(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IGPGPGT(1,K),IGPGPGT(NP1,K),
     >                      IGPGPGT(N2P1,K),IGPGPGT(N3P1,K),CS,DUMMY))
C
        GPGT2DTR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RGPGT2DTR2(1,K),
     >                                            RGPGT2DTR2(NP1,K),
     >                RGPGT2DTR2(N2P1,K),RGPGT2DTR2(N3P1,K),CS,DUMMY),
     >             SPWERT(NPSI,ZSR,IGPGT2DTR2(1,K),IGPGT2DTR2(NP1,K),
     >                 IGPGT2DTR2(N2P1,K),IGPGT2DTR2(N3P1,K),CS,DUMMY))
C
        DSR2_K(K)  = CMPLX(SPWERT(NPSI,ZSR,RDSR2(1,K),RDSR2(NP1,K),
     >                        RDSR2(N2P1,K),RDSR2(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IDSR2(1,K),IDSR2(NP1,K),
     >                        IDSR2(N2P1,K),IDSR2(N3P1,K),CS,DUMMY))
C
        DSGPSI_K(K)= CMPLX(SPWERT(NPSI,ZSR,RDSGPSI(1,K),RDSGPSI(NP1,K),
     >                       RDSGPSI(N2P1,K),RDSGPSI(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IDSGPSI(1,K),IDSGPSI(NP1,K),
     >                       IDSGPSI(N2P1,K),IDSGPSI(N3P1,K),CS,DUMMY))
C
        GPDSGP_K(K)=CMPLX(SPWERT(NPSI,ZSR,RGPDSGP(1,K),RGPDSGP(NP1,K),
     >                      RGPDSGP(N2P1,K),RGPDSGP(N3P1,K),CS,DUMMY),
     >                SPWERT(NPSI,ZSR,IGPDSGP(1,K),IGPDSGP(NP1,K),
     >                      IGPDSGP(N2P1,K),IGPDSGP(N3P1,K),CS,DUMMY))
C
        R2DSGP_K(K)= CMPLX(SPWERT(NPSI,ZSR,RR2DSGP(1,K),RR2DSGP(NP1,K),
     >                       RR2DSGP(N2P1,K),RR2DSGP(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IR2DSGP(1,K),IR2DSGP(NP1,K),
     >                      IR2DSGP(N2P1,K),IR2DSGP(N3P1,K),CS,DUMMY))
C
        GPTDSG_K(K)= CMPLX(SPWERT(NPSI,ZSR,RGPTDSG(1,K),RGPTDSG(NP1,K),
     >                       RGPTDSG(N2P1,K),RGPTDSG(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IGPTDSG(1,K),IGPTDSG(NP1,K),
     >                      IGPTDSG(N2P1,K),IGPTDSG(N3P1,K),CS,DUMMY))
C
        SOR2GP_K(K)= CMPLX(SPWERT(NPSI,ZSR,RSOR2GP(1,K),RSOR2GP(NP1,K),
     >                       RSOR2GP(N2P1,K),RSOR2GP(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,ISOR2GP(1,K),ISOR2GP(NP1,K),
     >                      ISOR2GP(N2P1,K),ISOR2GP(N3P1,K),CS,DUMMY))
C
        SOGPSI_K(K)= CMPLX(SPWERT(NPSI,ZSR,RSOGPSI(1,K),RSOGPSI(NP1,K),
     >                       RSOGPSI(N2P1,K),RSOGPSI(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,ISOGPSI(1,K),ISOGPSI(NP1,K),
     >                      ISOGPSI(N2P1,K),ISOGPSI(N3P1,K),CS,DUMMY))
C
        SGGG_K(K)  = CMPLX(SPWERT(NPSI,ZSR,RSGGG(1,K),RSGGG(NP1,K),
     >                        RSGGG(N2P1,K),RSGGG(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,ISGGG(1,K),ISGGG(NP1,K),
     >                        ISGGG(N2P1,K),ISGGG(N3P1,K),CS,DUMMY))
C
C
        SR2OGP_K(K)= CMPLX(SPWERT(NPSI,ZSR,RSR2OGP(1,K),RSR2OGP(NP1,K),
     >                       RSR2OGP(N2P1,K),RSR2OGP(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,ISR2OGP(1,K),ISR2OGP(NP1,K),
     >                      ISR2OGP(N2P1,K),ISR2OGP(N3P1,K),CS,DUMMY))
C
        SR4GGG_K(K)= CMPLX(SPWERT(NPSI,ZSR,RSR4GGG(1,K),RSR4GGG(NP1,K),
     >                       RSR4GGG(N2P1,K),RSR4GGG(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,ISR4GGG(1,K),ISR4GGG(NP1,K),
     >                      ISR4GGG(N2P1,K),ISR4GGG(N3P1,K),CS,DUMMY))
C
        SR2GGG_K(K)= CMPLX(SPWERT(NPSI,ZSR,RSR2GGG(1,K),RSR2GGG(NP1,K),
     >                       RSR2GGG(N2P1,K),RSR2GGG(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,ISR2GGG(1,K),ISR2GGG(NP1,K),
     >                      ISR2GGG(N2P1,K),ISR2GGG(N3P1,K),CS,DUMMY))
C
        R4GPT2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RR4GPT2(1,K),RR4GPT2(NP1,K),
     >                       RR4GPT2(N2P1,K),RR4GPT2(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IR4GPT2(1,K),IR4GPT2(NP1,K),
     >                      IR4GPT2(N2P1,K),IR4GPT2(N3P1,K),CS,DUMMY))
C
        R4GPSI_K(K)= CMPLX(SPWERT(NPSI,ZSR,RR4GPSI(1,K),RR4GPSI(NP1,K),
     >                       RR4GPSI(N2P1,K),RR4GPSI(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IR4GPSI(1,K),IR4GPSI(NP1,K),
     >                       IR4GPSI(N2P1,K),IR4GPSI(N3P1,K),CS,DUMMY))
C
        R4GPGT_K(K)= CMPLX(SPWERT(NPSI,ZSR,RR4GPGT(1,K),RR4GPGT(NP1,K),
     >                       RR4GPGT(N2P1,K),RR4GPGT(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IR4GPGT(1,K),IR4GPGT(NP1,K),
     >                       IR4GPGT(N2P1,K),IR4GPGT(N3P1,K),CS,DUMMY))
C
        FR4GPT_K(K)= CMPLX(SPWERT(NPSI,ZSR,RFR4GPT(1,K),RFR4GPT(NP1,K),
     >                       RFR4GPT(N2P1,K),RFR4GPT(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IFR4GPT(1,K),IFR4GPT(NP1,K),
     >                       IFR4GPT(N2P1,K),IFR4GPT(N3P1,K),CS,DUMMY))
C
        FR4GPS_K(K)= CMPLX(SPWERT(NPSI,ZSR,RFR4GPS(1,K),RFR4GPS(NP1,K),
     >                       RFR4GPS(N2P1,K),RFR4GPS(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IFR4GPS(1,K),IFR4GPS(NP1,K),
     >                       IFR4GPS(N2P1,K),IFR4GPS(N3P1,K),CS,DUMMY))
C
        FR2GPT_K(K)= CMPLX(SPWERT(NPSI,ZSR,RFR2GPT(1,K),RFR2GPT(NP1,K),
     >                       RFR2GPT(N2P1,K),RFR2GPT(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IFR2GPT(1,K),IFR2GPT(NP1,K),
     >                       IFR2GPT(N2P1,K),IFR2GPT(N3P1,K),CS,DUMMY))
C
        DSGPGT_K(K)= CMPLX(SPWERT(NPSI,ZSR,RDSGPGT(1,K),RDSGPGT(NP1,K),
     >                       RDSGPGT(N2P1,K),RDSGPGT(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IDSGPGT(1,K),IDSGPGT(NP1,K),
     >                       IDSGPGT(N2P1,K),IDSGPGT(N3P1,K),CS,DUMMY))
C
        R2GPTGPDSR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RR2GPTGPDSR2(1,K),
     >                                              RR2GPTGPDSR2(NP1,K),
     >             RR2GPTGPDSR2(N2P1,K),RR2GPTGPDSR2(N3P1,K),CS,DUMMY),
     >         SPWERT(NPSI,ZSR,IR2GPTGPDSR2(1,K),IR2GPTGPDSR2(NP1,K),
     >            IR2GPTGPDSR2(N2P1,K),IR2GPTGPDSR2(N3P1,K),CS,DUMMY))
C
        GPSI2DSR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RGPSI2DSR2(1,K),
     >                                              RGPSI2DSR2(NP1,K),
     >             RGPSI2DSR2(N2P1,K),RGPSI2DSR2(N3P1,K),CS,DUMMY),
     >         SPWERT(NPSI,ZSR,IGPSI2DSR2(1,K),IGPSI2DSR2(NP1,K),
     >            IGPSI2DSR2(N2P1,K),IGPSI2DSR2(N3P1,K),CS,DUMMY))
C
        R2GPSI2DSR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RR2GPSI2DSR2(1,K),
     >                                            RR2GPSI2DSR2(NP1,K),
     >            RR2GPSI2DSR2(N2P1,K),RR2GPSI2DSR2(N3P1,K),CS,DUMMY),
     >         SPWERT(NPSI,ZSR,IR2GPSI2DSR2(1,K),IR2GPSI2DSR2(NP1,K),
     >            IR2GPSI2DSR2(N2P1,K),IR2GPSI2DSR2(N3P1,K),CS,DUMMY))
C
        GPSIDSR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RGPSIDSR2(1,K),
     >                                              RGPSIDSR2(NP1,K),
     >             RGPSIDSR2(N2P1,K),RGPSIDSR2(N3P1,K),CS,DUMMY),
     >         SPWERT(NPSI,ZSR,IGPSIDSR2(1,K),IGPSIDSR2(NP1,K),
     >            IGPSIDSR2(N2P1,K),IGPSIDSR2(N3P1,K),CS,DUMMY))
C
       GPGT2DSR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RGPGT2DSR2(1,K),
     >                                              RGPGT2DSR2(NP1,K),
     >             RGPGT2DSR2(N2P1,K),RGPGT2DSR2(N3P1,K),CS,DUMMY),
     >         SPWERT(NPSI,ZSR,IGPGT2DSR2(1,K),IGPGT2DSR2(NP1,K),
     >            IGPGT2DSR2(N2P1,K),IGPGT2DSR2(N3P1,K),CS,DUMMY))
C
       R2GPGT2DSR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RR2GPGT2DSR2(1,K),
     >                                          RR2GPGT2DSR2(NP1,K),
     >          RR2GPGT2DSR2(N2P1,K),RR2GPGT2DSR2(N3P1,K),CS,DUMMY),
     >        SPWERT(NPSI,ZSR,IR2GPGT2DSR2(1,K),IR2GPGT2DSR2(NP1,K),
     >         IR2GPGT2DSR2(N2P1,K),IR2GPGT2DSR2(N3P1,K),CS,DUMMY))
C
       GPGTDSR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RGPGTDSR2(1,K),
     >                                              RGPGTDSR2(NP1,K),
     >             RGPGTDSR2(N2P1,K),RGPGTDSR2(N3P1,K),CS,DUMMY),
     >         SPWERT(NPSI,ZSR,IGPGTDSR2(1,K),IGPGTDSR2(NP1,K),
     >            IGPGTDSR2(N2P1,K),IGPGTDSR2(N3P1,K),CS,DUMMY))
C
       DSR2OR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RDSR2OR2(1,K),
     >                                              RDSR2OR2(NP1,K),
     >             RDSR2OR2(N2P1,K),RDSR2OR2(N3P1,K),CS,DUMMY),
     >         SPWERT(NPSI,ZSR,IDSR2OR2(1,K),IDSR2OR2(NP1,K),
     >            IDSR2OR2(N2P1,K),IDSR2OR2(N3P1,K),CS,DUMMY))
C
       GPGTGPDSR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RGPGTGPDSR2(1,K),
     >                                              RGPGTGPDSR2(NP1,K),
     >             RGPGTGPDSR2(N2P1,K),RGPGTGPDSR2(N3P1,K),CS,DUMMY),
     >         SPWERT(NPSI,ZSR,IGPGTGPDSR2(1,K),IGPGTGPDSR2(NP1,K),
     >            IGPGTGPDSR2(N2P1,K),IGPGTGPDSR2(N3P1,K),CS,DUMMY))
C
        PTOR2DSR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RPTOR2DSR2(1,K),
     >                                              RPTOR2DSR2(NP1,K),
     >             RPTOR2DSR2(N2P1,K),RPTOR2DSR2(N3P1,K),CS,DUMMY),
     >         SPWERT(NPSI,ZSR,IPTOR2DSR2(1,K),IPTOR2DSR2(NP1,K),
     >            IPTOR2DSR2(N2P1,K),IPTOR2DSR2(N3P1,K),CS,DUMMY))
C
        DSR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RDSR2(1,K),
     >                                              RDSR2(NP1,K),
     >             RDSR2(N2P1,K),RDSR2(N3P1,K),CS,DUMMY),
     >         SPWERT(NPSI,ZSR,IDSR2(1,K),IDSR2(NP1,K),
     >            IDSR2(N2P1,K),IDSR2(N3P1,K),CS,DUMMY))
C
        R2GPGT2DTR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RR2GPGT2DTR2(1,K),
     >                                              RR2GPGT2DTR2(NP1,K),
     >             RR2GPGT2DTR2(N2P1,K),RR2GPGT2DTR2(N3P1,K),CS,DUMMY),
     >         SPWERT(NPSI,ZSR,IR2GPGT2DTR2(1,K),IR2GPGT2DTR2(NP1,K),
     >            IR2GPGT2DTR2(N2P1,K),IR2GPGT2DTR2(N3P1,K),CS,DUMMY))
C
        R2GPGTDTR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RR2GPGTDTR2(1,K),
     >                                              RR2GPGTDTR2(NP1,K),
     >             RR2GPGTDTR2(N2P1,K),RR2GPGTDTR2(N3P1,K),CS,DUMMY),
     >         SPWERT(NPSI,ZSR,IR2GPGTDTR2(1,K),IR2GPGTDTR2(NP1,K),
     >            IR2GPGTDTR2(N2P1,K),IR2GPGTDTR2(N3P1,K),CS,DUMMY))
C
        GPGPGTDTR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RGPGPGTDTR2(1,K),
     >                                              RGPGPGTDTR2(NP1,K),
     >             RGPGPGTDTR2(N2P1,K),RGPGPGTDTR2(N3P1,K),CS,DUMMY),
     >         SPWERT(NPSI,ZSR,IGPGPGTDTR2(1,K),IGPGPGTDTR2(NP1,K),
     >            IGPGPGTDTR2(N2P1,K),IGPGPGTDTR2(N3P1,K),CS,DUMMY))
C
        DTR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RDTR2(1,K),
     >                                              RDTR2(NP1,K),
     >             RDTR2(N2P1,K),RDTR2(N3P1,K),CS,DUMMY),
     >         SPWERT(NPSI,ZSR,IDTR2(1,K),IDTR2(NP1,K),
     >            IDTR2(N2P1,K),IDTR2(N3P1,K),CS,DUMMY))
C
        FDTR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RFDTR2(1,K),
     >                                              RFDTR2(NP1,K),
     >             RFDTR2(N2P1,K),RFDTR2(N3P1,K),CS,DUMMY),
     >         SPWERT(NPSI,ZSR,IFDTR2(1,K),IFDTR2(NP1,K),
     >            IFDTR2(N2P1,K),IFDTR2(N3P1,K),CS,DUMMY))
C
        DTR2OR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RDTR2OR2(1,K),
     >                                              RDTR2OR2(NP1,K),
     >             RDTR2OR2(N2P1,K),RDTR2OR2(N3P1,K),CS,DUMMY),
     >         SPWERT(NPSI,ZSR,IDTR2OR2(1,K),IDTR2OR2(NP1,K),
     >            IDTR2OR2(N2P1,K),IDTR2OR2(N3P1,K),CS,DUMMY))
C
        DTR2OR4_K(K)= CMPLX(SPWERT(NPSI,ZSR,RDTR2OR4(1,K),
     >                                              RDTR2OR4(NP1,K),
     >             RDTR2OR4(N2P1,K),RDTR2OR4(N3P1,K),CS,DUMMY),
     >         SPWERT(NPSI,ZSR,IDTR2OR4(1,K),IDTR2OR4(NP1,K),
     >            IDTR2OR4(N2P1,K),IDTR2OR4(N3P1,K),CS,DUMMY))
C
        GPSIOR2DSR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RGPSIOR2DSR2(1,K),
     >                                            RGPSIOR2DSR2(NP1,K),
     >             RGPSIOR2DSR2(N2P1,K),RGPSIOR2DSR2(N3P1,K),CS,DUMMY),
     >         SPWERT(NPSI,ZSR,IGPSIOR2DSR2(1,K),IGPSIOR2DSR2(NP1,K),
     >            IGPSIOR2DSR2(N2P1,K),IGPSIOR2DSR2(N3P1,K),CS,DUMMY))
C
        R4GPTOGP_K(K)= CMPLX(SPWERT(NPSI,ZSR,RR4GPTOGP(1,K),
     >                                            RR4GPTOGP(NP1,K),
     >             RR4GPTOGP(N2P1,K),RR4GPTOGP(N3P1,K),CS,DUMMY),
     >         SPWERT(NPSI,ZSR,IR4GPTOGP(1,K),IR4GPTOGP(NP1,K),
     >            IR4GPTOGP(N2P1,K),IR4GPTOGP(N3P1,K),CS,DUMMY))
C
        R2GPTDSR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RR2GPTDSR2(1,K),
     >                                            RR2GPTDSR2(NP1,K),
     >             RR2GPTDSR2(N2P1,K),RR2GPTDSR2(N3P1,K),CS,DUMMY),
     >         SPWERT(NPSI,ZSR,IR2GPTDSR2(1,K),IR2GPTDSR2(NP1,K),
     >            IR2GPTDSR2(N2P1,K),IR2GPTDSR2(N3P1,K),CS,DUMMY))
C
        R2GPDSR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RR2GPDSR2(1,K),
     >                                            RR2GPDSR2(NP1,K),
     >             RR2GPDSR2(N2P1,K),RR2GPDSR2(N3P1,K),CS,DUMMY),
     >         SPWERT(NPSI,ZSR,IR2GPDSR2(1,K),IR2GPDSR2(NP1,K),
     >            IR2GPDSR2(N2P1,K),IR2GPDSR2(N3P1,K),CS,DUMMY))
C
       GPTDTR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RGPTDTR2(1,K),
     >                                            RGPTDTR2(NP1,K),
     >             RGPTDTR2(N2P1,K),RGPTDTR2(N3P1,K),CS,DUMMY),
     >         SPWERT(NPSI,ZSR,IGPTDTR2(1,K),IGPTDTR2(NP1,K),
     >            IGPTDTR2(N2P1,K),IGPTDTR2(N3P1,K),CS,DUMMY))
C
       R2GGG2DSR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RR2GGG2DSR2(1,K),
     >                                            RR2GGG2DSR2(NP1,K),
     >             RR2GGG2DSR2(N2P1,K),RR2GGG2DSR2(N3P1,K),CS,DUMMY),
     >         SPWERT(NPSI,ZSR,IR2GGG2DSR2(1,K),IR2GGG2DSR2(NP1,K),
     >            IR2GGG2DSR2(N2P1,K),IR2GGG2DSR2(N3P1,K),CS,DUMMY))
C
        R2GPGT3DTR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RR2GPGT3DTR2(1,K),
     >                                              RR2GPGT3DTR2(NP1,K),
     >             RR2GPGT3DTR2(N2P1,K),RR2GPGT3DTR2(N3P1,K),CS,DUMMY),
     >         SPWERT(NPSI,ZSR,IR2GPGT3DTR2(1,K),IR2GPGT3DTR2(NP1,K),
     >            IR2GPGT3DTR2(N2P1,K),IR2GPGT3DTR2(N3P1,K),CS,DUMMY))
C
        GPGPT2DSR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RGPGPT2DSR2(1,K),
     >                                              RGPGPT2DSR2(NP1,K),
     >             RGPGPT2DSR2(N2P1,K),RGPGPT2DSR2(N3P1,K),CS,DUMMY),
     >         SPWERT(NPSI,ZSR,IGPGPT2DSR2(1,K),IGPGPT2DSR2(NP1,K),
     >            IGPGPT2DSR2(N2P1,K),IGPGPT2DSR2(N3P1,K),CS,DUMMY))
C
        GPGT3DTR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RGPGT3DTR2(1,K),
     >                                              RGPGT3DTR2(NP1,K),
     >             RGPGT3DTR2(N2P1,K),RGPGT3DTR2(N3P1,K),CS,DUMMY),
     >         SPWERT(NPSI,ZSR,IGPGT3DTR2(1,K),IGPGT3DTR2(NP1,K),
     >            IGPGT3DTR2(N2P1,K),IGPGT3DTR2(N3P1,K),CS,DUMMY))
C
        R2GP2GPTDSR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RR2GP2GPTDSR2(1,K),
     >                                            RR2GP2GPTDSR2(NP1,K),
     >           RR2GP2GPTDSR2(N2P1,K),RR2GP2GPTDSR2(N3P1,K),CS,DUMMY),
     >       SPWERT(NPSI,ZSR,IR2GP2GPTDSR2(1,K),IR2GP2GPTDSR2(NP1,K),
     >          IR2GP2GPTDSR2(N2P1,K),IR2GP2GPTDSR2(N3P1,K),CS,DUMMY))
C
        R2GGG2DTR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RR2GGG2DTR2(1,K),
     >                                            RR2GGG2DTR2(NP1,K),
     >           RR2GGG2DTR2(N2P1,K),RR2GGG2DTR2(N3P1,K),CS,DUMMY),
     >       SPWERT(NPSI,ZSR,IR2GGG2DTR2(1,K),IR2GGG2DTR2(NP1,K),
     >          IR2GGG2DTR2(N2P1,K),IR2GGG2DTR2(N3P1,K),CS,DUMMY))
C
        R2GPDTR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RR2GPDTR2(1,K),
     >                                            RR2GPDTR2(NP1,K),
     >           RR2GPDTR2(N2P1,K),RR2GPDTR2(N3P1,K),CS,DUMMY),
     >       SPWERT(NPSI,ZSR,IR2GPDTR2(1,K),IR2GPDTR2(NP1,K),
     >          IR2GPDTR2(N2P1,K),IR2GPDTR2(N3P1,K),CS,DUMMY))
C
        GPSIDTR2_K(K)= CMPLX(SPWERT(NPSI,ZSR,RGPSIDTR2(1,K),
     >                                            RGPSIDTR2(NP1,K),
     >           RGPSIDTR2(N2P1,K),RGPSIDTR2(N3P1,K),CS,DUMMY),
     >       SPWERT(NPSI,ZSR,IGPSIDTR2(1,K),IGPSIDTR2(NP1,K),
     >          IGPSIDTR2(N2P1,K),IGPSIDTR2(N3P1,K),CS,DUMMY))
C

      ENDDO

      DO 100  MS = 1 , MANZ
     
      SMS(1) = MSTART(NI)   + FLOAT(MS-1)
      SMS(2) = MSTART(NI+1) + FLOAT(MS-1)
      SMS(3) = MSTART(NI)   + FLOAT(MS-1)
      SMS(4) = MSTART(NI+1) + FLOAT(MS-1)

      MSNQ(1:4) = SMS(1:4) + ZNKWEL*ZQ

      DO 100  MZ = 1,MANZ

      SMZ(1) = MSTART(NI)   + FLOAT(MZ-1)
      SMZ(2) = MSTART(NI)   + FLOAT(MZ-1)
      SMZ(3) = MSTART(NI+1) + FLOAT(MZ-1)
      SMZ(4) = MSTART(NI+1) + FLOAT(MZ-1)

      MZNQ(1:4) = SMZ(1:4) + ZNKWEL*ZQ
            
      DO I4=1,4
      
        FKDUMMY(I4) = 0.

        IF (SMZ(I4) .EQ. SMS(I4)) FKDUMMY(I4) = 1. 
        KI = ABS(SMS(I4) - SMZ(I4)) + 1
        IF ( (SMZ(I4) - SMS(I4) ) .GE. 0) THEN
          R2(I4)     = R2_K(KI)
          B02(I4)    = B02_K(KI)                        
          GPSI(I4)   = GPSI_K(KI)		 		 
          GPGT(I4)   = GPGT_K(KI)
          GPGT2(I4)  = GPGT2_K(KI)
          GPSI2(I4)  = GPSI2_K(KI)
          GPDSGP(I4) = GPDSGP_K(KI)
          R2DSGP(I4) = R2DSGP_K(KI)
          GPTDSG(I4) = GPTDSG_K(KI)
          GPGPGT(I4) = GPGPGT_K(KI)
	  R4GPT2(I4) = R4GPT2_K(KI)
          R4GPSI(I4) = R4GPSI_K(KI)
          GPSIOR(I4) = GPSIOR_K(KI)
          R4GPGT(I4) = R4GPGT_K(KI)
          R2GPSI(I4) = R2GPSI_K(KI)
	  OOR2(I4)   = OOR2_K(KI) 
          SOR2GP(I4) = SOR2GP_K(KI)	
          SOGPSI(I4) = SOGPSI_K(KI)		 		 
          SGGG(I4)   = SGGG_K(KI)		 		 
          DSR2(I4)   = DSR2_K(KI) 		 		 
          DSGPSI(I4) = DSGPSI_K(KI)
	  FR4GPS(I4) = FR4GPS_K(KI) 
	  FR4GPT(I4) = FR4GPT_K(KI) 
	  SR2OGP(I4) = SR2OGP_K(KI) 
	  SR4GGG(I4) = SR4GGG_K(KI) 
	  SR2GGG(I4) = SR2GGG_K(KI) 
	  FR2(I4)    = FR2_K(KI)      
	  FR2GPS(I4) = FR2GPS_K(KI)
          FR2GPT(I4) = FR2GPT_K(KI)
          DSGPGT(I4) = DSGPGT_K(KI)
          DTR2(I4)   = DTR2_K(KI)
          FDTR2(I4)   = FDTR2_K(KI)
          DTR2OR2(I4)   = DTR2OR2_K(KI)
          DTR2OR4(I4)   = DTR2OR4_K(KI)
          GPSIOR2DSR2(I4)   = GPSIOR2DSR2_K(KI)
          R2GPTGPDSR2(I4)   = R2GPTGPDSR2_K(KI)
          GPSI2DSR2(I4)   = GPSI2DSR2_K(KI)
          R2GPSI2DSR2(I4)   = R2GPSI2DSR2_K(KI)
          GPSIDSR2(I4)   = GPSIDSR2_K(KI)
          GPGT2DSR2(I4)   = GPGT2DSR2_K(KI)
          R2GPGT2DSR2(I4)   = R2GPGT2DSR2_K(KI)
          GPGTDSR2(I4)   = GPGTDSR2_K(KI)
          DSR2OR2(I4)   = DSR2OR2_K(KI)
          GPGTGPDSR2(I4)   = GPGTGPDSR2_K(KI)
          GPGT2DTR2(I4)   = GPGT2DTR2_K(KI)
          PTOR2DSR2(I4)   = PTOR2DSR2_K(KI)
          DSR2(I4)   = DSR2_K(KI)
          R2GPGT2DTR2(I4)   = R2GPGT2DTR2_K(KI)
          R2GPGTDTR2(I4)   = R2GPGTDTR2_K(KI)
          GPGPGTDTR2(I4)   = GPGPGTDTR2_K(KI)
          R4GPTOGP(I4)    = R4GPTOGP_K(KI)
          R2GPTDSR2(I4)   = R2GPTDSR2_K(KI)
          R2GPDSR2(I4)    = R2GPDSR2_K(KI)
          GPTDTR2(I4)     = GPTDTR2_K(KI)
          R2GGG2DSR2(I4)  = R2GGG2DSR2_K(KI)
          R2GPGT3DTR2(I4) = R2GPGT3DTR2_K(KI)
          GPGPT2DSR2(I4)  = GPGPT2DSR2_K(KI)
          GPGT3DTR2(I4)   = GPGT3DTR2_K(KI) 
          R2GP2GPTDSR2(I4)= R2GP2GPTDSR2_K(KI)
          R2GGG2DTR2(I4)  = R2GGG2DTR2_K(KI)
          R2GPDTR2(I4)    = R2GPDTR2_K(KI)
          GPSIDTR2(I4)    = GPSIDTR2_K(KI)
        ELSE
          R2(I4)     = CONJG(R2_K(KI))
          B02(I4)    = CONJG(B02_K(KI))		 		 
          GPSI(I4)   = CONJG(GPSI_K(KI))		 
          GPGT(I4)   = CONJG(GPGT_K(KI))
          GPGT2(I4)  = CONJG(GPGT2_K(KI))
          GPSI2(I4)  = CONJG(GPSI2_K(KI))
          GPDSGP(I4) = CONJG(GPDSGP_K(KI))
          GPGPGT(I4) = CONJG(GPGPGT_K(KI)) 
	  R4GPT2(I4) = CONJG(R4GPT2_K(KI))
          R4GPGT(I4) = CONJG(R4GPGT_K(KI))
          R4GPSI(I4) = CONJG(R4GPSI_K(KI))
          GPSIOR(I4) = CONJG(GPSIOR_K(KI))
          R2DSGP(I4) = CONJG(R2DSGP_K(KI))
          GPTDSG(I4) = CONJG(GPTDSG_K(KI))
          R2GPSI(I4) = CONJG(R2GPSI_K(KI))
	  OOR2(I4)   = CONJG(OOR2_K(KI)) 
          SOR2GP(I4) = CONJG(SOR2GP_K(KI))
          SOGPSI(I4) = CONJG(SOGPSI_K(KI))
          SGGG(I4)   = CONJG(SGGG_K(KI))
          DSR2(I4)   = CONJG(DSR2_K(KI))
          DSGPSI(I4) = CONJG(DSGPSI_K(KI))
  	  FR4GPS(I4) = CONJG(FR4GPS_K(KI))
	  FR4GPT(I4) = CONJG(FR4GPT_K(KI))
	  SR2OGP(I4) = CONJG(SR2OGP_K(KI)) 
	  SR4GGG(I4) = CONJG(SR4GGG_K(KI)) 
	  SR2GGG(I4) = CONJG(SR2GGG_K(KI)) 
	  FR2(I4)    = CONJG(FR2_K(KI))
	  FR2GPS(I4) = CONJG(FR2GPS_K(KI))   
          FR2GPT(I4) = CONJG(FR2GPT_K(KI)) 
          DSGPGT(I4) = CONJG(DSGPGT_K(KI))
          DTR2(I4)   = CONJG(DTR2_K(KI))
          FDTR2(I4)   = CONJG(FDTR2_K(KI))
          DTR2OR2(I4)   = CONJG(DTR2OR2_K(KI))
          DTR2OR4(I4)   = CONJG(DTR2OR4_K(KI))
          GPSIOR2DSR2(I4)   = CONJG(GPSIOR2DSR2_K(KI))
          R2GPTGPDSR2(I4)   = CONJG(R2GPTGPDSR2_K(KI))
          GPSI2DSR2(I4)   = CONJG(GPSI2DSR2_K(KI))
          GPSIDSR2(I4)   = CONJG(GPSIDSR2_K(KI))
          R2GPSI2DSR2(I4)   = CONJG(R2GPSI2DSR2_K(KI))
          GPGT2DSR2(I4)   = CONJG(GPGT2DSR2_K(KI))
          R2GPGT2DSR2(I4)   = CONJG(R2GPGT2DSR2_K(KI))
          GPGTDSR2(I4)   = CONJG(GPGTDSR2_K(KI))
          DSR2OR2(I4)   = CONJG(DSR2OR2_K(KI))
          GPGTGPDSR2(I4)   = CONJG(GPGTGPDSR2_K(KI))
          GPGT2DTR2(I4)   = CONJG(GPGT2DTR2_K(KI))
          PTOR2DSR2(I4)   = CONJG(PTOR2DSR2_K(KI))
          DSR2(I4)   = CONJG(DSR2_K(KI))
          R2GPGT2DTR2(I4)   = CONJG(R2GPGT2DTR2_K(KI))
          R2GPGTDTR2(I4)   = CONJG(R2GPGTDTR2_K(KI))
          GPGPGTDTR2(I4)   = CONJG(GPGPGTDTR2_K(KI))
          R4GPTOGP(I4)    = CONJG(R4GPTOGP_K(KI))
          R2GPTDSR2(I4)   = CONJG(R2GPTDSR2_K(KI))
          R2GPDSR2(I4)    = CONJG(R2GPDSR2_K(KI))
          GPTDTR2(I4)     = CONJG(GPTDTR2_K(KI))
          R2GGG2DSR2(I4)  = CONJG(R2GGG2DSR2_K(KI))
          R2GPGT3DTR2(I4) = CONJG(R2GPGT3DTR2_K(KI))
          GPGPT2DSR2(I4)  = CONJG(GPGPT2DSR2_K(KI))
          GPGT3DTR2(I4)   = CONJG(GPGT3DTR2_K(KI)) 
          R2GP2GPTDSR2(I4)= CONJG(R2GP2GPTDSR2_K(KI)) 
          R2GGG2DTR2(I4)  = CONJG(R2GGG2DTR2_K(KI)) 
          R2GPDTR2(I4)    = CONJG(R2GPDTR2_K(KI))
          GPSIDTR2(I4)    = CONJG(GPSIDTR2_K(KI))
       	 ENDIF
      ENDDO


C      DO 100  MS = 1 , MANZ - KF + 1
C     ------------------------------
C
C      MZ = MS + KF - 1
C
C  
C      SMZ  = RFOUR(MZ)
C      SMS  = RFOUR(MS)
C      MZNQ = SMZ+ZNKWEL*ZQ
C      MSNQ = SMS+ZNKWEL*ZQ
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(CUB)   FUER DIE GLEICHUNGEN
C     ----------------------
C     A(1,4),  A(4,1),  A(7,1), A(1,7), A(5,4),
C      v1A2,    A2v1,     pv1,    v1p,    A3A2,  
C
C     A(4,4),  A(4,5), A(1,5), A(5,5), A(7,7), A(5,7), A(4,7),
C       A2A2,   A2A3,   v1A3,   A3A3
C
C     A(1,1)
C
      FACT(1:4,1) = + 2.* DZQ**2 / (SPS2*ZT*ZQ**3) * GPSI
     >          - DZQ*DZT /(SPS2*ZT*ZT*ZQ*ZQ)  * GPSI
     >          + 2.*(0.,1.)*(SMZ-SMS)*DZQ/(ZT*ZQ*ZQ) * GPGT
     >          + SPS2*MZNQ*MSNQ/(ZT*ZQ*ZSR*ZSR) 
     >                                *(SGGG+TOQ**2 * SOR2GP)
     >          + DZQ / (SPS2*ZT*ZQ*ZQ) * DSGPSI 
      FACT(1:4,2) = - ZT*FKDUMMY - GPSI/ZT 

      FACT(1:4,3) = + (GAMMA-1.) * DT0 / ZT * R2
     >          - (0.,1.)* SPS2 * MZNQ*GAMMA * T0 * FR2GPT /ZT**3 

      FACT(1:4,4) = DSR2 / (SPS2*ZT) - DZT/(SPS2*ZT**2) * R2
      FACT(1:4,5) = - ZETA * SPS2/(ZT*ZQ) * MSNQ * ZNKWEL * OOR2
     >          + ZETA * (0.,1.)*SMZ/(ZT*ZQ) * DZQ * GPGT
     >          + ZETA * SPS2 * SMZ/(ZT*ZSR**2) * MSNQ *
     >                  (SGGG + (ZT/ZQ)**2 * SOR2GP)
     >          - (0.,1.) * ZTE * DT0 * MSNQ*FKDUMMY/(ZRHO*ZQ*ZT)
     >          -(0.,1.)*MSNQ/(ZQ*ZT)*V03*SPS2*FKDUMMY
      FACT(1:4,6) = 0.
     >           - ZETA * ZT/ZQ**2 * MZNQ * MSNQ * OOR2
     >            - (0.,1.)*V03*MSNQ*ZT/ZQ*FKDUMMY
      FACT(1:4,7) = 0.
     >           + ZETA * ZT/ZQ * SMS * MZNQ * OOR2 
     >            + (0.,1.)*V03*ZT*(SMS*FKDUMMY
     >                                -ZNKWEL*ZQ*GPSI/ZT**2)
      FACT(1:4,8) = DZT/(ZT*SPS2)*(DZT*FKDUMMY + DT0/ZT * R2 )
     >         + DZT**2/(SPS2*ZT**3) * GPSI
     >   + SPS2*ZNKWEL/(ZT*ZQ) * MZNQ * OOR2
     >   - SPS2*ZT*SMS*MZNQ/(ZQ*ZQ*ZSR**2) *
     >                  (SOR2GP + (ZQ/ZT)**2 * SGGG)
     >   + (0.,1.)*(SMS*DZQ/(ZT*ZQ) - (SMS-SMZ)/ZT**2 * DZT)*GPGT
     >   + ZNKWEL*SPS2*ZQ/ZT**3 * MZNQ * GPGT2
     >   - (0.,1.) * ZNKWEL*DZQ/ZT**3 * GPGPGT
      FACT(1:4,9)=+ZETA*(0.,1.)*SMZ*ZQ/ZT**3*(DZQ/ZQ-2.*DZT/ZT)*GPGPGT     
     >           + ZETA * DZT*ZQ/(SPS2*ZT**4) * GPDSGP
     >   + ZETA*ZQ/(SPS2*ZT**3) * (DZQ/ZQ-2.*DZT/ZT) * 
     >                            ( ZT*DZT*GPSI + DT0 * R2GPSI)  
     >   - ZETA * ZNKWEL**2 * SPS2*ZQ/ZT**3 * GPSIOR   
     >   + ZETA * ZQ*DZT/(SPS2*ZT**4) * (DZQ/ZQ-2.*DZT/ZT)*GPSI2
     >   + ZETA * ZQ/(SPS2*ZT**3) * (ZT*DZT * DSGPSI + DT0 * R2DSGP)
     >   + ZETA * (0.,1.)*SMZ*ZQ/ZT**3 * GPTDSG 
     >   - ZETA * SPS2*SMS*SMZ*ZQ/(ZT*ZSR**2) *
     >                      (SGGG + (ZT/ZQ)**2 * SOR2GP)
     >   + ZETA * SPS2*SMZ*ZNKWEL*ZQ**2/ZT**3 * GPGT2
     >   + ZETA * SPS2*(SMZ+SMS)*ZNKWEL/ZT * OOR2
     >   + (0.,1.) * ZTE*SMS*DT0*FKDUMMY/(ZT*ZRHO)
     >   - (0.,1.) * ZTE*ZNKWEL*ZQ*DT0/(ZRHO*ZT**3) * GPSI
     >   + (0.,1.)*SPS2/ZT*V03*(SMS*FKDUMMY
     >                                   -ZNKWEL*ZQ*GPSI/ZT**2)
      FACT(1:4,10) = - (0.,1.) * ZTI * DT0 * SMS /(SPS2*ZT*ZRHO) * FR2
     >     + (0.,1.) * ZNKWEL*ZQ*DT0*ZTI/(ZRHO*SPS2*ZT**3) * FR2GPS
     >      -(0.,1.)*ZNKWEL*V03*ZQ*R2/ZT
      FACT(1:4,11) = + (0.,1.)*ZTE*MSNQ*FKDUMMY/(ZRHO*ZT)
      FACT(1:4,12) = - (0.,1.)*ZTI*ZT*SMS/(SPS2*ZRHO)*FKDUMMY
     >           + (0.,1.)*ZTI*ZNKWEL*ZQ/(SPS2*ZT*ZRHO) * GPSI 
      FACT(1:4,13) = -(0.,1.)*ZRHO*ZNKWEL*SPS2*V03/
     >                                     (ZT*ZQ)*SR2OGP/ZSR**2
     >          - (0.,1.)*ZRHO*ZNKWEL*V03*SPS2*ZQ/ZT**3*SR4GGG/ZSR**2
     >             + (0.,1.)*ZRHO*ZNKWEL*SPS2*ZQ*V03/ZT**3*R4GPT2
     >             + ZRHO*V03/ZT * R2GPTDSR2
C      FACT(1:4,13) =  (- (0.,1.)*ZRHO*ZNKWEL*V03/(SPS2**2*ZQ**2)
C     >         + ZRHO*R2GPTGPDSR2/SPS2**3*V03
C     >         + ZRHO*R2GPGT2DTR2/SPS2**2*V03)
C     >       + CW / (SPS2**2 * ZT**3 * ZQ) *                        
C     >           (DT0*DZQ*SPS2 + DT0*DSPS*ZQ + DT0*SPS2*ZQ*DZRHO/ZRHO 
C     >          - DDT0*SPS2*ZQ) * FR4GPT                                
C     >          - (0.,1.)*CW*SMS/(ZT**3 * ZSR**2) * DT0 *               
C     >                                (SR4GGG + ZT**2/ZQ**2 * SR2OGP)   
C
      CALL FKUBL(MZ,MS,MANZ,13,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),HC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     ----------------------
C     A(1,3), A(7,2), A(5,3), A(4,3), A(7,6), A(1,2), A(1,6), A(6,1)
C      v1A1,   pv2,    A3A1,   A2A1,   pv3
C
      FACT(1:4,1) = -(0.,1.)*ZNKWEL*(2*SMZ-SMS+ZNKWEL*ZQ)/ZT * GPGT
     >          + ZNKWEL*(ZQ*DZT-2*ZT*DZQ)/(SPS2*ZT*ZT*ZQ) * GPSI  
     >          - ZNKWEL/(SPS2*ZT) * DSGPSI
     >          + SMZ * DZT / (SPS2*ZQ) * FKDUMMY
      FACT(1:4,2) =-GAMMA*T0 /ZT*(SMZ*FR2 - ZNKWEL*ZQ/ZT**2 * FR2GPS) 
      FACT(1:4,3) = - ZETA*SMS/(ZT*SPS2)*(ZT*DZT*FKDUMMY + DT0*R2)
     >          - ZETA*SMS/(ZT**2 * SPS2) * DZT * GPSI
     >          - ZETA*(0.,1.)*SMZ/ZT * MSNQ * GPGT 
      FACT(1:4,4) = 0.
     >        + ZETA * SMS*ZT/(SPS2**2) * DSGPSI
     >        + ZETA * SMS*ZT/(SPS2**2) *(DZQ/ZQ-DSPS/SPS2)*GPSI
     >        - ZETA * (0.,1.)* SMS**2 * ZT/SPS2 * GPGT
      FACT(1:4,5) = - GAMMA * T0 * MSNQ * FKDUMMY
      FACT(1:4,6) = ZRHO*ZNKWEL*ZQ*V03/ZT * R4GPGT
     >           +(0.,1.)*ZRHO*V03/(SPS2*ZT)*R2GPDSR2
C      FACT(1:4,6) = ((0.,1.)*ZRHO*GPSI2DSR2/(SPS2**3*ZT**2)*V03
C     >            + (0.,1.)*ZRHO*GPGTGPDSR2/(SPS2**2*ZT**2)*V03)
C      FACT(1:4,7) = -ZRHO*V03*ZNKWEL/ZT*GPGT
      FACT(1:4,7) = -(0.,1.)*ZRHO*V03*DSR2
     >         + ZNKWEL*ZRHO*SPS2*ZQ*V03/ZT**2*R2GPGT
C      FACT(1:4,8) = -ZRHO*V03*ZNKWEL*SPS2*ZQ*ZQ/ZT**2*R2GPGT
      FACT(1:4,8) = -(0.,1.)*ZRHO*V03*DSR2
     >              -(0.,1.)*ZRHO*SPS2*V03*GPTDTR2
     >           -(0.,1.)*ZRHO*SPS2**2*ZQ**2*V03/ZT**2*
     >          (R2GGG2DSR2/SPS2**2+R2GPGT3DTR2/SPS2)
     >          +(0.,1.)*ZRHO*SPS2*ZQ**2*V03/ZT**2*
     >       (GPGPT2DSR2/SPS2 + ZT**2/ZQ**2*PTOR2DTR2 + GPGT3DTR2)
C
      CALL FKUBL(MZ,MS,MANZ,8,INDCQ,NBG,NZMA,ZMA,FACT,GEWI(I),HC,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(CUB)   FUER DIE GLEICHUNGEN
C     ----------------------
C     A(2,4),  A(2,7), A(3,4), A(3,5), A(2,5), A(6,5), A(6,7), A(6,4), A(3,7), A(2,1)
C      v2A2,    v2p,    A1A2,   A1A3,   v2A3,   v3A3,   v3p,    v3A2,  A1p
C
      FACT(1:4,1) = - (0.,1.)*MSNQ*(SMS-SMZ+ZNKWEL*ZQ)/(ZQ*ZT) * GPGT
     >          + (SMS+2*ZNKWEL*ZQ)/(SPS2*ZQ*ZQ*ZT) * DZQ  * GPSI
     >          - MSNQ * DZT/(SPS2*ZT*ZT*ZQ) * GPSI
     >          + MSNQ/(SPS2*ZT*ZQ)          * DSGPSI
      FACT(1:4,2) = + SMS / (SPS2*ZT) * R2
      FACT(1:4,3) = ZETA1 * ZNKWEL * DZQ/ZQ**2 * OOR2
     >           +(0.,1.)*V03*DZQ/(ZQ**2)*FKDUMMY
      FACT(1:4,4) = 0.
      FACT(1:4,5) = - MZNQ*DZT/(SPS2*ZT*ZT) * GPSI
     >          - ZNKWEL*ZQ*DT0/(SPS2*ZT**3) * R2GPSI
     >          + (SMS-SMZ) * DT0 * R2 /(SPS2 * ZT)
     >          + (SMS-SMZ) * DZT/SPS2 * FKDUMMY
     >          + (0.,1.)*(SMZ*SMS-SMZ**2+SMS*ZNKWEL*ZQ)/ZT * GPGT
     >          - (0.,1.)*(ZNKWEL*ZQ)**2/ZT**3 * GPGPGT 
      FACT(1:4,6) = SMS * DT0 * FKDUMMY 
     >          - ZNKWEL * ZQ / ZT**2 * DT0 * GPSI
      FACT(1:4,7) = + MSNQ * FKDUMMY 
      FACT(1:4,8) = - MSNQ/ZQ * DT0 * FKDUMMY
      FACT(1:4,9) = + (0.,1.)*ZTI*DZRHO*FKDUMMY/(SPS2*ZRHO*ZRHO)
     >          + (0.,1.)*ZTI*DSPS*FKDUMMY/(SPS2*SPS2*ZRHO)
     >          +((0.,1.)*CWW*FKDUMMY*DZRHO/(SPS2*ZRHO*ZRHO)
     >          + (0.,1.)*CWW*FKDUMMY*DSPS/(SPS2*SPS2*ZRHO))
      FACT(1:4,10) = -(-ZRHO*ZNKWEL*ZQ*V03/ZT*R4GPGT
     >          +(0.,1.)*ZRHO*V03/ZT* R2GPGTDTR2)
C      FACT(1:4,10) = (- ZRHO*ZNKWEL*V03*GPGT/(ZT*SPS2*ZQ)
C     >           -(0.,1.)*ZRHO/(SPS2*ZT)*V03*
C     >         (GPGT2DSR2/SPS2+0))
C     >          + (0.,1.) * CW /(SPS2**3 * ZT**3 * ZQ)                 
C     >          * (DDT0 * SPS2 * ZQ - DT0*DZQ *SPS2 - DT0*DSPS*ZQ     
C     >            - DT0 * SPS2 * ZQ * DZRHO/ZRHO) * FR4GPS             
C     >          - CW * SMS/(SPS2*ZT**3) * DT0 * FR4GPT               
C     
      CALL FKUBL(MZ,MS,MANZ,10,INDQC,NBG,NZMA,ZMA,FACT,GEWI(I),HQ,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     ----------------------
C     A(2,3),  A(3,2),  A(3,3), A(2,2), A(6,6), A(2,6), A(6,2)
C      v2A1,    A1v2,    A1A1
C
      FACT(1:4,1) = - ZNKWEL**2 * ZQ/(SPS2*ZT) * GPSI 
     >          - SMS*SMS* ZT/(SPS2*ZQ) * FKDUMMY 
      FACT(1:4,2) = FKDUMMY 
      FACT(1:4,3) = - ZETA1 * ZNKWEL**2 * OOR2
     >          - ZETA1 * SMS**2/ZSR**2 *((ZT/ZQ)**2 * SOR2GP + SGGG)  
     >          + (0.,1.)*ZETA1 * SMS/SPS2 *(DZT/ZT-DZQ/ZQ) * GPGT
     >          - (0.,1.)* ZETA1 * SMS/SPS2 * DSGPGT
     >          - (0.,1.)*ZNKWEL*V03*FKDUMMY
      FACT(1:4,4) = -(-(0.,1.)*ZRHO*ZNKWEL*ZQ*V03/
     >                                     (SPS2*ZT)*R4GPSI
     >         - ZRHO*V03/(SPS2*ZT)*R2GPDTR2)
C      FACT(1:4,4) = (0.,1.)*ZNKWEL*ZRHO*ZT*V03*FKDUMMY/(SPS2**2*ZQ)
C     >        +ZRHO*V03/(ZT*SPS2**2) *
C     >        (GPGTGPDSR2/SPS2+0)
      FACT(1:4,5) = -(0.,1.)*ZRHO*ZNKWEL*SPS2*ZQ/ZT*V03*B02
     >           -ZRHO*SPS2*ZT*V03*DTROR2
     >           -ZRHO*SPS2**2*ZQ**2*V03/ZT*
     >           (GPGTGPDSR2/SPS2**2 + GPGT2DTR2/SPS2)
     >          +ZRHO*SPS2*ZQ**2*V03/ZT*
     >          (GPGTGPDSR2/SPS2 + ZT**2/ZQ**2*DTR2OR2 + GPGT2DTR2)
C      FACT(1:4,5) = ((0.,1.)*ZNKWEL*ZRHO*ZT*V03*OOR2
C     >           + ZRHO*ZT*V03/ZQ*DTR2OR4)
C      FACT(1:4,6) = ((0.,1.)*ZT*ZNKWEL*ZRHO*V03/(SPS2*ZQ**2)*OOR2
C     >          -ZT*ZRHO*V03/(SPS2*ZQ) * (PTOR2DSR2/SPS2+0))
      FACT(1:4,6) = -(ZRHO*V03*DTR2
     >           -(0.,1.)*ZNKWEL*ZRHO*ZQ/ZT**2*V03*R2GPSI)
C      FACT(1:4,7) = -(0.,1.)*ZRHO*V03*ZNKWEL*R2GPSI/(SPS2*ZT**2)
      FACT(1:4,7) = - ZRHO*ZT*2*V03*FDTR2
     >          +ZRHO*SPS2*ZQ**2*V03/ZT**2*
     >          (R2GP2GPTDSR2/SPS2**2 + R2GGG2DTR2/SPS2)
     >          -ZRHO*ZQ**2*V03/ZT**2*
     >        (R2GP2GPTDSR2/SPS2 + ZT**2/ZQ**2*GPSIDTR2 + R2GGG2DTR2)
C
      CALL FKUBL(MZ,MS,MANZ,7,INDQQ,NBG,NZMA,ZMA,FACT,GEWI(I),HQ,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H'(CUB)   FUER DIE GLEICHUNG
C     ------------------------
C     A(1',4'), A(1',5'), A(5',5'), A(4',4')
C     dv1dA2,   dv1dA3,   dA3dA3,    dA2dA2
C
      FACT(1:4,1) =  ZT/(SPS2*ZQ) * FKDUMMY + GPSI / (SPS2*ZT*ZQ)
      FACT(1:4,2) = 0.
      FACT(1:4,3) = - ZETA * GPSI * ZQ / (SPS2 * ZT)
     >          - ZETA * GPSI2 * ZQ / (SPS2 * ZT**3) 
      FACT(1:4,4) = - ZETA * ZT /(SPS2**2) * GPSI
     >          - ZETA /(ZT*SPS2**2) * GPSI2 
C
      CALL FKUBL(MZ,MS,MANZ,4,IDCDC,NBG,NZMA,ZMA,FACT,GEWI(I),DHC,DHC)
C      
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(1',3),  A(5',3), A(4',3)
C     dv1A1,    dA3A1,    dA2A1
C
      FACT(1:4,1) = ZNKWEL/(SPS2*ZT) * GPSI - SMS*ZT/(SPS2*ZQ)*FKDUMMY
      FACT(1:4,2) = + ZETA * MSNQ * GPSI / (SPS2 * ZT)
      FACT(1:4,3) = + ZETA * SMS * ZT / (SPS2**2) * GPSI
     >          - ZETA * ZNKWEL*ZQ/(ZT*SPS2**2) * GPSI2 
C
      CALL FKUBL(MZ,MS,MANZ,3,IDCQ,NBG,NZMA,ZMA,FACT,GEWI(I),DHC,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H'(CUB)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(2,4'), A(2,5'), A(3,4'), A(3,5'), A(3,7'), A(2,1')
C     v2dA2,    v2dA3,   A1dA2,   A1dA3
C
      FACT(1:4,1) = - ZNKWEL/(SPS2*ZT) * GPSI + SMS*ZT/(SPS2*ZQ)*FKDUMMY
      FACT(1:4,2) = MZNQ / (ZT * SPS2) * GPSI
      FACT(1:4,3) = - ZETA1 * ZNKWEL * OOR2 / ZQ
     >          + ZETA1 * SMS / ZSR**2 *((ZT/ZQ)**2 * SOR2GP + SGGG) 
     >          - (0.,1.)* ZETA1 / SPS2 * (DZT/ZT-DZQ/ZQ) * GPGT
     >          + (0.,1.)* ZETA1 / SPS2 * DSGPGT
     >          + (0.,1.)* ZETA1 * GPGT 
     >          - (0.,1.)*V03/ZQ*FKDUMMY
      FACT(1:4,4) =   ZETA1 * MSNQ/ZQ * OOR2 
     >                 +(0.,1.)*V03*FKDUMMY
      FACT(1:4,5) = - (0.,1.)*ZTI*FKDUMMY/(SPS2*ZRHO)
     >          -(0.,1.)*FKDUMMY*CWW/(SPS2*ZRHO)*SPS2*ZQ
      FACT(1:4,6) = + (0.,1.)*CW*DT0/(SPS2**2*ZT**3) * FR4GPS      

C
      CALL FKUBL(MZ,MS,MANZ,6,IQDC,NBG,NZMA,ZMA,FACT,GEWI(I),HQ,DHC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H(CUB)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(1',4), A(5',4), A(1',7), A(7',1), A(5',5), A(1',5), A(4',4), A(4',5)
C     dv1A2,    dA3A2,   dv1p,    p'v1,    dA3A3,   dv1A3,  dA2A2,    dA2A3
C
      FACT(1:4,1) = + (0.,1.)*MSNQ/(ZT*ZQ) * GPGT
     >          - DZQ/(SPS2*ZT*ZQ*ZQ)  * GPSI 
      FACT(1:4,2) = - ZETA * DZQ /(SPS2 * ZT * ZQ) * GPSI
     >          + ZETA * (0.,1.)/ZT * MSNQ * GPGT 
      FACT(1:4,3) = + R2 / (SPS2*ZT)
      FACT(1:4,4) = + GAMMA * R2 / ZT * T0
      FACT(1:4,5) = + ZETA * ZQ/(SPS2*ZT**3) * (2.*DZT/ZT-DZQ/ZQ)*GPSI2
     >          - ZETA * ZQ/(SPS2*ZT**3) * GPDSGP
     >          - ZETA * (0.,1.) * SMS*ZQ/ZT * GPGT 
     >          + ZETA * (0.,1.) * ZNKWEL*ZQ**2/ZT**3 * GPGPGT 
      FACT(1:4,6) = - 1./(SPS2*ZT) * (ZT*DZT*FKDUMMY + DT0*R2)
     >          - DZT/(SPS2*ZT**2) * GPSI
     >          - (0.,1.) * SMZ/ZT * GPGT 
     >          + (0.,1.) * ZNKWEL * ZQ/ZT**3 * GPGPGT
      FACT(1:4,7) = + ZETA *DZQ/(ZT*ZQ*SPS2**2) * GPSI2
     >          - ZETA *(0.,1.)*MSNQ/(ZT*SPS2) * GPGPGT
      FACT(1:4,8) = - ZETA * ZQ/(ZT*SPS2**2) * GPDSGP
     >          - ZETA * ZQ/(ZT*SPS2**2) * (DZQ/ZQ-2*DZT/ZT)*GPSI2
     >          - ZETA * (0.,1.)*SMS*ZQ/(SPS2*ZT) * GPGPGT  
C
      CALL FKUBL(MZ,MS,MANZ,8,IDCC,NBG,NZMA,ZMA,FACT,GEWI(I),DHC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H'(CUB)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(1,4'), A(1,5'), A(5,5'), A(5,4'), A(4,4'), A(1,1')
C     v1dA2,    v1dA3,  A3dA3,   A3dA2,    A2dA2
C
      FACT(1:4,1) = - (0.,1.)*(2*SMZ-SMS+ZNKWEL*ZQ)/(ZT*ZQ)  * GPGT
     >          - (2*ZT*DZQ - ZQ*DZT)/(SPS2*ZT*ZT*ZQ*ZQ) * GPSI
     >          - 1./(SPS2*ZT*ZQ) * DSGPSI
     >          - 1./(SPS2*ZQ) * DZT * FKDUMMY 
      FACT(1:4,2) = - 1./(SPS2*ZT) * (ZT*DZT*FKDUMMY + DT0*R2)
     >          + (DZQ/ZQ - DZT/ZT)/(SPS2*ZT) * GPSI
     >          + (0.,1.) * MZNQ/ZT * GPGT 
      FACT(1:4,3) = + ZETA * ZQ/(SPS2*ZT**3) * 
     >                     (DZT*ZT * GPSI + DT0 * R2GPSI) 
     >      + ZETA * (0.,1.) * SMZ * ZQ/ZT  * GPGT
     >      + ZETA * (0.,1.)*ZQ/ZT**3 * SMZ * GPGPGT
     >      + ZETA * ZQ/(SPS2*ZT**4) * DZT  * GPSI2
      FACT(1:4,4) = + ZETA / (SPS2*ZT) * (ZT*DZT*FKDUMMY + DT0 * R2)
     >          + ZETA / (SPS2*ZT*ZT) * DZT * GPSI 
      FACT(1:4,5) = 0.
     >          - ZETA * ZT/(SPS2**2) * DSGPSI
     >          - ZETA * ZT/(SPS2**2) *(DZQ/ZQ-DSPS/SPS2)*GPSI
     >          + ZETA * (0.,1.)*SMS*ZT/SPS2 * GPGT
     >    - ZETA/(ZT*SPS2**2) * (GPDSGP - DSPS/SPS2 * GPSI2)
     >    + ZETA * (0.,1.) * SMS/(SPS2*ZT) * GPGPGT
      FACT(1:4,6) = - CW /(SPS2 * ZT**3) * DT0 * FR4GPT 

C
      CALL FKUBL(MZ,MS,MANZ,6,ICDC,NBG,NZMA,ZMA,FACT,GEWI(I),HC,DHC)
C
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     h'(QUA) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(3',3)
C     dA1A1
C
      FACT(1:4,1) = - ZETA1 * (0.,1.)*SMS/SPS2 * GPGT
C      
      CALL FKUBL(MZ,MS,MANZ,1,IDQQ,NBG,NZMA,ZMA,FACT,GEWI(I),DHQ,HQ)
C
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     h'(QUA) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(3',4')
C     dA1dA2
C
      FACT(1:4,1) = + ZETA1 * (0.,1.)/SPS2 * GPGT
C      
      CALL FKUBL(MZ,MS,MANZ,1,IDQDC,NBG,NZMA,ZMA,FACT,GEWI(I),DHQ,DHC)
C
C
C
C
C
C
      IF(MS.EQ.MZ) GOTO 100
C     ---------------------
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(CUB)   FUER DIE GLEICHUNGEN
C     ----------------------
C     A(1,4),  A(4,1),  A(7,1),  A(1,7), A(5,4),
C      v1A2,    A2v1,    pv1,     v1p,    A3A2,   
C
C     A(4,4),  A(4,5), A(1,5), A(5,5)
C       A2A2,   A2A3,   v1A3,   A3A3,
C
C      FACT(1) = + 2.* DZQ**2 / (SPS2*ZT*ZQ**3) * conjg(GPSI)
C     >          - DZQ*DZT /(SPS2*ZT*ZT*ZQ*ZQ)  * conjg(GPSI)
C     >          + 2.*(0.,1.)*(SMS-SMZ)*DZQ/(ZT*ZQ*ZQ) * conjg(GPGT)
C     >          + SPS2*MZNQ*MSNQ/(ZT*ZQ*ZSR*ZSR) 
C     >                        *(conjg(SGGG)+TOQ**2 * conjg(SOR2GP))
C     >          + DZQ / (SPS2*ZT*ZQ*ZQ) * conjg(DSGPSI) 
C      FACT(2) = - ZT*FKDUMMY - CONJG(GPSI)/ZT 
C
C      FACT(3) = + (GAMMA-1.) * DT0 / ZT * CONJG(R2)
C     >          - (0.,1.)*SPS2*MSNQ*GAMMA *T0* CONJG(FR2GPT) /ZT**3 
C      FACT(4) = + CONJG(DSR2) /(SPS2*ZT) - DZT/(SPS2*ZT**2) * CONJG(R2)
C      FACT(5) = - ZETA * SPS2/(ZT*ZQ) * MZNQ * ZNKWEL * CONJG(OOR2)
C     >          + ZETA * (0.,1.)*SMS/(ZT*ZQ) * DZQ * CONJG(GPGT)
C     >          + ZETA * SPS2 * SMS/(ZT*ZSR**2) * MZNQ *
C     >                  (CONJG(SGGG) + (ZT/ZQ)**2 * CONJG(SOR2GP))
C     >          - (0.,1.) * ZTE * DT0 * MZNQ*FKDUMMY/(ZRHO*ZQ*ZT)
C     >          -(0.,1.)*MZNQ/(ZQ*ZT)*V03*SPS2*FKDUMMY
C      FACT(6) = 0.
C     >           - ZETA * ZT/ZQ**2 * MZNQ * MSNQ * CONJG(OOR2)
C     >            - (0.,1.)*V03*MZNQ*ZT/ZQ*FKDUMMY
C      FACT(7) = 0.
C     >           + ZETA * ZT/ZQ * SMZ * MSNQ * CONJG(OOR2) 
C     >            + (0.,1.)*V03*ZT*(SMZ*FKDUMMY
C     >                             -ZNKWEL*ZQ*CONJG(GPSI)/ZT**2)
C      FACT(8) = DZT/(ZT*SPS2)*(DZT*FKDUMMY + DT0/ZT * CONJG(R2) )
C     >         + DZT**2/(SPS2*ZT**3) * CONJG(GPSI)
C     >   + SPS2*ZNKWEL/(ZT*ZQ) * MSNQ * CONJG(OOR2)
C     >   - SPS2*ZT*SMZ*MSNQ/(ZQ*ZQ*ZSR**2) *
C     >                  (CONJG(SOR2GP) + (ZQ/ZT)**2 * CONJG(SGGG))
C     >   + (0.,1.)*(SMZ*DZQ/(ZT*ZQ)-(SMZ-SMS)/ZT**2*DZT)*CONJG(GPGT)
C     >   + ZNKWEL*SPS2*ZQ/ZT**3 * MSNQ * CONJG(GPGT2)
C     >   - (0.,1.) * ZNKWEL*DZQ/ZT**3 * CONJG(GPGPGT)
C      FACT(9) = 
C     >   + ZETA*(0.,1.)*SMS*ZQ/ZT**3 *(DZQ/ZQ-2.*DZT/ZT)*CONJG(GPGPGT)     
C     >   + ZETA * DZT*ZQ/(SPS2*ZT**4) * CONJG(GPDSGP)
C     >   + ZETA*ZQ/(SPS2*ZT**3) * (DZQ/ZQ-2.*DZT/ZT) * 
C     >                   ( ZT*DZT*CONJG(GPSI) + DT0 * CONJG(R2GPSI))  
C     >   - ZETA * ZNKWEL**2 * SPS2*ZQ/ZT**3 * CONJG(GPSIOR)   
C     >   + ZETA * ZQ*DZT/(SPS2*ZT**4)*(DZQ/ZQ-2.*DZT/ZT)*CONJG(GPSI2)
C     >   + ZETA * ZQ/(SPS2*ZT**3) * 
C     >         (ZT*DZT * CONJG(DSGPSI) + DT0 * CONJG(R2DSGP))
C     >   + ZETA * (0.,1.)*SMS*ZQ/ZT**3 * CONJG(GPTDSG) 
C     >   - ZETA * SPS2*SMS*SMZ*ZQ/(ZT*ZSR**2) *
C     >                      (CONJG(SGGG) + (ZT/ZQ)**2 * CONJG(SOR2GP))
C     >   + ZETA * SPS2*SMS*ZNKWEL*ZQ**2/ZT**3 * CONJG(GPGT2)
C     >   + ZETA * SPS2*(SMZ+SMS)*ZNKWEL/ZT * CONJG(OOR2)
C     >   + (0.,1.) * ZTE*SMZ*DT0*FKDUMMY/(ZT*ZRHO)
C     >   - (0.,1.) * ZTE*ZNKWEL*ZQ*DT0/(ZRHO*ZT**3) * CONJG(GPSI)
C     >   + (0.,1.)*SPS2/ZT*V03*(SMZ*FKDUMMY
C     >                              -ZNKWEL*ZQ*CONJG(GPSI)/ZT**2)
C      FACT(10) = - (0.,1.)*ZTI*DT0*SMZ/(SPS2*ZT*ZRHO) *CONJG(FR2)
C     >   + (0.,1.)*ZNKWEL*ZQ*DT0*ZTI/(ZRHO*SPS2*ZT**3)*CONJG(FR2GPS)
C      FACT(11) = + (0.,1.)*ZTE*MZNQ*FKDUMMY/(ZRHO*ZT)
C      FACT(12) = - (0.,1.)*ZTI*ZT*SMZ/(SPS2*ZRHO)*FKDUMMY
C     >           + (0.,1.)*ZTI*ZNKWEL*ZQ/(SPS2*ZT*ZRHO) * CONJG(GPSI) 
C      FACT(13) =  - (0.,1.)*ZNKWEL*V03*
C     >              (ZRHO*(SPS2/(ZSR**2*ZQ*ZT)*(CONJG(SR2OGP)
C     >                       +QOT**2*CONJG(SR4GGG))
C     >             - ZRHO*CONJG(R4GPT2)*SPS2*ZQ/ZT**3))   
C     >        +CW / (SPS2**2 * ZT**3 * ZQ) *                        
C     >           (DT0*DZQ*SPS2 + DT0*DSPS*ZQ + DT0*SPS2*ZQ*DZRHO/ZRHO 
C     >          - DDT0*SPS2*ZQ) * CONJG(FR4GPT)                        
C     >          - (0.,1.)*CW*SMZ/(ZT**3 * ZSR**2) * DT0 *               
C     >          (CONJG(SR4GGG) + ZT**2/ZQ**2 * CONJG(SR2OGP))   
C      FACT(14) = -(0.,1.)*ZNKWEL*V03*ZQ*CONJG(R2)/ZT
CC
C      CALL FKUBL(MS,MZ,MANZ,14,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),HC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     ----------------------
C     A(1,3), A(7,2), A(5,3), A(4,3), A(7,6), A(1,2)
C      v1A1,   pv2,    A3A1,   A2A1,   pv3
C
C      FACT(1) = -(0.,1.)*ZNKWEL*(2*SMS-SMZ+ZNKWEL*ZQ)/ZT  *CONJG(GPGT)
C     >          + ZNKWEL*(ZQ*DZT-2*ZT*DZQ)/(SPS2*ZT*ZT*ZQ) *CONJG(GPSI)  
C     >          - ZNKWEL/(SPS2*ZT) * CONJG(DSGPSI)
C     >          + SMS * DZT / (SPS2*ZQ) * FKDUMMY
C      FACT(2) = - GAMMA*T0 /ZT * (SMS*CONJG(FR2)
C     >                          - ZNKWEL*ZQ/ZT**2 * CONJG(FR2GPS)) 
C      FACT(3) = - ZETA*SMZ/(ZT*SPS2)*(ZT*DZT*FKDUMMY + DT0*CONJG(R2))
C     >          - ZETA*SMZ/(ZT**2 * SPS2) * DZT * CONJG(GPSI)
C     >          - ZETA*(0.,1.)*SMS/ZT * MZNQ * CONJG(GPGT) 
C      FACT(4) = 0.
C     >        + ZETA * SMZ*ZT/(SPS2**2) * CONJG(DSGPSI)
C     >        + ZETA * SMZ*ZT/(SPS2**2) *(DZQ/ZQ-DSPS/SPS2)*CONJG(GPSI)
C     >        - ZETA * (0.,1.)* SMZ**2 * ZT/SPS2 * CONJG(GPGT)
C      FACT(5) = - GAMMA * T0 * MSNQ * FKDUMMY
C      FACT(6) = ZNKWEL*V03*ZRHO*ZQ / ZT**3 * CONJG(R4GPGT)
C
C      CALL FKUBL(MS,MZ,MANZ,6,INDCQ,NBG,NZMA,ZMA,FACT,GEWI(I),HC,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(CUB)   FUER DIE GLEICHUNGEN
C     ----------------------
C     A(2,4),  A(2,7), A(3,4), A(3,5), A(2,5), A(6,5), A(6,7), A(6,4), A(3,7), A(2,1)
C      v2A2,    v2p,    A1A2,   A1A3,   v2A3,   v3A3,   v3p,    v3A2
C
C      FACT(1) =-(0.,1.)*MZNQ*(SMZ-SMS+ZNKWEL*ZQ)/(ZQ*ZT)*conjg(GPGT)
C     >         + (SMZ+2*ZNKWEL*ZQ)/(SPS2*ZQ*ZQ*ZT) * DZQ*conjg(GPSI)
C     >         - MZNQ * DZT/(SPS2*ZT*ZT*ZQ) * conjg(GPSI)
C     >         + MZNQ/(SPS2*ZT*ZQ)          * conjg(DSGPSI)
C      FACT(2) = + SMZ / (SPS2*ZT) * CONJG(R2)
C      FACT(3) = ZETA1 * ZNKWEL * DZQ/ZQ**2 * CONJG(OOR2)
C     >           +(0.,1.)*V03*DZQ/(ZQ**2)*FKDUMMY
C      FACT(4) = 0.
C      FACT(5) = - MSNQ*DZT/(SPS2*ZT*ZT) * CONJG(GPSI)
C     >        - ZNKWEL*ZQ*DT0/(SPS2*ZT**3) * CONJG(R2GPSI)
C     >        + (SMZ-SMS) * DT0 * CONJG(R2) /(SPS2 * ZT)
C     >        + (SMZ-SMS) * DZT/SPS2 * FKDUMMY
C     >        + (0.,1.)*(SMS*SMZ-SMS**2+SMZ*ZNKWEL*ZQ)/ZT*CONJG(GPGT)
C     >        - (0.,1.)*(ZNKWEL*ZQ)**2/ZT**3 * CONJG(GPGPGT)   
C      FACT(6) = + SMZ * DT0 * FKDUMMY 
C     >          - ZNKWEL * ZQ / ZT**2 * DT0 * CONJG(GPSI)
C      FACT(7) = + MZNQ * FKDUMMY 
C      FACT(8) = - MZNQ/ZQ * DT0 * FKDUMMY
C      FACT(9) = + (0.,1.)*ZTI*DZRHO*FKDUMMY/(SPS2*ZRHO*ZRHO)
C     >          + (0.,1.)*ZTI*DSPS*FKDUMMY/(SPS2*SPS2*ZRHO) 
C     >          +((0.,1.)*CWW*FKDUMMY*DZRHO/(SPS2*ZRHO*ZRHO)
C     >          + (0.,1.)*CWW*FKDUMMY*DSPS/(SPS2*SPS2*ZRHO))
C      FACT(10) = - ZNKWEL*V03*ZRHO*ZQ / ZT**3 * CONJG(R4GPGT)
C     >          + (0.,1.) * CW /(SPS2**3 * ZT**3 * ZQ)                 
C     >          * (DDT0 * SPS2 * ZQ - DT0*DZQ *SPS2 - DT0*DSPS*ZQ     
C     >            - DT0 * SPS2 * ZQ * DZRHO/ZRHO) * CONJG(FR4GPS) 
C     >          - CW * SMZ/(SPS2*ZT**3) * DT0 * CONJG(FR4GPT)        
C
C      CALL FKUBL(MS,MZ,MANZ,10,INDQC,NBG,NZMA,ZMA,FACT,GEWI(I),HQ,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     ----------------------
C     A(2,3),   A(3,2), A(3,3), A(2,2)
C      v2A1,     A1v2,   A1A1    
C
C      FACT(1) = - ZNKWEL**2 * ZQ/(SPS2*ZT) * CONJG(GPSI) 
C     >          - SMZ*SMZ* ZT/(SPS2*ZQ) * FKDUMMY 
C      FACT(2) = FKDUMMY 
C      FACT(3) = - ZETA1 * ZNKWEL**2 * CONJG(OOR2)
C     >          - ZETA1 * SMZ**2 / ZSR**2 *
C     >            ((ZT/ZQ)**2 * CONJG(SOR2GP) + CONJG(SGGG))  
C     >          + (0.,1.)*ZETA1*SMZ/SPS2*(DZT/ZT-DZQ/ZQ)*CONJG(GPGT)
C     >          - (0.,1.)*ZETA1*SMZ/SPS2 * CONJG(DSGPGT)
C     >          - (0.,1.)*ZNKWEL*V03*FKDUMMY
C      FACT(4) = -(0.,1.)*ZNKWEL*V03*ZRHO
C     >                      *CONJG(R4GPSI)*ZQ/(SPS2*ZT**3)
C
C      CALL FKUBL(MS,MZ,MANZ,4,INDQQ,NBG,NZMA,ZMA,FACT,GEWI(I),HQ,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H'(CUB)   FUER DIE GLEICHUNG
C     ------------------------
C     A(1',4'), A(1',5'), A(5',5'), A(4',4')
C     dv1dA2,   dv1dA3,   dA3dA3,    dA2dA2
C
C      FACT(1) = + ZT/(SPS2*ZQ) * FKDUMMY + CONJG(GPSI) / (SPS2*ZT*ZQ)
C      FACT(2) = 0.
C      FACT(3) = - ZETA * CONJG(GPSI)  * ZQ / (SPS2 * ZT)
C     >          - ZETA * CONJG(GPSI2) * ZQ / (SPS2 * ZT**3) 
C      FACT(4) = - ZETA * ZT /(SPS2**2) * CONJG(GPSI)
C     >          - ZETA /(ZT*SPS2**2) * CONJG(GPSI2) 
C
C      CALL FKUBL(MS,MZ,MANZ,4,IDCDC,NBG,NZMA,ZMA,FACT,GEWI(I),DHC,DHC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(1',3), A(5',3), A(4',3)
C     dv1A1,    dA3A1,    dA2A1
C
C      FACT(1) = ZNKWEL/(SPS2*ZT) * CONJG(GPSI)
C     >        - SMZ*ZT/(SPS2*ZQ)*FKDUMMY
C      FACT(2) = + ZETA * MZNQ * CONJG(GPSI) / (SPS2 * ZT)
C      FACT(3) = + ZETA * SMZ * ZT / (SPS2**2) * CONJG(GPSI)
C     >          - ZETA * ZNKWEL*ZQ/(ZT*SPS2**2) * CONJG(GPSI2) 
C
C      CALL FKUBL(MS,MZ,MANZ,3,IDCQ,NBG,NZMA,ZMA,FACT,GEWI(I),DHC,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H'(CUB)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(2,4'), A(2,5'), A(3,4'), A(3,5'), A(3,7'), A(2,1')
C     v2dA2,   v2dA3,    A1dA2,   A1dA3
C
C      FACT(1) = - ZNKWEL/(SPS2*ZT) * CONJG(GPSI)
C     >        + SMZ*ZT/(SPS2*ZQ)*FKDUMMY
C      FACT(2) =  MSNQ / (SPS2 * ZT) * CONJG(GPSI)
C      FACT(3) = - ZETA1 * ZNKWEL * CONJG(OOR2) / ZQ
C     >          + ZETA1 * SMZ / ZSR**2 *((ZT/ZQ)**2 
c     >                              * CONJG(SOR2GP) + CONJG(SGGG)) 
C     >          - (0.,1.)* ZETA1 / SPS2 * (DZT/ZT-DZQ/ZQ) * CONJG(GPGT)
C     >          + (0.,1.)* ZETA1 / SPS2 * CONJG(DSGPGT)
C     >          + (0.,1.)* ZETA1 * CONJG(GPGT) 
C     >          - (0.,1.)*V03/ZQ*FKDUMMY
C      FACT(4) =   ZETA1 * MZNQ/ZQ * CONJG(OOR2)
C     >                 +(0.,1.)*V03*FKDUMMY
C      FACT(5) = - (0.,1.)*ZTI*FKDUMMY/(SPS2*ZRHO)  
C     >          -(0.,1.)*FKDUMMY*CWW/(SPS2*ZRHO)*SPS2*ZQ       
C      FACT(6) = + (0.,1.)*CW*DT0/(SPS2**2*ZT**3) * CONJG(FR4GPS)
C
C      CALL FKUBL(MS,MZ,MANZ,6,IQDC,NBG,NZMA,ZMA,FACT,GEWI(I),HQ,DHC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H(CUB)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(1',4), A(5',4), A(1',7), A(7',1), A(5',5), A(1',5), A(4',4), A(4',5)
C     dv1A2,    dA3A2,   dv1p,    p'v1,    dA3A3,   dv1A3,   dA2A2,   dA2A3
C
C      FACT(1) = + (0.,1.)*MZNQ/(ZT*ZQ) * CONJG(GPGT)
C     >          - DZQ/(SPS2*ZT*ZQ*ZQ)  * CONJG(GPSI) 
C      FACT(2) = - ZETA * DZQ /(SPS2 * ZT * ZQ) * CONJG(GPSI)
C     >          + ZETA * (0.,1.)/ZT * MZNQ * CONJG(GPGT) 
C      FACT(3) = + CONJG(R2) / (SPS2*ZT)
C      FACT(4) = + GAMMA * CONJG(R2) / ZT * T0
C      FACT(5) = + ZETA*ZQ/(SPS2*ZT**3)*(2.*DZT/ZT-DZQ/ZQ)*CONJG(GPSI2)
C     >          - ZETA * ZQ/(SPS2*ZT**3) * CONJG(GPDSGP)
C     >          - ZETA * (0.,1.) * SMZ*ZQ/ZT * CONJG(GPGT) 
C     >          + ZETA * (0.,1.) * ZNKWEL*ZQ**2/ZT**3 * CONJG(GPGPGT) 
C      FACT(6) = - 1./(SPS2*ZT) * (ZT*DZT*FKDUMMY + DT0*CONJG(R2))
C     >          - DZT/(SPS2*ZT**2) * CONJG(GPSI)
C     >          - (0.,1.) * SMS/ZT * CONJG(GPGT) 
C     >          + (0.,1.) * ZNKWEL * ZQ/ZT**3 * CONJG(GPGPGT)
C      FACT(7) = + ZETA *DZQ/(ZT*ZQ*SPS2**2) * CONJG(GPSI2)
C     >          - ZETA *(0.,1.)*MZNQ/(ZT*SPS2) * CONJG(GPGPGT)
C      FACT(8) = - ZETA * ZQ/(ZT*SPS2**2) * CONJG(GPDSGP)
C     >       - ZETA * ZQ/(ZT*SPS2**2) * (DZQ/ZQ-2*DZT/ZT)*CONJG(GPSI2)
C     >       - ZETA * (0.,1.)*SMZ*ZQ/(SPS2*ZT) * CONJG(GPGPGT)  
C
C      CALL FKUBL(MS,MZ,MANZ,8,IDCC,NBG,NZMA,ZMA,FACT,GEWI(I),DHC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H'(CUB)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(1,4'), A(1,5'), A(5,5'), A(5,4'), A(4,4'), A(1,1')
C     v1dA2,    v1dA3,  A3dA3,   A3dA2,    A2dA2
C
C      FACT(1) = - (0.,1.)*(2*SMS-SMZ+ZNKWEL*ZQ)/(ZT*ZQ) *CONJG(GPGT)
C     >        - (2*ZT*DZQ - ZQ*DZT)/(SPS2*ZT*ZT*ZQ*ZQ)*CONJG(GPSI)
C     >        - 1./(SPS2*ZT*ZQ) * CONJG(DSGPSI)
C     >        - 1./(SPS2*ZQ) * DZT * FKDUMMY 
C      FACT(2) = - 1./(SPS2*ZT) * (ZT*DZT*FKDUMMY + DT0*CONJG(R2))
C     >          + (DZQ/ZQ - DZT/ZT)/(SPS2*ZT) * CONJG(GPSI)
C     >          + (0.,1.) * MSNQ/ZT * CONJG(GPGT) 
C      FACT(3) = + ZETA * ZQ/(SPS2*ZT**3) * 
C     >                  (DZT*ZT * CONJG(GPSI) + DT0 * CONJG(R2GPSI)) 
C     >      + ZETA * (0.,1.) * SMS * ZQ/ZT  * CONJG(GPGT)
C     >      + ZETA * (0.,1.)*ZQ/ZT**3 * SMS * CONJG(GPGPGT)
C     >      + ZETA * ZQ/(SPS2*ZT**4) * DZT  * CONJG(GPSI2)
C      FACT(4) = + ZETA / (SPS2*ZT) * (ZT*DZT*FKDUMMY + DT0*CONJG(R2))
C     >          + ZETA / (SPS2*ZT*ZT) * DZT * CONJG(GPSI) 
C      FACT(5) = 0.
C     >          - ZETA * ZT/(SPS2**2) * CONJG(DSGPSI)
C     >          - ZETA * ZT/(SPS2**2) *(DZQ/ZQ-DSPS/SPS2)*CONJG(GPSI)
C     >          + ZETA * (0.,1.)*SMZ*ZT/SPS2 * CONJG(GPGT)
C     >    - ZETA/(ZT*SPS2**2) * (CONJG(GPDSGP)-DSPS/SPS2*CONJG(GPSI2))
C     >    + ZETA * (0.,1.) * SMZ/(SPS2*ZT) * CONJG(GPGPGT)
C      FACT(6) = - CW /(SPS2 * ZT**3) * DT0 * CONJG(FR4GPT) 
C
C
C      CALL FKUBL(MS,MZ,MANZ,6,ICDC,NBG,NZMA,ZMA,FACT,GEWI(I),HC,DHC)
C
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     h'(QUA) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(3',3)
C     dA1A1
C
C      FACT(1) = - ZETA1 * (0.,1.)*SMZ/SPS2 * CONJG(GPGT)
C      
C      CALL FKUBL(MS,MZ,MANZ,1,IDQQ,NBG,NZMA,ZMA,FACT,GEWI(I),DHQ,HQ)
C
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     h'(QUA) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(3',4')
C     dA1dA2
C
C      FACT(1) = + ZETA1 * (0.,1.)/SPS2 * CONJG(GPGT)
C      
C      CALL FKUBL(MS,MZ,MANZ,1,IDQDC,NBG,NZMA,ZMA,FACT,GEWI(I),DHQ,DHC)
C
C
  100 CONTINUE
C     --------
C
      FKDUMMY =  0.0
C
  200 CONTINUE
C     --------
C
      DO 210 I = 1, NZMA*NZMA
         ZMA(I) = ZDIF * ZMA(I)
  210 CONTINUE
C
C
C ... REGULARITAETSBEDINGUNG FUER S = 0 ...
C
      IF(NI.EQ.1) THEN
        DO 230 I=1,2*MANZ,2
        DO 220 J=1,NZMA
           ZMA((I-1)*NZMA+J) = (0.0,0.0)
           ZMA(I+(J-1)*NZMA) = (0.0,0.0)
  220   CONTINUE
        ZMA((I-1)*NZMA+I) = ZBIG
  230   CONTINUE
        DO 232 I=2*MANZ+1,12*MANZ,2
        DO 234 J=1,NZMA
           ZMA((I-1)*NZMA+J) = (0.0,0.0)
           ZMA(I+(J-1)*NZMA) = (0.0,0.0)
  234   CONTINUE
        ZMA((I-1)*NZMA+I) = ZBIG
  232   CONTINUE
        DO 236 I=12*MANZ+1,14*MANZ,2
        DO 238 J=1,NZMA
           ZMA((I-1)*NZMA+J) = (0.0,0.0)
           ZMA(I+(J-1)*NZMA) = (0.0,0.0)
  238   CONTINUE
        ZMA((I-1)*NZMA+I) = ZBIG
  236   CONTINUE
      ENDIF
C
C ... RANDBEDINGUNG FUER  S = 1 ...
C
      IF (NI.EQ.NGINT) THEN
        CALL ADDBND(ZMA)
      ENDIF  
      IF(RWALL.GT.1.AND.NI.EQ.NGINT) THEN
ccc         CALL ADDBND(ZMA)
      ELSEIF(NI.EQ.NGINT) THEN
C
C V1 AT PLASMA BOUNDARY SET TO ZERO
C
         DO 250 I=NBG+1,NBG+2*MANZ,2
            DO 240 J=1,NZMA
               ZMA((I-1)*NZMA+J) = (0.0,0.0)
               ZMA(I+(J-1)*NZMA) = (0.0,0.0)
  240       CONTINUE
            ZMA((I-1)*NZMA+I) = ZBIG
  250    CONTINUE
C
C A2, A3 AT PLASMA BOUNDARY SET TO ZERO
C
         DO 270 I=NBG+6*MANZ+1,NBG+10*MANZ,2
            DO 260 J=1,NZMA
               ZMA((I-1)*NZMA+J) = (0.0,0.0)
               ZMA(I+(J-1)*NZMA) = (0.0,0.0)
  260       CONTINUE
            ZMA((I-1)*NZMA+I) = ZBIG
  270    CONTINUE
        ENDIF
C
      RETURN
      END
************************************************************************
*DECK FKUBL
      SUBROUTINE FKUBL(MZ,MS,L,IANZ,INDHG,NBG,NZMA,ZMA,FACT,GEW,H1,H2)
C-----------------------------------------------------------------------
C
C     BESETZT DIE ZUSAMMENGEHOERENDEN UNTERBLOECKE U(MZ,MS)
C     DER FOUR.KOEFF. MZ,MS DER UNTERBLOECKE A(IZ,IS).
C                (IZ=MOD(INDHG(I)-1,NGL)+1)
C                (IS=   (INDHG(I)-1)/NGL+1)
C     (D.H. DEN UNTERBLOCK U(MZ,MS) VON A(IZ,IS) IM HAUPTDIAGONAL-BLOCK,
C     IM OBEREN UND UNTEREN NEBENDIAGONALBLOCK UND DEN ERSTEN TEIL
C     DES FOLGENDEN HAUPTDIAGONAL-BLOCKS VON AMAT(SR. CONAMAT).)
C          Z M A ( J )  =  Z M A ( J ) + H 1 * F A C T * H 2
C
C
C     AMAT:
C
C     =========
C     = Z |   =
C     =-- M --=----
C     =   | A =   |
C     =========--------
C         |   |   |   |
C         -------------
C           .   .   .
C
C               .   .   .
C
C                   .   .   .
C                   -------------
C                   |   |   |   |
C                   -------------
C                       |   |   |
C                       ---------
C
C      ZMA:
C
C     --------------------------------------------------------------
C     | A(1,1)  A(1,2)  . . . A(1,8) | A(1,1)  A(1,2) . . . A(1,8) |
C     | A(2,1)  A(2,2)  . . .   .    | A(2,1)    .    . . .    .   |
C     |   .                     .    |   .                     .   |
C     |   .                     .    |   .                     .   |
C     |   .                     .    |   .                     .   |
C     | A(8,1)   .      . . . A(8,8) | A(8,1)    .    . . . A(8,8) |
C     --------------------------------------------------------------
C     | A(1,1)  A(1,2)  . . . A(1,8) | A(1,1)  A(1,2) . . . A(1,8) |
C     | A(2,1)  A(2,2)  . . .   .    | A(2,1)    .    . . .    .   |
C     |   .                     .    |   .                     .   |
C     |   .                     .    |   .                     .   |
C     |   .                     .    |   .                     .   |
C     | A(8,1)   .      . . . A(8,8) | A(8,1)    .    . . . A(8,8) |
C     --------------------------------------------------------------
C
C
C     JEDES A(IZ,IS) BESTEHT AUS EINEM BLOCK DER DIMENSION 2*L
C     IN FOLGENDER FORM:
C     MZ,MS IST DIE NUMERIERUNG DER STOERUNGEN.DER JEWEILIGE FOURIER-
C     KOEFFIZIENT FUER MZ,MS IST: MS-MZ
C
C            MS=1       MS=2      .  .  .  MS=L
C          |--------------------------------------|
C     MZ=1 | U11 U12 | U11 U12 |          |       |
C      "   | U21 U22 | U21 U22 |          |       |
C          |---------|---------|----------|-------|
C     MZ=2 | U11 U12 |                    |       |
C      "   | U21 U22 |                    |       |
C          |---------|--------------------|-------|
C      .   |         |                    |       |
C          |         |                    |       |
C      .   |         |                    |       |
C          |         |                    |       |
C      .   |         |                    |       |
C          |         |                    |       |
C          |------------------------------|-------|
C     MZ=L | U11 U12 |                    |       |
C      "   | U21 U22 |                    |       |
C          |--------------------------------------|
C
C-----------------------------------------------------------------------
C modified to allow different factors coming from left and right element
C nodes (i.e. to allow different harmonics on the nodes)
C   fact(1,:) MS from LEFT,  MZ from LEFT
C   fact(2,:) MS from RIGHT, MZ from LEFT
C   fact(3,:) MS from LEFT,  MZ from RIGHT
C   fact(4,:) MS from RIGHT, MZ from RIGHT
C Guido Huysmans (9-2-2004) 
C-----------------------------------------------------------------------
C
      INTEGER  INDHG(*), NGL, NBG, NZMA, IND,INDO,INDU,INDN,MZ,MS,IANZ,L
      COMPLEX  ZMA(*), FACT(4,*)
      REAL     H1(*), H2(*), GEW
C
      NGL=NBG/(2*L)
C
      DO 5 J=1,IANZ
         FACT(1:4,J)=FACT(1:4,J)*GEW
    5 CONTINUE
C
      DO 10 I=1,IANZ
         IND    = ((INDHG(I)-1)/NGL * 2 * L + (MS-1) * 2 ) * NZMA
     >           + MOD(INDHG(I)-1,NGL) * 2 * L + (MZ - 1) * 2 + 1
         INDO   = IND+NBG*NZMA
         INDU   = IND+NBG
         INDN   = INDO+NBG
         IZ = MOD(INDHG(I)-1,NGL)+1
         IS =   (INDHG(I)-1)/NGL+1
C
         ZMA(IND)         = ZMA(IND)         + H1(2) * FACT(1,I) * H2(2)
         ZMA(IND+NZMA)    = ZMA(IND+NZMA)    + H1(2) * FACT(1,I) * H2(4)
         ZMA(IND+1)       = ZMA(IND+1)       + H1(4) * FACT(1,I) * H2(2)
         ZMA(IND+1+NZMA)  = ZMA(IND+1+NZMA)  + H1(4) * FACT(1,I) * H2(4)
C
         ZMA(INDO)        = ZMA(INDO)        + H1(2) * FACT(2,I) * H2(1)
         ZMA(INDO+NZMA)   = ZMA(INDO+NZMA)   + H1(2) * FACT(2,I) * H2(3)
         ZMA(INDO+1)      = ZMA(INDO+1)      + H1(4) * FACT(2,I) * H2(1)
         ZMA(INDO+1+NZMA) = ZMA(INDO+1+NZMA) + H1(4) * FACT(2,I) * H2(3)
C
         ZMA(INDU)        = ZMA(INDU)        + H1(1) * FACT(3,I) * H2(2)
         ZMA(INDU+NZMA)   = ZMA(INDU+NZMA)   + H1(1) * FACT(3,I) * H2(4)
         ZMA(INDU+1)      = ZMA(INDU+1)      + H1(3) * FACT(3,I) * H2(2)
         ZMA(INDU+1+NZMA) = ZMA(INDU+1+NZMA) + H1(3) * FACT(3,I) * H2(4)
C
         ZMA(INDN)        = ZMA(INDN)        + H1(1) * FACT(4,I) * H2(1)
         ZMA(INDN+NZMA)   = ZMA(INDN+NZMA)   + H1(1) * FACT(4,I) * H2(3)
         ZMA(INDN+1)      = ZMA(INDN+1)      + H1(3) * FACT(4,I) * H2(1)
         ZMA(INDN+1+NZMA) = ZMA(INDN+1+NZMA) + H1(3) * FACT(4,I) * H2(3)
C
   10 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK ADDBND
      SUBROUTINE ADDBND(ZMA)
C-----------------------------------------------------------------------
C     ADDS THE BOUNDARY CONTRIBUTIONS TO THE ZMA MATRIX OF THE LAST
C     INTERVAL IN TERMS OF THE PLASMA VELOCITY AND VACUUM FIELD
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPAR
*CALL COMWEL
*CALL COMGRID
*CALL COMEQUI
*CALL COMIOD
*CALL COMSPL
*CALL COMFFT
*CALL COMEQV
*CALL COMVRSP
*CALL COMVAC
C
      COMPLEX  ZMA(*),FACT(4),R2,GPGT,ZETA
      REAL     ZSR,ZRHO,ZQ,MSNQ,MZNQ,FKDUMMY,
     >         ZT,T0,QOT,TOQ,T2OQ,DOQDOT,SPS2,
     >         DUMMY(3)
      INTEGER  NI,MS,MZ,I,K,
     >         INDBM(1),INDA1(1),INDA2(1)
C
      DATA INDBM /  1 /
      DATA INDA1 / 17 /
      DATA INDA2 / 24 /
C
      ZSR  = 1.
      ZQ   = Q1(NPSI)
      ZT   = RBP1(NPSI)
      ZETA = ETA
      T0   = P1(NPSI)
C
      QOT  = ZQ/ZT
      TOQ  = ZT/ZQ
      SPS2 = 2.*CPSURF
C
C-----------------------------------------------------------------------
C           A(3,3), A(3,4')
C-----------------------------------------------------------------------
C
      DO 200  KF = 1, LANZ
        K = (KF-1) * MDIF + 1
        GPGT = CMPLX(SPWERT(NPSI,ZSR,RGPGT(1,K),RGPGT(NP1,K),
     >                        RGPGT(N2P1,K),RGPGT(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IGPGT(1,K),IGPGT(NP1,K),
     >                        IGPGT(N2P1,K),IGPGT(N3P1,K),CS,DUMMY))
        DO 100  MS = 1 , MANZ - KF + 1
          MZ = MS + KF - 1
          SMZ  = RFOUR(MZ)
          SMS  = RFOUR(MS)
c------------------------------------------ A(3,3)          
          FACT(1)= (0.,1.)*ZETA*SMS*GPGT/SPS2
          CALL FBOUND(MZ,MS,MANZ,1,INDA1,NBG,NZMA,ZMA,FACT,1)
c----------------------------------------------------------------------          
c idrv=1 in case of derivative of cubic OR quadratic FEM at boundary   
c----------------------------------------------------------------------       
c------------------------------------------ A(3,4')          
          FACT(1)= -(0.,1.)*ZETA*GPGT/SPS2
          CALL FBOUND(MZ,MS,MANZ,1,INDA2,NBG,NZMA,ZMA,FACT,1)        
c    
          IF(MS.EQ.MZ) GOTO 100
c
c------------------------------------------ A(3,3)          
          FACT(1)= (0.,1.)*ZETA*SMZ*CONJG(GPGT)/SPS2
          CALL FBOUND(MS,MZ,MANZ,1,INDA1,NBG,NZMA,ZMA,FACT,0)
c------------------------------------------ A(3,4')          
          FACT(1)= -(0.,1.)*ZETA*CONJG(GPGT)/SPS2
          CALL FBOUND(MS,MZ,MANZ,1,INDA2,NBG,NZMA,ZMA,FACT,1)        
  100   CONTINUE
  200 CONTINUE    
C
C-----------------------------------------------------------------------
C           A(1,1)      MOMENTUM EQ.
C-----------------------------------------------------------------------
      IF (RWALL .GT. 1.) THEN
        DO 20 MS = 1, MANZ
           SMS = MSTART(NG) + FLOAT(MS-1) 
           DO 10 MZ = 1, MANZ
              SMZ = MSTART(NG) + FLOAT(MZ-1) 
             FACT(1)=-(0.,1.)*(SMZ+ZNKWEL*ZQ)*(SMS+ZNKWEL*ZQ)/ZNKWEL
     >              /ZQ**2  * B3B1(MS,MZ) 
             CALL FBOUND(MZ,MS,MANZ,1,INDBM,NBG,NZMA,ZMA,FACT,0)
   10      CONTINUE
   20   CONTINUE
      ENDIF
C
      RETURN
      END
************************************************************************
*DECK FBOUND
      SUBROUTINE FBOUND(MZ,MS,L,IANZ,INDHG,NBG,NZMA,ZMA,FACT,IDRV)
C-----------------------------------------------------------------------
C     ADDS THE BOUNDARY TERMS TO THE ZMA MATRIX
C          DESCRIPTION SEE FKUBL
C          IDRV = 0 NO DERIVATIVES IN THE SECOND VARIABLE
C          IDRV = 1 ADDS TO THE DERIVATIVE OF THE SECOND VARIABLE
C-----------------------------------------------------------------------
C
      COMPLEX  ZMA(*), FACT(*)
      INTEGER  INDHG(*), NGL, NBG, NZMA, IND, INDO, INDU, INDN,
     >         MZ, MS, IANZ, L
C
      NGL = NBG/(2*L)
C
      DO 10 I=1,IANZ
         IND  = ((INDHG(I)-1)/NGL * 2 * L + (MS-1) * 2 ) * NZMA
     >          + MOD(INDHG(I)-1,NGL) * 2 * L + (MZ - 1) * 2 + 1
         INDO = IND+NBG*NZMA
         INDN = INDO+NBG
C
         IF(IDRV.EQ.0) THEN
            ZMA(INDN)      = ZMA(INDN) + FACT(I)
         ELSE
            ZMA(INDN+NZMA) = ZMA(INDN+NZMA) + FACT(I)
         ENDIF
   10 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK CUBFCT
      SUBROUTINE CUBFCT(S,SL,SU,H)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      REAL  H(4)
        DS= SU-SL
        Q1= (S-SL)/DS
        Q2= (SU-S)/DS
        H(1)= 3.*Q1**2 - 2.*Q1**3
        H(2)= 3.*Q2**2 - 2.*Q2**3
        H(3)= (S-SU)*Q1**2
        H(4)= (S-SL)*Q2**2
      RETURN
      END
************************************************************************
*DECK QUAFCT
      SUBROUTINE QUAFCT(S,SL,SU,H)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      REAL  H(4)
c        DS2=(SU-SL)**2
c        SM=(SU+SL)/2.
c        H(1)= 4.*(S-SL)*(SU-S)/DS2
c        H(2)= 0.0
c        H(3)= 2.*(S-SM)*(S-SL)/DS2
c        H(4)= 2.*(S-SM)*(S-SU)/DS2


        DS= SU-SL
        Q1= (S-SL)/DS
        Q2= (SU-S)/DS
        H(1)= 3.*Q1**2 - 2.*Q1**3
        H(2)= 3.*Q2**2 - 2.*Q2**3
        H(3)= (S-SU)*Q1**2
        H(4)= (S-SL)*Q2**2

      RETURN
      END
************************************************************************
*DECK DCUBF
      SUBROUTINE DCUBF(S,SL,SU,H)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      REAL  H(4)
        DS= SU-SL
        H(1)= 6.*(S-SL)/DS**2-6.*(S-SL)**2/DS**3
        H(2)= -6.*(SU-S)/DS**2+6.*(SU-S)**2/DS**3
        H(3)= ((S-SU)*2.*(S-SL)+(S-SL)**2)/DS**2
        H(4)= ((S-SL)*2.*(S-SU)+(S-SU)**2)/DS**2
      RETURN
      END
************************************************************************
*DECK DQUAF
      SUBROUTINE DQUAF(S,SL,SU,H)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      REAL  H(4)
c        DS2=(SU-SL)**2
c        H(1)= 4.*(-2.*S+SU+SL)/DS2
c        H(2)= 0.0
c        H(3)= (4.*S-SU-3.*SL)/DS2
c        H(4)= (4.*S-SL-3.*SU)/DS2

        DS= SU-SL
        H(1)= 6.*(S-SL)/DS**2-6.*(S-SL)**2/DS**3
        H(2)= -6.*(SU-S)/DS**2+6.*(SU-S)**2/DS**3
        H(3)= ((S-SU)*2.*(S-SL)+(S-SL)**2)/DS**2
        H(4)= ((S-SL)*2.*(S-SU)+(S-SU)**2)/DS**2

      RETURN
      END

************************************************************************
*DECK SOLV1
      SUBROUTINE SOLV1
C
************************************************************************
************************************************************************
**                                                                    **
**    BEGIN : MODULES SOLV1 - SOLV5  (EIGENVALUE SOLVERS)             **
**    -----------------------------                                   **
**                                                                    **
************************************************************************
************************************************************************
**                                                                    **
**    SOLV1 : QR-SOLVER (COMPLEX)                                     **
**    CHANGED TO ALL LAPACK ROUTINES (GTA 13-9-2001)                  **
************************************************************************
************************************************************************
C
*CALL COMMAX
*CALL COMPAR
*CALL COMPIO
*CALL COMDIM
*CALL CORE1
C
      INTEGER  LWORK,IPIV(NDIM1)
      REAL     RWORK(2*NDIM1)
      COMPLEX  WORK(2*NDIM1)
C
C
C ... QR - ALGORITHM ...
C
      CALL ZPOTRF('U',NDIM,BMAT,NDIM1,INFO)
      IF(INFO.NE.0) THEN
         WRITE(NOUT,3)
         WRITE(NOUT,4) INFO
         STOP
      ENDIF
 
      CALL ZPOTRS('U',NDIM,NDIM,BMAT,NDIM1,AMAT,NDIM1,INFO)
      IF(INFO.NE.0) THEN
         WRITE(NOUT,4) INFO
         STOP
      ENDIF
      
      LWORK = 2*NDIM1
      CALL ZGEEV('N','N', NDIM, AMAT, NDIM1, WRI, VL, NDIM1,
     >            VRI, NDIM1, WORK, LWORK, RWORK, INFO)
C

      WRITE(NOUT,43) (I,WRI(I),I=1,NDIM)
      WRITE(NOUTI,43)(I,WRI(I),I=1,NDIM)
C
      RETURN
C
    3 FORMAT(///5X,'MATRIX BMAT1 NOT POSITIVE DEFINIT')
    4 FORMAT(' ZPOTRF : INFO = ',I4)
    5 FORMAT(' ZGEEV  : INFO = ',I4)
   43 FORMAT(3X,'VSHIFT(',I3,') = (',1P,E16.6,',',E16.6,'),')
      END
************************************************************************
*DECK SOLV2
      SUBROUTINE SOLV2
C
************************************************************************
************************************************************************
**                                                                    **
**    SOLV2 : INVERSE VECTOR ITERATION, IN-CORE (WITHOUT BMAT)        **
**            VERSION B,  4.7.91                                      **
**                                                                    **
**    STRUCTURE :                                                      **
**                 SOLV2                                              **
**                   MATVER                                           **
**                     CONBMAT                                        **
**                   CSHIFT                                           **
**                     CONBMAT                                        **
**                   CGBFA                                            **
**                     (IZAMAX)                                      **
**                     (ZSCAL)                                       **
**                     (ZAXPY)                                       **
**                   (ZSCAL)                                         **
**                   CGBSL                                            **
**                     (ZAXPY)                                       **
**                     (ZCOPY)                                       **
**                     (ZDOTC)                                       **
**                   (ZDOTC)                                         **
**                   (ZCOPY)                                         **
**                                                                    **
************************************************************************
************************************************************************
C
*CALL COMMAX
*CALL COMPAR
*CALL COMPAR2
*CALL COMPIO
*CALL CORE2
*CALL COMIT
*CALL COMGRID
*CALL COMDIM
*CALL COMEQUI
C
      COMPLEX  EWALT, EWNEU
      COMPLEX  ZDOTC, PRD1, PRD2, CONE, CZERO
C
      DATA INC /1/
      DATA QQ /1.E-4/

      CONE  = (1.0,0.0)
      CZERO = (0.0,0.0)
C
C ... R(0) = B * X(0) ...
C
C      CALL MATVER(ZMA,NDIM,NZMA,NGINT,X0,X1,Y0,Y1)

      CALL ZGBMV('N',NDIM,NDIM,ML,MU,CONE,BMAT,LDB,X0,1,CZERO,X1,1)
      CALL ZGBMV('N',NDIM,NDIM,ML,MU,CONE,BMAT,LDB,Y0,1,CZERO,Y1,1)


C      CALL CSHIFT(AMAT,BMAT,LDA,NDIM,EWSHIFT,ZMA,NZMA)
      
      DO J=1,NDIM                                                       
        DO I=1,LDB                                                      
          AMAT(NZMA+I-1,J) = AMAT(NZMA+I-1,J) - EWSHIFT * BMAT(I,J)     
        ENDDO                                                           
      ENDDO   

C      AMAT(NZMA:LDA,1:NDIM) = AMAT(NZMA:LDA,1:NDIM) 
C     >                       - EWSHIFT * BMAT(1:LDB,1:NDIM)
C
C ... ABSCHAETZEN START-EIGENWERT ...
C
      EWALT = QQ * EWSHIFT
C
C
C ... ZERLEGEN AMAT : AMAT = L * R ...
C
C      CALL CGBFA(AMAT,LDA,NDIM,ML,MU,IPVT,INFO)
C-----------------------------------------  use LAPACK ROUTINE
      CALL ZGBTRF(NDIM,NDIM,ML,MU,AMAT,LDA,IPVT,INFO)
C
      IF(INFO.NE.0) THEN
         WRITE(NOUT,1) INFO
         STOP
      ENDIF
C
C
      IT = 0
C
C ... ITERATIONEN ...
C
   10 CONTINUE
C
      IT = IT + 1
C
C ... R(I) = DELTA(LAMBDA(I-1)) * R(I-1) ...
C
      CALL ZSCAL(NDIM,EWALT,X1,INC)
      CALL ZSCAL(NDIM,CONJG(EWALT),Y1,INC)
C
C ... LOESUNG GL.SYST  ( A - LMBDA * B ) * X(I) = X(I-1) ...
C
C
C      CALL CGBSL(AMAT,LDA,NDIM,ML,MU,IPVT,X1,0,X0)
C      CALL CGBSL(AMAT,LDA,NDIM,ML,MU,IPVT,Y1,1,Y0)
C---------------------------------------------- use LAPACK routine
      CALL XGBTRS('N',NDIM,ML,MU,1,AMAT,LDA,IPVT,X1,NDIM2,INFO,X0)
      CALL XGBTRS('C',NDIM,ML,MU,1,AMAT,LDA,IPVT,Y1,NDIM2,INFO,Y0)
C
C ... PRD1 = HERM(Y) * L * R * X ...
C
      PRD1 = ZDOTC(NDIM,Y0,INC,X0,INC)
C
C ... KOPIEREN ITERIERTE WERTE ...
C
      CALL ZCOPY(NDIM,X1,INC,X0,INC)
      CALL ZCOPY(NDIM,Y1,INC,Y0,INC)
C
C ... R(I) = B * X(I) ...
C
C      CALL MATVER(ZMA,NDIM,NZMA,NGINT,X0,X1,Y0,Y1)
      CALL ZGBMV('N',NDIM,NDIM,ML,MU,CONE,BMAT,LDB,X0,1,CZERO,X1,1)
      CALL ZGBMV('N',NDIM,NDIM,ML,MU,CONE,BMAT,LDB,Y0,1,CZERO,Y1,1)

C
C ... PRD2 = HERM(Y) * B * X ...
C
      PRD2 = ZDOTC(NDIM,Y0,INC,X1,INC)
C
C
      EWNEU = PRD1/PRD2
C
      EW = EWSHIFT + EWNEU
C
C ... RELATIVE AENDERUNG DES EIGENWERTES ...
C
      DE = ABS(CABS(EWNEU/EWALT)-1.0)
      WRITE(NOUT,11) IT,EW,DE
      WRITE(*,*)'ITERATION:',IT,'EIGENVALUE:',EW,'CHANGE:',DE      

      IF(DE.LE.EPS ) GOTO 30
      IF(IT.GE.ITER ) GOTO 20
C
      EWALT = EWNEU
      DEALT = DE
C
      GOTO 10
   20 WRITE(NOUT,21) IT
C
   30 CONTINUE
C     WRITE(NOUT,31) (X0(JJJ),JJJ=1,NDIM)
C
      RETURN
C
    1 FORMAT(' INFO =',I4,' ON DECOMPOSITION')
   11 FORMAT(1X,' IT : ',I2,' EIGENVALUE : ',1P,2E12.4,
     >       '   REL. CHANGE : ',E12.4,0P)
   21 FORMAT(' STOPPED AFTER ',I4,' ITERATIONS')
   31 FORMAT(' EIGENVECTOR:'/(5(1X,1P,2E12.4,0P)))
      END
************************************************************************
C*DECK SOLV1
C      SUBROUTINE SOLV1
C
************************************************************************
************************************************************************
**                                                                    **
**    BEGIN : MODULES SOLV1 - SOLV5  (EIGENVALUE SOLVERS)             **
**    -----------------------------                                   **
**                                                                    **
************************************************************************
************************************************************************
**                                                                     **
**    SOLV1 : QR-SOLVER (COMPLEX)                                     **
**            VERSION C,  4.7.91                                      **
**                                                                    **
**    STRUCTURE :                                                     **
**                 SOLV1                                              **
**                   (CPOCO)                                          **
**                   (CPOSL)                                          **
**                   (CBAL)                                           **
**                   (CORTH)                                          **
**                   (COMQR)                                          **
**                   (ORDERS)                                         **
**                                                                    **
************************************************************************
************************************************************************
C
C*CALL COMMAX
C*CALL COMPAR
C*CALL COMPIO
C*CALL COMDIM
C*CALL CORE1
C
C      INTEGER  IWORK(257)
C      INTEGER INTGER(NDIM1)
C      CHARACTER*1 UPLO
C      REAL     AMATR(NDIM1,NDIM1), AMATI(NDIM1,NDIM1), DUMMY(NDIM1)
C      COMPLEX  HVEC(NDIM1), WORK(2*NDIM1),RWORK(NDIM1)
C
C      EQUIVALENCE (BMAT(1,1), AMATR(1,1))
C      EQUIVALENCE (BMAT(1,NDIM1/2+1), AMATI(1,1))
C      EQUIVALENCE (HVEC(1), WR(1))
C      EQUIVALENCE (HVEC(NDIM1/2+1), WI(1))
C      EQUIVALENCE (INTGER(1),RWORK(1))
C
C ... QR - ALGORITHM ...
C
C*IF CRAY
C      TIME1 = SECOND()
C*ELSE
C      TIME1 = X05BAF()
C*ENDIF
C
C      CALL CPOCO(BMAT,NDIM1,NDIM,RCOND,HVEC,INFO)
C 
C ... LINPACK ZPOCO REPLACED TO LAPACK ZPOTRF (ALSO USABLE WITH NAG)
C ... FIRST CALCULATE 1-NORM (REQUIRED FOR CONDITION NUMBER)
C     
C      UPLO = 'U'
C      ANORM = F06UCF('1',UPLO,NDIM,BMAT,NDIM1,RWORK)
C      INFO = 0
C      CALL ZPOTRF(UPLO,NDIM,BMAT,NDIM1,INFO)
C      IF(INFO.NE.0) THEN
C         WRITE(NOUT,3)
C         WRITE(NOUT,4) INFO
C         STOP
C      ENDIF
C
C ... ESTIMATE CONDITION NUMBER WITH LAPACK ZPOCON
C
C      CALL ZPOCON(UPLO,NDIM,BMAT,NDIM1,ANORM,RCOND,WORK,RWORK,INFO)
C      IF(INFO.NE.0) THEN
C         WRITE(NOUT,4) INFO
C         STOP
C      ENDIF
C      WRITE(NOUT,1) RCOND
C 
C      CALL ZPOTRS(UPLO,NDIM,NDIM,BMAT,NDIM1,AMAT,NDIM1,INFO)
C      IF(INFO.NE.0) THEN
C         WRITE(NOUT,4) INFO
C         STOP
C      ENDIF
C 
C     DO 10 K=1,NDIM
C        CALL CPOSL(BMAT,NDIM1,NDIM,AMAT(1,K))
C  10 CONTINUE
C  
C      DO 20 J=1,NDIM
C      DO 20 I=1,NDIM
C         AMATR(I,J)=REAL(AMAT(I,J))
C   20 CONTINUE
C      DO 30 J=1,NDIM
C      DO 30 I=1,NDIM
C         AMATI(I,J)=AIMAG(AMAT(I,J))
C   30 CONTINUE
C     CALL CBAL(NDIM1,NDIM,AMATR,AMATI,IS1,IS2,WI)
C     CALL CORTH(NDIM1,NDIM,IS1,IS2,AMATR,AMATI,WR,WI)
C     CALL COMQR(NDIM1,NDIM,IS1,IS2,AMATR,AMATI,WR,WI,IERR)
C      CALL F02AJF(AMATR,NDIM1,AMATI,NDIM1,NDIM,WR,WI,INTGER,IERR)
C
C      IF(INFO.NE.0) THEN
C         WRITE(NOUT,3)
C         STOP
C      ENDIF
C
C*IF CRAY
C      TIME2 = SECOND()
C      WRITE(NOUT,11) TIME2-TIME1
C*ELSE
C      TIME2 = X05BAF()
C      TIMDIF = (TIME2-TIME1)*1.0E-06
C      WRITE(NOUT,11) TIMDIF
C*ENDIF
C
C
C      IF(IERR.NE.0) THEN
C         WRITE(NOUT,31) IERR
C         STOP
C      ENDIF
C
C      DO 40 I=1,NDIM
C         EVMAG(1,I) = CABS(CMPLX(WR(I),WI(I)))
C   40 CONTINUE
C
C*IF CRAY
C      CALL ORDERS(2,IWORK,EVMAG,INDEX,NDIM,1,8,1)
C*ELSE
C      CALL DSORTX(EVMAG,+1,NDIM,INDEX)
C      CALL DSVRGP(NDIM,EVMAG,DUMMY,INDEX)
C*ENDIF
C
C      WRITE(NOUT,41)(I,WR(INDEX(I)),WI(INDEX(I)),I=1,NDIM)
C*IF IBM
C      WRITE(NOUTI,43)(I,WR(INDEX(I)),WI(INDEX(I)),I=1,NDIM)
C*ENDIF
C
C      RETURN
C
C    1 FORMAT('   KONDITION = ',1P,E16.6,0P)
C    3 FORMAT(///5X,'MATRIX BMAT1 NICHT POSITIV DEFINIT')
C    4 FORMAT(' INFO = ',I4)
C   11 FORMAT('   TIME = ',1P,E16.6,0P)
C   31 FORMAT(//' HQR/HQZ :  IERR=',I5)
C   41 FORMAT(1X,I3,'.-EIGENWERT:',1P,2E16.6,0P)
C   43 FORMAT(3X,'VSHIFT(',I3,') = (',1P,E16.6,',',E16.6,'),')
C      END

************************************************************************
C*DECK SOLV2
C      SUBROUTINE SOLV2
C
************************************************************************
************************************************************************
**                                                                    **
**    SOLV2 : INVERSE VECTOR ITERATION, IN-CORE (WITHOUT BMAT)        **
**            VERSION B,  4.7.91                                      **
**                                                                    **
**    STRUCTURE :                                                      **
**                 SOLV2                                              **
**                   MATVER                                           **
**                     CONBMAT                                        **
**                   CSHIFT                                           **
**                     CONBMAT                                        **
**                   CGBFA                                            **
**                     (ICXAMAX)                                      **
**                     (CXSCAL)                                       **
**                     (CXAXPY)                                       **
**                   (CXSCAL)                                         **
**                   CGBSL                                            **
**                     (CXAXPY)                                       **
**                     (CXCOPY)                                       **
**                     (CXDOTC)                                       **
**                   (CXDOTC)                                         **
**                   (CXCOPY)                                         **
**                                                                    **
************************************************************************
************************************************************************
C
C*CALL COMMAX
C*CALL COMPAR
C*CALL COMPAR2
C*CALL COMPIO
C*CALL CORE2
C*CALL COMIT
C*CALL COMGRID
C*CALL COMDIM
C*CALL COMEQUI
C
C      COMPLEX  EWALT, EWNEU
C      COMPLEX  CXDOTC, PRD1, PRD2
C
C      DATA INC /1/
C      DATA QQ /1.E-4/
C
C ... R(0) = B * X(0) ...
C
C      CALL MATVER(ZMA,NDIM,NZMA,NGINT,X0,X1,Y0,Y1)
C      CALL CSHIFT(AMAT,LDA,NDIM,EWSHIFT,ZMA,NZMA)
C
C ... ABSCHAETZEN START-EIGENWERT ...
C
C      EWALT = QQ * EWSHIFT
C
C
C ... ZERLEGEN AMAT : AMAT = L * R ...
C
C      CALL CGBFA(AMAT,LDA,NDIM,ML,MU,IPVT,INFO)
C
C      IF(INFO.NE.0) THEN
C         WRITE(NOUT,1) INFO
C         STOP
C      ENDIF
C
C
C      IT = 0
C
C ... ITERATIONEN ...
C
C   10 CONTINUE
C
C      IT = IT + 1
C
C ... R(I) = DELTA(LAMBDA(I-1)) * R(I-1) ...
C
C      CALL CXSCAL(NDIM,EWALT,X1,INC)
C      CALL CXSCAL(NDIM,CONJG(EWALT),Y1,INC)
C
C ... LOESUNG GL.SYST  ( A - LMBDA * B ) * X(I) = X(I-1) ...
C
C
C      CALL CGBSL(AMAT,LDA,NDIM,ML,MU,IPVT,X1,0,X0)
C      CALL CGBSL(AMAT,LDA,NDIM,ML,MU,IPVT,Y1,1,Y0)
C
C ... PRD1 = HERM(Y) * L * R * X ...
C
C      PRD1 = CXDOTC(NDIM,Y0,INC,X0,INC)
C
C ... KOPIEREN ITERIERTE WERTE ...
C
C      CALL CXCOPY(NDIM,X1,INC,X0,INC)
C      CALL CXCOPY(NDIM,Y1,INC,Y0,INC)
C
C ... R(I) = B * X(I) ...
C
C      CALL MATVER(ZMA,NDIM,NZMA,NGINT,X0,X1,Y0,Y1)
C
C ... PRD2 = HERM(Y) * B * X ...
C
C      PRD2 = CXDOTC(NDIM,Y0,INC,X1,INC)
C      WRITE(NOUT,11) PRD1,PRD2
C
C
C      EWNEU = PRD1/PRD2
C
C      WRITE(NOUT,13) IT
C      EW = EWSHIFT + EWNEU
C      WRITE(NOUT,15) EW
C
C ... RELATIVE AENDERUNG DES EIGENWERTES ...
C
C      DE = ABS(CABS(EWNEU/EWALT)-1.0)
C      WRITE(NOUT,17) EWNEU,DE
C      IF(DE.LE.EPS ) GOTO 30
C      IF(IT.GE.ITER ) GOTO 20
C
C      EWALT = EWNEU
C      DEALT = DE
C
C      GOTO 10
C   20 WRITE(NOUT,21) IT
C
C   30 CONTINUE
C     WRITE(NOUT,31) (X0(JJJ),JJJ=1,NDIM)
C
C      RETURN
C
C    1 FORMAT(' INFO =',I4,' ON DECOMPOSITION')
C   11 FORMAT(///' PRD1 = HERM(Y) * L * R * X  PRD2 = HERM(Y) * B * X',
C     >       1P,2E12.4,2X,2E12.4,0P)
C   13 FORMAT(' ITERATION =',I5)
C   15 FORMAT(' EIGENWERT =',1P,2E14.5)
C   17 FORMAT(' DELT(EW) =',1P,E21.11,E20.11,/' DE =',E20.11,0P)
C   21 FORMAT(' NACH',I4,' ITERATIONEN ABGEBROCHEN')
C   31 FORMAT(' EIGENVEKTOR:'/(5(1X,1P,2E12.4,0P)))
C      END
************************************************************************
*DECK XGBTRS      
      SUBROUTINE XGBTRS( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV, B, LDB,
     $                   INFO, Q )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          TRANS
      INTEGER            INFO, KL, KU, LDAB, LDB, N, NRHS
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * )
      COMPLEX*16         AB( LDAB, * ), B( LDB, * ), Q(LDB,*)
*     ..
*
*  Purpose
*  =======
*
*  ZGBTRS solves a system of linear equations
*     A * X = B,  A**T * X = B,  or  A**H * X = B
*  with a general band matrix A using the LU factorization computed
*  by ZGBTRF.
*
*  Arguments
*  =========
*
*  TRANS   (input) CHARACTER*1
*          Specifies the form of the system of equations.
*          = 'N':  A * X = B     (No transpose)
*          = 'T':  A**T * X = B  (Transpose)
*          = 'C':  A**H * X = B  (Conjugate transpose)
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  KL      (input) INTEGER
*          The number of subdiagonals within the band of A.  KL >= 0.
*
*  KU      (input) INTEGER
*          The number of superdiagonals within the band of A.  KU >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of columns
*          of the matrix B.  NRHS >= 0.
*
*  AB      (input) COMPLEX*16 array, dimension (LDAB,N)
*          Details of the LU factorization of the band matrix A, as
*          computed by ZGBTRF.  U is stored as an upper triangular band
*          matrix with KL+KU superdiagonals in rows 1 to KL+KU+1, and
*          the multipliers used during the factorization are stored in
*          rows KL+KU+2 to 2*KL+KU+1.
*
*  LDAB    (input) INTEGER
*          The leading dimension of the array AB.  LDAB >= 2*KL+KU+1.
*
*  IPIV    (input) INTEGER array, dimension (N)
*          The pivot indices; for 1 <= i <= N, row i of the matrix was
*          interchanged with row IPIV(i).
*
*  B       (input/output) COMPLEX*16 array, dimension (LDB,NRHS)
*          On entry, the right hand side matrix B.
*          On exit, the solution matrix X.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,N).
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ONE
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      LOGICAL            LNOTI, NOTRAN
      INTEGER            I, J, KD, L, LM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZGEMV, ZGERU, ZLACGV, ZSWAP, ZTBSV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      NOTRAN = LSAME( TRANS, 'N' )
      IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) .AND. .NOT.
     $    LSAME( TRANS, 'C' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( KL.LT.0 ) THEN
         INFO = -3
      ELSE IF( KU.LT.0 ) THEN
         INFO = -4
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -5
      ELSE IF( LDAB.LT.( 2*KL+KU+1 ) ) THEN
         INFO = -7
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -10
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZGBTRS', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 .OR. NRHS.EQ.0 )
     $   RETURN
*
      KD = KU + KL + 1
      LNOTI = KL.GT.0
*
      IF( NOTRAN ) THEN
*
*        Solve  A*X = B.
*
*        Solve L*X = B, overwriting B with X.
*
*        L is represented as a product of permutations and unit lower
*        triangular matrices L = P(1) * L(1) * ... * P(n-1) * L(n-1),
*        where each transformation L(i) is a rank-one modification of
*        the identity matrix.
*
         IF( LNOTI ) THEN
            DO 10 J = 1, N - 1
               LM = MIN( KL, N-J )
               L = IPIV( J )
               IF( L.NE.J )
     $            CALL ZSWAP( NRHS, B( L, 1 ), LDB, B( J, 1 ), LDB )
               CALL ZGERU( LM, NRHS, -ONE, AB( KD+1, J ), 1, B( J, 1 ),
     $                     LDB, B( J+1, 1 ), LDB )
   10       CONTINUE
         END IF
*
*        add this line, needed for eigenvalue solver (GTAH)
*
         DO L=1,NRHS
           CALL ZCOPY(N,B(L,1),1,Q(L,1),1)
         ENDDO
*
         DO 20 I = 1, NRHS
*
*           Solve U*X = B, overwriting B with X.
*
            CALL ZTBSV( 'Upper', 'No transpose', 'Non-unit', N, KL+KU,
     $                  AB, LDAB, B( 1, I ), 1 )
   20    CONTINUE
*
      ELSE IF( LSAME( TRANS, 'T' ) ) THEN
*
*        Solve A**T * X = B.
*
         DO 30 I = 1, NRHS
*
*           Solve U**T * X = B, overwriting B with X.
*
            CALL ZTBSV( 'Upper', 'Transpose', 'Non-unit', N, KL+KU, AB,
     $                  LDAB, B( 1, I ), 1 )
   30    CONTINUE
*
*        add this line, needed for eigenvalue solver (GTAH)
*
         DO L=1,NRHS
           CALL ZCOPY(N,B(L,1),1,Q(L,1),1)
         ENDDO
*
*        Solve L**T * X = B, overwriting B with X.
*
         IF( LNOTI ) THEN
            DO 40 J = N - 1, 1, -1
               LM = MIN( KL, N-J )
               CALL ZGEMV( 'Transpose', LM, NRHS, -ONE, B( J+1, 1 ),
     $                     LDB, AB( KD+1, J ), 1, ONE, B( J, 1 ), LDB )
               L = IPIV( J )
               IF( L.NE.J )
     $            CALL ZSWAP( NRHS, B( L, 1 ), LDB, B( J, 1 ), LDB )
   40       CONTINUE
         END IF
*
      ELSE
*
*        Solve A**H * X = B.
*
         DO 50 I = 1, NRHS
*
*           Solve U**H * X = B, overwriting B with X.
*
            CALL ZTBSV( 'Upper', 'Conjugate transpose', 'Non-unit', N,
     $                  KL+KU, AB, LDAB, B( 1, I ), 1 )
   50    CONTINUE
*
*        add this line, needed for eigenvalue solver (GTAH)
*
         DO L=1,NRHS
           CALL ZCOPY(N,B(L,1),1,Q(L,1),1)
         ENDDO
*
*        Solve L**H * X = B, overwriting B with X.
*
         IF( LNOTI ) THEN
            DO 60 J = N - 1, 1, -1
               LM = MIN( KL, N-J )
               CALL ZLACGV( NRHS, B( J, 1 ), LDB )
               CALL ZGEMV( 'Conjugate transpose', LM, NRHS, -ONE,
     $                     B( J+1, 1 ), LDB, AB( KD+1, J ), 1, ONE,
     $                     B( J, 1 ), LDB )
               CALL ZLACGV( NRHS, B( J, 1 ), LDB )
               L = IPIV( J )
               IF( L.NE.J )
     $            CALL ZSWAP( NRHS, B( L, 1 ), LDB, B( J, 1 ), LDB )
   60       CONTINUE
         END IF
      END IF
      RETURN
*
*     End of ZGBTRS
*
      END
      
************************************************************************
*DECK MATVER
      SUBROUTINE MATVER(ZMA,NDIM,NZMA,NGINT,X0,X1,Y0,Y1)
C-----------------------------------------------------------------------
C     REDUCED MEMORY STEUERWALD SOLVER ROUTINES
C-----------------------------------------------------------------------
C
      INTEGER NZMA,I,J,K,NBG,L,NGINT
      COMPLEX ZMA(NZMA,NZMA),CTE
      COMPLEX X0(*),X1(*),Y0(*),Y1(*)
C
      DO 10 I=1,NDIM
         X1(I) = (0.0,0.0)
         Y1(I) = (0.0,0.0)
   10 CONTINUE
C
      NBG = NZMA/2
C
      DO 60 NI=1,NGINT
C
         CALL CONBMAT(NI,NZMA,ZMA)
C
C ... PERFORM MULTIPLICATION ...
C
         DO 30 J=1,NZMA
            CTE = X0((NI-1)*NBG+J)
            DO 20 I=1,NZMA
               X1((NI-1)*NBG+I)=X1((NI-1)*NBG+I)+ZMA(I,J)*CTE
   20       CONTINUE
   30    CONTINUE
         DO 50 J=1,NZMA
            CTE = X0((NI-1)*NBG+J)
            DO 40 I=1,NZMA
               Y1((NI-1)*NBG+I)=Y1((NI-1)*NBG+I)+ZMA(I,J)*CTE
   40       CONTINUE
   50    CONTINUE
   60 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK CSHIFT
      SUBROUTINE CSHIFT(A,LDA,N,EW,ZMA,NZMA)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMGRID
C
      INTEGER  LDA, N, I, K, NBG, NZMA
      COMPLEX  A(LDA,*), EW, ZMA(NZMA,*)
C
      NBG = NZMA/2
C
      DO 30 NI=1,NGINT
         CALL CONBMAT(NI,NZMA,ZMA)
         NB = (NI-1)*NZMA/2
         DO 10 L=1,NZMA
            JZ = NB+L-1
            IZ = 2*NZMA
            DO 10 K=L,NZMA
               JZ = JZ+1
               IZ = IZ-1
               A(IZ,JZ) = A(IZ,JZ)-ZMA(L,K)*EW
   10    CONTINUE
         DO 20 L=2,NZMA
            IZ = 2*NZMA+L-1
            JZ = NB
            DO 20 K=1,L-1
               IZ = IZ-1
               JZ = JZ+1
               A(IZ,JZ) = A(IZ,JZ)-ZMA(L,K)*EW
   20    CONTINUE
   30 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK CGBFA
      SUBROUTINE CGBFA(ABD,LDA,N,ML,MU,IPVT,INFO)
C-----------------------------------------------------------------------
C
C     CGBFA FACTORS A COMPLEX BAND MATRIX BY ELIMINATION.
C
C     CGBFA IS USUALLY CALLED BY CGBCO, BUT IT CAN BE CALLED
C     DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.
C
C     ON ENTRY
C
C        ABD     COMPLEX(LDA, N)
C                CONTAINS THE MATRIX IN BAND STORAGE.  THE COLUMNS
C                OF THE MATRIX ARE STORED IN THE COLUMNS OF  ABD  AND
C                THE DIAGONALS OF THE MATRIX ARE STORED IN ROWS
C                ML+1 THROUGH 2*ML+MU+1 OF  ABD .
C                SEE THE COMMENTS BELOW FOR DETAILS.
C
C          SPEICHERUNG DER MATRIX ABD:
C
C                   1      2  .  . .  .  MU      MU+1 . . . . .  N
C                 ____________________________________________________
C             1  |            FREI
C             2  |            FREI
C             .  |
C            ML  |            FREI
C                 ____________________________________________________
C          ML+1  |   -    - .   .   .    -    A(1,MU+1). . . .A(N-MU,N)
C          ML+2  |   -    - .   .   . A(1,MU) A(2,MU+1). . . A(N-MU+1,N)
C            .   |
C            .   |
C         ML+MU  |   -   A(1,2) A(2,3) . . . . . . . . . . . . A(N-1,N)
C      HAUPTDIAG | A(1,1) A(2,2) . . . . . . . . . . . . . . .  A(N,N)
C        ML+MU+2 | A(2,1) A(3,2) . . . . . . . . . . A(N,N-1)    -
C        ML+MU+3 | A(3,1) A(4,2) . . . . . . A(N,N-2)    -       -
C           .    |
C           .    |
C     2*ML+MU+1  | A(ML+1,1) . . . .  A(N,N-ML) - . . . . . . .  -
C                 ____________________________________________________
C
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  ABD .
C                LDA MUST BE .GE. 2*ML + MU + 1 .
C
C        N       INTEGER
C                THE ORDER OF THE ORIGINAL MATRIX.
C
C        ML      INTEGER
C                NUMBER OF DIAGONALS BELOW THE MAIN DIAGONAL.
C                0 .LE. ML .LT. N .
C
C        MU      INTEGER
C                NUMBER OF DIAGONALS ABOVE THE MAIN DIAGONAL.
C                0 .LE. MU .LT. N .
C                MORE EFFICIENT IF  ML .LE. MU .
C     ON RETURN
C
C        ABD     AN UPPER TRIANGULAR MATRIX IN BAND STORAGE AND
C                THE MULTIPLIERS WHICH WERE USED TO OBTAIN IT.
C                THE FACTORIZATION CAN BE WRITTEN  A = L*U  WHERE
C                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER
C                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.
C
C        IPVT    INTEGER(N)
C                AN INTEGER VECTOR OF PIVOT INDICES.
C
C        INFO    INTEGER
C                = 0  NORMAL VALUE.
C                = K  IF  U(K,K) .EQ. 0.0 .  THIS IS NOT AN ERROR
C                      CONDITION FOR THIS SUBROUTINE, BUT IT DOES
C                     INDICATE THAT SGBSL WILL DIVIDE BY ZERO IF
C                     CALLED.  USE  RCOND  IN SGBCO FOR A RELIABLE
C                     INDICATION OF SINGULARITY.
C
C     BAND STORAGE
C
C           IF  A  IS A BAND MATRIX, THE FOLLOWING PROGRAM SEGMENT
C           WILL SET UP THE INPUT.
C
C                   ML = (BAND WIDTH BELOW THE DIAGONAL)
C                   MU = (BAND WIDTH ABOVE THE DIAGONAL)
C                   M = ML + MU + 1
C                   DO 20 J=1,N
C                      I1 = MAX0(1, J-MU)
C                      I2 = MIN0(N, J+ML)
C                      DO 10 I=I1,I2
C                         K = I - J + M
C                         ABD(K,J) = A(I,J)
C                10    CONTINUE
C                20 CONTINUE
C
C           THIS USES ROWS  ML+1  THROUGH  2*ML+MU+1  OF  ABD .
C           IN ADDITION, THE FIRST  ML  ROWS IN  ABD  ARE USED FOR
C           ELEMENTS GENERATED DURING THE TRIANGULARIZATION.
C           THE TOTAL NUMBER OF ROWS NEEDED IN  ABD  IS  2*ML+MU+1 .
C           THE  ML+MU BY ML+MU  UPPER LEFT TRIANGLE AND THE
C           ML BY ML  LOWER RIGHT TRIANGLE ARE NOT REFERENCED.
C
C     LINPACK. THIS VERSION DATED 08/14/78 .
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
C
C     SUBROUTINES AND FUNCTIONS
C
C     BLAS CXAXPY=C(Z)AXPY,CXSCAL=C(Z)SCAL,ICXAMAX=IC(Z)AMAX
C     FORTRAN MAX0,MIN0,ABS,AIMAG,REAL
C-----------------------------------------------------------------------
C
      INTEGER  I, ICXAMAX, I0, J, JU, JZ, J0, J1, K, KP1, L, LM,M,MM,NM1
      INTEGER  LDA, N, ML, MU, IPVT(*), INFO
      REAL     CABS1
      COMPLEX  ABD(LDA,*)
      COMPLEX  T, ZDUM
C
      CABS1(ZDUM) = ABS(REAL(ZDUM))+ABS(AIMAG(ZDUM))
      M = ML + MU + 1
      INFO = 0
C
C ... ZERO INITIAL FILL-IN COLUMNS ...
C
      J0 = MU + 2
      J1 = MIN0(N,M) - 1
      IF(J1.LT.J0) GOTO 30
         DO 20 JZ=J0,J1
            I0 = M + 1 - JZ
            DO 10 I=I0,ML
               ABD(I,JZ) = (0.0E0,0.0E0)
   10       CONTINUE
   20    CONTINUE
   30 CONTINUE
      JZ = J1
      JU = 0
C
C ... GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING ...
C
      NM1 = N - 1
      IF(NM1.LT.1) GOTO 130
C
      DO 120 K=1,NM1
         KP1 = K + 1
C
C ... ZERO NEXT FILL-IN COLUMN ...
C
         JZ = JZ + 1
         IF(JZ.GT.N) GOTO 50
         IF(ML.LT.1) GOTO 50
            DO 40 I=1,ML
               ABD(I,JZ) = (0.0E0,0.0E0)
   40       CONTINUE
   50    CONTINUE
C
C ... FIND L = PIVOT INDEX ...
C
         LM = MIN0(ML,N-K)
         L = ICXAMAX(LM+1,ABD(M,K),1) + M - 1
         IPVT(K) = L + K - M
C
C ... ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED ...
C
         IF(CABS1(ABD(L,K)).EQ.0.0E0) GOTO 100
C
C ... INTERCHANGE IF NECESSARY ...
C
         IF(L.EQ.M) GOTO 60
            T = ABD(L,K)
            ABD(L,K) = ABD(M,K)
            ABD(M,K) = T
   60    CONTINUE
C
C ... COMPUTE MULTIPLIERS ...
C
         T = -(1.0E0,0.0E0)/ABD(M,K)
         CALL CXSCAL(LM,T,ABD(M+1,K),1)
C
C ... ROW ELIMINATION WITH COLUMN INDEXING ...
C
         JU = MIN0(MAX0(JU,MU+IPVT(K)),N)
         MM = M
         IF(JU.LT.KP1) GOTO 90
            DO 80 J=KP1,JU
               L = L - 1
               MM = MM - 1
               T = ABD(L,J)
               IF(L.EQ.MM) GOTO 70
                  ABD(L,J) = ABD(MM,J)
                  ABD(MM,J) = T
   70          CONTINUE
               CALL CXAXPY(LM,T,ABD(M+1,K),1,ABD(MM+1,J),1)
   80       CONTINUE
   90    CONTINUE
         GOTO 110
  100    CONTINUE
         INFO = K
  110    CONTINUE
  120 CONTINUE
C
  130 CONTINUE
      IPVT(N) = N
      IF(CABS1(ABD(M,N)).EQ.0.0E0) INFO = N
C
      RETURN
      END
************************************************************************
*DECK CGBSL
      SUBROUTINE CGBSL(ABD,LDA,N,ML,MU,IPVT,B,JOB,Q)
C-----------------------------------------------------------------------
C     ======================================================
C     =    CGBSL SOLVES THE COMPLEX BAND SYSTEM            =
C     =    A * X = B  OR  TRANS(A) * X = B                 =
C     =    USING THE FACTORS COMPUTED BY CGBCO OR CGBFA.   =
C     ======================================================
C
C     ON ENTRY
C
C        ABD     COMPLEX(LDA, N)
C                THE OUTPUT FROM CGBCO OR CGBFA.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  ABD .
C
C        N       INTEGER
C                THE ORDER OF THE ORIGINAL MATRIX.
C
C        ML      INTEGER
C                NUMBER OF DIAGONALS BELOW THE MAIN DIAGONAL.
C
C        MU      INTEGER
C                NUMBER OF DIAGONALS ABOVE THE MAIN DIAGONAL.
C
C        IPVT    INTEGER(N)
C                THE PIVOT VECTOR FROM SGBCO OR SGBFA.
C
C        B       COMPLEX(N)
C                THE RIGHT HAND SIDE VECTOR.
C
C        JOB     INTEGER
C                = 0         TO SOLVE  A*X = B ,
C                = NONZERO   TO SOLVE  HERM(A)*X = B , WHERE
C                         HERM(A)  IS THE CONJUGATE TRANSPOSE.
C                             OF MATRIX A.
C
C     ON RETURN
C
C        B       THE SOLUTION VECTOR  X .
C
C     ERROR CONDITION
C
C        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS A
C        ZERO ON THE DIAGONAL.  TECHNICALLY THIS INDICATES SINGULARITY
C        BUT IT IS OFTEN CAUSED BY IMPROPER ARGUMENTS OR IMPROPER
C        SETTING OF LDA .  IT WILL NOT OCCUR IF THE SUBROUTINES ARE
C        CALLED CORRECTLY AND IF CGBCO HAS SET RCOND .GT. 0.0
C        OR CGBFA HAS SET INFO .EQ. 0 .
C
C     TO COMPUTE  INVERSE(A) * C  WHERE  C  IS A MATRIX
C     WITH  P  COLUMNS
C           CALL CGBCO(ABD,LDA,N,ML,MU,IPVT,RCOND,Z)
C           IF(RCOND IS TOO SMALL) GOTO ...
C           DO 10 J=1,P
C              CALL CGBSL(ABD,LDA,N,ML,MU,IPVT,C(1,J),0)
C        10 CONTINUE
C
C     LINPACK. THIS VERSION DATED 08/14/78 .
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
C
C     SUBROUTINES AND FUNCTIONS
C
C     BLAS CXAXPY=C(Z)AXPY,CXDOTC=C(Z)DOTC
C     FORTRAN MIN0,CONJG
C
C-----------------------------------------------------------------------
C
      INTEGER  LDA, N, ML, MU, IPVT(*), JOB
      INTEGER  K, KB, L, LA, LB, LM, M, NM1
      COMPLEX  ABD(LDA,*), B(*), Q(*)
      COMPLEX  CXDOTC, T
C
      M = MU + ML + 1
      NM1 = N - 1
C
      IF(JOB.NE.0) GOTO 60
C
C ... JOB = 0 , SOLVE  A * X = B ...
C ... FIRST SOLVE L*Y = B ...
C
      IF(ML.EQ.0) GOTO 30
      IF(NM1.LT.1) GOTO 30
         DO 20 K=1,NM1
            LM = MIN0(ML,N-K)
            L = IPVT(K)
            T = B(L)
            IF(L.EQ.K) GOTO 10
               B(L) = B(K)
               B(K) = T
   10       CONTINUE
            CALL CXAXPY(LM,T,ABD(M+1,K),1,B(K+1),1)
   20    CONTINUE
   30 CONTINUE
      CALL CXCOPY(N,B,1,Q,1)
C
C ... NOW SOLVE  U*X = Y ...
C
      DO 50 KB=1,N
         K = N + 1 - KB
         T = -B(K)/ABD(M,K)
         LM = MIN0(K,M) - 1
         LA = M - LM
         LB = K - LM
         B(K) = -T
         DO 40 KL=1,LM
            B(LB)=B(LB)+T*ABD(LA,K)
            LA=LA+1
            LB=LB+1
   40    CONTINUE
   50 CONTINUE
C
      GOTO 110
C
   60 CONTINUE
C
C ... JOB = NONZERO, SOLVE  HERM(A) * X = B ...
C ... FIRST SOLVE  HERM(U)*Y = B ...
C
      DO 70 K=1,N
         LM = MIN0(K,M) - 1
         LA = M - LM
         LB = K - LM
         T = CXDOTC(LM,ABD(LA,K),1,B(LB),1)
         B(K) = (B(K) - T)/CONJG(ABD(M,K))
   70 CONTINUE
      CALL CXCOPY(N,B,1,Q,1)
C
C ... NOW SOLVE HERM(L)*X = Y ...
C
      IF(ML.EQ.0) GOTO 100
      IF(NM1.LT.1) GOTO 100
         DO 90 KB=1,NM1
            K = N - KB
            LM = MIN0(ML,N-K)
            B(K) = B(K) + CXDOTC(LM,ABD(M+1,K),1,B(K+1),1)
            L = IPVT(K)
            IF(L.EQ.K) GOTO 80
               T = B(L)
               B(L) = B(K)
               B(K) = T
   80       CONTINUE
   90    CONTINUE
  100 CONTINUE
C
  110 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK SOLV3
      SUBROUTINE SOLV3
C
************************************************************************
************************************************************************
**                                                                    **
**    SOLV3 : INVERSE VECTOR ITERATION, OUT-OF-CORE (SCHWARZ)         **
**            VERSION G, 6.5.92                                       **
**                 (FEHLER AUS VERS.F (SR.RITER) FUER N*NCV=NG        **
**                  BESEITIGT)                                        **
**                                                                    **
**    STRUCTURE :                                                     **
**                 SOLV3                                              **
**                   VIT                                              **
**                     (CXSCAL)                                       **
**                     CGES2P                                         **
**                   VITER                                            **
**                     (CXSCAL)                                       **
**                     CGES2P                                         **
**                   (SKIPR)                                          **
**                   RITER                                            **
**                     CGES2P                                         **
**                     (CXCOPY)                                       **
**                     CONBMAT                                        **
**                                                                    **
************************************************************************
************************************************************************
C
*CALL COMMAX
*CALL COMPAR
*CALL COMPAR3
*CALL COMPIO
*CALL CORE3
*CALL COMIT
*CALL COMINT
*CALL COMGRID
*CALL COMEQUI
C
      COMPLEX  QP, YR, DLI, DLIM1
C
      DATA QQ /1.E-4/
C
      DLIM1 = QQ*EWSHIFT
C
*IF KUL
      WAIT(ND4,ID=LWRIT,COND=I1)
      IF(I1.NE.1) STOP ' ND4/SOLV3'
      WAIT(ND3,ID=LWRIT,COND=I1)
      IF(I1.NE.1) STOP ' ND3/SOLV3'
      WAIT(ND6,ID=LWRIT,COND=I1)
      IF(I1.NE.1) STOP ' ND6/SOLV3'
*ENDIF
*IF CRAY
      IF(UNIT(ND4).GE.0.0) STOP 'ND4/SOLV3'
      IF(UNIT(ND3).GE.0.0) STOP 'ND3/SOLV3'
      IF(UNIT(ND6).GE.0.0) STOP 'ND6/SOLV3'
*ENDIF
      REWIND ND3
      REWIND ND4
      REWIND ND6
C
C ... LOESUNG DES GLEICHUNGSSYSTEMS ...
C
      IT = 1
C
      CALL VIT(NBG,NB3,NCV,NG,HVX,QP,APR,IPVT,X,DLIM1)
C     --------
      GOTO 30
C
   10 IT = IT + 1
C
      IF(IT.GT.ITER) GOTO 40
      DLIM1 = DLI
C
      CALL VITER(NBG,NB3,NCV,NG,HVX,QP,APR,IPVT,X,DLIM1)
C     ----------
C
   30 CALL RITER(NBG,NB3,NCV,NG,HVX,HVX2,YR,APR,IPVT,ZMA,BUFF,X,EV)
C     ----------
C
      DLI = QP/YR
C     WRITE(NOUT,31) IT,DLI,DLIM1,QP,YR
C
      EW = EWSHIFT+DLI
      WRITE(NOUT,33) IT,EW
      IF(ABS(CABS(DLI/DLIM1)-1.0).LE.EPS ) GOTO 50
      GOTO 10
   40 CONTINUE
      WRITE(NOUT,41) IT-1
   50 CONTINUE
*IF CRAY
      WRITE(NOUTI,51) IT,EW
*ENDIF
*IF KUL
      WAIT(ND4,ID=NREC,COND=I1)
      IF(I1.NE.1) STOP ' ND4/SOLV3'
*ENDIF
      RETURN
C
   31 FORMAT(///1X,I2,'-TE ITERATION, DELTA LAMBDA (I)=',1P,2E12.4,
     >       ' DLIM1=',2E12.4/18X,'QP=',2E12.4/18X,'YR=',2E12.4)
   33 FORMAT(///1X,I3,' ITERATIONEN, EIGENWERT:',1P,2E14.6)
   41 FORMAT(' NACH',I4,' ITERATIONEN ABGEBROCHEN')
   51 FORMAT(1X,I3,' ITERATIONEN, EIGENWERT:',1P,2E14.6)
      END
************************************************************************
*DECK VIT
      SUBROUTINE VIT (NBG,NB3,NCV,NG,HVX,QP,APR,IPVT,X,DLIALT)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMPIO
*CALL COMINT
C
      INTEGER  IPVT(NBG,NCV,*)
      COMPLEX  SUM, CXDOTU, CXDOTC, QP, DLIALT
      COMPLEX  APR(NBG,NB3,NCV,*), X(NBG,NCV,2,*), HVX(NBG,*)
C
      NBG1 = NBG+1
      LREAD = 1
      LWRIT = 0
*IF KUL
      READ(ND3,ID=LREAD) APR(1,1,1,1)...APR(NBG,NB3,NCV,1)
      READ(ND4,ID=LREAD) X(1,1,1,1)...X(NBG,NCV,2,1)
      READ(ND6,ID=LREAD) IPVT(1,1,1)...IPVT(NBG,NCV,1)
*ELSEIF IBM
      READ(ND3) (((APR(II,IJ,IK,1),II=1,NBG),IJ=1,NB3),IK=1,NCV)
      READ(ND4) (((X(II,IJ,IK,1),II=1,NBG),IJ=1,NCV),IK=1,2)
      READ(ND6) ((IPVT(II,IJ,1),II=1,NBG),IJ=1,NCV)
*ENDIF
*IF CRAY
      BUFFER IN(ND3,0) (APR(1,1,1,1),APR(NBG,NB3,NCV,1))
      BUFFER IN(ND4,0) (X(1,1,1,1),X(NBG,NCV,2,1))
      BUFFER IN(ND6,0) (IPVT(1,1,1),IPVT(NBG,NCV,1))
*ENDIF
      CPLG = 1
      CPLGX = 1
      CPLGXA = 3
      CPLGXS = 2
C
      QP = (0.0,0.0)
C
      DO 50 I=1,NG
C     ------------
C
      IZAS = MOD(I-1,NCV)+1
      IF(IZAS.EQ.1) THEN
         CPLGA = CPLG
         CPLG = 3-CPLG
         IND3XA = CPLGXA
         CPLGXA = CPLGX
         CPLGX = CPLGXS
         CPLGXS = IND3XA
*IF KUL
         WAIT(ND3,ID=LREAD,COND=I1)
         IF(I1.NE.1) STOP ' VIT/ND3 '
         WAIT(ND6,ID=LREAD,COND=I1)
         IF(I1.NE.1) STOP ' VIT/ND6 '
         WAIT(ND4,ID=LREAD,COND=I1)
         IF(I1.NE.1) STOP ' VIT/ND4 '
*ENDIF
*IF CRAY
         IF(UNIT(ND3).GE.0.0) STOP ' VIT/ND3 '
         IF(UNIT(ND6).GE.0.0) STOP ' VIT/ND6 '
         IF(UNIT(ND4).GE.0.0) STOP ' VIT/ND4 '
*ENDIF
         IF(LREAD.LT.NREC) THEN
            LREAD = LREAD+1
*IF KUL
            READ(ND3,ID=LREAD) APR(1,1,1,CPLG)...APR(NBG,NB3,NCV,CPLG)
            READ(ND6,ID=LREAD) IPVT(1,1,CPLG)...IPVT(NBG,NCV,CPLG)
            READ(ND4,ID=LREAD) X(1,1,1,CPLGX)...X(NBG,NCV,2,CPLGX)
*ELSEIF IBM
            READ(ND3)(((APR(II,IJ,IK,CPLG),II=1,NBG),IJ=1,NB3),IK=1,NCV)
            READ(ND6) ((IPVT(II,IJ,CPLG),II=1,NBG),IJ=1,NCV)
            READ(ND4) (((X(II,IJ,IK,CPLGX),II=1,NBG),IJ=1,NCV),IK=1,2)
*ENDIF
*IF CRAY
            BUFFER IN(ND3,0) (APR(1,1,1,CPLG),APR(NBG,NB3,NCV,CPLG))
            BUFFER IN(ND6,0) (IPVT(1,1,CPLG),IPVT(NBG,NCV,CPLG))
            BUFFER IN(ND4,0) (X(1,1,1,CPLGX),X(NBG,NCV,2,CPLGX))
*ENDIF
         ENDIF
         IF(I.GT.NCV) THEN
            LWRIT = LWRIT+1
*IF KUL
            IF(LWRIT.NE.1) THEN
               WAIT(ND5,ID=LWRIT-1,COND=I1)
               IF(I1.NE.1) STOP ' VIT/ND5 '
            ENDIF
            WRITE(ND5,ID=LWRIT) X(1,1,1,CPLGXS)...X(NBG,NCV,2,CPLGXS)
*ELSEIF IBM
            WRITE(ND5) (((X(II,IJ,IK,CPLGXS),II=1,NBG),IJ=1,NCV),IK=1,2)
*ENDIF
*IF CRAY
            IF(LWRIT.NE.1) THEN
               IF(UNIT(ND5).GE.0.0) STOP 'VIT/ND5 '
            ENDIF
            BUFFER OUT(ND5,0) (X(1,1,1,CPLGXS),X(NBG,NCV,2,CPLGXS))
*ENDIF
         ENDIF
      ENDIF
C
      CALL CXSCAL(NBG,DLIALT,X(1,IZAS,1,CPLGXA),1)
      CALL CXSCAL(NBG,CONJG(DLIALT),X(1,IZAS,2,CPLGXA),1)
C
      IF(I.EQ.1) GOTO 20
      DO 10 K1=1,NBG
         SUM = CXDOTU(NBG,APR(K1,1,IZAS,CPLGA),NBG,HVX(1,2),1)
         X(K1,IZAS,1,CPLGXA) = X(K1,IZAS,1,CPLGXA)-SUM
         X(K1,IZAS,2,CPLGXA) = X(K1,IZAS,2,CPLGXA)-HVX(K1,1)
   10 CONTINUE
   20 CALL CGES2P(APR(1,NBG1,IZAS,CPLGA),NBG,NBG,X(1,IZAS,1,CPLGXA),
     >            X(1,IZAS,2,CPLGXA),IPVT(1,IZAS,CPLGA),HVX(1,3),0)
C
      DO 30 K1=1,NBG
         SUM = CXDOTC(NBG,APR(1,K1+2*NBG,IZAS,CPLGA),1,
     >               X(1,IZAS,2,CPLGXA),1)
         HVX(K1,1) = SUM
   30 CONTINUE
C
C ... Q(*)*P ...
C
      SUM = (0.0,0.0)
      DO 40 J=1,NBG
         JPV = IPVT(J,IZAS,CPLGA)
         SUM=SUM+CONJG(X(JPV,IZAS,2,CPLGXA))*X(J,IZAS,1,CPLGXA)
   40 CONTINUE
      QP = QP+SUM
      CALL CXCOPY (NBG,X(1,IZAS,1,CPLGXA),1,HVX(1,2),1)
C
   50 CONTINUE
C     --------
C
      LWRIT = LWRIT+1
*IF KUL
      IF(LWRIT.NE.1) THEN
         WAIT(ND5,ID=LWRIT-1,COND=I1)
         IF(I1.NE.1) STOP ' VIT/ND5 '
      ENDIF
      WRITE(ND5,ID=LWRIT) X(1,1,1,CPLGXA)...X(NBG,NCV,2,CPLGXA)
*ELSEIF IBM
      WRITE(ND5) (((X(II,IJ,IK,CPLGXA),II=1,NBG),IJ=1,NCV),IK=1,2)
*ENDIF
*IF CRAY
      IF(LWRIT.NE.1) THEN
         IF(UNIT(ND5).GE.0.0) STOP 'VIT/ND5 '
      ENDIF
      BUFFER OUT(ND5,0) (X(1,1,1,CPLGXA),X(NBG,NCV,2,CPLGXA))
*ENDIF
      REWIND ND4
C
      RETURN
      END
************************************************************************
*DECK VITER
      SUBROUTINE VITER(NBG,NB3,NCV,NG,HVX,QP,APR,IPVT,X,DLIALT)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMPIO
*CALL COMINT
C
      INTEGER  IPVT(NBG,NCV,*)
      COMPLEX  SUM, CXDOTU, CXDOTC, QP, DLIALT
      COMPLEX  APR(NBG,NB3,NCV,*), X(NBG,NCV,2,*), HVX(NBG,*)
C
      NBG1 = NBG+1
      LREAD = 1
      LWRIT = 0
      CPLG = CPLGA
      IND3XA = CPLGXS
      CPLGXS = CPLGX
      CPLGX = CPLGXA
      CPLGXA = IND3XA
C
      QP = (0.0,0.0)
*IF KUL
      WAIT(ND4,ID=NREC,COND=I1)
      IF(I1.NE.1) STOP ' VIT/ND4 '
*ENDIF
*IF CRAY
      IF(UNIT(ND4).GE.0.0) STOP 'VIT/ND4 '
*ENDIF
C
      DO 50 I=1,NG
C     ------------
C
      IZAS = MOD(I-1,NCV)+1
      IF(IZAS.EQ.1) THEN
         CPLGA = CPLG
         CPLG = 3-CPLG
         IND3XA = CPLGXA
         CPLGXA = CPLGX
         CPLGX = CPLGXS
         CPLGXS = IND3XA
*IF KUL
         IF(LREAD.NE.1) THEN
            WAIT(ND3,ID=LREAD,COND=I1)
            IF(I1.NE.1) STOP ' VIT/ND3 '
            WAIT(ND6,ID=LREAD,COND=I1)
            IF(I1.NE.1) STOP ' VIT/ND6 '
            WAIT(ND4,ID=LREAD,COND=I1)
            IF(I1.NE.1) STOP ' VIT/ND4 '
         ENDIF
*ENDIF
*IF CRAY
         IF(LREAD.NE.1) THEN
            IF(UNIT(ND3).GE.0.0) STOP 'VIT/ND3 '
            IF(UNIT(ND6).GE.0.0) STOP 'VIT/ND6 '
            IF(UNIT(ND4).GE.0.0) STOP 'VIT/ND4 '
         ENDIF
*ENDIF
         IF(LREAD.LT.NREC) THEN
            LREAD = LREAD+1
*IF KUL
            READ(ND3,ID=LREAD) APR(1,1,1,CPLG)...APR(NBG,NB3,NCV,CPLG)
            READ(ND6,ID=LREAD) IPVT(1,1,CPLG)...IPVT(NBG,NCV,CPLG)
*ELSEIF IBM
            READ(ND3)(((APR(II,IJ,IK,CPLG),II=1,NBG),IJ=1,NB3),IK=1,NCV)
            READ(ND6) ((IPVT(II,IJ,CPLG),II=1,NBG),IJ=1,NCV)
*ENDIF
*IF CRAY
            BUFFER IN(ND3,0) (APR(1,1,1,CPLG),APR(NBG,NB3,NCV,CPLG))
            BUFFER IN(ND6,0) (IPVT(1,1,CPLG),IPVT(NBG,NCV,CPLG))
*ENDIF
            BACKSPACE ND4
            BACKSPACE ND4
*IF KUL
            READ(ND4,ID=LREAD) X(1,1,1,CPLGX)...X(NBG,NCV,2,CPLGX)
*ELSEIF IBM
            READ(ND4) (((X(II,IJ,IK,CPLGX),II=1,NBG),IJ=1,NCV),IK=1,2)
*ENDIF
*IF CRAY
            BUFFER IN(ND4,0) (X(1,1,1,CPLGX),X(NBG,NCV,2,CPLGX))
*ENDIF
         ENDIF
         IF(I.GT.NCV) THEN
            LWRIT = LWRIT+1
*IF KUL
            IF(LWRIT.NE.1) THEN
               WAIT(ND5,ID=LWRIT-1,COND=I1)
               IF(I1.NE.1) STOP ' VIT/ND5 '
            ENDIF
            WRITE(ND5,ID=LWRIT) X(1,1,1,CPLGXS)...X(NBG,NCV,2,CPLGXS)
*ELSEIF IBM
            WRITE(ND5) (((X(II,IJ,IK,CPLGXS),II=1,NBG),IJ=1,NCV),IK=1,2)
*ENDIF
*IF CRAY
            IF(LWRIT.NE.1) THEN
               IF(UNIT(ND5).GE.0.0) STOP 'VIT/ND5 '
            ENDIF
            BUFFER OUT (ND5,0) (X(1,1,1,CPLGXS),X(NBG,NCV,2,CPLGXS))
*ENDIF
         ENDIF
      ENDIF
C
      CALL CXSCAL(NBG,DLIALT,X(1,IZAS,1,CPLGXA),1)
      CALL CXSCAL(NBG,CONJG(DLIALT),X(1,IZAS,2,CPLGXA),1)
C
      IF(I.EQ.1) GOTO 20
      DO 10 K1=1,NBG
         SUM = CXDOTU(NBG,APR(K1,1,IZAS,CPLGA),NBG,HVX(1,2),1)
         X(K1,IZAS,1,CPLGXA) = X(K1,IZAS,1,CPLGXA)-SUM
         X(K1,IZAS,2,CPLGXA) = X(K1,IZAS,2,CPLGXA)-HVX(K1,1)
   10 CONTINUE
   20 CALL CGES2P(APR(1,NBG1,IZAS,CPLGA),NBG,NBG,X(1,IZAS,1,CPLGXA),
     >            X(1,IZAS,2,CPLGXA),IPVT(1,IZAS,CPLGA),HVX(1,3),0)
C
      DO 30 K1=1,NBG
         SUM = CXDOTC(NBG,APR(1,K1+2*NBG,IZAS,CPLGA),1,
     >               X(1,IZAS,2,CPLGXA),1)
        HVX(K1,1) = SUM
   30 CONTINUE
C
C ... Q(*)*P ...
C
      SUM = (0.0,0.0)
      DO 40 J=1,NBG
         JPV = IPVT(J,IZAS,CPLGA)
         SUM = SUM+CONJG(X(JPV,IZAS,2,CPLGXA))*X(J,IZAS,1,CPLGXA)
   40 CONTINUE
      QP = QP+SUM
      CALL CXCOPY(NBG,X(1,IZAS,1,CPLGXA),1,HVX(1,2),1)
C
   50 CONTINUE
C     --------
C
      LWRIT = LWRIT+1
*IF KUL
      IF(LWRIT.NE.1) THEN
         WAIT(ND5,ID=LWRIT-1,COND=I1)
         IF(I1.NE.1) STOP ' VIT/ND5 '
      ENDIF
      WRITE(ND5,ID=LWRIT) X(1,1,1,CPLGXA)...X(NBG,NCV,2,CPLGXA)
*ELSEIF IBM
      WRITE(ND5) (((X(II,IJ,IK,CPLGXA),II=1,NBG),IJ=1,NCV),IK=1,2)
*ENDIF
*IF CRAY
      IF(LWRIT.NE.1) THEN
         IF(UNIT(ND5).GE.0.0) STOP 'VIT/ND5 '
      ENDIF
      BUFFER OUT(ND5,0) (X(1,1,1,CPLGXA),X(NBG,NCV,2,CPLGXA))
*ENDIF
      REWIND ND4
C
      RETURN
      END
************************************************************************
*DECK RITER
      SUBROUTINE RITER(NBG,NB3,NCV,NG,HVX,HVX2,YR,APR,IPVT,ZMA,BUFF,
     >                 X,EV)
C-----------------------------------------------------------------------
C     VERSION : 18.9.92
C        BMAT WIRD JEDESMAL NEU BERECHNET
C     SEIT 6.5.92:
C        FEHLER FUER ALLE N*NCV=NG BESEITIGT
C     SEIT 9.9.91:
C     1. BMAT MUSS NICHT MEHR SYMMETRISCH SEIN
C     2. EIGENVEKTOR FUER PLOT WIRD NICHT MEHR WEGGESSCHRIEBEN,SONDERN
C        IN EV GESPEICHERT
C-----------------------------------------------------------------------
C
*CALL COMPIO
*CALL COMINT
C
      INTEGER  IPVT(NBG,NCV,*)
      COMPLEX  BUFF(NBG,*),ZMA(2*NBG,*), EV(*)
      COMPLEX  APR(NBG,NB3,NCV,*), X(NBG,NCV,2,*),HVX(NBG,*),HVX2(NBG,*)
      COMPLEX  SUM, SUMY, CXDOTU, CXDOTC, YR
C
      NBG1 = NBG+1
      NZMA=2*NBG
      LWRIT = 0
      CPLG = CPLGA
      IND3XA = CPLGXS
      CPLGXS = CPLGX
      CPLGX = CPLGXA
      CPLGXA = IND3XA
C
      YR = (0.0,0.0)
      DO 10 K=1,NBG
         HVX(K,2)  = (0.0,0.0)
         HVX2(K,2) = (0.0,0.0)
   10 CONTINUE
         DO 20 J=1,NB3
         DO 20 L=1,NBG
            BUFF(L,J) = (0.0,0.0)
   20    CONTINUE
C
C
      DO 130 NI=NG,1,-1
C     ----------------
C
      IZAS = MOD(NI-1,NCV)+1
      IF(IZAS.EQ.NCV.OR.NI.EQ.NG) THEN
         CPLGA = CPLG
         CPLG = 3-CPLG
         JHILF = CPLGXA
         CPLGXA = CPLGX
         CPLGX = CPLGXS
         CPLGXS = JHILF
         IND3X = CPLGXA
*IF KUL
         IF(LREAD.NE.NREC) THEN
            WAIT(ND3,ID=LREAD,COND=I1)
            IF(I1.NE.1) STOP ' RIT/ND3 '
            WAIT(ND6,ID=LREAD,COND=I1)
            IF(I1.NE.1) STOP ' RIT/ND6 '
         ENDIF
         WAIT(ND5,ID=LREAD,COND=I1)
         IF(I1.NE.1)  STOP ' RIT/ND5 '
*ENDIF
*IF CRAY
         IF(LREAD.NE.NREC) THEN
            IF(UNIT(ND3).GE.0.0) STOP 'RIT/ND3 '
            IF(UNIT(ND6).GE.0.0) STOP 'RIT/ND6 '
         ENDIF
         IF(UNIT(ND5).GE.0.0) STOP 'RIT/ND5 '
*ENDIF
         IF(LREAD.GT.1) THEN
            LREAD = LREAD-1
            BACKSPACE ND3
            BACKSPACE ND3
*IF KUL
            READ(ND3,ID=LREAD) APR(1,1,1,CPLG)...APR(NBG,NB3,NCV,CPLG)
*ELSEIF IBM
            READ(ND3)(((APR(II,IJ,IK,CPLG),II=1,NBG),IJ=1,NB3),IK=1,NCV)
*ENDIF
*IF CRAY
            BUFFER IN(ND3,0) (APR(1,1,1,CPLG),APR(NBG,NB3,NCV,CPLG))
*ENDIF
            BACKSPACE ND6
            BACKSPACE ND6
*IF KUL
            READ(ND6,ID=LREAD) IPVT(1,1,CPLG)...IPVT(NBG,NCV,CPLG)
*ELSEIF IBM
            READ(ND6) ((IPVT(II,IJ,CPLG),II=1,NBG),IJ=1,NCV)
*ENDIF
*IF CRAY
            BUFFER IN(ND6,0) (IPVT(1,1,CPLG),IPVT(NBG,NCV,CPLG))
*ENDIF
            BACKSPACE ND5
            BACKSPACE ND5
*IF KUL
            READ(ND5,ID=LREAD) X(1,1,1,CPLGX)...X(NBG,NCV,2,CPLGX)
*ELSEIF IBM
            READ(ND5) (((X(II,IJ,IK,CPLGX),II=1,NBG),IJ=1,NCV),IK=1,2)
*ENDIF
*IF CRAY
            BUFFER IN(ND5,0) (X(1,1,1,CPLGX),X(NBG,NCV,2,CPLGX))
*ENDIF
         ENDIF
      ENDIF
C
      IF(NI.EQ.NG) GOTO 40
      DO 30 K1=1,NBG
         IPVK1 = IPVT(K1,IZAS,CPLGA)
         SUM = CXDOTU(NBG,APR(IPVK1,2*NBG+1,IZAS,CPLGA),NBG,HVX(1,2),1)
         X(K1,IZAS,1,CPLGXA) = X(K1,IZAS,1,CPLGXA)-SUM
         X(IPVK1,IZAS,2,CPLGXA) = X(IPVK1,IZAS,2,CPLGXA)-HVX(K1,1)
   30 CONTINUE
   40 CALL CGES2P(APR(1,NBG1,IZAS,CPLGA),NBG,NBG,X(1,IZAS,1,CPLGXA),
     >            X(1,IZAS,2,CPLGXA),IPVT(1,IZAS,CPLGA),HVX(1,3),1)
      DO 50 K1=1,NBG
         SUM = CXDOTC(NBG,APR(1,K1,IZAS,CPLGA),1,X(1,IZAS,2,CPLGXA),1)
         HVX(K1,1) = SUM
   50 CONTINUE
      CALL CXCOPY(NBG,HVX(1,2),1,HVX(1,3),1)
      CALL CXCOPY(NBG,HVX2(1,2),1,HVX2(1,3),1)
      CALL CXCOPY(NBG,X(1,IZAS,1,CPLGXA),1,HVX(1,2),1)
      CALL CXCOPY(NBG,X(1,IZAS,2,CPLGXA),1,HVX2(1,2),1)
C
      INDEX = (NI-1)*NBG+1
      CALL CXCOPY(NBG,HVX(1,2),1,EV(INDEX),1)
C
C ... B*X ...
C
      IF(NI.EQ.NG) GOTO 100
      DO 60 L=1,NBG
         SUM = CXDOTU(NBG,BUFF(L,1),NBG,HVX(1,2),1)
         X(L,JZAS,1,IND3XA) = X(L,JZAS,1,IND3XA)+SUM
   60 CONTINUE
      YR = YR+CXDOTC(NBG,X(1,JZAS,2,IND3XA),1,X(1,JZAS,1,IND3XA),1)
      DO 70 L=1,NBG
         SUMY = CXDOTU(NBG,BUFF(L,1),NBG,HVX2(1,2),1)
         X(L,JZAS,2,IND3XA) = HVX2(L,1)+SUMY
   70 CONTINUE
      DO 80 J=1,NB3
      DO 80 L=1,NBG
          BUFF(L,J) = (0.0,0.0)
   80 CONTINUE
      DO 90 L=1,NZMA
         CALL CXCOPY(NBG,ZMA(1,L),1,BUFF(1,NBG+L),1)
   90 CONTINUE
  100 IF(NI.GT.1) THEN
         CALL CONBMAT(NI-1,NZMA,ZMA)
         DO 110 J=1,NZMA
         DO 110 L=1,NBG
            BUFF(L,J) = BUFF(L,J)+ZMA(NBG+L,J)
  110    CONTINUE
      ENDIF
      IF(IZAS.EQ.NCV.AND.NI.LT.NG) THEN
         LWRIT = LWRIT+1
*IF KUL
         IF(LWRIT.NE.1) THEN
            WAIT(ND4,ID=LWRIT-1,COND=I1)
            IF(I1.NE.1) STOP ' RIT/ND4 '
         ENDIF
         WRITE(ND4,ID=LWRIT) X(1,1,1,CPLGXS)...X(NBG,NCV,2,CPLGXS)
*ELSEIF IBM
         WRITE(ND4) (((X(II,IJ,IK,CPLGXS),II=1,NBG),IJ=1,NCV),IK=1,2)
*ENDIF
*IF CRAY
         IF(LWRIT.NE.1) THEN
            IF(UNIT(ND4).GE.0.0) STOP 'RIT/ND4 '
         ENDIF
         BUFFER OUT (ND4,0) (X(1,1,1,CPLGXS),X(NBG,NCV,2,CPLGXS))
*ENDIF
      ENDIF
      DO 120 L=1,NBG
         SUM  = CXDOTU(NBG,BUFF(L,NBG1),NBG,HVX(1,2),1)
     >          +CXDOTU(NBG,BUFF(L,2*NBG+1),NBG,HVX(1,3),1)
         SUMY = CXDOTU(NBG,BUFF(L,NBG1),NBG,HVX2(1,2),1)
     >          +CXDOTU(NBG,BUFF(L,2*NBG+1),NBG,HVX2(1,3),1)
         X(L,IZAS,1,CPLGXA) = SUM
         HVX2(L,1) = SUMY
  120 CONTINUE
C
      JZAS = IZAS
      IND3XA = IND3X
      INDA = CPLGA
C
  130 CONTINUE
C     --------
C
      YR = YR+CXDOTC(NBG,X(1,IZAS,2,CPLGXA),1,X(1,IZAS,1,CPLGXA),1)
      CALL CXCOPY(NBG,HVX2(1,1),1,X(1,IZAS,2,CPLGXA),1)
      LWRIT = LWRIT+1
*IF KUL
      IF(LWRIT.NE.1) THEN
         WAIT(ND4,ID=LWRIT-1,COND=I1)
         IF(I1.NE.1) STOP ' RIT/ND4 '
      ENDIF
      WRITE(ND4,ID=LWRIT) X(1,1,1,CPLGXA)...X(NBG,NCV,2,CPLGXA)
*ELSEIF IBM
      WRITE(ND4) (((X(II,IJ,IK,CPLGXA),II=1,NBG),IJ=1,NCV),IK=1,2)
*ENDIF
*IF CRAY
      IF(LWRIT.NE.1) THEN
         IF(UNIT(ND4).GE.0.0) STOP 'RIT/ND4 '
      ENDIF
      BUFFER OUT(ND4,0) (X(1,1,1,CPLGXA),X(NBG,NCV,2,CPLGXA))
*ENDIF
      REWIND ND5
C
      RETURN
      END
************************************************************************
*DECK SOLV4
      SUBROUTINE SOLV4
C
************************************************************************
************************************************************************
**                                                                    **
**    SOLV4 : INVERSE VECTOR ITERATION,                               **
**            IN-CORE VERSION OF THE OUT-OF-CORE SOLVER               **
**            VERSION C,  4.7.91                                      **
**                                                                    **
**    STRUCTURE :                                                     **
**                 SOLV4                                              **
**                   ASLUIC                                           **
**                     CGESLP                                         **
**                       (CXDOTU)                                     **
**                     CGEFAP                                         **
**                       ICMAXP                                       **
**                   VITERIC                                          **
**                     (CXSCAL)                                       **
**                     (CXDOTU)                                       **
**                     CGES2P                                         **
**                       (CXCOPY)                                     **
**                     (CXDOTC)                                       **
**                     (CXCOPY)                                       **
**                   RITERIC                                          **
**                     (CXDOTU)                                       **
**                     CGES2P                                         **
**                       (CXCOPY)                                     **
**                     (CXDOTC)                                       **
**                     (CXCOPY)                                       **
**                     CONBMAT                                        **
**                                                                    **
************************************************************************
************************************************************************
C
*CALL COMMAX
*CALL COMPAR
*CALL COMPAR4
*CALL COMPIO
*CALL CORE4
*CALL COMIT
*CALL COMGRID
*CALL COMEQUI
C
      COMPLEX  QP, YR, DLI, DLIM1, EWT
C
      DATA QQ /1.E-4/
C
      DLI = QQ*EWSHIFT
C
C ... L-U-ZERLEGUNG VON APR ...
C
      CALL ASLUIC(NBG,NB3,NCVIC,NG,APR,IPVT,HVX)
C     -----------
C
C ... LOESUNG DES GLEICHUNGSSYSTEMS ...
C
      IT = 0
C
   10 IT = IT + 1
      IF(IT.GT.ITER) GOTO 20
      DLIM1 = DLI
C
      CALL VITERIC(NBG,NB3,NCVIC,NG,HVX,HVX2,QP,APR,IPVT,X,DLIM1)
C     ------------
C
      CALL RITERIC(NBG,NB3,NCVIC,NG,HVX,HVX2,YR,APR,IPVT,X,EV,ZMA,NZMA)
C     ------------
C
      DLI = QP/YR
      EW = EWSHIFT+DLI
      WRITE(NOUT,11) IT,EW,CABS(DLI/DLIM1)-1.
      WRITE(*,11) IT,EW,CABS(DLI/DLIM1)-1.
      IF(ABS(CABS(DLI/DLIM1)-1.0).LE.EPS ) GOTO 30
      GOTO 10
C
   20 WRITE(NOUT,21) IT-1
C
   30 CONTINUE
C
      RETURN
C
   11 FORMAT(1X,' IT : ',I2,' EIGENVALUE : ',1P,2E12.4,
     >       '   REL. CHANGE : ',E12.4)
   21 FORMAT(' STOPPED AFTER ',I4,' ITERATIONS')
      END
************************************************************************
*DECK ASLUIC
      SUBROUTINE ASLUIC(NBG,NB3,NCV,NG,APR,IPVT,HVX)
C-----------------------------------------------------------------------
C     L-U-ZERLEGUNG VON APR
C-----------------------------------------------------------------------
C
      INTEGER  IPVT(NBG,NCV)
      COMPLEX  SUM, ZZ, CXDOTU
      COMPLEX  APR(NBG,NB3,*), HVX(NBG,*)
C
      NBG1 = NBG+1
      NBG2 = 2*NBG+1
C
      DO 50 I=1,NG
C
C ... A' = L*U ...
C ... L-U-ZERLEGUNG VON APR ...
C
         IF(I.EQ.1) GOTO 40
C
         CALL CGESLP(APR(1,NBG1,I-1),NBG,NBG,
     >               APR(1,1,I),IPVT(1,I-1),HVX,1)
C
         DO 30 K2=1,NBG
            K2A = 2*NBG+K2
            DO 10 KUS=1,NBG
               HVX(KUS,1) = APR(IPVT(KUS,I-1),K2A,I-1)
   10       CONTINUE
            DO 20 K1=1,NBG
               SUM = CXDOTU(NBG,APR(K1,1,I),NBG,HVX(1,1),1)
               ZZ = APR(K1,NBG+K2,I)-SUM
               APR(K1,NBG+K2,I) = ZZ
   20       CONTINUE
   30    CONTINUE
C
   40    CALL CGEFAP(APR(1,NBG1,I),NBG,NBG,IPVT(1,I),IER)
         IF(I.EQ.NG) GOTO 50
         CALL CGESLP(APR(1,NBG1,I),NBG,NBG,
     >               APR(1,NBG2,I),IPVT(1,I),HVX,0)
C
   50 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK VITERIC
      SUBROUTINE VITERIC(NBG,NB3,NCV,NG,HVX,HVX2,QP,APR,
     >                   IPVT,X,DLIALT)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
      INTEGER  IPVT(NBG,*)
      COMPLEX  SUM, CXDOTU, CXDOTC, QP, DLIALT
      COMPLEX  APR(NBG,NB3,*), X(NBG,NCV,*), HVX(NBG,*), HVX2(NBG,*)
C
      NBG1 = NBG+1
      QP = (0.0,0.0)
C
      DO 50 I=1,NG
C
         CALL CXSCAL(NBG,DLIALT,X(1,I,1),1)
         CALL CXSCAL(NBG,CONJG(DLIALT),X(1,I,2),1)
C
         IF(I.EQ.1) GOTO 20
         DO 10 K1=1,NBG
            SUM = CXDOTU(NBG,APR(K1,1,I),NBG,HVX(1,2),1)
            X(K1,I,1) = X(K1,I,1)-SUM
            X(K1,I,2) = X(K1,I,2)-HVX(K1,1)
   10    CONTINUE
   20    CALL CGES2P(APR(1,NBG1,I),NBG,NBG,X(1,I,1),
     >               X(1,I,2),IPVT(1,I),HVX(1,3),0)
C
         DO 30 K1=1,NBG
            SUM = CXDOTC(NBG,APR(1,K1+2*NBG,I),1,X(1,I,2),1)
            HVX(K1,1) = SUM
   30    CONTINUE
C
C ... Q(*)*P ...
C
         SUM = (0.0,0.0)
         DO 40 J=1,NBG
            JPV = IPVT(J,I)
            SUM = SUM+CONJG(X(JPV,I,2))*X(J,I,1)
   40    CONTINUE
         QP = QP+SUM
         CALL CXCOPY(NBG,X(1,I,1),1,HVX(1,2),1)
C
   50 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK RITERIC
      SUBROUTINE RITERIC(NBG,NB3,NCV,NG,HVX,HVX2,YR,APR,
     >                   IPVT,X,EV,ZMA,NZMA)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMINT
C
      INTEGER  IND1, IPVT(NBG,*)
      COMPLEX  SUM, CXDOTU, CXDOTC, YR
      COMPLEX  EV(NBG,*)
      COMPLEX  ZMA(NZMA,*)
      COMPLEX  APR(NBG,NB3,*), X(NBG,NCV,*), HVX(NBG,*), HVX2(NBG,*)
C
      N1 = NBG+1
      NGINT = NG - 1
C
      YR=(0.0,0.0)
C
      DO 40 I=NG,1,-1
C     -----------------
C
      IF(I.EQ.NG) GOTO 20
      DO 10 K1=1,NBG
         IPVK1 = IPVT(K1,I)
         SUM = CXDOTU(NBG,APR(IPVK1,2*NBG+1,I),NBG,HVX(1,2),1)
         X(K1,I,1) = X(K1,I,1)-SUM
         X(IPVK1,I,2) = X(IPVK1,I,2)-HVX(K1,1)
   10 CONTINUE
   20 CALL CGES2P(APR(1,N1,I),NBG,NBG,X(1,I,1),
     >            X(1,I,2),IPVT(1,I),HVX(1,2),1)
      DO 30 K1=1,NBG
         SUM = CXDOTC(NBG,APR(1,K1,I),1,X(1,I,2),1)
         HVX(K1,1) = SUM
   30 CONTINUE
      CALL CXCOPY(NBG,X(1,I,1),1,HVX(1,2),1)
      CALL CXCOPY(NBG,X(1,I,1),1,EV(1,I),1)
C
   40 CONTINUE
C     --------
C
      CALL CXCOPY(NBG,X(1,NG,1),1,HVX(1,1),1)
      CALL CXCOPY(NBG,X(1,NG,2),1,HVX(1,2),1)
C
      CALL CONBMAT(NGINT,NZMA,ZMA)
C
      DO 50 K=1,NBG
         X(K,NG,1) = CXDOTU(NBG,ZMA(NBG+K,N1),NZMA,HVX(1,1),1)
         X(K,NG,2) = CXDOTU(NBG,ZMA(NBG+K,N1),NZMA,HVX(1,2),1)
   50 CONTINUE
C
      DO 90 I=NGINT,1,-1
C     ----------------------
C
      DO 60 K=1,NBG
         X(K,I+1,1) =X(K,I+1,1)+CXDOTU(NBG,ZMA(NBG+K,1),NZMA,X(1,I,1),1)
         X(K,I+1,2) =X(K,I+1,2)+CXDOTU(NBG,ZMA(NBG+K,1),NZMA,X(1,I,2),1)
   60 CONTINUE
C
      YR = YR+CXDOTC(NBG,HVX(1,2),1,X(1,I+1,1),1)
C
      CALL CXCOPY(NBG,HVX(1,1),1,HVX2(1,1),1)
      CALL CXCOPY(NBG,HVX(1,2),1,HVX2(1,2),1)
      CALL CXCOPY(NBG,X(1,I,1),1,HVX(1,1),1)
      CALL CXCOPY(NBG,X(1,I,2),1,HVX(1,2),1)
C
      DO 70 K=1,NBG
         X(K,I,1) = CXDOTU(NBG,ZMA(K,1),NZMA,HVX(1,1),1)
     >            + CXDOTU(NBG,ZMA(K,N1),NZMA,HVX2(1,1),1)
         X(K,I,2) = CXDOTU(NBG,ZMA(K,1),NZMA,HVX(1,2),1)
     >            + CXDOTU(NBG,ZMA(K,N1),NZMA,HVX2(1,2),1)
   70 CONTINUE
C
      IF(I.GT.1) THEN
C
         CALL CONBMAT(I-1,NZMA,ZMA)
C
         DO 80 K=1,NBG
            X(K,I,1) = X(K,I,1) +
     >                 CXDOTU(NBG,ZMA(NBG+K,N1),NZMA,HVX(1,1),1)
            X(K,I,2) = X(K,I,2) +
     >                 CXDOTU(NBG,ZMA(NBG+K,N1),NZMA,HVX(1,2),1)
   80    CONTINUE
C
      ENDIF
C
   90 CONTINUE
C     --------
C
      YR = YR+CXDOTC(NBG,HVX(1,2),1,X(1,1,1),1)
C
      RETURN
      END
************************************************************************
*DECK CGESLP
      SUBROUTINE CGESLP(A,LDA,N,B,IPVT,HVX,JOB)
C-----------------------------------------------------------------------
C
C     CGESL SOLVES THE COMPLEX SYSTEM
C     L * X = B  OR  CTRANS(U) * X = B  (A=L*U)
C     USING THE FACTORS COMPUTED BY CGECO OR CGEFA.
C
C     ON ENTRY
C
C        A       COMPLEX(LDA, N)
C                THE OUTPUT FROM CGECO OR CGEFA.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  A .
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX  A .
C
C        B       COMPLEX(LDA,N)
C                THE RIGHT HAND SIDE VECTOR.
C
C        IPVT    INTEGER(N)
C                PIVOT VECTOR
C
C        JOB     INTEGER
C                = 0         TO SOLVE  L*X = B ,
C                = NONZERO   TO SOLVE  X*U = B
C
C     ON RETURN
C
C        B       THE SOLUTION VECTOR  X .
C
C     ERROR CONDITION
C
C        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS A
C        ZERO ON THE DIAGONAL.  TECHNICALLY THIS INDICATES SINGULARITY
C        BUT IT IS OFTEN CAUSED BY IMPROPER ARGUMENTS OR IMPROPER
C        SETTING OF LDA .  IT WILL NOT OCCUR IF THE SUBROUTINES ARE
C        CALLED CORRECTLY AND IF CGECO HAS SET RCOND .GT. 0.0
C        OR CGEFA HAS SET INFO .EQ. 0 .
C-----------------------------------------------------------------------
C
      INTEGER  LDA, N, JOB
      INTEGER  IPVT(*)
      INTEGER  K, KB, NM1
      COMPLEX  A(LDA,*), B(LDA,*), HVX(*)
      COMPLEX  CXDOTU, T
C
      NM1 = N - 1
      IF(JOB.NE.0) GOTO 60
C
C ... JOB = 0 , SOLVE  L * X = B ...
C
      IF(NM1.LT.1) GOTO 100
      DO 50 J=1,N
         DO 10 II=1,N
            HVX(II) = B(IPVT(II),J)
   10    CONTINUE
         DO 30 K=1,NM1
            T = -HVX(K)
            DO 20 I=1,N-K
               KPI = K+I
               HVX(KPI) = HVX(KPI)+T*A(IPVT(KPI),K)
   20       CONTINUE
   30    CONTINUE
         DO 40 II=1,N
            B(IPVT(II),J) = HVX(II)
   40    CONTINUE
   50 CONTINUE
C
      GOTO 100
   60 CONTINUE
C
C ... JOB = NONZERO, SOLVE  X * U = B ...
C
      DO 90 J=1,N
         B(J,1) = B(J,1)/A(IPVT(1),1)
         DO 80 K=2,N
            DO 70 I=1,K-1
              HVX(I) = A(IPVT(I),K)
   70       CONTINUE
            T = CXDOTU(K-1,HVX,1,B(J,1),LDA)
            B(J,K) = (B(J,K)-T)/A(IPVT(K),K)
   80    CONTINUE
   90 CONTINUE
C
  100 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK CGEFAP
      SUBROUTINE CGEFAP(A,LDA,N,IPVT,INFO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
      INTEGER  LDA, N, INFO, INC, IPVT(*)
      INTEGER  ICMAXP, I, J, K, KP1, L
      REAL     CABS1
      COMPLEX  A(LDA,*)
      COMPLEX  T, ZDUM
C
      DATA INC/1/
C
      CABS1(ZDUM) = ABS(REAL(ZDUM))+ABS(AIMAG(ZDUM))
      INFO = 0
      DO 10 I=1,N
         IPVT(I) = I
   10 CONTINUE
C
      DO 50 K=1,N
         KP1 = K+1
         L = ICMAXP(N-K+1,A(1,K),IPVT(K),INC)+K-1
         IF(L.NE.K) THEN
            I = IPVT(K)
            IPVT(K) = IPVT(L)
            IPVT(L) = I
         ENDIF
         L  = IPVT(K)
         IF(CABS1(A(L,K)).EQ.0.0E0) THEN
            INFO = K
            GOTO 50
         ENDIF
         T = (1.0,0.0)/A(L,K)
         DO 20 J=KP1,N
            A(IPVT(J),K) = T*A(IPVT(J),K)
   20    CONTINUE
         DO 40 J=KP1,N
            T = A(L,J)
            DO 30 I=KP1,N
               A(IPVT(I),J) = A(IPVT(I),J)-T*A(IPVT(I),K)
   30       CONTINUE
   40    CONTINUE
   50 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK ICMAXP
      INTEGER FUNCTION ICMAXP(L,Q,IP,INC)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
      INTEGER  IP(*)
      COMPLEX  Q(*)
C
      ICMAXP = 1
      IPVM = IP(ICMAXP)
      RMAX = ABS(REAL(Q(IPVM)))+ABS(AIMAG(Q(IPVM)))
      IF(L.LE.1) RETURN
C
      DO 10 I=INC+1,L,INC
         IF(ABS(REAL(Q(IP(I))))+ABS(AIMAG(Q(IP(I)))).LE.RMAX) GOTO 10
         ICMAXP = I
         IPVM = IP(I)
         RMAX = ABS(REAL(Q(IPVM)))+ABS(AIMAG(Q(IPVM)))
   10 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK CGES2P
      SUBROUTINE CGES2P(A,LDA,N,R,S,IPVT,HVX,JOB)
C-----------------------------------------------------------------------
C
C     CGESL2 SOLVES THE COMPLEX SYSTEM
C     L * P = R AND U(*) * Q = S  OR  U * X = R AND L(*) * Y = S
C     USING THE FACTORS COMPUTED BY CGECO OR CGEFA.
C
C     ON ENTRY
C
C        A       COMPLEX(LDA, N)
C                THE OUTPUT FROM CGECO OR CGEFA.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  A .
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX  A .
C
C        R,S     COMPLEX(LDA)
C                THE RIGHT HAND SIDE VECTOR.
C
C        IPVT    INTEGER(LDA)
C                PIVOT VECTOR.
C
C        JOB     INTEGER
C                = 0         TO SOLVE  L*P = R , U(*)*Q = S
C                = NONZERO   TO SOLVE  U*X = R , L(*)*Y = S
C
C     ON RETURN
C
C        R,S     THE SOLUTION VECTOR  P,Q OR X,Y
C
C-----------------------------------------------------------------------
C
      INTEGER  LDA, N, JOB, IPVT(*)
      INTEGER  K, KB, NM1
      COMPLEX  A(LDA,*), R(*), S(*), HVX(*)
      COMPLEX  T
C
      NM1 = N - 1
      IF(JOB.NE.0) GOTO 50
C     ---------------------
C
C ... JOB = 0 , SOLVE  L * P = R  AND  U(*) * Q = S ...
C
      IF(NM1.LT.1) GOTO 100
      DO 10 I=1,N
         HVX(I) = R(IPVT(I))
   10 CONTINUE
      DO 20 K=1,NM1
         T = -HVX(K)
         DO 20 I=1,N-K
            KPI = K+I
            HVX(KPI) = HVX(KPI)+T*A(IPVT(KPI),K)
   20 CONTINUE
      CALL CXCOPY(N,HVX,1,R,1)
C
      HVX(IPVT(1)) = S(1)
      IF(A(IPVT(1),1).NE.(0.,0.)) HVX(IPVT(1))=S(1)/CONJG(A(IPVT(1),1))
      DO 40 K=2,N
         IPK = IPVT(K)
         IF(A(IPK,K).EQ.(0.0,0.0)) GOTO 40
         T = (0.0,0.0)
         DO 30 I=1,K-1
            IPI = IPVT(I)
            T = T+CONJG(A(IPI,K))*HVX(IPI)
   30    CONTINUE
         HVX(IPK) = (S(K)-T)/CONJG(A(IPK,K))
   40 CONTINUE
      CALL CXCOPY(N,HVX,1,S,1)
C
      GOTO 100
   50 CONTINUE
C     --------
C
C ... JOB = NONZERO, SOLVE  U * X = R  AND  L(*) * Y = S ...
C
      IF(A(IPVT(N),N).NE.(0.0,0.0)) R(N) = R(N)/A(IPVT(N),N)
      DO 70 K=NM1,1,-1
         IPK = IPVT(K)
         IF(A(IPK,K).EQ.(0.0,0.0)) GOTO 70
         T = (0.0,0.0)
         DO 60 I=K+1,N
            T = T+A(IPK,I)*R(I)
   60    CONTINUE
         R(K) = (R(K)-T)/A(IPK,K)
   70 CONTINUE
C
      DO 90 K=N-1,1,-1
         IPK = IPVT(K)
         T = (0.0,0.0)
         DO 80 I=K+1,N
            IPI = IPVT(I)
            T = T+CONJG(A(IPI,K))*S(IPI)
   80    CONTINUE
         S(IPK) = S(IPK)-T
   90 CONTINUE
C
  100 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK SOLV5
      SUBROUTINE SOLV5
C
************************************************************************
************************************************************************
**                                                                    **
**    SOLV5 : LANCZOS SOLVER BY JANE CULLUM                           **
**            MODIFIED BY J. STEUERWALD                               **
**            MODIFIED BY ELISABETH SCHWARZ                           **
**            VERSION C, 16.7.91                                      **
**                                                                    **
**    STRUCTURE :                                                     **
**                 SOLV5                                              **
**                   (RANSET)                                         **
**                   ABSHIF                                           **
**                     CGBFA                                          **
**                   LANCZS                                           **
**                     BMATV                                          **
**                     ABSOLV                                         **
**                       CGBSLL                                       **
**                   TNORM                                            **
**                   COMPEV                                           **
**                     CMTQL1                                         **
**                     SINVER                                         **
**                   LUMP                                             **
**                   COMGAP                                           **
**                   ISOEV                                            **
**                   INVERR                                           **
**                                                                    **
************************************************************************
************************************************************************
C
*CALL COMMAX
*CALL COMPAR
*CALL COMPAR5
*CALL COMPIO
*CALL COMPCON
*CALL CORE5
*CALL COMDIM
*CALL MACHT
*CALL SHIFT
*CALL LANTOL
*CALL COMDIM5
*CALL LANCZOS
*CALL CONLAN
*CALL ISEED
C
      INTEGER  SVSOLD
C
      REAL     ERRTOL, CLSTOL, CR, GTEMP
      REAL     TEMPX, TEMPY
      REAL     SEED, TEMP4, TEMP5, TEMP6, TEMP7
      REAL     BTOL, GAPTOL, EPSM, TSCALE
      REAL     SCALE1, SCALE2, SPUTOL, MULTOL, MSPUTO
      REAL     TEMP, TKMAX, EVMAX, BKMIN, T0, TEMP2
C
      COMPLEX  ALPHA(MXAB), BETA(MXAB+1)
      COMPLEX  SSIGMA(MXLP)
      COMPLEX  BETAM, Z, LAMBDS, LAMBDA, RESIDX, RESIDY
      COMPLEX   HVVS, HVWS, HCONEV
C
*IF CRAY
      CALL RANSET(3141593)
*ENDIF
C
*IF CRAY
      MACHEP = SMACH(1)*.3136E-1
*ELSE
      MACHEP = 0.5*X02AJF(0.0)*.3136E-1
*ENDIF
      EPSM = 2.0E0*MACHEP
      SCALE1 = 5.0E2
      SCALE2 = 5.0E0
      BTOL = 0.E0
      GAPTOL = 1.0E-7
      CLSTOL = 1.E-4
      ERRTOL = 1.E-4
      LTOL = 1.E6
      LST = 5
      MOLD = 0
      MOLD1 = 1
      NLOOP = 0
      NCONV = 0
C
      IF(SAVTEV.LT.0.AND.ISTART.EQ.0) THEN
         WRITE(NOUT,1) SAVTEV,ISTART
         STOP
      ENDIF
      LS = NUS
      LC = 0
C
      IF(ISHIFT.NE.1) THEN
         AMY   = AMAX1(ZERO,YLIMB)
         DX    = (AMIN1(ZERO,XLIMR) - XLIML)/4.0
         DY    = (YLIMT - AMY)/4.0
         OWS(1) = CMPLX(XLIML+2.*DX,AMY+2.*DY)
         OWS(2) = CMPLX(XLIML+DX,AMY+DY)
         OWS(3) = CMPLX(XLIML+3.*DX,AMY+3.*DY)
         OWS(4) = CMPLX(XLIML+DX,AMY+3.*DY)
         OWS(5) = CMPLX(XLIML+3.*DX,AMY+DY)
         LS = 5
         LC = 0
         WRITE(NOUT,3) (OWS(L), L=1,LS)
         DO 20 J=1,LS
            IF(OWS(J).EQ.SIGMA) THEN
               IF(J.LT.LS) THEN
                  DO 10 K=J+1,LS
                     OWS(K-1) = OWS(K)
   10             CONTINUE
               ENDIF
               LS = LS -1
            ENDIF
   20    CONTINUE
      ELSE
         LS = NUS
         WRITE(NOUT,21) (J,OWS(J),J=1,NUS)
      ENDIF
C
      LSO = LS
C
      IF(ISTART.EQ.0) GOTO 100
C
      READ(NIN2,23) MOLD,NOLD,SVSOLD
      IF(KMAX.LT.MOLD) KMAX = MOLD
      KMAX1 = KMAX + 1
      ITEMP = (NOLD-NDIM)**2+(SVSEED-SVSOLD)**2
C
      IF(ITEMP.NE.0) THEN
         WRITE(NOUT,25) NOLD,NDIM,SVSEED,SVSOLD
         STOP
      ENDIF
      MOLD1 = MOLD+1
      READ(NIN2,27) (ALPHA(J),J=1,MOLD)
      READ(NIN2,27) (BETA(J),J=1,MOLD1)
      IF(KMAX.EQ.MOLD) GOTO 110
C
C----------------------------------------------------------------------
C
  100 CALL ABSHIF(SIGMA,AA,BB,AORIG,IPVT,LDAA,LDAL,LDBL,NDIM)
C     -----------
      CALL LANCZS(V1,VS,V2,W1,WS,W2,ALPHA,BETA,GR,GC,
     >            KMAX,MOLD1,AA,BB,IPVT,LDAA,LDBL,NDIM)
C     -----------
      IF(GR(1).EQ.-1.E3) THEN
         LC = LC+1
         IF(LC.GT.LS) THEN
            WRITE(NOUT,101) NLOOP
            GOTO 600
         ELSE
            GOTO 510
         ENDIF
      ENDIF
      NLOOP = NLOOP + 1
       LSO = LS
      SSIGMA(NLOOP) = SIGMA
      KMAX1 = KMAX + 1
      IF(ISTOP.LE.0) THEN
         WRITE(NOUT2,103) KMAX,NDIM,SVSEED,SIGMA
         WRITE(NOUT2,105) (ALPHA(I), I=1,KMAX)
         WRITE(NOUT2,105) (BETA(I), I=1,KMAX1)
         IF(ISTOP.EQ.0) THEN
            WRITE(NOUT,107)
            IF(KMAX.NE.MOLD) WRITE(NOUT2,109)
            STOP
         ENDIF
      ENDIF
  110 BKMIN = BTOL
C     WRITE(NOUT,111)
      CALL TNORM(ALPHA,BETA,BKMIN,TKMAX,KMAX,IB)
C     ----------
      TSCALE = TKMAX
      MEV = KMAX
      MP1 = MEV + 1
      BETAM = BETA(MP1)
      IF(IB.LT.0) THEN
         T0 = BTOL
         CALL TNORM(ALPHA,BETA,T0,TSCALE,MEV,IBMEV)
C        ----------
         TEMP = T0/TKMAX
         IBMEV = IABS(IBMEV)
         IF(TEMP.LT.BTOL) THEN
            TEMP = CABS(BETA(IBMEV))
            WRITE(NOUT,113) MEV,IBMEV,TEMP
            STOP
         ENDIF
      ENDIF
  120 CONTINUE
      MULTOL = 500.E0 * FLOAT(MEV+1000) * EPSM
      SPUTOL =  MULTOL
      KT = 1
      V1(KT) = CMPLX(TSCALE,ZERO)
      CALL COMPEV(ALPHA,BETA,V1,V2,VS,GR,GC,
     >            MULTOL,SPUTOL,EPSM,MP,MP2,MEV,NDIS,SAVTEV)
C     -----------
      IF(NDIS.EQ.0) THEN
         WRITE(NOUT,121)
         STOP
      ENDIF
      LOOP = NDIS
      MSPUTO = 500.E0*SPUTOL
      CALL LUMP(VS,V1,GR,RELTOL,MSPUTO,SCALE2,MP,MP2,LOOP)
C     ---------
      IF(LOOP.LT.0) STOP
      IF(NDIS.NE.LOOP) WRITE(NOUT,123) NDIS,LOOP,MEV
      NDIS = LOOP
      ITAG = 1
      CALL COMGAP(VS,GR,GG,MP,MP2,NDIS,ITAG)
C     -----------
       BETA(MP1) = BETAM
      CALL ISOEV(VS,GR,GG,GAPTOL,SPUTOL,SCALE1,MP,NDIS,NNG,NISO)
C     ----------
      WRITE(NOUT,125) NNG,NISO,NDIS
      IF(NISO.NE.0) THEN
         IT = MXINIT
         CALL INVERR(ALPHA,BETA,V1,V2,VS,EPSM,GR,GC,G,GG,
     >               MP,MP2,MEV,NDIS,NISO,IT)
C        -----------
         DO 130 I=1,NISO
  130       WS(I) = ONE/V2(I)+SIGMA
      ELSE
         WRITE(NOUT,131)
      ENDIF
      T0 = CABS(BETAM)
C     WRITE(NOUT,133)
C     WRITE(NOUT,135) (J,VS(J),G(J),GR(J),J=1,NISO)
      DO 140 I=1,NDIS
         WS(I) = (ONE/VS(I)) + SIGMA
  140 CONTINUE
      DO 150 I=1,NDIS
         GC(I) = ZERO
  150 CONTINUE
      L=0
      IC = 0
      DO 160 I=1,NDIS
         IF(MP(I).EQ.0) THEN
            IC = IC + 1
         ELSE
            IF(MP(I).EQ.1) THEN
               L = L+1
               GC(I) = ABS(G(L))/T0
            ENDIF
         ENDIF
  160 CONTINUE
      DO 190 K=2,NDIS
         HVGC = GC(K)
         IHV = K
         DO 170 J=K-1,1,-1
            IF(HVGC.LT.GC(J)) IHV = J
  170    CONTINUE
         IF(IHV.NE.K) THEN
            HVVS = VS(K)
            HVWS = WS(K)
            IHVMP = MP(K)
            DO 180 I=K,IHV+1,-1
               GC(I) = GC(I-1)
               VS(I) = VS(I-1)
               WS(I) = WS(I-1)
               MP(I) = MP(I-1)
  180       CONTINUE
            GC(IHV) = HVGC
            VS(IHV) = HVVS
            WS(IHV) = HVWS
            MP(IHV) = IHVMP
         ENDIF
  190 CONTINUE
      L = 0
      DO 200 J=1,NDIS
         IF(MP(J).NE.0) THEN
            L = L+1
            CONEVB(L) = WS(J)
            MULEVB(L) = MP(J)
            ERREVB(L) = GC(J)
            MP2(L) = J
         ENDIF
  200 CONTINUE
      NEVG = L
      WRITE(NOUT,201) NEVG,NLOOP,MEV
      WRITE(NOUT,203) SIGMA, T0
C     WRITE(NOUT,205)
C     WRITE(NOUT,207)
C     WRITE(NOUT,209) (J,CONEVB(J),MULEVB(J),ERREVB(J),J=1,NEVG)
C
      DO 210 I=1,NEVG
         IF(ABS(ERREVB(I)).GT.ERRTOL) GOTO 220
  210 CONTINUE
  220 NCONVB = I-1
C
      IF(NCONVB.LE.1) GOTO 250
C
      DO  240 K=1,NCONVB-1
         IF(MULEVB(K).EQ.0) GOTO 240
         K1 = K+1
         DO 230 J=K1,NCONVB
            IF((MULEVB(J).EQ.0).OR.
     >      (CABS(CONEVB(K) - CONEVB(J))/CABS(CONEVB(K)).GT.CLSTOL))
     >         GOTO 230
            IF(ERREVB(J).GT.ERREVB(K)) THEN
               MULEVB(J) = 0
               MP(MP2(J)) = 0
            ELSE
               MULEVB(K) = 0
               MP(MP2(K)) = 0
            ENDIF
  230    CONTINUE
  240 CONTINUE
C
  250 WRITE(NOUT,251) NCONVB,NLOOP,MEV
      IF(NCONVB.EQ.0) GOTO 300
C     WRITE(NOUT,253)
C     WRITE(NOUT,207)
C     WRITE(NOUT,209) (J,CONEVB(J),MULEVB(J),ERREVB(J),J=1,NCONVB)
      IF(NLOOP.EQ.1.OR.NCONV.EQ.0) GOTO 280
      DO 270 K=1,NCONVB
         IF(MULEVB(K).EQ.0) GOTO 270
         ICLOSE = 1
         LAMBDS = CONEVB(K)
         IF(NCONV.NE.1) THEN
            DO 260 J=2,NCONV
               IF(MULEV(J).EQ.0) GOTO 260
               IF(CABS(LAMBDS-CONEV(J)).LT.CABS(LAMBDS-CONEV(ICLOSE)))
     >                              ICLOSE = J
  260       CONTINUE
         ENDIF
         IF(CABS(LAMBDS-CONEV(ICLOSE))/CABS(LAMBDS).GT.CLSTOL)
     >      GOTO 270
         IF(ERREV(ICLOSE).GT.ERREVB(K)) THEN
            CONEV(ICLOSE) = CONEVB(K)
            ERREV(ICLOSE) = ERREVB(K)
            MULEV(ICLOSE) = MULEVB(K)
            ISIGMA(ICLOSE) = NLOOP
         ENDIF
         MULEVB(K) = 0
         MP(MP2(K)) = 0
  270 CONTINUE
  280 L = NCONV
      ICOUNT = 0
      DO 290 I=1,NCONVB
         IF(MULEVB(I).NE.0) THEN
            L = L+1
            ICOUNT = ICOUNT + 1
            CONEV(L) = CONEVB(I)
            MULEV(L) = MULEVB(I)
            ERREV(L) = ERREVB(I)
            ISIGMA(L) = NLOOP
            ILOOP(L) = NLOOP
         ENDIF
  290 CONTINUE
      NCONV = L
      BETA(MP1) = BETAM
C
       IF(ISHIFT.EQ.1) GOTO 500
C
  300 IF(NCONVB.EQ.NEVG) WRITE(NOUT,301) NLOOP
      ITEMP =0
      LS = 0
      DO 320 I=NDIS,1,-1
         IF(MP(I).EQ.0) GOTO 320
         IF(GC(I).GT.ERRTOL) THEN
            AIT  = AIMAG(WS(I))
            AAIT = ABS(AIT)
            RT   = REAL(WS(I))
            IF(AAIT.LT.ABS(YLIMB).OR.AAIT.GT.YLIMT) GOTO 320
            IF(RT.LT.XLIML.OR.RT.GT.XLIMR) GOTO 320
            IF(IHOLE) THEN
               IF(AIT.GE.YHOLEB.AND.AIT.LE.YHOLET) GOTO 320
               IF(RT.GE.XHOLEL.AND.RT.LE.XHOLER) GOTO 320
            ENDIF
            IF(LS.NE.0) THEN
               DO 310 K=1,LS
                  IF(CABS(WS(I)-OWS(K))/CABS(WS(I)).LT.1.E-2) GOTO 320
  310          CONTINUE
            ENDIF
            LS = LS + 1
            OWS(LS) = WS(I)
         ENDIF
  320 CONTINUE
C
      IF(LS.LT.LST) THEN
         IF(LC.EQ.LSO) THEN
            WRITE(NOUT,321) NLOOP
            GOTO 600
         ENDIF
         LC1 = LC + 1
         DO 330 K=LC1,LSO
            LS = LS + 1
            OWS(LS) = OWS(K)
  330    CONTINUE
      ENDIF
      WRITE(NOUT,331) (OWS(K),K=1,LS)
      LC = 0
C----------------------------------------------------------------------
  500 LC = LC + 1
      IF(LC.GT.LS) GOTO 600
  510 SIGMA = OWS(LC)
      DO 520 II=1,NLOOP
         IF(CABS(SIGMA - SSIGMA(II))/CABS(SIGMA).LE.1.E-2) GOTO 500
  520 CONTINUE
      SIGMA = CMPLX(REAL(SIGMA),ABS(AIMAG(SIGMA)))
      WRITE(NOUT,521) NLOOP,SIGMA
      IF(NLOOP.LE.MXLOOP) GOTO 100
      WRITE(NOUT,523) NLOOP,MXLOOP
C----------------------------------------------------------------------
  600 CONTINUE
      DO 610 I=1,NCONV
         GR(I) = CABS(CONEV(I))
  610 CONTINUE
      DO 640 K=2,NCONV
         HVGR = GR(K)
         IHV = K
         DO 620 J=K-1,1,-1
            IF(HVGR.LT.GR(J)) IHV = J
  620    CONTINUE
         IF(IHV.NE.K) THEN
            HCONEV = CONEV(K)
            HERREV = ERREV(K)
            IHSIGM = ISIGMA(K)
            IHLOOP = ILOOP(K)
            IHMULV = MULEV(K)
            DO 630 I=K,IHV+1,-1
               GR(I) = GR(I-1)
               CONEV(I) = CONEV(I-1)
               ERREV(I) = ERREV(I-1)
               ISIGMA(I) = ISIGMA(I-1)
               ILOOP(I) = ILOOP(I-1)
               MULEV(I) = MULEV(I-1)
  630       CONTINUE
            GR(IHV) = HVGR
            CONEV(IHV) = HCONEV
            ERREV(IHV) = HERREV
            ISIGMA(IHV) = IHSIGM
            ILOOP(IHV) = IHLOOP
            MULEV(IHV) = IHMULV
         ENDIF
  640 CONTINUE
C
      IF(NLOOP.GT.MXLOOP) NLOOP = NLOOP - 1
C
      ITAG = 0
      CALL COMGAP(CONEV,GR,GG,MULEV,MP2,NCONV,ITAG)
C     -----------
      WRITE(NOUT,641) NCONV,NDIM,MEV,SVSEED
      WRITE(NOUT,642) SIGMA,ERRTOL
      WRITE(NOUT,643)
      WRITE(NOUT,644) (I,CONEV(I),MULEV(I),ERREV(I),GG(I),ISIGMA(I),
     >              ILOOP(I),I=1,NCONV)
      WRITE(NOUT,645) NDIM
      WRITE(NOUT,646) (J,SSIGMA(J),J=1,NLOOP)
C
      WRITE(NOUT,647)
      IF(ISTOP.EQ.0) THEN
         WRITE(NOUT,107)
         IF(KMAX.NE.MOLD) WRITE(NOUT2,109)
      ELSE
         WRITE(NOUT,648)
      ENDIF
      RETURN
C
    1 FORMAT(2I6,' = SAVTEV,ISTART'/' WHEN SAVTEV = -1, WE MUST',
     >       ' HAVE ISTART = 1'/)
    3 FORMAT(/' SHIFTS SPECIFIED INTIALLY ='/1P,2(2E20.12))
   21 FORMAT(/' USER SUPPLIED SHIFTS TO BE CONSIDERED'
     >       / (I6,1P,2E15.6,0P))
   23 FORMAT(2I6,I12)
   25 FORMAT(' PROGRAM TERMINATES'/    ' READ FROM FILE 2',
     >       ' CORRESPONDS TO DIFFERENT MATRIX THAN MATRIX SPECIFIED'/
     >       ' NOLD, NDIM =', 2I10/' SVSEED AND SVSOLD =', 2I14/)
   27 FORMAT(4Z20)
  101 FORMAT(/' PROGRAM TRIES TO GET NEW SHIFT BUT NO MORE SHIFTS',
     >       'ARE AVAILABLE'/ ' PROGRAM TERMINATES FOR USER TO SUPPLY',
     >       ' SHIFT'/' NLOOP =',I6/)
  103 FORMAT(2I6,I12,1P,2E9.2,' KMAX,NDIM,SVSEED,SIGMA')
  105 FORMAT(4Z20)
  107 FORMAT(/' T-MATRICES (ALPHA AND BETA) ARE NOW AVAILABLE,',
     >       ' TERMINATE')
  109 FORMAT(/' ABOVE ARE THE FOLLOWING VECTORS '/
     >       '  ALPHA(I), I = 1,KMAX'/
     >       '  BETA(I), I = 1,KMAX+1'/
     >       ' FINAL LANCZOS VECTORS OF ORDER N V1,VS,V2,W1,WS,W2'/
     >       ' ALPHA BETA ARE IN HEX FORMAT 4Z20 '/
     >       ' LANCZOS VECTORS ARE IN HEX FORMAT 4Z20 '/
     >       ' ----- END OF FILE 1 NEW ALPHA, BETA HISTORY---------'///)
  111 FORMAT(/' T-MATRICES (ALPHA AND BETA) ARE NOW AVAILABLE'/)
  113 FORMAT(/' PROGRAM TERMINATES BECAUSE MEV REQUESTED = ',I6,
     >       ' IS .GT.',I6/' AT WHICH AN ABNORMALLY SMALL BETA = ' ,
     >       1P,E13.4,' OCCURRED'/)
  121 FORMAT(/' INTERVALS SPECIFIED FOR BISECT DID NOT CONTAIN',
     >       ' ANY T-EIGENVALUES'/' PROGRAM TERMINATES')
  123 FORMAT(' AFTER LUMP NDIS, LOOP, MEV =',3I6)
  125 FORMAT(/I6,' GOOD T-EIGENVALUES HAVE BEEN COMPUTED'/
     >       I6,' OF THESE ARE ISOLATED'/
     >       I6,' = NUMBER OF DISTINCT T-EIGENVALUES COMPUTED'/)
  131 FORMAT(/' ALL COMPUTED GOOD T-EIGENVALUES ARE T-MULTIPLE'/
     >       ' THEREFORE THESE EIGENVALUES ARE ASSUMED TO HAVE',
     >       ' CONVERGED')
  133 FORMAT(/' COMPARE WITH ESTIMATES WRITTEN FROM INVERR'/)
  135 FORMAT(I10,1P,4E14.3)
  201 FORMAT(' NEVG, NLOOP, MEV, EQUAL,EVALS GEN'/ (2I4, I6))
  203 FORMAT(1P,2E20.12, E13.4,' SIGMA, BETAM')
  205 FORMAT(' RUNNING LIST OF GOODEV OBTAINED FOR EACH SHIFT USED')
  207 FORMAT(' J, CONEVB(J), MULEVB(J), ERREVB(J) EQUAL ')
  209 FORMAT(I4,1P, 2E20.12,0P,I4,1P,E13.4,0P)
  251 FORMAT(' NCONVB,NLOOP, MEV EQUAL,EVALS GEN'/ (2I4,I6))
  253 FORMAT(' RUNNING LIST OF EIGENVALUES THAT GOTO KERINV')
  301 FORMAT(' ALL EIGENVALUE APPROXIMATIONS OBTAINED WITH',
     >       ' CURRENT SHIFT'/' HAVE CONVERGED WEAKLY'/' THEREFORE',
     >       ' PROGRAM WILL TRY TO USE SHIFTS FROM PREVIOUS LIST'/,
     >       ' NLOOP=',I5)
  321 FORMAT(' TRIED TO PICK EIGENVALUE IN BOX AS POSSIBLE',
     >       ' NEW SHIFT'/  ' BUT WAS UNSUCCESSFUL AND THERE WERE NO',
     >       ' POSSIBLE SHIFTS LEFT FROM THE PREVIOUS ITERATION.',
     >       ' PROCEDURE TERMINATES FOR USER TO SUPPLY SHIFTS'/,
     >       ' NLOOP=',I5)
  331 FORMAT(' POSSIBLE SHIFTS CHOSEN EQUAL'/3(1P,2E13.5,0P))
  521 FORMAT(' ON',I4,' LOOP, NEW SHIFT CHOSEN EQUALS',1P,2E20.12)
  523 FORMAT(' NLOOP =',I6,' EXCEEDS MXLOOP =',I6,
     >       'PROGRAM TERMINATES')
  641 FORMAT(' NCONV, NDIM, MEV, SVSEED'/ 3I6,I12)
  642 FORMAT( 1P,2E20.12,E13.4, ' SIGMA AND ERRTOL')
  643 FORMAT(' OVERALL LIST OF CONVERGED GOODEV OBTAINED FROM ALL',
     >       ' SIGMAS'/
     >       ' EVNO',7X,'R(GOODEV)',8X,'I(GOODEV)',2X,'MUL',
     >       1X,'ERR',3X,'AMGP',3X,'ISIG',1X,'FSIG')
  644 FORMAT(I4,1P,2E20.12,0P,I4,1P,E13.4,E11.2,0P,I4,I4)
  645 FORMAT(/' KERNER   NDIM =',I8)
  646 FORMAT(/' SHIFTS USED' / (I6, 1P,2E20.12,0P))
  647 FORMAT(' LANCZOS EIGENVALUE COMPUTATION COMPLETE, NOW COMPUTE',
     >       ' EIGENVECTORS'/)
  648 FORMAT(/' ABOVE ARE COMPUTED GOOD T-EIGENVALUES'/
     >       ' NG = NUMBER OF GOOD T-EIGENVALUES COMPUTED'/
     >       ' NDIS = NUMBER OF COMPUTED DISTINCT EIGENVALUES OF',
     >       ' T(1,MEV)'/
     >       ' NDIM = ORDER OF A,  MATNO = MATRIX IDENT'/
     >       ' MULTOL = T-MULTIPLICITY TOLERANCE FOR T-EIGENVALUES'/
     >       ' SPUTOL = SPURIOUS TOLERANCE FOR T-EIGENVALUES'/
     >       ' MULT IS THE T-MULTIPLICITY OF GOOD T-EIGENVALUE'/
     >       ' MULT = -1 MEANS SPURIOUS T-EIGENVALUE TOO CLOSE'/
     >       ' DO NOT COMPUTE ERROR ESTIMATES FOR SUCH T-EIGENVALUES'/
     >       ' AMINGAP= MINIMAL GAP BETWEEN THE COMPUTED A-EIGENVALUES'/
     >       ' TMINGAP= MINIMAL GAP W.R.T.  DISTINCT EIGENVALUES IN',
     >       '  T(1,MEV)'/
     >       ' ----- END OF FILE 3 GOOD T-EIGENVALUES--------------'///)
      END
************************************************************************
*DECK ABSHIF
      SUBROUTINE ABSHIF(EW,AAMAT,BMAT,AORIG,IPVT,LDAA,LDAL,LDBL,NDIM)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMPIO
*CALL COMDIM5
*CALL COMPCON
C
      INTEGER  NDIM, LDAA, LDAL, LDBL, LDB1, MUN1, MUU, J, KF, KL
      INTEGER  KA, L, INFO, KS, KB, IPVT(*)
      REAL     BMAT(LDBL,*), AORIG(LDAL,*)
      REAL     SR, SI
      COMPLEX  AAMAT(LDAA,*)
      COMPLEX  EW
C
      LDB1 = LDBL+1
      MUN1 = MUL + NDIM + 1
      MUU = 2*(MUL+1)
      SR = REAL(EW)
      SI = AIMAG(EW)
      CALL CXSCAL(LDAA*NDIM,ZEROC,AAMAT,1)
      DO 20 J=1,NDIM
         KF = MAX0(1,LDB1-J)
         KL = MIN0(LDAL,MUN1-J)
         DO 10 KA=KF,KL
            KS = KA + MLL
            IF(KA.LE.LDBL) THEN
               KB = KA
               L = J
            ELSE
               KB = MUU - KA
               L = J + KA - LDBL
            ENDIF
            AAMAT(KS,J)=CMPLX((AORIG(KA,J)-SR*BMAT(KB,L)),
     >                  -SI*BMAT(KB,L))
   10    CONTINUE
   20 CONTINUE
      CALL CGBFA(AAMAT,LDAA,NDIM,MLL,MUL,IPVT,INFO)
      IF(INFO.NE.0) THEN
         WRITE(NOUT,21) INFO
         STOP
      ENDIF
      RETURN
C
   21 FORMAT(' PROGRAM TERMINATES BECAUSE ON RETURN FROM',
     >       ' FACTORIZATION INFO =',I7)
      END
************************************************************************
*DECK LANCZS
      SUBROUTINE LANCZS(V1,VS,V2,W1,WS,W2,ALPHA,BETA,GR,GC,KMAX,
     >                  MOLD1,AA,BB,IPVT,LDAA,LDBL,NDIM)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMPIO
*CALL LANTOL
*CALL ISEED
*CALL COMPCON
C
      INTEGER  IPVT(*), LDAA, LDBL,NDIM,ISEED,KMAX,MOLD1,K,IVEC,ISOLV,IN
      REAL     GR(*), GC(*)
      REAL     BB(LDBL,*)
      COMPLEX  ALPHA(*), BETA(*), V1(*), V2(*), VS(*), W1(*),W2(*),WS(*)
      COMPLEX  SUMC, TEMPC, SUMCT, ALFA, BATA, CXDOTU
      COMPLEX  AA(LDAA,*)
C
      IF(MOLD1.GT.1) GOTO 60
C
      BETA(1) = ZEROC
*IF CRAY
      DO 10 K=1,NDIM
         GR(K) = RANF( )
   10 CONTINUE
      DO 20 K=1,NDIM
         GC(K) = RANF( )
   20 CONTINUE
*ELSE
C
C     TEST HORUS
C
      RSEED= 1.0
      CALL DURAND(RSEED,NDIM,GR)
      CALL DURAND(RSEED,NDIM,GC)
C      ISEED= 1
C      CALL RNSET(ISEED)
C      CALL DRNUN(NDIM,GR)
C      CALL DRNUN(NDIM,GC)
*ENDIF
      DO 30 K=1,NDIM
         V2(K) = CMPLX(GR(K),GC(K))
   30 CONTINUE
      CALL BMATV(V2,VS,BB,LDBL,NDIM)
      SUMC= ONE/CSQRT(CXDOTU(NDIM,V2,1,VS,1))
      DO 40 K=1,NDIM
         VS(K) = SUMC*VS(K)
         V2(K) = SUMC*V2(K)
   40 CONTINUE
      DO 50 K=1,NDIM
         V1(K) = ZEROC
         W1(K) = V1(K)
         W2(K) = V2(K)
         WS(K) = VS(K)
   50 CONTINUE
   60 CONTINUE
C
      DO 140 IVEC=MOLD1,KMAX
C     ------------------------
C
         SUMC = BETA(IVEC)
         DO 70 K=1,NDIM
            GR(K) = REAL(VS(K))
            GC(K) = AIMAG(VS(K))
   70    CONTINUE
         JOB = 0
         CALL ABSOLV(VS,V1,SUMC,JOB,AA,IPVT,LDAA,NDIM)
         DO 80 K=1,NDIM
            VS(K) = CMPLX(GR(K),GC(K))
   80    CONTINUE
         DO 90 K=1,NDIM
            GR(K) = REAL(WS(K))
            GC(K) = AIMAG(WS(K))
   90    CONTINUE
         JOB = 1
         CALL ABSOLV(WS,W1,SUMC,JOB,AA,IPVT,LDAA,NDIM)
         DO 100 K=1,NDIM
            WS(K) = CMPLX(GR(K),GC(K))
  100    CONTINUE
         SUMC = .5E0*(CXDOTU(NDIM,WS,1,V1,1) + CXDOTU(NDIM,VS,1,W1,1))
         ALPHA(IVEC) = SUMC
         ALFA = SUMC
         DO 110 K=1,NDIM
            W1(K) = W1(K) - SUMC*W2(K)
            V1(K) = V1(K) - SUMC*V2(K)
  110    CONTINUE
         DO 120 K=1,NDIM
            TEMPC = V1(K)
            V1(K) = V2(K)
            V2(K) = TEMPC
            TEMPC = W1(K)
            W1(K) = W2(K)
            W2(K) = TEMPC
  120    CONTINUE
         CALL BMATV(V2,VS,BB,LDBL,NDIM)
         CALL BMATV(W2,WS,BB,LDBL,NDIM)
         IN = IVEC+1
         SUMC = CSQRT(CXDOTU(NDIM,VS,1,W2,1))
         SUMCT = CSQRT(CXDOTU(NDIM,WS,1,V2,1))
         BATA = .5E0*(SUMC + SUMCT)
         IF(CABS(ALFA).GT.LTOL.OR.CABS(BATA).GT.LTOL) THEN
            GR(1) = -1.E3
            WRITE(NOUT,121) ALFA,BATA
            RETURN
         ENDIF
         BETA(IN) = BATA
         SUMC = ONE/BATA
         DO 130 K=1,NDIM
            V2(K) = SUMC*V2(K)
            VS(K) = SUMC*VS(K)
            W2(K) = SUMC*W2(K)
            WS(K) = SUMC*WS(K)
  130    CONTINUE
C
  140 CONTINUE
C     --------
C
C     WRITE(NOUT,141)
C     WRITE(NOUT,143) (ALPHA(K),K=1,KMAX)
C     WRITE(NOUT,145)
C     WRITE(NOUT,143) (BETA(K),K=2,KMAX)
      RETURN
C
  121 FORMAT(/' EITHER ALPHA OR BETA EXCEEDED LTOL.  ALPHA,BETA ='/
     >       1P,2(E20.8))
  141 FORMAT(' DIAGONALE 1:KMAX'/)
  143 FORMAT(1P,2(E17.7,E16.7))
  145 FORMAT(/' SUBDIAGONALE 2:KMAX'/)
      END
************************************************************************
*DECK BMATV
      SUBROUTINE BMATV(W,U,BB,LDBL,NDIM)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMDIM5
C
      INTEGER  NDIM, LDBL, J, LDB1, KF, K
      REAL     BB(LDBL,*)
      COMPLEX  U(*), W(*)
C
      DO 10 J=1,NDIM
         U(J) = BB(LDBL,J)*W(J)
   10 CONTINUE
      LDB1 = LDBL+1
      DO 30 J=2,NDIM
         KF = MAX0(1,LDB1-J)
         DO 20 K=KF,MUL
            I = J+K-LDBL
            U(I) = U(I) + BB(K,J)*W(J)
            U(J) = U(J) + BB(K,J)*W(I)
   20    CONTINUE
   30 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK ABSOLV
      SUBROUTINE ABSOLV(W,U,SUMC,JOB,AA,IPVT,LDAA,NDIM)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMDIM5
C
      INTEGER  IPVT(*), NDIM, LDAA, JOB, K
      COMPLEX  AA(LDAA,*), U(*), W(*), SUMC
C
      CALL CGBSLL(AA,LDAA,NDIM,MLL,MUL,IPVT,W,JOB)
      DO 10 K=1,NDIM
         U(K) = W(K) - SUMC*U(K)
   10 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK CGBSLL
      SUBROUTINE CGBSLL(ABD,LDA,N,ML,MU,IPVT,B,JOB)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
      INTEGER  LDA, N, ML, MU, IPVT(*), JOB
      INTEGER  K, KB, L, LA, LB, LM, M, NM1
      COMPLEX  ABD(LDA,*), B(*)
      COMPLEX  CXDOTU, T
C
      M = MU + ML + 1
      NM1 = N - 1
      IF(JOB.NE.0) GOTO 50
C     ---------------------
C
C ... JOB = 0 , SOLVE  A * X = B ...
C ... FIRST SOLVE L*Y = B ...
C
      IF(ML.EQ.0) GOTO 30
      IF(NM1.LT.1) GOTO 30
         DO 20 K=1,NM1
            LM = MIN0(ML,N-K)
            L = IPVT(K)
            T = B(L)
            IF(L.EQ.K) GOTO 10
               B(L) = B(K)
               B(K) = T
   10       CONTINUE
            CALL CXAXPY(LM,T,ABD(M+1,K),1,B(K+1),1)
   20    CONTINUE
   30 CONTINUE
C
C ... NOW SOLVE  U*X = Y ...
C
      DO 40 KB=1,N
         K = N + 1 - KB
         B(K) = B(K)/ABD(M,K)
         LM = MIN0(K,M) - 1
         LA = M - LM
         LB = K - LM
         T = -B(K)
         CALL CXAXPY(LM,T,ABD(LA,K),1,B(LB),1)
   40 CONTINUE
      GOTO 100
C
   50 CONTINUE
C     --------
C
C ... JOB = NONZERO, SOLVE  HERM(A) * X = B ...
C ... FIRST SOLVE  HERM(U)*Y = B ...
C
      DO 60 K=1,N
         LM = MIN0(K,M) - 1
         LA = M - LM
         LB = K - LM
         T = CXDOTU(LM,ABD(LA,K),1,B(LB),1)
          B(K) = (B(K) - T)/ABD(M,K)
   60 CONTINUE
C
C ... NOW SOLVE HERM(L)*X = Y ...
C
      IF(ML.EQ.0) GOTO 90
      IF(NM1.LT.1) GOTO 90
         DO 80 KB=1,NM1
            K = N - KB
            LM = MIN0(ML,N-K)
            B(K) = B(K) + CXDOTU(LM,ABD(M+1,K),1,B(K+1),1)
            L = IPVT(K)
            IF(L.EQ.K) GOTO 70
               T = B(L)
               B(L) = B(K)
               B(K) = T
   70       CONTINUE
   80    CONTINUE
   90 CONTINUE
  100 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK TNORM
      SUBROUTINE TNORM(ALPHA,BETA,BTOL,AMAX,MEV,IB)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMPIO
      INTEGER  IB, I, MEV
      REAL     AMAX, BMIN, BMAX, BSIZE, BTOL, ABATA, AALFA
      COMPLEX  ALPHA(*), BETA(*)
C
      IB = 2
      BMIN = CABS(BETA(2))
      BMAX = BMIN
      AMAX = CABS(ALPHA(1))
C
      DO 10 I=2,MEV
         AMAX = AMAX1(AMAX,CABS(ALPHA(I)))
         BMAX = AMAX1(BMAX,CABS(BETA(I)))
         IF(CABS(BETA(I)).LT.BMIN) THEN
            IB = I
            BMIN = CABS(BETA(I))
         ENDIF
   10 CONTINUE
      AMAX = AMAX1(BMAX,AMAX)
      BSIZE = BMIN/AMAX
      IF(BSIZE.LT.BTOL) THEN
         IB = -IB
         WRITE(NOUT,11) MEV
      ENDIF
      BTOL = BMIN
C     WRITE(NOUT,13) IB
C     WRITE(NOUT,15) MEV,BMIN,AMAX,BSIZE
      RETURN
C
   11 FORMAT(/' BETA TEST INDICATES POSSIBLE LOSS OF LOCAL',
     >       ' ORTHOGONALITY OVER 1ST',I6,' LANCZOS VECTORS'/)
   13 FORMAT(/' MINIMUM BETA RATIO OCCURS AT',I6,' TH BETA' )
   15 FORMAT(/1X,'TSIZE',6X,'MIN BETA',5X,'TKMAX',6X,'MIN RATIO'/
     >       I6,1P,E14.3,E10.3,E15.3 )
      END
************************************************************************
*DECK COMPEV
      SUBROUTINE COMPEV(ALPHA,BETA,V1,V2,VS,EVMAG,GC,MULTOL,SPUTOL,
     >                  EPS,MP,T2FLAG,MEV,NDIS,SAVTEV)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMPIO
C
      INTEGER  MP(*), T2FLAG(*), SAVTEV, MEV1, J, IERR, K, KK, KP1
      INTEGER  NDIS, INDEX, I, JP1, IMIN
      REAL     EVMAG(*), GC(*)
      REAL     TEMP, TOL, DELMIN, EPS
      REAL     MULTOL, SPUTOL
      COMPLEX  ALPHA(*), BETA(*), VS(*), V1(*), V2(*), EVAL,CTEMP,TSCALE
C
      IF(SAVTEV.LE.0) THEN
         WRITE(NOUT,1) SAVTEV
         STOP
      ENDIF
      TSCALE = V1(1)
      MEV1 = MEV - 1
C     WRITE(NOUT,3) TSCALE
      CALL CXCOPY(MEV,ALPHA,1,VS,1)
      CALL CXCOPY(MEV,BETA,1,V1,1)
      V1(1) = TSCALE
C     WRITE(NOUT,5) MEV
      CALL CMTQL1(MEV,VS,V1,IERR)
      IF(IERR.NE.0) THEN
         WRITE(NOUT,7)
         STOP
      ENDIF
      DO 10 J=1,MEV
         EVMAG(J) = CABS(VS(J))
   10 CONTINUE
C
C ... SORTIERT VS UND EVMAG  EVMAG(1) < EVMAG(2) ...
C
      DO 30 K=2,MEV
         DO 20 KK=K-1,1,-1
            KP1 = KK+1
            IF(EVMAG(KP1).GE.EVMAG(KK)) GOTO 30
            EVAL = VS(KK)
            VS(KK) = VS(KP1)
            VS(KP1) = EVAL
            TEMP = EVMAG(KK)
            EVMAG(KK) = EVMAG(KP1)
            EVMAG(KP1) = TEMP
   20    CONTINUE
   30 CONTINUE
      IF(SAVTEV.GE.1) THEN
         WRITE(NOUT3,31)
         WRITE(NOUT3,33) MEV
         WRITE(NOUT3,35)
         WRITE(NOUT3,37) (VS(J),J=1,MEV)
      ENDIF
C
      MULTOL = MULTOL*EVMAG(MEV)
      SPUTOL = SPUTOL*EVMAG(MEV)
      TOL = 1000.0E0*SPUTOL
C     WRITE(NOUT,39) MULTOL,SPUTOL
      NDIS = 0
      DO 40 I=1,MEV
         T2FLAG(I) = 0
   40 CONTINUE
      DO 70 J=1,MEV
         IF(T2FLAG(J).EQ.1) GOTO 70
         CTEMP = VS(J)
         NDIS = NDIS + 1
         INDEX = 1
         T2FLAG(J) = 1
         IF(I.LT.MEV) THEN
            DO 50 I=J+1,MEV
               IF(T2FLAG(I).EQ.1) GOTO 50
               IF(EVMAG(I)-EVMAG(J).GT.MULTOL) GOTO 60
               IF(CABS(VS(J)-VS(I)).LE.MULTOL) THEN
                  INDEX = INDEX + 1
                  CTEMP = CTEMP + VS(I)
                  T2FLAG(I) = 1
               ENDIF
   50       CONTINUE
         ENDIF
   60    VS(NDIS) = CTEMP/FLOAT(INDEX)
         MP(NDIS) = INDEX
   70 CONTINUE
      IF(SAVTEV.GE.1) THEN
         IF(SAVTEV.EQ.1000) THEN
            WRITE(NOUT,71) MEV
            CALL CXCOPY(MEV1,ALPHA(2),1,V2,1)
            CALL CXCOPY(MEV1,BETA(2),1,V1,1)
            V1(1) = TSCALE
            CALL CMTQL1(MEV1,V2,V1,IERR)
            WRITE(NOUT,73) IERR
            IF(IERR.NE.0) STOP
         ELSE
            CALL SINVER(ALPHA,BETA,V1,V2,VS,EPS,EVMAG,GC,SPUTOL,
     >                  MP,T2FLAG,MEV,NDIS,NISO)
            DO 80 J=1,NDIS
               EVMAG(J) = CABS(VS(J))
   80       CONTINUE
            RETURN
         ENDIF
      ENDIF
C
      DO 90 J=1,MEV1
         EVMAG(J) = CABS(V2(J))
   90 CONTINUE
      DO 110 K=2,MEV1
         KM1 = K-1
         DO 100 L=1,KM1
            KK = K-L
            KP1 = KK+1
            IF(EVMAG(KP1).GE.EVMAG(KK)) GOTO 110
            EVAL = V2(KK)
            V2(KK) = V2(KP1)
            V2(KP1) = EVAL
            TEMP = EVMAG(KK)
            EVMAG(KK) = EVMAG(KP1)
            EVMAG(KP1) = TEMP
  100    CONTINUE
  110 CONTINUE
      DO 120 J=1,MEV1
         EVMAG(J) = CABS(V2(J))
  120 CONTINUE
      IF(SAVTEV.GE.1) THEN
         WRITE(NOUT3,121) (V2(J),J=1,MEV1)
      ENDIF
      DO 130 I=1,MEV1
         T2FLAG(I) = 0
  130 CONTINUE
C
      DO 180 J=1,MEV1
         DELMIN = 2.E0*CABS(VS(MEV))
         IMIN = 0
         DO 140 I=J,1,-1
            K = I
            IF(I.GT.NDIS) K = NDIS
            TEMP = CABS(VS(K))
            IF(CABS(VS(K)).LT.EVMAG(J)-SPUTOL) GOTO 150
            IF(MP(K).NE.0.AND.CABS(VS(K)-V2(J)).LT.DELMIN) THEN
               DELMIN = CABS(VS(K)-V2(J))
               IMIN = K
            ENDIF
  140    CONTINUE
  150    IF(J.LT.NDIS) THEN
            DO 160 I=J+1,NDIS
               IF(CABS(VS(I)).GT.EVMAG(J)+SPUTOL) GOTO 170
               IF(MP(I).NE.0.AND.CABS(VS(I)-V2(J)).LT.DELMIN) THEN
                  DELMIN = CABS(VS(I)-V2(J))
                  IMIN = I
               ENDIF
  160       CONTINUE
         ENDIF
  170    IF(IMIN.GT.0.AND.DELMIN.LE.SPUTOL.AND.MP(IMIN).LE.1) MP(IMIN)=0
C
  180 CONTINUE
C
      DO 190 J=1,NDIS
         EVMAG(J) = CABS(VS(J))
  190 CONTINUE
      RETURN
C
    1 FORMAT(' VALUE OF SAVTEV ',I5)
    3 FORMAT(/' IN COMPEV TSCALE EQUALS',1P,2E20.12/)
    5 FORMAT(/' COMPUTE EIGENVALUES OF T(1,',I4,') USING CMTQL1'/)
    7 FORMAT(' ON RETURN FROM CMTQL1 ERROR FLAG WAS NOT ZERO'/)
   31 FORMAT(' T-T2EVAL')
   33 FORMAT(I6,' = ORDER OF T-MATRIX, T-EIGVALS =')
   35 FORMAT(' T(1,MEV) EVAL')
   37 FORMAT(1P,4E20.12)
   39 FORMAT(/' TOLERANCES USED IN T-MULTIPLICITY AND SPURIOUS',
     >       ' TESTS =',1P,2E10.3/)
   71 FORMAT(/' COMPUTE T(2,',I4,') EIGENVALUES'/)
   73 FORMAT(' T2-HAT EIGENVALUES VIA CMTQL1'/' IERR = ',I6/)
   75 FORMAT(' DISTINCT T-EVAL ON RETURN FROM SINVER'
     >       /(I6,1P,2E20.12,0P))
  121 FORMAT(' T(2,MEV) EVALS'/(1P,4E20.12))
      END
************************************************************************
*DECK CMTQL1
      SUBROUTINE CMTQL1(N,D,E,IERR)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL MACHT
*CALL COMPCON
C
      COMPLEX  D(*), E(*), B, C , F, G, P, R, S, W
      COMPLEX  TEMPC, T0C
      REAL     EPS, EPSTWO, TEMP, TEMP0, T0, T1, T2
      INTEGER  I, J, L, M, N, MML, IERR, K
C
      EPS = 100.E0*MACHEP
      EPSTWO = SQRT(2.0E0)*MACHEP
      IERR = 0
C
      IF(N.EQ.1) RETURN
C
      DO 10 I=2,N
          E(I-1) = E(I)
   10 CONTINUE
      E(N) = ZEROC
C
      DO 60 L=1,N-1
         J = 0
   20    DO 30 M=L,N-1
            TEMP = ABS(REAL(D(M))) + ABS(AIMAG(D(M)))
     >             + ABS(REAL(D(M+1))) + ABS(AIMAG(D(M+1)))
            T2   = ABS(REAL(E(M))) + ABS(AIMAG(E(M)))
            IF(T2.LE.TEMP*EPSTWO) GOTO 40
   30    CONTINUE
   40    P = D(L)
         IF(M.EQ.L) GOTO 60
         IF(J.EQ.100) THEN
            IERR = L
            RETURN
         ENDIF
         J = J+1
         G = (D(L+1) - P)*CHALF
         TEMP = ABS(REAL(G))+ABS(AIMAG(G))
         T1 = ABS(REAL(E(L)))+ABS(AIMAG(E(L)))
         IF(TEMP.LE.T1) THEN
            W = G/E(L)
            R = CSQRT(ONEC + W*W)
            T0 = REAL(W)*REAL(R) + AIMAG(W)*AIMAG(R)
            T0C = ONEC
            IF(T0.LT.ZERO) T0C = -ONEC
            G = D(M) - P + E(L)/(W + T0C*R)
         ELSE
            W = E(L)/G
            R = CSQRT(ONEC + W*W)
            T0C = ONEC
            IF(REAL(R).LT.ZERO) T0C = -ONEC
            G = D(M) - P + W*E(L)/(ONEC + T0C*R)
         ENDIF
         S = ONEC
         C = -ONEC
         P = ZEROC
C
         DO 50 I=M-1,L,-1
            F =  S*E(I)
            B = -C*E(I)
            T0 = ABS(REAL(G)) + ABS(AIMAG(G))
            T1 = ABS(REAL(F)) + ABS(AIMAG(F))
            IF(T1.LE.T0) THEN
               W = F/G
               T2 = T1/T0
               TEMP = T2*T2 + ONE
               R = ONEC + W*W
               TEMP0 = ABS(REAL(R)) + ABS(AIMAG(R))
               R = CSQRT(R)
               E(I+1) = G*R
               C = ONEC/R
               S = W*C
            ELSE
               W = G/F
               T2 = T0/T1
               TEMP = ONE + T2*T2
               R = ONEC + W*W
               TEMP0 = ABS(REAL(R)) + ABS(AIMAG(R))
               R = CSQRT(R)
               E(I+1) = F*R
               S = ONEC/R
               C = W*S
            ENDIF
            IF(TEMP0.LE.EPS*TEMP) THEN
               IERR = -L
               RETURN
            ENDIF
            G = D(I+1)-P
            R = (D(I)-G)*S+CTWO*C*B
            P = S*R
            D(I+1) = G+P
            G = B-C*R
   50    CONTINUE
C
         D(L) = D(L)-P
         E(L) = G
         E(M) = ZEROC
         GOTO 20
   60 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK SINVER
      SUBROUTINE SINVER(ALPHA,BETA,V1,V2,VS,EPS,GR,GC,SPUTOL,MP,
     >                  INTERC,MEV,NDIS,NISO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMPIO
*CALL COMPCON
*CALL ISEED
C
      INTEGER  MP(*), INTERC(*), ISEED, NISO, JEV, NG, MEV, MP1, MM1
      INTEGER  MEV1, I, II, ISPUR, NGOOD, NDIS
      REAL     XU, NORM, TSUM, GSUM, SPUTOL
      REAL     EPS, EPS3, EPS4, GR(*), GC(*)
      COMPLEX  ALPHA(*), BETA(*), V1(*), V2(*), VS(*)
      COMPLEX  U, Z, X1, RATIO, BETAM, TEMP, SUMC, CXDOTU
C
C     WRITE(NOUT,1)
      NISO = 0
      DO 10 JEV=1,NDIS
         IF(MP(JEV).LE.1) NISO = NISO + 1
   10 CONTINUE
      IF(NISO.EQ.0) THEN
         WRITE(NOUT,11)
         RETURN
      ENDIF
C
      NG = 0
      NISO = 0
      MP1 = MEV+1
      MM1 = MEV-2
      MEV1 = MEV - 1
      BETAM = BETA(MP1)
      BETA(MP1) = ZEROC
      TSUM = CABS(ALPHA(2))
      DO 20 I=3,MEV
         TSUM = TSUM + CABS(ALPHA(I)) + CABS(BETA(I))
   20 CONTINUE
      EPS3 = EPS*TSUM
      EPS4 = FLOAT(MEV1)*EPS3
*IF CRAY
      DO 30 I=1,MEV1
         GR(I) = RANF( )
   30 CONTINUE
      DO 40 I=1,MEV1
         GC(I) = RANF( )
   40 CONTINUE
*ELSE
C
C     TEST HORUS
C
      RSEED = 1.0
      CALL DURAND(RSEED,MEV1,GR)
      CALL DURAND(RSEED,MEV1,GC)
C      ISEED= 1
C      CALL RNSET(ISEED)
C      CALL DRNUN(N,SX)
C      CALL DRNUN(N,SY)
*ENDIF
      GSUM = ZERO
      DO 50 I=1,MEV1
         GSUM = GSUM + ABS(GR(I)) + ABS(GC(I))
   50 CONTINUE
      GSUM = EPS4/GSUM
      DO 60 I=1,MEV1
         GR(I) = GSUM*GR(I)
         GC(I) = GSUM*GC(I)
   60 CONTINUE
C
      DO 130 JEV=1,NDIS
C     -------------------
C
         IF(MP(JEV).EQ.0) GOTO 130
         NG = NG + 1
         IF(MP(JEV).NE.1) GOTO 130
         NISO = NISO + 1
         X1 = VS(JEV)
         DO 70 I=1,MEV1
            INTERC(I) = 0
            V2(I) = CMPLX(GR(I),GC(I))
   70    CONTINUE
         U = ALPHA(2)-X1
         Z = BETA(3)
         DO 80 I=3,MEV
            IF(CABS(BETA(I)).LE.CABS(U)) THEN
               V1(I-2) = Z/U
               V2(I-2) = V2(I-2)/U
               V2(I-1) = V2(I-1)-BETA(I)*V2(I-2)
               RATIO = BETA(I)/U
               U = ALPHA(I)-X1-Z*RATIO
               Z = BETA(I+1)
            ELSE
               RATIO = U/BETA(I)
               INTERC(I-1) = 1
               V1(I-2) = ALPHA(I)-X1
               U = Z-RATIO*V1(I-2)
               Z = -RATIO*BETA(I+1)
               TEMP = V2(I-2)
               V2(I-2) = V2(I-1)
               V2(I-1) = TEMP-RATIO*V2(I-1)
            ENDIF
   80    CONTINUE
         IF(CABS(U).EQ.ZERO) U = CMPLX(EPS3,EPS3)
         V2(MEV1) = V2(MEV1)/U
         DO 90 I=MM1,1,-1
            IF(INTERC(I+1).NE.1) THEN
               V2(I) = V2(I)-V1(I)*V2(I+1)
            ELSE
               V2(I) = (V2(I)-V1(I)*V2(I+1)-BETA(I+3)*V2(I+2))/BETA(I+2)
            ENDIF
   90    CONTINUE
         NORM = CABS(V2(MEV1))
         DO 100 I=MM1,1,-1
            NORM = NORM + CABS(V2(I))
  100    CONTINUE
         SUMC = CXDOTU(MEV1,V2,1,V2,1)
         SUMC = ONEC/CSQRT(SUMC)
         DO 110 II=1,MEV1
            V2(II) = SUMC*V2(II)
  110    CONTINUE
         V1(1) = ALPHA(2)*V2(1) + BETA(3)*V2(2)
         V1(MEV1) = BETA(MEV)*V2(MEV1-1) + ALPHA(MEV)*V2(MEV1)
         DO 120 I=2,MM1
            V1(I) = BETA(I+1)*V2(I-1) + ALPHA(I+1)*V2(I) +
     >              BETA(I+2)*V2(I+1)
  120    CONTINUE
         SUMC = CXDOTU(MEV1,V1,1,V2,1)
         NORM = CABS(SUMC - X1)
         IF(NORM.LE.SPUTOL) MP(JEV) = 0
C
  130 CONTINUE
C     --------
C
      ISPUR = 0
      DO 140 J=1,NDIS
         IF(MP(J).EQ.0) ISPUR = ISPUR + 1
  140 CONTINUE
      NGOOD = NDIS - ISPUR
C     WRITE(NOUT,141) ISPUR,NGOOD,NDIS
C
      BETA(MP1) = BETAM
      RETURN
C
    1 FORMAT(' DETERMINE WHICH EIGENVALUES ARE SPURIOUS'/)
   11 FORMAT(/' THERE ARE NO ISOLATED T-EIGENVALUES SO SPURIOUS',
     >       ' TEST IS NOT REQUIRED')
  141 FORMAT(I5,' EIGENVALUES WERE LABELLED SPURIOUS, LEAVING',I5,
     >       ' T-EIGENVALUES'/' OUT OF',I6,' DISTINCT ONES.'/' SOME OF',
     >       ' THESE MAY GET LUMPED BY SUBROUTINE LUMP'/)
      END
************************************************************************
*DECK LUMP
      SUBROUTINE LUMP(VC,V1,VA,RELTOL,SPUTOL,SCALE2,LINDEX,TFLAG,LOOP)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMPCON
C
      INTEGER  LINDEX(*), TFLAG(*), K, LOOP, NLOOP, J
      INTEGER  ICOUNT, JN, INDSUM, ISPUR, IN
      REAL     VA(*), RELTOL, SPUTOL, SCALE2
      REAL      THOLD, TH1, TH2, DGAP
      COMPLEX  VC(*), V1(*), SUMC
C
      TH2 = SCALE2*SPUTOL
      DO 10 K=1,LOOP
         TFLAG(K) = 0
   10 CONTINUE
      NLOOP = 0
C
      DO 40 J=1,LOOP
C
         IF(TFLAG(J).EQ.1) GOTO 40
         NLOOP = NLOOP + 1
         TFLAG(J) = 1
         V1(1) = VC(J)
         ICOUNT = 1
         JN = LINDEX(J)
         TH1 = RELTOL*VA(J)
         THOLD = AMAX1(TH1,TH2)
         IF(JN.NE.0) THEN
            INDSUM = JN
            ISPUR = 0
            SUMC = FLOAT(JN)*VC(J)
         ELSE
            INDSUM = 1
            ISPUR = 1
            SUMC = ZEROC
         ENDIF
         IF(J.NE.LOOP) THEN
             DO 20 I=J+1,LOOP
               IF(TFLAG(I).EQ.1) GOTO 20
               IF(VA(I)-VA(J).GE.THOLD) GOTO 30
               IF(CABS(VC(I)-VC(J)).GE.THOLD) GOTO 20
               ICOUNT = ICOUNT + 1
               TFLAG(I) = 1
               V1(ICOUNT) = VC(I)
               IF(LINDEX(I).EQ.0) THEN
                  ISPUR = ISPUR + 1
                  INDSUM = INDSUM + 1
               ELSE
                 INDSUM = INDSUM + LINDEX(I)
                 SUMC = SUMC + FLOAT(LINDEX(I))*VC(I)
               ENDIF
   20       CONTINUE
         ENDIF
   30    IF(ICOUNT.EQ.1) INDSUM = JN
         IDIF = INDSUM - ISPUR
         IF(ICOUNT.EQ.1.OR.(ICOUNT.GT.1.AND.IDIF.EQ.0)) THEN
            VC(NLOOP) = VC(J)
            VA(NLOOP) = VA(J)
         ELSE
            SUMC = SUMC/FLOAT(IDIF)
            VC(NLOOP) = SUMC
            VA(NLOOP) = CABS(SUMC)
         ENDIF
         LINDEX(NLOOP) = INDSUM
C
   40 CONTINUE
C
      LOOP = NLOOP
C
      RETURN
      END
************************************************************************
*DECK COMGAP
      SUBROUTINE COMGAP(VC,VA,GG,MP,IND,M,ITAG)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
      INTEGER  MP(*), IND(*), M, INDEX, ITAG
      REAL     VA(*), GG(*), T1
      COMPLEX  VC(*)
C
      DO 50 K=1,M
C
         INDEX = 0
         T1 = 2.*VA(M)
C
         IF(K.EQ.1) GOTO 20
         DO 10 J=K-1,1,-1
            IF(VA(K)-VA(J).GT.T1) GOTO 20
            IF(T1.GT.CABS(VC(K)-VC(J))) THEN
               T1 = CABS(VC(K)-VC(J))
               INDEX = J
            ENDIF
   10    CONTINUE
C
   20    IF(K.EQ.M) GOTO 40
         DO 30 J=K+1,M
            IF(VA(J)-VA(K).GT.T1) GOTO 40
            IF(T1.GT.CABS(VC(K)-VC(J))) THEN
               T1=CABS(VC(K)-VC(J))
               INDEX = J
            ENDIF
   30    CONTINUE
C
   40    IND(K) = INDEX
         GG(K) = T1
C
   50 CONTINUE
C
      IF(ITAG.EQ.0) RETURN
C
      DO 60 K=1,M
         IF(MP(IND(K)).EQ.0) GG(K) = -GG(K)
   60 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK ISOEV
      SUBROUTINE ISOEV(VS,GR,GG,GAPTOL,SPUTOL,SCALE1,MP,NDIS,NG,NISO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
      INTEGER  MP(*), NISO, NG, J, NDIS, I
      REAL     GR(*), SPUTOL, GAPTOL, SCALE1, TEMP, TOL, DGAP
      REAL     GG(*)
      COMPLEX  VS(*)
C
      DGAP = SCALE1*SPUTOL
      NISO = 0
      NG = 0
      DO 40 J=1,NDIS
         IF(MP(J).EQ.0) GOTO 40
         NG = NG+1
         IF(MP(J).NE.1) GOTO 40
         TOL = AMAX1(DGAP,GAPTOL*GR(J))
         NISO = NISO + 1
         IF(ABS(GG(J)).GT.TOL) GOTO 40
         IF(J.GT.1) THEN
            DO 10 I=J-1,1,-1
               IF(GR(J)-GR(I).GT.TOL) GOTO 20
               IF(MP(I).EQ.0) THEN
                  TEMP = CABS(VS(J)-VS(I))
                  IF(TEMP.LE.TOL) THEN
                     MP(J) = -MP(J)
                     NISO = NISO-1
                     GOTO 40
                  ENDIF
               ENDIF
   10       CONTINUE
         ENDIF
   20    IF(J.LT.NDIS) THEN
            DO 30 I=J+1,NDIS
               IF(GR(I)-GR(J).GT.TOL) GOTO 40
               IF(MP(I).EQ.0) THEN
                  TEMP = CABS(VS(J)-VS(I))
                  IF(TEMP.LE.TOL) THEN
                        MP(J) = -MP(J)
                        NISO = NISO-1
                        GOTO 40
                   ENDIF
               ENDIF
   30       CONTINUE
         ENDIF
   40 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK INVERR
      SUBROUTINE INVERR(ALPHA,BETA,V1,V2,VS,EPS,GR,GC,G,GG,MP,INTERC,
     >                   MEV,NDIS,NISO,IT)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMPIO
*CALL COMPCON
*CALL ISEED
C
      INTEGER  MP(*), INTERC(*)
      INTEGER  SEED, MEV, NDIS, NISO, IT
      INTEGER  NG, ITER, MP1, MM1, I, II, ISO
      REAL     EST,      XU, NORM, TSUM, GSUM
      REAL     EPS, EPS3, EPS4
      REAL     G(*), GG(*), GR(*), GC(*)
      COMPLEX  ALPHA(*), BETA(*), V1(*), V2(*), VS(*)
      COMPLEX  U, Z, X1, RATIO, BETAM, TEMP, SUM
      COMPLEX  CXDOTC
C
      NG = 0
      NISO = 0
      ITER = IT
      MP1 = MEV+1
      MM1 = MEV-1
      BETAM = BETA(MP1)
      BETA(MP1) = ZEROC
      TSUM = CABS(ALPHA(1))
C
      DO 10 I=2,MEV
         TSUM = TSUM + CABS(ALPHA(I)) + CABS(BETA(I))
   10 CONTINUE
C
      EPS3 = EPS*TSUM
      EPS4 = FLOAT(MEV)*EPS3
*IF CRAY
      DO 20 I=1,MEV
         GR(I) = RANF( )
   20 CONTINUE
      DO 30 I=1,MEV
         GC(I) = RANF( )
   30 CONTINUE
*ELSE
C
C     TEST HORUS
C
      RSEED = 1.0
      CALL DURAND(RSEED,MEV,GR)
      CALL DURAND(RSEED,MEV,GC)
C      ISEED= 1
C      CALL RNSET(ISEED)
C      CALL DRNUN(N,SX)
C      CALL DRNUN(N,SY)
*ENDIF
      GSUM = ZERO
      DO 40 I=1,MEV
         GSUM = GSUM + ABS(GR(I)) + ABS(GC(I))
   40 CONTINUE
      GSUM = EPS4/GSUM
      DO 50 I=1,MEV
         GR(I) = GSUM*GR(I)
         GC(I) = GSUM*GC(I)
   50 CONTINUE
C
      DO 140 JEV=1,NDIS
C     -------------------
C
         IF(MP(JEV).EQ.0) GOTO 140
         NG = NG + 1
         IF(MP(JEV).NE.1) GOTO 140
         IT = 1
         NISO = NISO + 1
         X1 = VS(JEV)
         DO 60 I=1,MEV
            INTERC(I) = 0
            V2(I) = CMPLX(GR(I),GC(I))
   60    CONTINUE
   70    U = ALPHA(1)-X1
         Z = BETA(2)
         DO 80 I=2,MEV
            IF(CABS(BETA(I)).LE.CABS(U)) THEN
               V1(I-1) = Z/U
               V2(I-1) = V2(I-1)/U
               V2(I) = V2(I)-BETA(I)*V2(I-1)
               RATIO = BETA(I)/U
               U = ALPHA(I)-X1-Z*RATIO
               Z = BETA(I+1)
            ELSE
               RATIO = U/BETA(I)
               INTERC(I) = 1
               V1(I-1) = ALPHA(I)-X1
               U = Z-RATIO*V1(I-1)
               Z = -RATIO*BETA(I+1)
               TEMP = V2(I-1)
               V2(I-1) = V2(I)
               V2(I) = TEMP-RATIO*V2(I)
            ENDIF
   80    CONTINUE
         IF(CABS(U).EQ.ZERO) U = CMPLX(EPS3,EPS3)
         V2(MEV) = V2(MEV)/U
         DO 90 I=MM1,1,-1
            IF(INTERC(I+1).NE.1) THEN
               V2(I) = V2(I)-V1(I)*V2(I+1)
            ELSE
               V2(I)=(V2(I)-V1(I)*V2(I+1)-BETA(I+2)*V2(I+2))/BETA(I+1)
            ENDIF
   90    CONTINUE
         NORM = CABS(V2(MEV))
         DO 100 I=MM1,1,-1
            NORM = NORM + CABS(V2(I))
  100    CONTINUE
         IF(NORM.GE.ONE) GOTO 120
         IT = IT+1
         IF(IT.GT.ITER) GOTO 120
         XU = EPS4/NORM
         DO 110 I=1,MEV
            V2(I) = V2(I)*XU
  110    CONTINUE
         GOTO 70
  120    CONTINUE
         SUM = CXDOTC(MEV,V2,1,V2,1)
         SUM = ONE/SQRT(SUM)
         DO 130 II=1,MEV
            V2(II) = SUM*V2(II)
  130    CONTINUE
         EST = CABS(BETAM)*CABS(V2(MEV))
         GSUM = CABS(BETAM)
         IF(IT.GT.ITER) EST = -EST
         G(NISO) = EST
C
  140 CONTINUE
C     --------
C
      ISO = 0
      DO 150 J=1,NDIS
         IF(MP(J).EQ.1) THEN
            ISO = ISO+1
            GR(ISO) = GG(J)
            V2(ISO) = VS(J)
         ENDIF
  150 CONTINUE
      IF(NISO.EQ.0) WRITE(NOUT,151)
      BETA(MP1) = BETAM
      RETURN
C
  151 FORMAT(/' THERE ARE NO ISOLATED T-EIGENVALUES SO NO ERROR',
     >       ' ESTIMATES WERE COMPUTED')
      END
************************************************************************
*DECK DIAG1
      SUBROUTINE DIAG1
C
************************************************************************
************************************************************************
**                                                                    **
**    BEGIN : MODULE DIAG  (DIAGNOSTICS)                              **
**    -------------------                                             **
**                                                                    **
**    STRUCTURE :                                                     **
**                                                                    **
**        DIAG1              DIAG234              DIAG5               **
**          QRPLOT             XINP                 (LBLTOP)          **
**            (LBLTOP)         EIGVFK               (NFRAME)          **
**            (LPLOT)            KOMPFK             (LPLOT)           **
**                                 CUBFCT                              **
**                               PPPEVP                               **
**                                 (NFRAME)                           **
**                                 (LPLOT)                            **
**                                 (DLCH)                             **
**                             DIAGXTAX                               **
**                               CONAMAT                              **
**                               (CXDOTU)                             **
**                                                                    **
************************************************************************
************************************************************************
C
C-----------------------------------------------------------------------
C     DIAGNOSTICS FOR QR
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPAR
*CALL COMDIM
*CALL CORE1D
C
C     PLOT THE EIGENVALUES
C
      CALL QRPLOT(WR,WI,NDIM,X,Y)
C
      RETURN
      END
************************************************************************
*DECK QRPLOT
      SUBROUTINE QRPLOT(WR,WI,NDIM,X,Y)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMPIO
*CALL COMPLOT
*CALL COMLAB
C
      INTEGER  NDIM
      REAL     WR(*), WI(*), XMIN, XMAX, YMIN, YMAX
      REAL     X(*), Y(*)
      INTEGER  L,LOR
      CHARACTER*10 XNAME, YNAME
      CHARACTER*24 TOP
C
      WRITE(NOUTE) LABEL, EQNAME
      WRITE(NOUTE) NDIM
      WRITE(NOUTE) (WR(I),I=1,NDIM)
      WRITE(NOUTE) (WI(I),I=1,NDIM)
 
      WRITE(TOP,'(''QR-PLOT (VERSION '',A3,'') : '')') LABEL(1:3)
      LABEL=TOP//EQNAME
      WRITE(NOUT,*) LABEL
      CALL LBLTOP(LABEL,34)
      CALL LBLBOT(' ',1)
C
      XNAME = 'RE(LAMBDA)'
      YNAME = 'IM(LAMBDA)'
C
C     TOTAL SPECTRUM
C
      XMIN = XMINQR(1)
      XMAX = XMAXQR(1)
      YMIN = YMINQR(1)
      YMAX = YMAXQR(1)
C
      L = 1
      DO 10 I = 1,NDIM
         IF((WI(I).GE.YMIN).AND.(WI(I).LE.YMAX)
     >   .AND.(WR(I).GE.XMIN).AND.(WR(I).LE.XMAX)) THEN
            Y(L) = WI(I)
            X(L) = WR(I)
            L    = L + 1
         ENDIF
   10 CONTINUE
      L = L - 1
      IF(L.LE.1) RETURN
      WRITE(NOUT,11) L,XMIN,XMAX,YMIN,YMAX
C
      CALL LPLOT(2,1,1201,X,Y,L,1,'TOTAL SPECTRUM',14,XNAME,10,YNAME,10)
C
C     SPECTRUM
C
      IF(NPLOT.GT.1) THEN
C
         DO 30 IP=2,NPLOT
            XMIN = XMINQR(IP)
            XMAX = XMAXQR(IP)
            YMIN = YMINQR(IP)
            YMAX = YMAXQR(IP)
            LOR  = 3-MOD(IP,2)
            L = 1
            DO 20 I = 1,NDIM
               IF((WI(I).GE.YMIN).AND.(WI(I).LE.YMAX)
     >         .AND.(WR(I).GE.XMIN).AND.(WR(I).LE.XMAX)) THEN
                  Y(L) = WI(I)
                  X(L) = WR(I)
                  L    = L + 1
               ENDIF
   20       CONTINUE
            L = L - 1
            IF(L.LE.1) RETURN
C
            CALL LPLOT
     >        (LOR,1,1201,X,Y,L,1,'(PART)SPECTRUM',12,XNAME,10,YNAME,10)
            WRITE(NOUT,11) L,XMIN,XMAX,YMIN,YMAX
   30    CONTINUE
C
      ENDIF
C
      RETURN
C
   11 FORMAT(1X,I5,' WERTE IM RAHMEN ',1P,2E12.4,2X,
     >       2E12.4,' GEPLOTTET')
      END
************************************************************************
*DECK DIAG234
      SUBROUTINE DIAG234
C-----------------------------------------------------------------------
C     DIAGNOSTICS FOR INVERSE VECTOR ITERATION
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPAR
*CALL COMP234
*CALL COMPIO
*CALL COMDIAG
*CALL CORE234D
*CALL COMGRID
*CALL COMMOD
*CALL COMWEL
*CALL COMIOD
*CALL COMSPL
*CALL COMFFT
*CALL COMBDIA
*CALL COMVRSP
*CALL COMVAC
*CALL COMIT
C
      COMPLEX  CSUM, CSUM1, CSUM2, CSUM3
      COMPLEX  A1(MANZ), A2(MANZ), DA2(MANZ), A3(MANZ), DA3(MANZ),
     >         JB1(MANZ), JB2(MANZ), JB3(MANZ), JB1V(MANZ), B3V(MANZ),
     >         BDB(MANZ), BDBV(MANZ), RHO1(MANZ), T1(MANZ),
     >         B3L(MANZ)
      COMPLEX  OOR2, PSOR2, PTOR2
      REAL     DUMMY(3),ABLTG(3)
C
      EXTERNAL CONAMAT, CONBMAT
C
C     DO 123 J = 1,NCVD
C     DO 123 I = 1,NBG
C        WRITE(NOUTE) EV(I,J)
C 123 CONTINUE
C
c-----------------------------------------------------------------------
c write eigenvector for use in seperate plot programs
c-----------------------------------------------------------------------
      write(22,11) ew
      write(22,12) ng,manz
      write(22,11) (rfour(m),m=1,manz)
      write(22,11) (sgrid(i),i=1,ng)
      do 10 i=1,ng
        write(22,11) (ev(j,i),j=1,nbg)
   10 continue
   11 format(4e16.8)
   12 format(2i8)


      IF(RWALL.GT.1.) THEN
C     --------------------
C

      SPS2 = 2. * CPSURF
      ZQ   = Q1(NPSI)
      T2OQ = RBP1(NPSI)**2 / ZQ
      TOQ  = RBP1(NPSI) / ZQ

      Qtemp   = SPWERT(NPSI,1.,Q1,Q2,Q3,Q4,CS,ABLTG)
      DQ1     = ABLTG(1)
      Ptemp   = SPWERT(NPSI,1.,P1,P2,P3,P4,CS,ABLTG)
      DP1     = ABLTG(1)
C
C ... VECTOR POTENTIAL AT THE BOUNDARY ...
C
      DO 700 M=1,MANZ
         A1(M)   =  EV(2*MANZ+2+2*(M-1),NG)
         A2(M)   =  EV(1+2*(M-1),NG)
         DA2(M)  =  EV(2+2*(M-1),NG)
  700 CONTINUE
C
C ... MAGNETIC FIELD AT THE BOUNDARY ...
C
C      WRITE(NOUT,*)
C      WRITE(NOUT,*) 'PLASMA MAGNETIC FIELD AT BOUNDARY, JB1, B3L : '
C      WRITE(NOUT,*)
      DO 710 M=1,MANZ
         SMS = RFOUR(M)
         JB1(M) = -(0.,1.)*(SMS+ZNKWEL*ZQ)/ZQ * A2(M)
         JB2(M) = ZNKWEL*A1(M) + DA2(M)/ZQ - DQ1 * A2(M)/ZQ**2
         JB3(M) = DA2(M) - SMS*A1(M)
         B3L(M) = JB3(M) * TOQ / SPS2
         JB1V(M) = JB1(M)
         CSUM1 = 0.
         CSUM2 = 0.
         CSUM3 = 0.
C         WRITE(NOUT,701) M,JB1(M),B3L(M)
  710 CONTINUE
C
C      WRITE(NOUT,*)
C      WRITE(NOUT,*) ' VACUUM MAGNETIC FIELD AT BOUNDARY, JB1V, B3LV : '
C      WRITE(NOUT,*)
      DO 730 M=1,MANZ
         B3V(M) = (0.,0.)
         DO 720 L=1,MANZ
            B3V(M) = B3V(M) + B3B1(L,M) * JB1V(L)
  720    CONTINUE
C         WRITE(NOUT,721) M,JB1V(M),B3V(M)
  730 CONTINUE
C
      DO 750 M=1,MANZ
         CSUM = (0.,0.)
         DO 740 L=1,MANZ
            SMS = RFOUR(M)
            SML = RFOUR(L)
            K = ABS(INT(SMS)-INT(SML)) + 1
C
            OOR2 = CMPLX(SPWERT(NPSI,1.,ROOR2(1,K),ROOR2(NP1,K),
     >                           ROOR2(N2P1,K),ROOR2(N3P1,K),CS,DUMMY),
     >                   SPWERT(NPSI,1.,IOOR2(1,K),IOOR2(NP1,K),
     >                           IOOR2(N2P1,K),IOOR2(N3P1,K),CS,DUMMY))
C
            PSOR2 = CMPLX(SPWERT(NPSI,1.,RPSOR2(1,K),RPSOR2(NP1,K),
     >                          RPSOR2(N2P1,K),RPSOR2(N3P1,K),CS,DUMMY),
     >                    SPWERT(NPSI,1.,IPSOR2(1,K),IPSOR2(NP1,K),
     >                          IPSOR2(N2P1,K),IPSOR2(N3P1,K),CS,DUMMY))
C
            PTOR2 = CMPLX(SPWERT(NPSI,1.,RPTOR2(1,K),RPTOR2(NP1,K),
     >                          RPTOR2(N2P1,K),RPTOR2(N3P1,K),CS,DUMMY),
     >                    SPWERT(NPSI,1.,IPTOR2(1,K),IPTOR2(NP1,K),
     >                          IPTOR2(N2P1,K),IPTOR2(N3P1,K),CS,DUMMY))
C
            IF (INT(SMS).LT.INT(SML)) THEN
               OOR2  = CONJG(OOR2)
               PSOR2 = CONJG(PSOR2)
               PTOR2 = CONJG(PTOR2)
            ENDIF
C
            CSUM = CSUM - PTOR2 * JB1(L)
     >                  + PSOR2 * JB2(L)  / SPS2
     >                  + OOR2  * JB3(L)  * T2OQ / SPS2
  740    CONTINUE
         BDB(M) = CSUM
  750 CONTINUE
C
      DO 770 M=1,MANZ
         CSUM = (0.,0.)
         DO 760 L=1,MANZ
            SMS = RFOUR(M)
            SML = RFOUR(L)
            K = ABS(INT(SMS)-INT(SML)) + 1
C
            OOR2 = CMPLX(SPWERT(NPSI,1.,ROOR2(1,K),ROOR2(NP1,K),
     >                            ROOR2(N2P1,K),ROOR2(N3P1,K),CS,DUMMY),
     >                    SPWERT(NPSI,1.,IOOR2(1,K),IOOR2(NP1,K),
     >                           IOOR2(N2P1,K),IOOR2(N3P1,K),CS,DUMMY))
C
            IF(INT(SMS).LT.INT(SML)) OOR2  = CONJG(OOR2)
            CSUM = CSUM + OOR2 *B3V(L)*TOQ*(SML+ZNKWEL*ZQ)/ZNKWEL
C
  760    CONTINUE
         BDBV(M) = CSUM
  770 CONTINUE
C
      WRITE(NOUT,*)
      WRITE(NOUT,*) ' *************************************************'
      WRITE(NOUT,*) ' *            CHECK BOUNDARY CONDITIONS          *'
      WRITE(NOUT,*) ' *************************************************'
      DO 780 M=1,MANZ
         WRITE(NOUT,771) INT(RFOUR(M)),BDB(M)+DP1*A2(M)/(SPS2*ZQ)
         WRITE(NOUT,772) BDBV(M)
  780 CONTINUE
      ENDIF
C     -----
C
      CALL EIGVFK(NG,NPDIM,EV,XP,YP,NBG)
C
C     WRITE DATA FOR VACUUM MAGNETIC FIELD RECONSTRUCTION
C
      IF (IBVAC.NE.0) THEN
         DO 790 M = 1, MANZ
            WRITE(NOUTB,*) REAL(JB1(M)),AIMAG(JB1(M))
 790     CONTINUE
      ENDIF
C
C     WRITE(NOUT,1)
C     CALL DIAGXTAX(NBG,NGL,3*NBG,NZMA,EV,ZMA,
C    >              BUFF,XTAX,PROZ,CSUM1,CONAMAT)
C     WRITE(NOUT,2)
C     CALL DIAGXTAX(NBG,NGL,3*NBG,NZMA,EV,ZMA,
C    >              BUFF,XTAX,PROZ,CSUM2,CONBMAT)
C     CSUM=CSUM1/CSUM2
C     WRITE(NOUT,3) CSUM1,CSUM2,CSUM
CC    CALL DIAGNOS(NG,EV)
      RETURN
C
  701 FORMAT(1X,I3,6E12.4)
  721 FORMAT(1X,I3,4E12.4)
  771 FORMAT(' M = ',I3,'   p1+B.b(plasma)  = ',2E16.8)
  772 FORMAT('           B.b(vacuum)     = ',2E16.8)
    1 FORMAT(///' XT*A*X :')
    2 FORMAT(///' XT*B*X :')
    3 FORMAT(/////' CSUM1,CSUM2',1P,4E12.4,0P/' DIAGNOSTIK:'/
     >       ' (XT*A*X)/(XT*B*X):',1P,2E12.4,0P)
      END
************************************************************************
*DECK EIGVFK
      SUBROUTINE EIGVFK(NG,NPDIM,X0,X,Y,NBG)
C-----------------------------------------------------------------------
C     PLOTS OF THE EIGENVECTOR FOR A NUMBER OF RFOUR(I)'S
C     VERSION FOR PPPLIB
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPIO
*CALL COMPARP
*CALL COMLAB
*CALL COMWEL
*CALL COMIT
C
      REAL     X(*), Y(NPDIM,2,MANZ)
      COMPLEX  X0(*)
      INTEGER  NG, NPDIM, NBG, IEIG, IK(7)
      CHARACTER*3 KOMP(7)
      DATA IK  / 1, 2, 3, 4, 5, 6, 7/
      DATA KOMP/'V1 ','V2 ','A1 ','A2 ','A3 ','V3 ','P '/
C
      EXTERNAL QUAFCT, CUBFCT
C
      NP = 2 * NG - 1
      WRITE(NOUTE) LABEL(1:3), EQNAME
C
      DO 30 IEIG=1,7
         DO 10 IM=1,MANZ
         DO 10 I=1,NPDIM
            Y(I,1,IM) = 0.0
            Y(I,2,IM) = 0.0
   10    CONTINUE
C
         IF ((IEIG.EQ.1).OR.(IEIG.EQ.4).OR.(IEIG.EQ.5)
     >       .OR.(IEIG.EQ.6).OR.(IEIG.EQ.7)    ) THEN
           CALL KOMPFK(IK(IEIG),NBG,NPDIM,1.,IF1,X0,X,Y,CUBFCT)
         ELSE
           CALL KOMPFK(IK(IEIG),NBG,NPDIM,1.,IF1,X0,X,Y,QUAFCT)
         ENDIF  
         CALL PPPEVP(X,Y,KOMP(IK(IEIG)),NP,NPDIM,EW,IEIG)
   30 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK KOMPFK
      SUBROUTINE KOMPFK(MKP,NBG,NPDIM,FAC,IFAC,X0,X,Y,CUBFCT)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMGRID
C
      INTEGER  MKP, NBG, NPDIM, IFAC
      REAL     X(*), Y(NPDIM,2,MANZ), H(4), FAC
      COMPLEX  X0(*)
C
C
      DO 20 IM = 1, MANZ
C
         K1   = 2*MANZ*(MKP-1)+(IM*2-1)
         K2   = K1+1
C
C ...... FUNKTIONSWERTE DER EIGENFUNKTION ...
C
         ZSR  = SGRID(1)
         FACT = FAC
         IF(IFAC.NE.0) FACT = FACT*ZSR**IFAC
         NP   = 1
C
         X(NP) = ZSR
C
         SL   = SGRID(1)
         SU   = SGRID(2)
C
C ...... BEITRAG DES KOEFFIZIENTEN A(1) ...
C
         CALL CUBFCT(ZSR,SL,SU,H)
         ZA   = H(2)
         ZB   = H(4)
C
         Y(NP,1,IM) = Y(NP,1,IM) + FACT * (REAL(X0(K1)) * ZA
     >                                   + REAL(X0(K2)) * ZB)
         Y(NP,2,IM) = Y(NP,2,IM) + FACT * (AIMAG(X0(K1)) * ZA
     >                                   + AIMAG(X0(K2)) * ZB)
C
C ...... BEITRAG DER KOEFFIZIENTEN  A(N+1)  UND  B(N+1) ...
C
         ZA   = H(1)
         ZB   = H(3)
C
         Y(NP,1,IM) = Y(NP,1,IM) + FACT * (REAL(X0(K1+NBG))*ZA
     >                                   + REAL(X0(K2+NBG))*ZB)
         Y(NP,2,IM) = Y(NP,2,IM) + FACT * (AIMAG(X0(K1+NBG))*ZA
     >                                   + AIMAG(X0(K2+NBG))*ZB)
C
C ...... SCHLEIFE UEBER DIE INTERVALLE ...
C
         DO 10  N = 1, NGINT
C
            SL    = SGRID(N)
            SU    = SGRID(N+1)
C
C ......... INTERVALL MITTE ...
C
            ZSR   = (SU + SL) / 2.
            FACT  = FAC*ZSR**IFAC
            NP    = NP + 1
C
            X(NP) = ZSR
C
C ......... BEITRAG DER KOEFFIZIENTEN  A(N)  UND  B(N) ...
C
            CALL CUBFCT(ZSR,SL,SU,H)
C
            ZA    = H(2)
            ZB    = H(4)
C
            Y(NP,1,IM) = Y(NP,1,IM) + FACT * (REAL(X0(K1)) * ZA
     >                                       + REAL(X0(K2)) * ZB)
            Y(NP,2,IM) = Y(NP,2,IM) + FACT * (AIMAG(X0(K1)) * ZA
     >                                      + AIMAG(X0(K2)) * ZB)
C
C ......... BEITRAG DER KOEFFIZIENTEN  A(N+1)  UND  B(N+1) ...
C
            ZA    = H(1)
            ZB    = H(3)
C
            Y(NP,1,IM) = Y(NP,1,IM) + FACT * (REAL(X0(K1+NBG))*ZA
     >                                      + REAL(X0(K2+NBG))*ZB)
            Y(NP,2,IM) = Y(NP,2,IM) + FACT * (AIMAG(X0(K1+NBG))*ZA
     >                                      + AIMAG(X0(K2+NBG))*ZB)
C
C ......... INTERVALL ENDE ...
C
            ZSR   = SU
            FACT  = FAC*ZSR**IFAC
            NP    = NP + 1
C
            X(NP) = ZSR
C
C ......... BEITRAG DES KOEFFIZIENTEN  B(N+1) ...
C
            CALL CUBFCT(ZSR,SL,SU,H)
            ZA    = H(2)
            ZB    = H(4)
C
            Y(NP,1,IM) = Y(NP,1,IM) + FACT * (REAL(X0(K1)) * ZA
     >                                      + REAL(X0(K2)) * ZB)
            Y(NP,2,IM) = Y(NP,2,IM) + FACT * (AIMAG(X0(K1)) * ZA
     >                                      + AIMAG(X0(K2)) * ZB)
C
C ......... BEITRAG DER KOEFFIZIENTEN  A(N+1)  UND  B(N+1) ...
C
            ZA    = H(1)
            ZB    = H(3)
C
            Y(NP,1,IM) = Y(NP,1,IM) + FACT * (REAL(X0(K1+NBG))*ZA
     >                                      + REAL(X0(K2+NBG))*ZB)
            Y(NP,2,IM) = Y(NP,2,IM) + FACT * (AIMAG(X0(K1+NBG))*ZA
     >                                      + AIMAG(X0(K2+NBG))*ZB)
C
            K1     = K1 + NBG
            K2     = K2 + NBG
C
   10    CONTINUE
C
   20 CONTINUE
 
      RETURN
      END
************************************************************************
*DECK PPPEVP
      SUBROUTINE PPPEVP(X,Y,YNAME,NPOINT,NPDIM,EW,IPN)
C-----------------------------------------------------------------------
C PLOTS THE REAL AND IMAGINARY PART OF ONE EIGENVECTOR
C USING PPPLIB, JANUAR 1990              (E.SCHWARZ)
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPIO
*CALL COMWEL
*CALL COMLAB
C
      REAL     X(*), Y(NPDIM,2,*)
      INTEGER  IOP(7),IOP7(7), IP(7), NPOINT, IPN
      COMPLEX  EW
      CHARACTER*7   TITLE
      CHARACTER*(*) YNAME
      CHARACTER*30  YNAMER,YNAMEI
      CHARACTER*39  EIGENW
      CHARACTER*52  EWEQ
C
*IF IPP
C     DATA IOP7 /1121,871,151,111,141,121,131/
      DATA IOP7 /1111,2061,461,421,451,431,441/
*ELSE
C     DATA IOP7 /190971,190641,190981,190991,191001,191101,191201/
      DATA IOP7 /190981,190991,191001,191011,191021,191031,191041/
*ENDIF
C
      XMIN = X(1)
      XMAX = X(NPOINT)
      MZ = MIN0(MANZ,7)
C
      WRITE(YNAMER,'(''RE'',A5,22X)') YNAME
      WRITE(YNAMEI,'(''IM'',A5,22X)') YNAME
C
      MMP  = (MANZ+1)/2
      MZP  = (MZ+1)/2
      MLOR = MZ/2
      IOP(MZP) = IOP7(4)
      IP(MZP)  = MMP
      DO 10 IM=1,MLOR
         IOP(MZP+IM) = IOP7(4+IM)
         IOP(MZP-IM) = IOP7(4-IM)
         IP(MZP+IM)  = MMP+IM
         IP(MZP-IM)  = MMP-IM
   10 CONTINUE
C
C ... REALTEIL ...
C
      IOMU = IPN + 1
      
      IF (IPN.GT.5) IOMU = IPN - 2

      ILR = 2
C
      YMIN = 0.0
      YMAX = 0.0
      DO 20 I  = 1,MZ
      DO 20 J  = 1,NPOINT
         IM = IP(I)
         IF (Y(J,1,IM).GT.YMAX) YMAX = Y(J,1,IM)
         IF (Y(J,1,IM).LT.YMIN) YMIN = Y(J,1,IM)
   20 CONTINUE
      YMIN = YMIN * 1.1
      YMAX = YMAX * 1.1
C-----------------------------------------------------------------------
C PLOT THE EIGENFUNKTION 
C-----------------------------------------------------------------------
      CALL NFRAME(ILR,IOMU,1,XMIN,XMAX,YMIN,YMAX,YNAMER,30,'S',1,' ',1)
      DO 30 I = 1,MZ
c---------------------- determine number of zeros in eigenfunction
        nzero = 0
        DO 35 j=2,npoint-1
          if (y(j,1,im)*y(j+1,1,im) .LT. 0) nzero = nzero + 1
   35   CONTINUE
        write(20,*) ' harmonic : ',im,' number of zeros :  ',nzero
         IM = IP(I)
         CALL LPLOT(ILR,IOMU,IOP(I),X,Y(1,1,IM),-NPOINT,1,YNAMER,30,
     >              'S',1,' ',1)
   30 CONTINUE
C
      IF(IPN.EQ.1) THEN
         IY = 0
         DO 40 I=1,MZ
            IM = IP(I)
            M  = RFOUR(IM)
            WRITE(TITLE,'(''M='',I2,3X)') M
            IXA = 85+(I-1)*11*12
*IF IPP
            IPC = IOP(I)/10
*ELSE
            IPC = (IOP(I)-190001)/10
*ENDIF
            CALL DLCH(IXA,IY,' ',IPC,2)
            CALL DLCH(IXA+12,IY,' ',IPC,2)
            CALL DLCH(IXA+24,IY,' ',IPC,2)
            CALL DLCH(IXA+48,IY,TITLE,7,2)
   40    CONTINUE
         WRITE(EIGENW,'('' EIGENVALUE:'',1P,2E13.5)')EW
         EWEQ = EQNAME//EIGENW
         CALL DLCH(112,779,EWEQ,52,2)
      ENDIF
C----------------------------------------------------------------------
C IMAGINARY PART
C----------------------------------------------------------------------
      ILR = 3
C
      YMIN = 0.0
      YMAX = 0.0
      DO 50 I = 1,MZ
      DO 50 J = 1,NPOINT
         IM = IP(I)
         IF (Y(J,2,IM).GT.YMAX) YMAX = Y(J,2,IM)
         IF (Y(J,2,IM).LT.YMIN) YMIN = Y(J,2,IM)
   50 CONTINUE
      YMIN = YMIN * 1.1
      YMAX = YMAX * 1.1
C-----------------------------------------------------------------------
C  PLOT THE EIGENFUNCTION 
C-----------------------------------------------------------------------
      CALL NFRAME(ILR,IOMU,1,XMIN,XMAX,YMIN,YMAX,YNAMEI,30,'S',1,' ',1)
      DO 60 I = 1,MZ
         IM = IP(I)
         CALL LPLOT(ILR,IOMU,IOP(I),X,Y(1,2,IM),-NPOINT,1,YNAMEI,30,
     >              'S',1,' ',1)
   60 CONTINUE
C
      RETURN
      END
************************************************************************
*DECK DIAGXTAX
      SUBROUTINE DIAGXTAX (NBG,NGL,NB3,NZMA,X,ZMA,
     >                     BUFF,XTAX,PROZ,CSUM,CONAMAT)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
*CALL COMPIO
*CALL COMMAX
*CALL COMGRID
C
      REAL     PROZ(NGL,NGL)
      COMPLEX  ZMA(NZMA,NZMA),BUFF(NBG,3*NBG)
      COMPLEX  X(*),XTAX(NGL,NGL)
      COMPLEX  CXDOTU,CSUM1,CSUM2,CSUM3,CSUM
C
      DO 10 I=1,NB3
      DO 10 J=1,NBG
         BUFF(J,I) = (0.,0.)
   10 CONTINUE
C
      DO 20 J=1,NGL
      DO 20 I=1,NGL
         XTAX(I,J)=(0.0,0.0)
   20 CONTINUE
C
C ... SCHLEIFE UEBER  N INTERVALLE ...
C
      DO 80  NI = 1,NGINT
C     -------------------
C
      CALL CONAMAT(NI,NZMA,ZMA)
C
      DO 30 K = 1, NBG
      DO 30 L = 1, NZMA
         J = NBG + L
         BUFF(K,J) = BUFF(K,J) + ZMA(K,L)
   30 CONTINUE
C
      IXM1 = MAX0((NI-2)*NBG,0)
      IX   = (NI-1)*NBG
      IXP1 = NI*NBG
C
      DO 50 KK=1,NGL
      DO 50 JJ=1,NGL
         JA = (JJ-1)*MANZ*2 + 1
         KA = (KK-1)*MANZ*2 + 1
         JE = JJ*MANZ*2
         KE = KK*MANZ*2
         KANZ = KE-KA+1
C
         DO 40 J=JA,JE
C
            CSUM1 = CXDOTU(KANZ,BUFF(J,KA),      NBG,X(IXM1+KA),1)
            CSUM2 = CXDOTU(KANZ,BUFF(J,NBG+KA),  NBG,X(IX+KA),  1)
            CSUM3 = CXDOTU(KANZ,BUFF(J,2*NBG+KA),NBG,X(IXP1+KA),1)
C
            XTAX(JJ,KK) = XTAX(JJ,KK) + X(IX+J)*(CSUM1+CSUM2+CSUM3)
C
   40    CONTINUE
   50 CONTINUE
C
      DO 60  K = 1, NBG
      DO 60  L = 1, NB3
         BUFF(K,L) = (0.,0.)
   60 CONTINUE
C
      DO 70  K = NBG+1, NZMA
      DO 70  L = 1, NZMA
         J = K - NBG
         BUFF(J,L) = ZMA(K,L)
   70 CONTINUE
C
   80 CONTINUE
C     --------
C
      IXM1 = (NG-2)*NBG
      IX   = (NG-1)*NBG
C
      DO 100 KK=1,NGL
      DO 100 JJ=1,NGL
         JA = (JJ-1)*MANZ*2 + 1
         KA = (KK-1)*MANZ*2 + 1
         JE = JJ*MANZ*2
         KE = KK*MANZ*2
         KANZ = KE-KA+1
C
         DO 90 J=JA,JE
C
            CSUM1 = CXDOTU(KANZ,BUFF(J,KA),    NBG,X(IXM1+KA),1)
            CSUM2 = CXDOTU(KANZ,BUFF(J,NBG+KA),NBG,X(IX+KA),  1)
C
            XTAX(JJ,KK) = XTAX(JJ,KK) + X(IX+J)*(CSUM1+CSUM2)
C
   90    CONTINUE
  100 CONTINUE
C
C     ANTEILE DER GLEICHUNGEN IN PROZENT:
C
      CSUM = (0.0,0.0)
      SUM  = 0.0
      DO 110 J=1,NGL
      DO 110 I=1,NGL
         CSUM = CSUM+XTAX(I,J)
         SUM  = SUM+CABS(XTAX(I,J))
  110 CONTINUE
      DO 120 J=1,NGL
      DO 120 I=1,NGL
         PROZ(I,J) = 100.*CABS(XTAX(I,J))/SUM
  120 CONTINUE
      WRITE(NOUT,121) ((I,J,XTAX(I,J),PROZ(I,J),J=1,NGL),I=1,NGL)
      RETURN
C
  121 FORMAT(///(' (',2I3,')=',1P,2E12.4,' : ',E12.4))
      END
************************************************************************
*DECK DIAG5
      SUBROUTINE DIAG5
C-----------------------------------------------------------------------
C     DIAGNOSTICS FOR LANCZOS
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPAR
*CALL COMPAR5
*CALL COMPIO
*CALL CORE5
*CALL COMDIM5
*CALL LANCZOS
*CALL COMLAB
 
      INTEGER      L
      REAL         XMIN, XMAX, YMIN, YMAX
      REAL         X(NDIM5), Y(NDIM5)
      CHARACTER*10 XNAME, YNAME
      CHARACTER*24 TOPN
 
      EQUIVALENCE (GR(1), X(1))
      EQUIVALENCE (GC(1), Y(1))
 
      WRITE(NOUTE) LABEL, EQNAME
      WRITE(NOUTE) NCONV
      WRITE(NOUTE) XLIML, XLIMR, YLIMB, YLIMT
      WRITE(NOUTE) (CONEV(J),J=1,NCONV)
 
      WRITE(TOPN,'(''LANCZOS (VERSION '',A3,'') : '')') LABEL(1:3)
      LABEL = TOPN//EQNAME
      CALL LBLTOP(LABEL,34)
      CALL LBLBOT(' ',1)
 
      XNAME = 'RE(LAMBDA)'
      YNAME = 'IM(LAMBDA)'
 
C ... SPECTRUM ...
 
      XMIN = XLIML
      XMAX = XLIMR
      YMIN = YLIMB
      YMAX = YLIMT
      WRITE(NOUT,*)' SR.DIAG5 XMIN,XMAX,YMIN,YMAX ',XMIN,XMAX,YMIN,YMAX
 
      L = 1
      DO 10 I=1,NCONV
         IF((AIMAG(CONEV(I)).GE.YMIN).AND.(AIMAG(CONEV(I)).LE.YMAX).AND.
     >   (REAL(CONEV(I)).GE.XMIN).AND.(REAL(CONEV(I)).LE.XMAX)) THEN
            Y(L) = AIMAG(CONEV(I))
            X(L) = REAL(CONEV(I))
            L    = L + 1
         ENDIF
   10 CONTINUE
      L = L - 1
      WRITE(NOUT,11) L,XMIN,XMAX,YMIN,YMAX
      IF(L.LT.1) RETURN
      WRITE(NOUT,12) (X(I),Y(I),I=1,L)
      REWIND NOUTP
      WRITE(NOUTP,12) (X(I),Y(I),I=1,L)
      CALL WRTEXT(-NOUTP)
 
      CALL NFRAME(11,1,1,XMIN,XMAX,YMIN,YMAX,'SPECTRUM',8,XNAME,10,
     >            YNAME,10)
      CALL LPLOT(11,1,1201,X,Y,-L,1,' ',1,' ',1,' ',1)
 
      RETURN
 
   11 FORMAT(1X,I5,' WERTE IM RAHMEN ',1P,2E12.4,2X,2E12.4,' GEPLOTTET')
   12 FORMAT(////////'  EIGENWERTE IM GEGEBENEN RAHMEN:'
     >       //(1X,1P,2E16.8,0P))
      END
C***********************************************************************
*DECK SGCOPY
      SUBROUTINE SGCOPY(N,V1,INC1,V2,INC2)
C-----------------------------------------------------------------------
C     COPY(SINGLE)
C-----------------------------------------------------------------------
      INTEGER  N, INC1, INC2
      REAL     V1(*), V2(*)
C
*IF CRAY
      CALL SCOPY(N,V1,INC1,V2,INC2)
*ELSE
      CALL DCOPY(N,V1,INC1,V2,INC2)
*ENDIF
      RETURN
      END
C***********************************************************************
*DECK SGSCAL
      SUBROUTINE SGSCAL(N,FAC,V,INC)
C-----------------------------------------------------------------------
C     FACTOR*VEKTOR(SINGLE)
C-----------------------------------------------------------------------
      INTEGER  N, INC
      REAL     V(*), FAC
C
*IF CRAY
      CALL SSCAL(N,FAC,V,INC)
*ELSE
      CALL DSCAL(N,FAC,V,INC)
*ENDIF
      RETURN
      END
C***********************************************************************
*DECK CXDOTU
      COMPLEX FUNCTION CXDOTU(N,CV1,INC1,CV2,INC2)
C-----------------------------------------------------------------------
C     CV1*CV2(COMPLEX)
C-----------------------------------------------------------------------
      INTEGER  N, INC1, INC2
      COMPLEX  CV1(*), CV2(*)
C
*IF CRAY
      COMPLEX  CDOTU
      CXDOTU = CDOTU(N,CV1,INC1,CV2,INC2)
*ELSE
      COMPLEX  ZDOTU
      CXDOTU = ZDOTU(N,CV1,INC1,CV2,INC2)
*ENDIF
      RETURN
      END
C***********************************************************************
*DECK CXDOTC
      COMPLEX FUNCTION CXDOTC(N,CV1,INC1,CV2,INC2)
C-----------------------------------------------------------------------
C     CONJG(CV1)*CV2(COMPLEX)
C-----------------------------------------------------------------------
      INTEGER  N, INC1, INC2
      COMPLEX  CV1(*), CV2(*)
C
*IF CRAY
      COMPLEX  CDOTC
      CXDOTC =  CDOTC(N,CV1,INC1,CV2,INC2)
*ELSE
      COMPLEX  ZDOTC
      CXDOTC =  ZDOTC(N,CV1,INC1,CV2,INC2)
*ENDIF
      RETURN
      END
C***********************************************************************
*DECK CXCOPY
      SUBROUTINE CXCOPY(N,CV1,INC1,CV2,INC2)
C-----------------------------------------------------------------------
C     COPY(COMPLEX)
C-----------------------------------------------------------------------
      INTEGER  N, INC1, INC2
      COMPLEX  CV1(*), CV2(*)
C
*IF CRAY
      CALL CCOPY(N,CV1,INC1,CV2,INC2)
*ELSE
      CALL ZCOPY(N,CV1,INC1,CV2,INC2)
*ENDIF
      RETURN
      END
C***********************************************************************
*DECK CXSCAL
      SUBROUTINE CXSCAL(N,CFAC,CV,INC)
C-----------------------------------------------------------------------
C     SCALE ARRAY(COMPLEX)
C-----------------------------------------------------------------------
      INTEGER  N, INC
      COMPLEX  CV(*), CFAC
C
*IF CRAY
      CALL CSCAL(N,CFAC,CV,INC)
*ELSE
      CALL ZSCAL(N,CFAC,CV,INC)
*ENDIF
      RETURN
      END
C***********************************************************************
*DECK CXAXPY
      SUBROUTINE CXAXPY(N,CFAC,CV1,INC1,CV2,INC2)
C-----------------------------------------------------------------------
C     CFAC*CV1+CV2 (COMPLEX)
C-----------------------------------------------------------------------
      INTEGER  N, INC1, INC2
      COMPLEX  CV1(*), CV2(*), CFAC
C
*IF CRAY
      CALL CAXPY(N,CFAC,CV1,INC1,CV2,INC2)
*ELSE
      CALL ZAXPY(N,CFAC,CV1,INC1,CV2,INC2)
*ENDIF
      RETURN
      END
C***********************************************************************
*DECK ICXAMAX
      INTEGER FUNCTION ICXAMAX(N,CV,INC)
C-----------------------------------------------------------------------
C     INDEX OF ELEMENT WITH MAXIMUM ABSOLUTE VALUE
C-----------------------------------------------------------------------
      COMPLEX  CV(*)
      INTEGER  N, INC
*IF CRAY
      INTEGER  ICAMAX
      ICXAMAX = ICAMAX(N,CV,INC)
*ELSE
      INTEGER  IZAMAX
      ICXAMAX = IZAMAX(N,CV,INC)
*ENDIF
      RETURN
      END
      SUBROUTINE RFT2(DATA,NR,KR)                                       
C                                                                       
C     ******************************************************************
C     * REAL FOURIER TRANSFORM.                                        *
C     * INPUT:  NR REAL COEFFICIENTS                                   *
C     *             DATA(1),DATA(1+KR),....,DATA(1+(NR-1)*KR).         *
C     * OUTPUT: NR/2+1 COMPLEX COEFFICIENTS                            *
C     *            (DATA(1),      DATA(1+KR))                          *
C     *            (DATA(1+2*KR), DATA(1+3*KR))                        *
C     *             .............................                      *
C     *            (DATA(1+NR*KR),DATA(1+(NR+1)*KR).                   *
C     * THE CALLING PROGRAM SHOULD HAVE DATA DIMENSIONED WITH AT LEAST *
C     * (NR+1)*KR+1 ELEMENTS. (I.E., NR+2 IF INCREMENT KR=1).          *
C     * LASL ROUTINE MAY 75, CALLING FFT2 AND RTRAN2.                  *
C     ******************************************************************
C                                                                       
      DIMENSION DATA(*)                                               
      CALL FFT2(DATA(1),DATA(KR+1),NR/2,-(KR+KR))                
      CALL RTRAN2(DATA,NR,KR,1)                               
      RETURN                                                   
      END                                                        
      SUBROUTINE FFT2 (DATAR,DATAI,N,INC)                               
C                                                                       
C     ******************************************************************
C     * FFT2 FORTRAN VERSION CLAIR NIELSON MAY 75.                     *
C     ******************************************************************
C                                                                 
      DIMENSION DATAR(*), DATAI(*)                                 
      KTRAN=ISIGN(-1,INC)                                            
      KS=IABS(INC)                                                    
      IP0=KS                                                     
      IP3=IP0*N                                                       
      IREV=1                                                         
      DO 20 I=1,IP3,IP0                                             
         IF(I.LT.IREV) THEN                                           
            TEMPR=DATAR(I)                                          
            TEMPI=DATAI(I)                                           
            DATAR(I)=DATAR(IREV)                                     
            DATAI(I)=DATAI(IREV)                                     
            DATAR(IREV)=TEMPR                                          
            DATAI(IREV)=TEMPI                                         
         ENDIF                                                         
         IBIT=IP3/2                                                 
   10    IF(IREV.GT.IBIT) THEN                                      
            IREV=IREV-IBIT                                          
            IBIT=IBIT/2                                          
            IF(IBIT.GE.IP0) GOTO 10                                
         ENDIF                                                   
   20    IREV=IREV+IBIT                                           
      IP1=IP0                                                     
      THETA=REAL(KTRAN)*3.1415926535898                          
   30 IF(IP1.GE.IP3) GOTO 60                                        
      IP2=IP1+IP1                                                 
      SINTH=SIN(.5*THETA)                                        
      WSTPR=-2.*SINTH*SINTH                                       
      WSTPI=SIN(THETA)                                             
      WR=1.                                                      
      WI=0.                                                         
      DO 50 I1=1,IP1,IP0                                             
         DO 40 I3=I1,IP3,IP2                                           
            J0=I3                                                    
            J1=J0+IP1                                                  
            TEMPR=WR*DATAR(J1)-WI*DATAI(J1)                            
            TEMPI=WR*DATAI(J1)+WI*DATAR(J1)                         
            DATAR(J1)=DATAR(J0)-TEMPR                               
            DATAI(J1)=DATAI(J0)-TEMPI                                
            DATAR(J0)=DATAR(J0)+TEMPR                             
   40       DATAI(J0)=DATAI(J0)+TEMPI                           
         TEMPR=WR                                                  
         WR=WR*WSTPR-WI*WSTPI+WR                                      
   50    WI=WI*WSTPR+TEMPR*WSTPI+WI                                  
      IP1=IP2                                                         
      THETA=.5*THETA                                                  
      GOTO 30                                                         
   60 RETURN                                                         
      END                                                               
      SUBROUTINE RTRAN2(DATA,NR,KR,KTRAN)                              
C                                                                       
C     ******************************************************************
C     * INTERFACE BETWEEN RFT2, RFI2, AND FFT2.                        *
C     * THE CALLING PROGRAM SHOULD HAVE DATA DIMENSIONED WITH AT LEAST *
C     * (NR+1)*KR+1 ELEMENTS.                                          *
C     * LASL ROUTINE MAY 75, CALLED FROM RFT2 AND RFI2.                *
C     ******************************************************************
C                                                                
      DIMENSION DATA(*)                                         
      KS=2*KR                                               
      N=NR/2                                                   
      NMAX=N*KS+2                                                 
      KMAX=NMAX/2                                                 
      THETA=1.5707963267949/N                                         
      DC=2.*SIN(THETA)**2                                            
      DS=SIN(2.*THETA)                                              
      WS=0.                                                         
      IF(KTRAN.LE.0) THEN                                        
         WC=-1.0                                                 
         DS=-DS                                                     
      ELSE                                                            
         WC=1.0                                                        
         DATA(NMAX-1)=DATA(1)                                          
         DATA(NMAX-1+KR)=DATA(KR+1)                                  
      ENDIF                                                          
      DO 10 K=1,KMAX,KS                                            
         NK=NMAX-K                                                 
         SUMR=.5*(DATA(K)+DATA(NK))                                  
         DIFR=.5*(DATA(K)-DATA(NK))                                   
         SUMI=.5*(DATA(K+KR)+DATA(NK+KR))                               
         DIFI=.5*(DATA(K+KR)-DATA(NK+KR))                            
         TR=WC*SUMI-WS*DIFR                                          
         TI=WS*SUMI+WC*DIFR                                           
         DATA(K)=SUMR+TR                                              
         DATA(K+KR)=DIFI-TI                                          
         DATA(NK)=SUMR-TR                                             
         DATA(NK+KR)=-DIFI-TI                                       
         WCA=WC-DC*WC-DS*WS                                           
         WS=WS+DS*WC-DC*WS                                             
         WC=WCA                                                       
   10 CONTINUE                                                         
      RETURN                                                            
      END                    

      SUBROUTINE RFI2(DATA,NR,KR)                                       
C                                                                       
C     ******************************************************************
C     * INVERSE OF RFT2.                                               *
C     * WHEN USING RFI2 IT IS NECESSARY TO HAVE VANISHING IMAGINARY    *
C     * PARTS OF THE FIRST AND LAST ELEMENT OF THE INPUT VECTOR:       *
C     *   DATA(1+KR)=DATA(1+(NR+1)*KR)=0.                              *
C     * THE CALLING PROGRAM SHOULD HAVE DATA DIMENSIONED WITH AT LEAST *
C     * (NR+1)*KR+1 ELEMENTS.                                          *
C     * LASL ROUTINE MAY 75, CALLING RTRAN2 AND FFT2.                  *
C     ******************************************************************
C                                                                       
      DIMENSION DATA(*)                                                 
      CALL RTRAN2(DATA,NR,KR,-1)                                        
      MR=NR*KR                                                          
      FNI=2./NR                                                         
      DO 10 I=1,MR,KR                                                   
   10 DATA(I)=FNI*DATA(I)                                               
      CALL FFT2(DATA(1),DATA(KR+1),NR/2,(KR+KR))                        
      RETURN                                                            
      END                                                               


*DECK SGTSL                                                             CAS02750
*** FROM NETLIB, TUE AUG 28 08:28:34 EDT 1990 ***                               
C                                                                               
      SUBROUTINE SGTSL(N,C,D,E,B,INFO)                                          
      INTEGER N,INFO                                                            
      REAL C(1),D(1),E(1),B(1)                                                  
C                                                                               
C     SGTSL GIVEN A GENERAL TRIDIAGONAL MATRIX AND A RIGHT HAND                 
C     SIDE WILL FIND THE SOLUTION.                                              
C                                                                               
C     ON ENTRY                                                                  
C                                                                               
C        N       INTEGER                                                        
C                IS THE ORDER OF THE TRIDIAGONAL MATRIX.                        
C                                                                               
C        C       REAL(N)                                                        
C                IS THE SUBDIAGONAL OF THE TRIDIAGONAL MATRIX.                  
C                C(2) THROUGH C(N) SHOULD CONTAIN THE SUBDIAGONAL.              
C                ON OUTPUT C IS DESTROYED.                                      
C                                                                               
C        D       REAL(N)                                                        
C                IS THE DIAGONAL OF THE TRIDIAGONAL MATRIX.                     
C                ON OUTPUT D IS DESTROYED.                                      
C                                                                               
C        E       REAL(N)                                                        
C                IS THE SUPERDIAGONAL OF THE TRIDIAGONAL MATRIX.                
C                E(1) THROUGH E(N-1) SHOULD CONTAIN THE SUPERDIAGONAL.          
C                ON OUTPUT E IS DESTROYED.                                      
C                                                                               
C        B       REAL(N)                                                        
C                IS THE RIGHT HAND SIDE VECTOR.                                 
C                                                                               
C     ON RETURN                                                                 
C                                                                               
C        B       IS THE SOLUTION VECTOR.                                        
C                                                                               
C        INFO    INTEGER                                                        
C                = 0 NORMAL VALUE.                                              
C                = K IF THE K-TH ELEMENT OF THE DIAGONAL BECOMES                
C                    EXACTLY ZERO.  THE SUBROUTINE RETURNS WHEN                 
C                    THIS IS DETECTED.                                          
C                                                                               
C     LINPACK. THIS VERSION DATED 08/14/78 .                                    
C     JACK DONGARRA, ARGONNE NATIONAL LABORATORY.                               
C                                                                               
C     NO EXTERNALS                                                              
C     FORTRAN ABS                                                               
C                                                                               
C     INTERNAL VARIABLES                                                        
C                                                                               
      INTEGER K,KB,KP1,NM1,NM2                                                  
      REAL T                                                                    
C     BEGIN BLOCK PERMITTING ...EXITS TO 100                                    
C                                                                               
         INFO = 0                                                               
         C(1) = D(1)                                                            
         NM1 = N - 1                                                            
         IF (NM1 .LT. 1) GO TO 40                                               
            D(1) = E(1)                                                         
            E(1) = 0.0E0                                                        
            E(N) = 0.0E0                                                        
C                                                                               
            DO 30 K = 1, NM1                                                    
               KP1 = K + 1                                                      
C                                                                               
C              FIND THE LARGEST OF THE TWO ROWS                                 
C                                                                               
               IF (ABS(C(KP1)) .LT. ABS(C(K))) GO TO 10                         
C                                                                               
C                 INTERCHANGE ROW                                               
C                                                                               
                  T = C(KP1)                                                    
                  C(KP1) = C(K)                                                 
                  C(K) = T                                                      
                  T = D(KP1)                                                    
                  D(KP1) = D(K)                                                 
                  D(K) = T                                                      
                  T = E(KP1)                                                    
                  E(KP1) = E(K)                                                 
                  E(K) = T                                                      
                  T = B(KP1)                                                    
                  B(KP1) = B(K)                                                 
                  B(K) = T                                                      
   10          CONTINUE                                                         
C                                                                               
C              ZERO ELEMENTS                                                    
C                                                                               
               IF (C(K) .NE. 0.0E0) GO TO 20                                    
                  INFO = K                                                      
C     ............EXIT                                                          
                  GO TO 100                                                     
   20          CONTINUE                                                         
               T = -C(KP1)/C(K)                                                 
               C(KP1) = D(KP1) + T*D(K)                                         
               D(KP1) = E(KP1) + T*E(K)                                         
               E(KP1) = 0.0E0                                                   
               B(KP1) = B(KP1) + T*B(K)                                         
   30       CONTINUE                                                            
   40    CONTINUE                                                               
         IF (C(N) .NE. 0.0E0) GO TO 50                                          
            INFO = N                                                            
         GO TO 90                                                               
   50    CONTINUE                                                               
C                                                                               
C           BACK SOLVE                                                          
C                                                                               
            NM2 = N - 2                                                         
            B(N) = B(N)/C(N)                                                    
            IF (N .EQ. 1) GO TO 80                                              
               B(NM1) = (B(NM1) - D(NM1)*B(N))/C(NM1)                           
               IF (NM2 .LT. 1) GO TO 70                                         
               DO 60 KB = 1, NM2                                                
                  K = NM2 - KB + 1                                              
                  B(K) = (B(K) - D(K)*B(K+1) - E(K)*B(K+2))/C(K)                
   60          CONTINUE                                                         
   70          CONTINUE                                                         
   80       CONTINUE                                                            
   90    CONTINUE                                                               
  100 CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
      END                                                                                                                               
      REAL FUNCTION X02AJF(AJUNK)
C     MARK 12 RELEASE. NAG COPYRIGHT 1986.
C
C     RETURNS  (1/2)*B**(1-P)  IF ROUNDS IS .TRUE.
C     RETURNS  B**(1-P)  OTHERWISE
C
C     .. Executable Statements ..
      X02AJF =     1.1102230246252d-16    
      RETURN
      END
