*COMDECK COMVER
      CHARACTER  VERSION*(*),   DD*(*)
      PARAMETER (VERSION = '1', DD = '5 JAN 1996')
*COMDECK COMMAX
      PARAMETER (LANZ=11, MANZ=LANZ, MDIF=1, LMAX=200)
      PARAMETER (LVANZ=51, MVANZ=LVANZ, LVMAX=LVANZ)
      PARAMETER (NGMAX=501, MXNINT=NGMAX-1, NDEQ=4*MXNINT)
      PARAMETER (NPSIMAX=201, NCHIMAX=513)
      PARAMETER (NPNC=NPSIMAX*NCHIMAX, NP4=4*NPSIMAX)
      PARAMETER (NVPSIMX=101, NVCHIMX=NCHIMAX)
      PARAMETER (NVPNVC=NVPSIMX*NVCHIMX,NVP4=4*NVPSIMX)
*COMDECK COMPAR
      PARAMETER (NGL=2, NBG=2*NGL*MANZ, NZMA=2*NBG, NB3=3*NBG)
c     PARAMETER (KILWOR= 8000000, NDIM1=1200/NBG*NBG)
c----------------------------- for m=61,ng=201
c      PARAMETER (KILWOR=80000000, NDIM1=1200/NBG*NBG)
c----------------------------- for m=71,ng=201
c      PARAMETER (KILWOR=110000000, NDIM1=1200/NBG*NBG)
c----------------------------- for m=81,ng=185
c      PARAMETER (KILWOR=120000000, NDIM1=1200/NBG*NBG)
c----------------------------- for m=41,ng=201
c      PARAMETER (KILWOR=35000000, NDIM1=1200/NBG*NBG)
      PARAMETER (KILWOR=10000000, NDIM1=1200/NBG*NBG)
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
      PARAMETER (LDA=3*(2*NBG-1)+1)
      PARAMETER (NDIM2=(KPMEX/(2*LDA+9))/NBG*NBG)
*IF CRAY
      PARAMETER (NREST2=KPMEX-2*LDA*NDIM2-9*NDIM2)
*ELSE
      PARAMETER (NREST2=KPMEX-2*LDA*NDIM2-8*NDIM2-NDIM2/2)
*ENDIF
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
*COMDECK COMPARV
      PARAMETER (NGLV=1, NBGV=2*NGLV*LVMAX, NZMAV=2*NBGV, NB3V=3*NBGV)
      PARAMETER (NGVMAX=101, NPDIMV=2*NGVMAX)
      PARAMETER (MLV=2*NBGV-1)
      PARAMETER (MUV=MLV)
      PARAMETER (LDAV=3*(2*NBGV-1)+1)
*COMDECK COMPIO
      PARAMETER (NIN=10, NIN2=11, NOUT=20, NOUTI=21, NOUT2=21, NOUT3=23)
      PARAMETER (NOUTP=24, NOUTV=25, NOUTE=26, NOUTVB = 27, NOUTB=28)
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
      PARAMETER (NMAX=1001)
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
     R         WR(NDIM1), WI(NDIM1),
     R         EVMAG(1,NDIM1),
     R         HCOR(NREST1),
     I         INDEX(NDIM1)
 
      COMPLEX  ZMA, AMAT, BMAT
      REAL     WR, WI, EVMAG, HCOR
      INTEGER  INDEX
C-----------------------------------------------------------------------
*COMDECK CORE2
      COMMON / CORE /
     C         ZMA(NZMA,NZMA),
     C         X0(NDIM2), X1(NDIM2), Y0(NDIM2), Y1(NDIM2),
     C         AMAT(LDA,NDIM2),
     R         HCOR(NREST2),
     I         IPVT(NDIM2)
 
      COMPLEX  ZMA, X0, X1, Y0, Y1, AMAT
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
      CHARACTER          LABEL*34,  EQNAME*10
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
      COMMON / COMWEL  / RFOUR(MANZ), VFOUR(MVANZ), ZNKWEL, NTOR
      REAL               RFOUR, VFOUR, ZNKWEL,NTOR
C      INTEGER            NTOR
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
     R         GAMMA, ETA, CWW, PSIS, Q0ZYL, DSURF, DSURF1, ALPHIN,
     R         DSURF2, DSURF3, DSURF4, DSURF5,
     I         IDPOW, IEQ, IAS
C
      REAL     GAMMA, ETA, PSIS, Q0ZYL, DSURF, DSURF1, ALPHIN
      REAL     DSURF2, DSURF3, DSURF4, DSURF5
      INTEGER  IDPOW, IEQ, IAS
C-----------------------------------------------------------------------
*COMDECK COMEQV
      COMMON / COMEQV /
     R         SGI(NDEQ), Q(NDEQ), DQ(NDEQ), T(NDEQ), DT(NDEQ),
     R         ETAV(NDEQ), DETA(NDEQ), RHO(NDEQ), DRHO(NDEQ),
     R         ZT0(NDEQ), ZDT0(NDEQ)
C
      REAL     SGI, Q, DQ, T, DT, ETAV, DETA, RHO, DRHO, ZT0, ZDT0
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
     R         RBP4(NPSIMAX),
     R         OM1(NPSIMAX),OM2(NPSIMAX),OM3(NPSIMAX),OM4(NPSIMAX)
C
      REAL     Q1, Q2, Q3, Q4, C1, C2, C3, C4, OM1, OM2, OM3, OM4,
     >         P1, P2, P3, P4, RBP1, RBP2, RBP3, RBP4
C-----------------------------------------------------------------------
*COMDECK COMFFT
      COMMON / COMFFT /
     R         RR2(NP4,LANZ), RR4(NP4,LANZ), RGPSI(NP4,LANZ),
     R         RSOGPSI(NP4,LANZ), RGPGT(NP4,LANZ), RSGGG(NP4,LANZ),
     R         RSR2GGG(NP4,LANZ), RR2GPSI(NP4,LANZ), RSOR2GP(NP4,LANZ),
     R         RSR2OGP(NP4,LANZ), RR2GPGT(NP4,LANZ), RR4GPSI(NP4,LANZ),
     R         RR4GPGT(NP4,LANZ), RDSR2(NP4,LANZ), RDSGPSI(NP4,LANZ),
     R         RSR4GGG(NP4,LANZ), RR4GPT2(NP4,LANZ),
     R         IR2(NP4,LANZ), IR4(NP4,LANZ), IGPSI(NP4,LANZ),
     R         ISOGPSI(NP4,LANZ), IGPGT(NP4,LANZ), ISGGG(NP4,LANZ),
     R         ISR2GGG(NP4,LANZ), IR2GPSI(NP4,LANZ), ISOR2GP(NP4,LANZ),
     R         ISR2OGP(NP4,LANZ), IR2GPGT(NP4,LANZ), IR4GPSI(NP4,LANZ),
     R         IR4GPGT(NP4,LANZ), IDSR2(NP4,LANZ), IDSGPSI(NP4,LANZ),
     R         ISR4GGG(NP4,LANZ), IR4GPT2(NP4,LANZ),
     I         NP1, N2P1, N3P1
C
      REAL     RR2, RR4, RGPSI, RSOGPSI, RGPGT, RSGGG, RSR2GGG,
     >         RR2GPSI, RSOR2GP, RSR2OGP, RR2GPGT, RR4GPSI, RR4GPGT,
     >         RDSR2, RDSGPSI, RSR4GGG, IR2, IR4, IGPSI, ISOGPSI,
     >         IGPGT, ISGGG, ISR2GGG, IR2GPSI, ISOR2GP, ISR2OGP,
     >         IR2GPGT, IR4GPSI, IR4GPGT, IDSR2, IDSGPSI, ISR4GGG,
     >         RR4GPT2, IR4GPT2
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
     R         ROOR2(NP4,LVANZ), IOOR2(NP4,LVANZ), RPSOR2(NP4,LVANZ),
     R         IPSOR2(NP4,LVANZ),RPTOR2(NP4,LVANZ),IPTOR2(NP4,LVANZ)
C
      REAL     ROOR2, IOOR2, RPSOR2, IPSOR2, RPTOR2, IPTOR2
C-----------------------------------------------------------------------
*COMDECK COMESH2
      COMMON / COMESH2 / RS0, RSA, BGF, XR1, XR2, SIG1,SIG2,FACT,IMESHAC
     >                   SBEGIN, SEND
      REAL               RS0, RSA, BGF, XR1, XR2, SIG1,SIG2,FACT
      INTEGER            IMESHAC
C---------------------------------------------------------------------
*COMDECK COMSTVR
      COMMON / COMSTVR / SX, SY
      REAL               SX(NGMAX*NBG), SY(NGMAX*NBG)
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
**  M A S T E R F I L E  :  MISHKA-1                                    **
**  ------------------------------                                    **
**                                                                    **
**  AUTHORS :         G. HUYSMANS, A. MIKHAILOVSKII,                  **
**                    W. KERNER, S. SHARAPOV                          **
**                                                                    **
**  VERSION :                                                         **
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
**               =1 ASYMMETRIC EQUILIBRIUM                            **
**                                                                    **                                                                    **
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
**    MISHKA - PRESET                                                 **
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
**    BLAS    :  ccopy  ,  cdotc  ,  cscal  ,  caxpy  ,  icamax  **
**              =C(Z)COPY, =C(Z)DOTC, =C(Z)SCAL, =C(Z)AXPY, =IC(Z)AMAX**
**               cdotu  ,  sscal  ,  scopy  ,  CX(SG)SCAL          **
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
*CALL COMPAR4
*CALL COMPARP
*CALL COMP234
*CALL COMPARV
*CALL COMPIO
*CALL COMMOD
*CALL COMDIM
*CALL COMLAB
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
*CALL CORE
*CALL COMVAC
*CALL COMJET
*CALL COMVGRD
*CALL COMVFT
*CALL COMBND
C
      CHARACTER*11  TXTPL(5)
C
      DATA TXTPL /'QR         ','STEUERWALD ',' ','IN-CORE-OOC',' '/
C
      NAMELIST / NEWRUN / MODE, EQNAME, NLTORE, NG,
     >                    RFOUR, VFOUR, NTOR, ETA, CWW, ITER,
     >                    ASPECT, Q0ZYL, SIG1, SIG2, XR1, XR2, RWALL,
     >                    NVPSI, NGV, SIGV, DSURF, IDPOW, NDIAGFK,
     >                    VSHIFT, NSHIFT,NRS, NIS, DRS, DIS, EPS,
     >                    NPLOT, XMINQR, YMINQR, XMAXQR, YMAXQR,
     >                    DSURF1,ALPHIN,IEQ,IAS,IBVAC,GAMMA,
     >                    DSURF2, DSURF3, IVAC, FW, NFW, RMIN, ZCNTR,
     >                    SBEGIN, SEND
C
CC
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
      IF(MODSOL.LT.1.OR.MODSOL.GT.4.OR.MODSOL.EQ.3) THEN
         WRITE(NOUT,*) 'WRONG MODE'
         GOTO 1000
      ENDIF
 
      IF(MODE.GT.10) CALL TESTS
C
      RFOUR1 = RFOUR(1)
      DO 20 JJ=1,MANZ
         RFOUR(JJ) = RFOUR1 + FLOAT((JJ-1)*MDIF)
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
C      ZNKWEL = FLOAT(NTOR)
      ZNKWEL = NTOR
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
C VACUUM RESPONSE 
C------------------------------------------------------------------------
      IF(RWALL.GT.1.) CALL VACUUM
C------------------------------------------------------------------------
C SOLVERS 
C------------------------------------------------------------------------
      REWIND(NOUTE)
C
      GOTO ( 100 , 200 , 300, 400 ) MODSOL
C
  100 CONTINUE
C-----------------------------------------------------------------------
C QR ALGORITHM
C-----------------------------------------------------------------------
      WRITE(NOUT,101)
C
      IF(NDIM.GT.NDIM1) THEN
         WRITE(NOUT,102) NDIM,NDIM1
         MNDIM1 = ((-3.5+SQRT(3.5**2+16.*KPMEX))/8.)/NBG*NBG
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
  400 CONTINUE
C-----------------------------------------------------------------------
C INVERSE VECTOR ITERATION, IN-CORE VERSION OF OUT-OF-CORE SOLVER
C-----------------------------------------------------------------------
      WRITE(NOUT,401) EWSHIFT
C
      IF(NCVIC.LT.NG) THEN
         WRITE(NOUT,402)
         WRITE(NOUT,403) NCVIC,NG
         STOP
      ENDIF
C
      DO ISH = 1, NSHIFT
        ISHIFT = ISH
        EWSHIFT = VSHIFT(ISH)
        CALL MAT4
        CALL SOLV4(EWOUT,NVI)
        IF ((NVI.LT.20).AND.(REAL(EWOUT).GT.0.)) GOTO 499
      ENDDO
  499 CONTINUE    
      CALL DIAG234
      GOTO 10
  500 CONTINUE
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
     >       '    ETA     = ',1P,E12.4,0P,5X,'Q0ZYL   = ',1P,E12.4,0P,/
     >       '    NTOR    = ',1P,E12.4,0P,5X,'NLTORE  = ',L7         ,/
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
   53 FORMAT(/5X,'DIMENSION OF THE A-MATRIX :     NDIM = ',I8)
   55 FORMAT(5X,'NUMBER OF FINITE ELEMENTS :    NGINT = ',I8)
   57 FORMAT(5X,'DIMENSION OF ZMA SUBBLOCK :     NZMA = ',I4/)
   59 FORMAT(/' ZNKWEL =',F7.3)
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
  401 FORMAT(//1X,80('*')/1X,'*',26X,'VECTOR ITERATION INCORE-OOC',
     >       25X,'*'/1X,80('*')//
     >       ' INITIAL GUESS EIGENVALUE :',1P,2E12.4,/)
  402 FORMAT(///' BLOCKS TOO LARGE - NCV < NG')
  403 FORMAT(/'NCVIC = ',I5,'  NG = ',I5)
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
      CWW       = 0.0
      ASPECT    =  1.
      DSURF     =  0.
      DSURF1    =  0.
      DSURF2    =  0.
      DSURF3    =  0.
      DSURF4    =  0.
      DSURF5    =  0.
      ALPHIN    =  1.
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
      SBEGIN    = 0.
      SEND      = 1.
C--------------------------------------------------------------------------
C VARIABLES FOR DIAGNOSTICS 
C--------------------------------------------------------------------------
      NDIAGFK   = 0
      IBVAC     = 0
C--------------------------------------------------------------------------
C NUMERICAL VARIABLES 
C--------------------------------------------------------------------------
      NG        =  11
      ITER      =  20
      EPS       = 1.E-6
C
      GEWI(1)   = .17392742256872693
      GEWI(2)   = .17392742256872693
      GEWI(3)   = .32607257743127307
      GEWI(4)   = .32607257743127307
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
         RFOUR(1)  =  1.-FLOAT(MANZ/2)*MDIF
         NLTORE    = .TRUE.
         NTOR      = -2
         ETA       = 0.
         Q0ZYL     = 0.3
         ASPECT    = 1.
         DSURF     = 1.
         IDPOW     = 1
C
C ...... NUMERICAL VARIABLES ...
C
         IF(MODE.EQ.11) THEN
            NG     = 7
            ETA    = 1.E-4
         ENDIF
C
C ...... RESULT ...
C
         EWTEST = (1.24153,0.0)
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
C
C IF XR1 and XR2 larger then 1. find the q=1 and q=2 surfaces
C
      IF ((XR1.GT.1.).AND.(XR2.GT.1.)) THEN
        IQ1 = 0
        IQ2 = 0
        DO I=1,NPSI
          IF (QS(I).LT.1.) IQ1=I
          IF (QS(I).LT.2.) IQ2=I
        ENDDO
        IF ((IQ1.GT.0).AND.(IQ1.LT.NPSI)) THEN
          XR1 = CS(IQ1) + (1.-QS(IQ1))/(QS(IQ1+1)-QS(IQ1)) 
     >        * (CS(IQ1+1)-CS(IQ1))
          WRITE(20,4) XR1
        ENDIF
        IF ((IQ2.GT.0).AND.(IQ2.LT.NPSI)) THEN
          XR2 = CS(IQ2) + (2.-QS(IQ2))/(QS(IQ2+1)-QS(IQ2)) 
     >        * (CS(IQ2+1)-CS(IQ2))
          WRITE(20,6) XR2
          IF ((XR2.GT.0.99).AND.(XR2.LE.1.000)) THEN
            XR2 = 0.99
          ENDIF
        ENDIF
        IF (IQ2.EQ.NPSI) THEN
          XR2=0.99
        ENDIF
      ENDIF        
    4 FORMAT(//' Localised q=1 : s = ',f6.3)
    6 FORMAT(' Localised q=2 : s = ',f6.3)      
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
         CALL SCOPY(NPSI-1,GEM12(NCHI+JC),NCHI,C1,1)
         CALL SPLINE(NPSI-1,CS(2),C1,0.0,0.0,3,Q1,Q2,Q3,Q4)
         GEM12(JC) = SPWERT(NPSI-1,0.0,Q1,Q2,Q3,Q4,CS(2),DUMMY)
   40 CONTINUE
C
C------------------------------------------------  SCALE TO Q ON AXIS
      IF (Q0ZYL.GT.0.0) THEN
        SCALEQ = QS(1)/Q0ZYL
C
        CPSURF = CPSURF*SCALEQ
        CALL sscal(NPSI,SCALEQ,CURJ,1)
        CALL sscal(NPSI,SCALEQ**2,P0,1)
        CALL sscal(NPSI*NCHI,-SCALEQ,GEM12,1)
        CALL sscal(NPSI*NCHI,SCALEQ**2,GEM11,1)
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
   50   CONTINUE
      ELSE
        CALL sscal(NPSI*NCHI,-1.,GEM12,1)
      ENDIF
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
C      QS(1) = CS(3)*CS(3)*QS(2)-CS(2)*CS(2)*QS(3)
C     >      /(CS(3)*CS(3)-CS(2)*CS(2))
      DQ0 = 0.
      
      CALL SPLINE(NPSI,CS,QS,DQ0,DQ1,1,Q1,Q2,Q3,Q4)
      CALL SPLINE(NPSI,CS,P0,DP0,DPE,1,P1,P2,P3,P4)
      CALL SPLINE(NPSI,CS,RBPHI,DRBPHI0,DRBPHIE,1,RBP1,RBP2,RBP3,RBP4)
      CALL scopy(NPSI,Q2,1,DQS,1)
C
C-------------- check spline for q-profile in centre
      DO I=1,100
        PLOTS(I) = REAL(i-1)*0.01
        PLOTQ(I) = SPWERT(NPSI,PLOTS(I),P1,P2,P3,P4,CS,DUMMY)
      ENDDO
      CALL LPLOT6(2,2,PLOTS,PLOTQ,100,'splined pressure')
      DO I=1,100
        PLOTS(I) = REAL(i-1)*0.01
        tmp = SPWERT(NPSI,PLOTS(I),P1,P2,P3,P4,CS,DUMMY)
        PLOTQ(I) = dummy(1)
      ENDDO
      CALL LPLOT6(3,2,PLOTS,PLOTQ,100,'pressure gradient')
      DO I=1,100
        PLOTS(I) = REAL(i-1)*0.002
        PLOTQ(I) = SPWERT(NPSI,PLOTS(I),Q1,Q2,Q3,Q4,CS,DUMMY)
      ENDDO
      CALL LPLOT6(3,3,PLOTS,PLOTQ,100,'splined q-profile')
      DO I=1,100
        PLOTS(I) = REAL(i-1)*0.01
        PLOTQ(I) = SPWERT(NPSI,PLOTS(I),Q1,Q2,Q3,Q4,CS,DUMMY)
      ENDDO
      CALL LPLOT6(2,3,PLOTS,PLOTQ,100,'splined q-profile')

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
      JINT = 2*NGINT+1
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
      J1INT = 30
      JINT  = J1INT * NGINT
      ZFD   = (RSA - RS0) / FLOAT(NGINT)
      ZS    = RS0
      ZSUM  = 0.0
      ZF    = FGAUS(ZS,BGF,XR1,XR2,SIG1,SIG2,FACT)
      ZF    = ZF * ZNORM
      ZDS0  = (RSA - RS0) * ZF / FLOAT(JINT)
      I = 2
   20 CONTINUE
      ZI    = FLOAT(I-1) * ZFD + RS0
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
            DENS    = 1.-(1.-DSURF)*SGI(J)**2
            IF(IEQ.EQ.2) THEN
               RHO(J) = (1.-DSURF1*SGI(J)**2)*(1.+DSURF*SGI(J)**2)
               DRHO(J) = -2.*DSURF1*SGI(J)*(1.+DSURF*SGI(J)**2)
     >                   +2.*DSURF*SGI(J)*(1.-DSURF1*SGI(J)**2)
            ELSEIF(IEQ.EQ.3) THEN
               RHO(J) = 1.+DSURF*SGI(J)**2+DSURF1*SGI(J)**4
     >                    +DSURF2*SGI(J)**6+DSURF3*SGI(J)**8
               DRHO(J) = 2.*DSURF*SGI(J)+4.*DSURF1*SGI(J)**3
     >                     +6.*DSURF2*SGI(J)**5+8.*DSURF3*SGI(J)**7
            ELSEIF(IEQ.EQ.4) THEN
               RHO(J) = 1.+ DSURF*SGI(J)     + DSURF1*SGI(J)**2
     >                    + DSURF2*SGI(J)**3 + DSURF3*SGI(J)**4
     >                    + DSURF4*SGI(J)**5 + DSURF5*SGI(J)**6
               DRHO(J) =    DSURF            + 2.*DSURF1*SGI(J)
     >                    + 3.*DSURF2*SGI(J)**2 + 4.*DSURF3*SGI(J)**3
     >                    + 5.*DSURF4*SGI(J)**4 + 6.*DSURF5*SGI(J)**5
            ELSEIF (IEQ.EQ.6) THEN
               RHO(J) = DSURF1 + (1.-DSURF1)*(ZP/P1(1))**DSURF
               DRHO(J) = (1.-DSURF1) * DSURF*(ZP/P1(1))**(DSURF-1.)
     >                 * ZDP/P1(1)
            ELSEIF (IEQ.EQ.7) THEN
               IF (SGI(J).LT.DSURF) THEN
                 RHO(J) = 1. - (1.-DSURF1)*(SGI(J)/DSURF)**2
                 DRHO(J) = - (1.-DSURF1)*2*SGI(J)/DSURF**2
               ELSE
                 RHO(J) = (SGI(J)-1.)/(DSURF-1.)*(DSURF1-DSURF2)+DSURF2
                 DRHO(J) = 1./(DSURF-1.)*(DSURF1-DSURF2)
               ENDIF
            ELSE
               RHO(J)  = DENS**IDPOW
               DRHO(J) = IDPOW*DENS**(IDPOW-1)*(-2*(1.-DSURF)*SGI(J))
            ENDIF
            ZT0(J)  = ZP
            ZDT0(J) = ZDP
C------------------------------------ omega*
            OMEGA(J) = SPWERT(NPSI,ZS(I),OM1,OM2,OM3,OM4,CS,ABLTG) /
     >                 RHO(J)
C------------------------------------ LArmor radius (normalised)
            RLARM(J) = CWW * SQRT(2.*ZP/RHO(J))
C
   10    CONTINUE
C
   20 CONTINUE

      WRITE(NOUT,21)
      DO 30 J=1,4*NGINT
        WRITE(NOUT,22) SGI(J),Q(J),DQ(J),OMEGA(J),T(J),
     >           RHO(J),DRHO(J),1000.*ZT0(J),1000.*ZDT0(J),RLARM(J)
   30 CONTINUE

   
      QWALL   = SPWERT(NPSI,SGRID(NI),Q1,Q2,Q3,Q4,CS,ABLTG)
      WRITE(NOUT,31) QWALL
      RETURN
   21 FORMAT(//5X,'S',4X,'Q',6X,'DQ',6X,'OMEGA*',4X,
     >       'T',5X,'RHO',5X,'DRHO',4X,'T0',6X,'DT0'/1X,78('-'))
   22 FORMAT(3F7.3,E11.3,5F7.3,E11.3)

   31 FORMAT(/' Q AT BOUNDARY :',E12.4)
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
      REAL     DUMMY(3)
      COMPLEX  ZFK(NCHIMAX,NPSIMAX)
      REAL     WORK(6*NCHIMAX)
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
C                        GRAD.PSI**2  -->  RGPSI,IGPSI
C-----------------------------------------------------------------------
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,GEM11,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RGPSI,IGPSI)
C-----------------------------------------------------------------------
C                        S**2/GRAD.PSI**2  -->  RSOGPSI,ISOGPSI
C-----------------------------------------------------------------------
      DO 50 I=NCHI+1,NGES
         HV(I) = CS(1+(I-1)/NCHI)**2 / GEM11(I)
   50 CONTINUE
      DO 60 J=1,NCHI
         CALL scopy(NPSI-1,HV(NCHI+J),NCHI,FWT,1)
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
C        S**2*(GRAD.PSI*GRAD.THETA)**2/GRAD.PSI**2  -->  RSGGG,ISGGG
C-----------------------------------------------------------------------
      DO 70 I=NCHI+1,NGES
         HV(I) = CS(1+(I-1)/NCHI)**2 * GEM12(I)**2 / GEM11(I)
   70 CONTINUE
      DO 80 J=1,NCHI
         CALL scopy(NPSI-1,HV(NCHI+J),NCHI,FWT,1)
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
         CALL scopy(NPSI-1,HV(NCHI+J),NCHI,FWT,1)
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
         CALL scopy(NPSI-1,HV(NCHI+J),NCHI,FWT,1)
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
C        S**2 / (R**2*GRAD.PSI**2)  -->  RSOR2GP,ISOR2GP
C-----------------------------------------------------------------------
      DO 140 I=NCHI+1,NGES
         HV(I) = CS(1+(I-1)/NCHI)**2 / (GEM33(I) * GEM11(I))
  140 CONTINUE
      DO 150 J=1,NCHI
         CALL scopy(NPSI-1,HV(NCHI+J),NCHI,FWT,1)
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
         CALL scopy(NPSI-1,HV(NCHI+J),NCHI,FWT,1)
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
C        R**4*GRAD.PSI*GRAD.THETA  -->  RR4GPGT,IR4GPGT
C-----------------------------------------------------------------------
      DO 200 I=1,NGES
         HV(I) = GEM33(I)**2*GEM12(I)
     >    / (1. + GEM11(I) / RBPHI(1+(I-1)/NCHI)**2  )         
  200 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RR4GPGT,IR4GPGT)
C-----------------------------------------------------------------------
C        R**4*GRAD.PSI*GRAD.THETA**2 -->  RR4GPGT,IR4GPGT
C-----------------------------------------------------------------------
      DO 205 I=1,NGES
         HV(I) = GEM33(I)**2*GEM12(I)**2
     >    / (1. + GEM11(I) / RBPHI(1+(I-1)/NCHI)**2  )         
  205 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RR4GPT2,IR4GPT2)
C-----------------------------------------------------------------------
C        DS R**2  -->  RDSR2,IDSR2
C-----------------------------------------------------------------------
      DO 210 J=1,NCHI
         CALL scopy(NPSI,GEM33(J),NCHI,FWT,1)
         CALL SPLINE(NPSI,CS,FWT,0.0,0.0,3,H1,H2,H3,H4)
         CALL scopy(NPSI,H2,1,HV(J),NCHI)
  210 CONTINUE
C
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RDSR2,IDSR2)
C-----------------------------------------------------------------------
C        DS GRAD.PSI**2  -->  RDSGPSI,IDSGPSI
C-----------------------------------------------------------------------
      DO 220 J=1,NCHI
         CALL scopy(NPSI,GEM11(J),NCHI,FWT,1)
         CALL SPLINE(NPSI,CS,FWT,0.0,0.0,3,H1,H2,H3,H4)
         CALL scopy(NPSI,H2,1,HV(J),NCHI)
  220 CONTINUE
C
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RDSGPSI,IDSGPSI)
C-----------------------------------------------------------------------
C ADDITIONAL FOURIER COEFF. FOR DIAGNOSTIC BOUND. COND. 
C-----------------------------------------------------------------------
C        1. / R**2   -->  ROOR2,IOOR2
C-----------------------------------------------------------------------
      DO 230 I=1,NGES
         HV(I)= 1. / GEM33(I)
  230 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,ROOR2,IOOR2)
C-----------------------------------------------------------------------
C        GRAD.PSI**2 / R**2   -->  RPSOR2,IPSOR2
C-----------------------------------------------------------------------
      DO 240 I=1,NGES
         HV(I) = GEM11(I) / GEM33(I)
  240 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RPSOR2,IPSOR2)
C-----------------------------------------------------------------------
C        GRAD.PSI*GRAD.THETA / R**2  -->  RPTOR2,IPTOR2
C-----------------------------------------------------------------------
      DO 250 I=1,NGES
         HV(I) = GEM12(I) / GEM33(I)
  250 CONTINUE
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
     >        CALL sscal(NCHI-2,-1.,FWT(NCHI+1),1)
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
c         DO 40 I=2,L
c            ZFK(I,J)=(N/(PI*(I-1)))**2*0.5*(1.-WORK((N+2)*2+I))*ZFK(I,J)
c   40    CONTINUE
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
      CALL DGTSL(N-2,B,D,C,A,IERR)
*ENDIF
      IF(IERR.NE.0) THEN
         WRITE(NOUT,21)
         STOP
      ENDIF
C
C     UEBERSCHREIBEN DES LOESUNGSVEKTORS
C
      CALL scopy(N-2,A,1,C(2),1)
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
      CALL scopy(N,Y,1,A,1)
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
      REAL     XWERT, A(N), B(N), C(N), D(N), X(N), ABLTG(3)
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
        WRITE(NOUT,6) XR1,BB1
      ENDIF
      IF (IQ2.LT.NPSI) THEN
        WRITE(NOUT,7) XR2,BB2
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
     >         RPLT(NVCHIMX),ZPLT(NVCHIMX),
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
         CALL NFRAME(21,11,1,1.5,4.5,-2.5,2.5,
     >        'GEOMETRY',8,'R',1,'Z',1)      
      ENDIF
      IF (IVAC.NE.4) THEN
         CALL LPLOT(1,1,1,RPP,ZPP,-NVCHI,1,' ',1,' ',1,' ',1)
         CALL LPLOT(1,1,1,RWP,ZWP,-NVCHI,1,' ',1,' ',1,' ',1)
      ELSE 
         RSCALE = RMIN/RADIUS
         DO 15 J=1,NVCHI
           RPLT(J) = RSCALE*(1.+RPP(J))
           ZPLT(J) = RSCALE*ZPP(J)
  15     CONTINUE
         CALL LPLOT(1,1,1,RPLT,ZPLT,-NVCHI,1,' ',1,' ',1,' ',1)
         DO 25 J=1,NVCHI
           RPLT(J) = RSCALE*(1.+RWP(J))
           ZPLT(J) = RSCALE*ZWP(J)
  25     CONTINUE
         CALL LPLOT(1,1,1,RPLT,ZPLT,-NVCHI,1,' ',1,' ',1,' ',1)
      ENDIF
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
     >          4.350,    0.000,   4.346,    0.118,                                        
     >          4.336,    0.236,   4.319,    0.353,                                        
     >          4.294,    0.471,   4.263,    0.589,                                        
     >          4.224,    0.707,   4.177,    0.824,                                        
     >          4.123,    0.942,   4.060,    1.060,                                        
     >          3.988,    1.178,   3.907,    1.295,                                        
     >          3.815,    1.412,   3.712,    1.527,                                        
     >          3.597,    1.640,   3.469,    1.750,                                        
     >          3.327,    1.853,   3.170,    1.945,                                        
     >          3.000,    2.022,   2.819,    2.074,                                        
     >          2.631,    2.092,   2.446,    2.067,                                        
     >          2.275,    1.993,   2.127,    1.873,                                        
     >          2.006,    1.721,   1.913,    1.552,                                        
     >          1.843,    1.379,   1.791,    1.210,                                        
     >          1.751,    1.048,   1.721,    0.895,                                        
     >          1.699,    0.751,   1.682,    0.615,                                        
     >          1.670,    0.484,   1.661,    0.359,                                        
     >          1.655,    0.237,   1.651,    0.118,                                        
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
      WRITE(20,*) ' USING JET WALL GEOMETRY : '
      WRITE(20,*) '   Rcenter = ',RCNTR
      WRITE(20,*) '   Zcenter = ',ZCNTR
      WRITE(20,*) '   Magnetic axis = ',RMAG
      WRITE(20,*) '   Minor radius = ',RMIN                                                                  
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

      R = 0.
      IF (IAS.EQ.0) THEN
         DO 10 M=1, NFR
            R = R + FR(M)*COS((M-1)*THETA)
 10      CONTINUE
      ELSE
         DO 20 M=1, NFR/2
            R = R + FR(M*2-1)*COS(M*THETA)+FR(M*2)*SIN(M*THETA)
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
      DO 50 M=1,MVANZ
C         VFOUR(M) = REAL(- LVMAX/2 + M - 1)
          VFOUR(M) = VFOUR1 + FLOAT(M) - 1.
   50 CONTINUE
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
      COMPLEX  G11, G12, G22, G33, ZMA(*), FACT(1)
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
      DO 200   KF = 1 , LVANZ
C
        K = (KF-1) * MDIF + 1
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
         FACT(1) =   SMS*SMZ * G22 + ZNKWEL**2 * G33
C
      CALL FKUBL(MZ,MS,MVANZ,1,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),HC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H'(CUB)   FUER DIE GLEICHUNG  P(1,1)
C     ------------------------
C
         FACT(1) =   G11
C
      CALL FKUBL(MZ,MS,MVANZ,1,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),DHC,DHC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H(CUB)   FUER DIE GLEICHUNG  P(1,1)
C     -----------------------
C
         FACT(1) = + (0.,1.) * SMS * G12
C
      CALL FKUBL(MZ,MS,MVANZ,1,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),DHC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H'(CUB)   FUER DIE GLEICHUNG  P(1,1)
C     -----------------------
C
         FACT(1) = - (0.,1.) * SMZ * G12
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
         FACT(1) =   SMS*SMZ * CONJG(G22) + ZNKWEL**2 * CONJG(G33)
C
      CALL FKUBL(MS,MZ,MVANZ,1,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),HC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H'(CUB)   FUER DIE GLEICHUNG   P(1,10
C     ------------------------
C
         FACT(1) = CONJG(G11)
C
      CALL FKUBL(MS,MZ,MVANZ,1,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),DHC,DHC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H(CUB)   FUER DIE GLEICHUNG  P(1,1)
C     -----------------------
C
         FACT(1) = + (0.,1.) * SMZ * CONJG(G12)
C
      CALL FKUBL(MS,MZ,MVANZ,1,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),DHC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H'(CUB)   FUER DIE GLEICHUNG  P(1,1)
C     -----------------------
C
         FACT(1) = - (0.,1.) * SMS * CONJG(G12)
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
**      CONBMAT      (RANSET)     (RANSET)     (RANSET)     (cscal)  **
**        CUBFCT     STVAL        STVAL        (STVAL)      CONBMAT   **
**        QUAFCT     CONAMAT      CONAMAT      CONAMAT        <--     **
**        SPWERT       <--          <--          <--        (sscal)  **
**        FKUBL                   (ccopy)     CONBMAT      CONAMAT   **
**      CONAMAT                   CONBMAT        <--          <--     **
**        CUBFCT                    <--                               **
**        QUAFCT                  (cdotu)                            **
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
      COMPLEX   cdotu
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
     >                    +cdotu(NBG2,ZMA(K,1),NZMA,EV(1,NI-1,1),1)
            X(K,NI,1)   =  cdotu(NBG2,ZMA(NBG+K,1),NZMA,EV(1,NI-1,1),1)
            X(K,NI-1,2) = X(K,NI-1,2)
     >                    +cdotu(NBG2,ZMA(K,1),NZMA,EV(1,NI-1,2),1)
            X(K,NI,2)   =  cdotu(NBG2,ZMA(NBG+K,1),NZMA,EV(1,NI-1,2),1)
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
      SUBROUTINE DURAND(RSEED,N,AR)
c***********************************************************************
c  ROUTINE TO REPLACE THE DURAND ROUTINE FROM ESSL (IBM) FOR RANDOM 
C  NUMBER GENERATION WITH STANDARD F90 ROUTINE
C***********************************************************************
      REAL AR(*)
c      CALL RANDOM_SEED
      DO I=1,N
c        CALL RANDOM_NUMBER(AR(I))
c------------------------------------- for use with f2c (or fort77)
         AR(I) = RAND()
      ENDDO
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
     >         INDCC(1),INDQQ(1),INDCQ(1),INDQC(1)
      REAL     SL,SU,ZDIF,ZA,ZB,ZC,ZSR,ZQ,ZT,QOT,TOQ,
     >         DZQ,DZT,ZETA,DZETA,ZRHO,DZRHO,T0,DT0,FKDUMMY,
     >         ZS(4),HC(4),HQ(4),DUMMY(3)
      COMPLEX  R2,R4,R2GPSI,R4GPSI,R2GPGT,R4GPGT,SOGPSI,
     >         SR2OGP,SR2GGG,SR4GGG,R4GPT2,
     >         ZMA(NZMA*NZMA),FACT(7)
C
      DATA INDQQ / 4 /
      DATA INDCC / 1 /
      DATA INDCQ / 3 /
      DATA INDQC / 2 /
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
!         ZETA    = ETAV((NI-1)*4+I)
!         DZETA   = DETA((NI-1)*4+I)
         ZRHO    = RHO((NI-1)*4+I)
         DZRHO   = DRHO((NI-1)*4+I)
         T0      = ZT0((NI-1)*4+I)
         DT0     = ZDT0((NI-1)*4+I)
C
         QOT     = ZQ/ZT
         TOQ     = ZT/ZQ
         SPS2    = 2.*ZSR*CPSURF
         FKDUMMY =  1.0
C
         CALL CUBFCT(ZSR,SL,SU,HC)
         CALL QUAFCT(ZSR,SL,SU,HQ)
C
C
      DO 200  KF = 1 , L A N Z
C     ------------------------
C
        K = (KF-1) * MDIF +1
C
        R2     = CMPLX(SPWERT(NPSI,ZSR,RR2(1,K),RR2(NP1,K),
     >                        RR2(N2P1,K),RR2(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IR2(1,K),IR2(NP1,K),
     >                        IR2(N2P1,K),IR2(N3P1,K),CS,DUMMY))
C
        R4GPSI = CMPLX(SPWERT(NPSI,ZSR,RR4GPSI(1,K),RR4GPSI(NP1,K),
     >                        RR4GPSI(N2P1,K),RR4GPSI(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IR4GPSI(1,K),IR4GPSI(NP1,K),
     >                        IR4GPSI(N2P1,K),IR4GPSI(N3P1,K),CS,DUMMY))
C
        R4GPGT = CMPLX(SPWERT(NPSI,ZSR,RR4GPGT(1,K),RR4GPGT(NP1,K),
     >                        RR4GPGT(N2P1,K),RR4GPGT(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IR4GPGT(1,K),IR4GPGT(NP1,K),
     >                        IR4GPGT(N2P1,K),IR4GPGT(N3P1,K),CS,DUMMY))
C
        R4GPT2 = CMPLX(SPWERT(NPSI,ZSR,RR4GPT2(1,K),RR4GPT2(NP1,K),
     >                        RR4GPT2(N2P1,K),RR4GPT2(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IR4GPT2(1,K),IR4GPT2(NP1,K),
     >                        IR4GPT2(N2P1,K),IR4GPT2(N3P1,K),CS,DUMMY))
C
        SR2OGP = CMPLX(SPWERT(NPSI,ZSR,RSR2OGP(1,K),RSR2OGP(NP1,K),
     >                        RSR2OGP(N2P1,K),RSR2OGP(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,ISR2OGP(1,K),ISR2OGP(NP1,K),
     >                        ISR2OGP(N2P1,K),ISR2OGP(N3P1,K),CS,DUMMY))
C
        SR4GGG = CMPLX(SPWERT(NPSI,ZSR,RSR4GGG(1,K),RSR4GGG(NP1,K),
     >                        RSR4GGG(N2P1,K),RSR4GGG(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,ISR4GGG(1,K),ISR4GGG(NP1,K),
     >                        ISR4GGG(N2P1,K),ISR4GGG(N3P1,K),CS,DUMMY))
C
      DO 100  MS = 1 , MANZ - KF + 1
C     ------------------------------
C
      MZ = MS + KF - 1
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(QUA)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(2,2)
C
      FACT(1) =  ZRHO * R4GPSI * ZQ / (SPS2*ZT**3)
C
      CALL FKUBL(MZ,MS,MANZ,1,INDQQ,NBG,NZMA,ZMA,FACT,GEWI(I),HQ,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(CUB)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(1,1)
C
      FACT(1) =  ZRHO*(SPS2/(ZSR**2 *ZQ*ZT) * (SR2OGP + QOT**2 * SR4GGG)
     >        +  R4GPT2 * SPS2 * ZQ / ZT**3 )
C
      CALL FKUBL(MZ,MS,MANZ,1,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),HC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(QUA)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(1,2)
C
      FACT(1)  = - ZRHO * (0.,1.) * ZQ / ZT**3 * R4GPGT
C
      CALL FKUBL(MZ,MS,MANZ,1,INDCQ,NBG,NZMA,ZMA,FACT,GEWI(I),HC,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(CUB)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(2,1)
C
      FACT(1)  = + ZRHO * (0.,1.)* ZQ / ZT**3 * R4GPGT 
C
      CALL FKUBL(MZ,MS,MANZ,1,INDQC,NBG,NZMA,ZMA,FACT,GEWI(I),HQ,HC)
C
      IF(MS.EQ.MZ)  GOTO 100
C     ----------------------
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(QUA)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(2,2)
C
      FACT(1) = ZRHO * CONJG(R4GPSI) * ZQ / (SPS2*ZT**3)
C
      CALL FKUBL(MS,MZ,MANZ,1,INDQQ,NBG,NZMA,ZMA,FACT,GEWI(I),HQ,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(CUB)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(1,1)
C
      FACT(1) =  ZRHO*(SPS2/(ZSR**2 *ZQ*ZT) * 
     >                (CONJG(SR2OGP) + QOT**2 * CONJG(SR4GGG))
     >        +  CONJG(R4GPT2) * SPS2 * ZQ / ZT**3 )
C
      CALL FKUBL(MS,MZ,MANZ,1,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),HC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(QUA)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(1,2)
C
      FACT(1)  = - ZRHO * (0.,1.) * ZQ / ZT**3 * CONJG(R4GPGT)
C
      CALL FKUBL(MS,MZ,MANZ,1,INDCQ,NBG,NZMA,ZMA,FACT,GEWI(I),HC,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(CUB)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(2,1)
C
      FACT(1)  = + ZRHO * (0.,1.)* ZQ / ZT**3 * CONJG(R4GPGT)
C
      CALL FKUBL(MS,MZ,MANZ,1,INDQC,NBG,NZMA,ZMA,FACT,GEWI(I),HQ,HC)
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
      IF (NI.EQ.-1) THEN
C*********************** axis conditions for v1 ******************
C all m -> v1=0.          , abs(m) =< 1 -> dv1 = 0.
C*****************************************************************
         DO 220 IM=1,MANZ
            I = 2*(IM-1) + 1
            DO 225 J=1,NZMA
              ZMA((I-1)*NZMA+J) = (0.0,0.0)
              ZMA(I+(J-1)*NZMA) = (0.0,0.0)
  225       CONTINUE
            ZMA((I-1)*NZMA+I) = (1.0,0.0)
            IF (ABS(RFOUR(IM)).GT.1.1) THEN
              I = 2*(IM-1) + 2
              DO 226 J=1,NZMA
                ZMA((I-1)*NZMA+J) = (0.0,0.0)
                ZMA(I+(J-1)*NZMA) = (0.0,0.0)
  226         CONTINUE
              ZMA((I-1)*NZMA+I) = (1.0,0.0)
            ENDIF
  220    CONTINUE
C*********************** axis conditions for v2 **********************
C -all m (except(m=1) -> v2=0. 
C -also the not-used component of the quad fem on axis
C*********************************************************************
         DO 230 IM=1,MANZ
            I = 2*MANZ + 2*(IM-1) + 1
            DO 235 J=1,NZMA
              ZMA((I-1)*NZMA+J) = (0.0,0.0)
              ZMA(I+(J-1)*NZMA) = (0.0,0.0)
  235       CONTINUE
            ZMA((I-1)*NZMA+I) = (1.0,0.0)
            IF (ABS(RFOUR(IM)).GT.1.1) THEN
              I = 2*MANZ + 2*(IM-1) + 2
              DO 236 J=1,NZMA
                ZMA((I-1)*NZMA+J) = (0.0,0.0)
                ZMA(I+(J-1)*NZMA) = (0.0,0.0)
  236         CONTINUE
              ZMA((I-1)*NZMA+I) = (1.0,0.0)
            ENDIF
  230    CONTINUE
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
     >         INDCC(1),INDCQ(1),INDQC(1),INDQQ(1),IDCDC(1),
     >         IDCQ(1),IQDC(1),IDCC(1),ICDC(1)
      REAL     SL,SU,ZDIF,ZSR,ZRHO,DZRHO,ZQ,DZQ,MSNQ,MZNQ,FKDUMMY,
     >         ZT,DZT,ZETA,DZETA,T0,DT0,QOT,TOQ,T2OQ,DOQDOT,SPS2,
     >         ZBIG, HC(4),HQ(4),DHC(4),DHQ(4),DUMMY(3)
      COMPLEX  R2,GPSI,GPGT,R2GPSI,R2GPGT,SOGPSI,
     >         SOR2GP,SGGG,SR2GGG,DSR2,DSGPSI,
     >         DTR2I, DTR2OI, DTGPSII, DTGPSIOI, DTGPGT, DTSGGGOI,
     >         DTR2IM,DTR2OIM,DTGPSIIM,DTGPSOIM,
     >         DTGPGTM,DTGGGOIM,
     >         ZMA(NZMA*NZMA),FACT(12)
C
      DATA INDCC / 1 /
      DATA INDCQ / 3 /
      DATA INDQC / 2 /
      DATA INDQQ / 4 /
      DATA IDCDC / 1 /
      DATA IDCQ  / 3 /
      DATA IQDC  / 2 /
      DATA IDCC  / 1 /
      DATA ICDC  / 1 /
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
      ZRHO    = RHO((NI-1)*4+I)
      DZRHO   = DRHO((NI-1)*4+I)
      T0      = ZT0((NI-1)*4+I)
      DT0     = ZDT0((NI-1)*4+I)
 
      QOT     = ZQ/ZT
      TOQ     = ZT/ZQ
      T2OQ    = ZT**2/ZQ
      DOQDOT  = DZQ/ZQ-DZT/ZT
      SPS2    = 2.*ZSR*CPSURF
      FKDUMMY =  1.0
C
      CALL CUBFCT(ZSR,SL,SU,HC)
      CALL QUAFCT(ZSR,SL,SU,HQ)
      CALL DCUBF (ZSR,SL,SU,DHC)
      CALL DQUAF (ZSR,SL,SU,DHQ)
C
      DO 200  KF = 1, LANZ
C     --------------------
C
        K = (KF-1) * MDIF + 1
C
        R2     = CMPLX(SPWERT(NPSI,ZSR,RR2(1,K),RR2(NP1,K),
     >                        RR2(N2P1,K),RR2(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IR2(1,K),IR2(NP1,K),
     >                        IR2(N2P1,K),IR2(N3P1,K),CS,DUMMY))
c      DO NP1=1,100
c      PRINT*,'RR2(NP1,1)=', RR2(NP1,1)
c      PRINT*,'IR2(NP1,1)=', IR2(NP1,1)
c      ENDDO
C
        GPSI   = CMPLX(SPWERT(NPSI,ZSR,RGPSI(1,K),RGPSI(NP1,K),
     >                        RGPSI(N2P1,K),RGPSI(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IGPSI(1,K),IGPSI(NP1,K),
     >                        IGPSI(N2P1,K),IGPSI(N3P1,K),CS,DUMMY))
C
        GPGT   = CMPLX(SPWERT(NPSI,ZSR,RGPGT(1,K),RGPGT(NP1,K),
     >                        RGPGT(N2P1,K),RGPGT(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IGPGT(1,K),IGPGT(NP1,K),
     >                        IGPGT(N2P1,K),IGPGT(N3P1,K),CS,DUMMY))
C
        DSR2   = CMPLX(SPWERT(NPSI,ZSR,RDSR2(1,K),RDSR2(NP1,K),
     >                        RDSR2(N2P1,K),RDSR2(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IDSR2(1,K),IDSR2(NP1,K),
     >                        IDSR2(N2P1,K),IDSR2(N3P1,K),CS,DUMMY))
C
        DSGPSI = CMPLX(SPWERT(NPSI,ZSR,RDSGPSI(1,K),RDSGPSI(NP1,K),
     >                        RDSGPSI(N2P1,K),RDSGPSI(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IDSGPSI(1,K),IDSGPSI(NP1,K),
     >                        IDSGPSI(N2P1,K),IDSGPSI(N3P1,K),CS,DUMMY))
C
        SOR2GP = CMPLX(SPWERT(NPSI,ZSR,RSOR2GP(1,K),RSOR2GP(NP1,K),
     >                        RSOR2GP(N2P1,K),RSOR2GP(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,ISOR2GP(1,K),ISOR2GP(NP1,K),
     >                        ISOR2GP(N2P1,K),ISOR2GP(N3P1,K),CS,DUMMY))
C
        SGGG   = CMPLX(SPWERT(NPSI,ZSR,RSGGG(1,K),RSGGG(NP1,K),
     >                        RSGGG(N2P1,K),RSGGG(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,ISGGG(1,K),ISGGG(NP1,K),
     >                        ISGGG(N2P1,K),ISGGG(N3P1,K),CS,DUMMY))
C
C	
      DO 100  MS = 1 , MANZ - KF + 1
C     ------------------------------
C
      MZ = MS + KF - 1
C
C  
      SMZ  = RFOUR(MZ)
      SMS  = RFOUR(MS)
      MZNQ = SMZ+ZNKWEL*ZQ
      MSNQ = SMS+ZNKWEL*ZQ
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(CUB)   FUER DIE GLEICHUNGEN
C     ----------------------
C     A(1,1)
C      v1A2
C
c-------------------------------------------------------------------------
c      FACT(1) = - DZQ**2 / (SPS2*ZT*ZQ**3) * GPSI
c     >        - (0.,1.)*(SMZ-SMS)*DZQ/(ZT*ZQ*ZQ) * GPGT
c     >        - (DSR2 - DZQ*R2/ZQ - DZT*R2/ZT )
c     >                                        *DT0 /(SPS2*ZT*ZQ)
c     >        - SPS2*MZNQ*MSNQ/(ZT*ZQ*ZSR*ZSR) 
c     >                         *(SGGG+TOQ**2 * SOR2GP)
c     >        + DZT * DZQ / (SPS2*ZQ*ZQ) * FKDUMMY 
c-------------------------------------------------------------------------
      FACT(1) = - 2.* DZQ**2 / (SPS2*ZT*ZQ**3) * GPSI
     >          + DZQ*DZT /(SPS2*ZT*ZT*ZQ*ZQ)  * GPSI
     >          - 2.*(0.,1.)*(SMZ-SMS)*DZQ/(ZT*ZQ*ZQ) * GPGT
     >          - (DSR2 - DZT*R2/ZT )*DT0 /(SPS2*ZT*ZQ)
     >          - SPS2*MZNQ*MSNQ/(ZT*ZQ*ZSR*ZSR) 
     >                                *(SGGG+TOQ**2 * SOR2GP)
     >          - DZQ / (SPS2*ZT*ZQ*ZQ) * DSGPSI 
C
      CALL FKUBL(MZ,MS,MANZ,1,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),HC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     ----------------------
C     A(1,2)
C      v1A1
C
c-----------------------------------------------------------------------
c      FACT(1) = + (0.,1.)*ZNKWEL*MZNQ/ZT * GPGT
c     >          + ZNKWEL/(SPS2*ZQ*ZT) * DZQ * GPSI
c     >          - MSNQ/(SPS2*ZQ)*DZT * FKDUMMY
c     >          - ZNKWEL/(SPS2*ZT) * DT0 * R2 
c-----------------------------------------------------------------------
      FACT(1) = (0.,1.)*ZNKWEL*(2*SMZ-SMS+ZNKWEL*ZQ)/ZT * GPGT
     >        - ZNKWEL*(ZQ*DZT-2*ZT*DZQ)/(SPS2*ZT*ZT*ZQ) * GPSI  
     >        + ZNKWEL/(SPS2*ZT) * DSGPSI
     >        - SMZ * DZT / (SPS2*ZQ) * FKDUMMY
C
      CALL FKUBL(MZ,MS,MANZ,1,INDCQ,NBG,NZMA,ZMA,FACT,GEWI(I),HC,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(CUB)   FUER DIE GLEICHUNGEN
C     ----------------------
C     A(2,1)
C      v2A2
C
c-----------------------------------------------------------------------
c      FACT(1) = - (0.,1.)*ZNKWEL*MSNQ/ZT    * GPGT
c     >          + ZNKWEL/(SPS2*ZQ*ZT) * DZQ * GPSI
c     >          - MSNQ/(SPS2*ZQ)*DZT * FKDUMMY
c     >          - ZNKWEL/(SPS2*ZT) * DT0    * R2
c------------------------------------------------------------------------
      FACT(1) = - (0.,1.)*MSNQ*(SMS-SMZ+ZNKWEL*ZQ)/(ZQ*ZT) * GPGT
     >          + (SMS+2*ZNKWEL*ZQ)/(SPS2*ZQ*ZQ*ZT) * DZQ  * GPSI
     >          - MSNQ * DZT/(SPS2*ZT*ZT*ZQ) * GPSI
     >          + MSNQ/(SPS2*ZT*ZQ)          * DSGPSI
     >          + SMS/(SPS2*ZT*ZQ) * DT0     * R2 
C     
      CALL FKUBL(MZ,MS,MANZ,1,INDQC,NBG,NZMA,ZMA,FACT,GEWI(I),HQ,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     ----------------------
C     A(2,2)
C      v2A1    
C
      FACT(1) = - ZNKWEL**2 * ZQ/(SPS2*ZT) * GPSI 
     >          - SMS*SMS* ZT/(SPS2*ZQ) * FKDUMMY 
C
      CALL FKUBL(MZ,MS,MANZ,1,INDQQ,NBG,NZMA,ZMA,FACT,GEWI(I),HQ,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H'(CUB)   FUER DIE GLEICHUNG
C     ------------------------
C     A(1',1')
C     dv1dA2
C
      FACT(1) = - ZT/(SPS2*ZQ) * FKDUMMY - GPSI / (SPS2*ZT*ZQ)
C
      CALL FKUBL(MZ,MS,MANZ,1,IDCDC,NBG,NZMA,ZMA,FACT,GEWI(I),DHC,DHC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(1',2)
C     dv1A1
C
      FACT(1) = -ZNKWEL/(SPS2*ZT) * GPSI + SMS*ZT/(SPS2*ZQ)*FKDUMMY
C
      CALL FKUBL(MZ,MS,MANZ,1,IDCQ,NBG,NZMA,ZMA,FACT,GEWI(I),DHC,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H'(CUB)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(2,1')
C     v2dA2
C
      FACT(1) = - ZNKWEL/(SPS2*ZT) * GPSI + SMS*ZT/(SPS2*ZQ)*FKDUMMY
C
      CALL FKUBL(MZ,MS,MANZ,1,IQDC,NBG,NZMA,ZMA,FACT,GEWI(I),HQ,DHC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H(CUB)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(1',1)
C     dv1A2
C
      FACT(1) = - (0.,1.)*MSNQ/(ZT*ZQ) * GPGT
     >          + DZQ/(SPS2*ZT*ZQ*ZQ)  * GPSI 
     >          - DT0/(SPS2*ZQ*ZT)     * R2 
C
      CALL FKUBL(MZ,MS,MANZ,1,IDCC,NBG,NZMA,ZMA,FACT,GEWI(I),DHC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H'(CUB)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(1,1')
C     v1dA2
C
c---------------------------------------------------------------------
c      FACT(1) = + (0.,1.)*MZNQ/(ZT*ZQ) * GPGT
c     >          + DZQ/(SPS2*ZT*ZQ*ZQ)  * GPSI 
c     >          - DT0/(SPS2*ZQ*ZT)     * R2 
c---------------------------------------------------------------------     
      FACT(1) = (0.,1.)*(2*SMZ-SMS+ZNKWEL*ZQ)/(ZT*ZQ)  * GPGT
     >        + (2*ZT*DZQ - ZQ*DZT)/(SPS2*ZT*ZT*ZQ*ZQ) * GPSI
     >        + 1./(SPS2*ZT*ZQ) * DSGPSI
     >        + 1./(SPS2*ZQ) * DZT * FKDUMMY 
C
      CALL FKUBL(MZ,MS,MANZ,1,ICDC,NBG,NZMA,ZMA,FACT,GEWI(I),HC,DHC)
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
C     A(1,1)
C      v1A2
C
c--------------------------------------------------------------------
c      FACT(1) = - DZQ**2 / (SPS2*ZT*ZQ**3) * CONJG(GPSI)
c     >        - (0.,1.)*(SMS-SMZ)*DZQ/(ZT*ZQ*ZQ) * CONJG(GPGT)
c     >        - (CONJG(DSR2) - DZQ*CONJG(R2)/ZQ - DZT*CONJG(R2)/ZT )
c     >                                        *DT0 /(SPS2*ZT*ZQ)
c     >        - SPS2*MZNQ*MSNQ/(ZT*ZQ*ZSR*ZSR) 
c     >                         *(CONJG(SGGG)+TOQ**2 * CONJG(SOR2GP))
c     >        + DZT * DZQ / (SPS2*ZQ*ZQ) * FKDUMMY
c---------------------------------------------------------------------
      FACT(1) = - 2.* DZQ**2 / (SPS2*ZT*ZQ**3) * conjg(GPSI)
     >          + DZQ*DZT /(SPS2*ZT*ZT*ZQ*ZQ)  * conjg(GPSI)
     >          - 2.*(0.,1.)*(SMS-SMZ)*DZQ/(ZT*ZQ*ZQ) * conjg(GPGT)
     >          - (conjg(DSR2) - DZT*conjg(R2)/ZT )*DT0 /(SPS2*ZT*ZQ)
     >          - SPS2*MZNQ*MSNQ/(ZT*ZQ*ZSR*ZSR) 
     >                        *(conjg(SGGG)+TOQ**2 * conjg(SOR2GP))
     >          - DZQ / (SPS2*ZT*ZQ*ZQ) * conjg(DSGPSI) 
C
C
      CALL FKUBL(MS,MZ,MANZ,1,INDCC,NBG,NZMA,ZMA,FACT,GEWI(I),HC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     ----------------------
C     A(1,2)
C      v1A1
C
C----------------------------------------------------------------------
c      FACT(1) = + (0.,1.)*ZNKWEL*MSNQ/ZT    * CONJG(GPGT)
c     >          + ZNKWEL/(SPS2*ZQ*ZT) * DZQ * CONJG(GPSI)
c     >          - MZNQ/(SPS2*ZQ)*DZT * FKDUMMY
c     >          - ZNKWEL/(SPS2*ZT) * DT0    * CONJG(R2) 
C----------------------------------------------------------------------
      FACT(1) = (0.,1.)*ZNKWEL*(2*SMS-SMZ+ZNKWEL*ZQ)/ZT  *CONJG(GPGT)
     >        - ZNKWEL*(ZQ*DZT-2*ZT*DZQ)/(SPS2*ZT*ZT*ZQ) *CONJG(GPSI)  
     >        + ZNKWEL/(SPS2*ZT) * CONJG(DSGPSI)
     >        - SMZ * DZT / (SPS2*ZQ) * FKDUMMY
C
      CALL FKUBL(MS,MZ,MANZ,1,INDCQ,NBG,NZMA,ZMA,FACT,GEWI(I),HC,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(CUB)   FUER DIE GLEICHUNGEN
C     ----------------------
C     A(2,1)
C      v2A2
C
C---------------------------------------------------------------------
c      FACT(1) = - (0.,1.)*ZNKWEL*MZNQ/ZT    * CONJG(GPGT)
c     >          + ZNKWEL/(SPS2*ZQ*ZT) * DZQ * CONJG(GPSI)
c     >          - MZNQ/(SPS2*ZQ)*DZT * FKDUMMY
c     >          - ZNKWEL/(SPS2*ZT) * DT0    * CONJG(R2) 
C---------------------------------------------------------------------
      FACT(1) =-(0.,1.)*MZNQ*(SMZ-SMS+ZNKWEL*ZQ)/(ZQ*ZT)*conjg(GPGT)
     >         + (SMZ+2*ZNKWEL*ZQ)/(SPS2*ZQ*ZQ*ZT) * DZQ*conjg(GPSI)
     >         - MZNQ * DZT/(SPS2*ZT*ZT*ZQ) * conjg(GPSI)
     >         + MZNQ/(SPS2*ZT*ZQ)          * conjg(DSGPSI)
     >         + SMZ/(SPS2*ZT*ZQ) * DT0     * conjg(R2) 
C     
      CALL FKUBL(MS,MZ,MANZ,1,INDQC,NBG,NZMA,ZMA,FACT,GEWI(I),HQ,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     ----------------------
C     A(2,2)
C      v2A1    
C
      FACT(1) = - ZNKWEL**2 * ZQ/(SPS2*ZT) * CONJG(GPSI) 
     >          - SMZ*SMZ* ZT/(SPS2*ZQ) * FKDUMMY 
C
      CALL FKUBL(MS,MZ,MANZ,1,INDQQ,NBG,NZMA,ZMA,FACT,GEWI(I),HQ,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H'(CUB)   FUER DIE GLEICHUNG
C     ------------------------
C     A(1',1')
C     dv1dA2
C
      FACT(1) = - ZT/(SPS2*ZQ) * FKDUMMY - CONJG(GPSI) / (SPS2*ZT*ZQ)
C
      CALL FKUBL(MS,MZ,MANZ,1,IDCDC,NBG,NZMA,ZMA,FACT,GEWI(I),DHC,DHC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(1',2)
C     dv1A1
C
      FACT(1) = -ZNKWEL/(SPS2*ZT) * CONJG(GPSI)
     >        + SMZ*ZT/(SPS2*ZQ)*FKDUMMY
C
      CALL FKUBL(MS,MZ,MANZ,1,IDCQ,NBG,NZMA,ZMA,FACT,GEWI(I),DHC,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H'(CUB)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(2,1')
C     v2dA2
C
      FACT(1) = - ZNKWEL/(SPS2*ZT) * CONJG(GPSI)
     >        + SMZ*ZT/(SPS2*ZQ)*FKDUMMY
C
      CALL FKUBL(MS,MZ,MANZ,1,IQDC,NBG,NZMA,ZMA,FACT,GEWI(I),HQ,DHC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H(CUB)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(1',1)
C     dv1A2
C
      FACT(1) = - (0.,1.)*MZNQ/(ZT*ZQ) * CONJG(GPGT)
     >          + DZQ/(SPS2*ZT*ZQ*ZQ)  * CONJG(GPSI) 
     >          - DT0/(SPS2*ZQ*ZT)     * CONJG(R2) 
C
      CALL FKUBL(MS,MZ,MANZ,1,IDCC,NBG,NZMA,ZMA,FACT,GEWI(I),DHC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H'(CUB)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(1,1')
C     v1dA2
C
C---------------------------------------------------------------------
c      FACT(1) = + (0.,1.)*MSNQ/(ZT*ZQ) * CONJG(GPGT)
c     >          + DZQ/(SPS2*ZT*ZQ*ZQ)  * CONJG(GPSI) 
c     >          - DT0/(SPS2*ZQ*ZT)     * CONJG(R2) 
C---------------------------------------------------------------------     
      FACT(1) = (0.,1.)*(2*SMS-SMZ+ZNKWEL*ZQ)/(ZT*ZQ) *CONJG(GPGT)
     >        + (2*ZT*DZQ - ZQ*DZT)/(SPS2*ZT*ZT*ZQ*ZQ)*CONJG(GPSI)
     >        + 1./(SPS2*ZT*ZQ) * CONJG(DSGPSI)
     >        + 1./(SPS2*ZQ) * DZT * FKDUMMY 
C
      CALL FKUBL(MS,MZ,MANZ,1,ICDC,NBG,NZMA,ZMA,FACT,GEWI(I),HC,DHC)
C
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
      IF (NI.EQ.1) THEN
C*********************** axis conditions for v1 ******************
C all m -> v1=0.          , abs(m) =< 1 -> dv1 = 0.
C*****************************************************************
         DO 220 IM=1,MANZ
            I = 2*(IM-1) + 1
            DO 225 J=1,NZMA
              ZMA((I-1)*NZMA+J) = (0.0,0.0)
              ZMA(I+(J-1)*NZMA) = (0.0,0.0)
  225       CONTINUE
            ZMA((I-1)*NZMA+I) = ZBIG
            IF (ABS(RFOUR(IM)).GT.1.1) THEN
              I = 2*(IM-1) + 2
              DO 226 J=1,NZMA
                ZMA((I-1)*NZMA+J) = (0.0,0.0)
                ZMA(I+(J-1)*NZMA) = (0.0,0.0)
  226         CONTINUE
              ZMA((I-1)*NZMA+I) = ZBIG
            ENDIF
  220    CONTINUE
C*********************** axis conditions for v2 **********************
C -all m (except(m=1) -> v2=0. 
C -also the not-used component of the quad fem on axis
C*********************************************************************
         DO 230 IM=1,MANZ
            I = 2*MANZ + 2*(IM-1) + 1
            DO 235 J=1,NZMA
              ZMA((I-1)*NZMA+J) = (0.0,0.0)
              ZMA(I+(J-1)*NZMA) = (0.0,0.0)
  235       CONTINUE
            ZMA((I-1)*NZMA+I) = ZBIG
            IF (ABS(RFOUR(IM)).GT.1.1) THEN
              I = 2*MANZ + 2*(IM-1) + 2
              DO 236 J=1,NZMA
                ZMA((I-1)*NZMA+J) = (0.0,0.0)
                ZMA(I+(J-1)*NZMA) = (0.0,0.0)
  236         CONTINUE
              ZMA((I-1)*NZMA+I) = ZBIG
            ENDIF
  230    CONTINUE
      ENDIF
C
C ... RANDBEDINGUNG FUER  S = 1 ...
C
      IF(RWALL.GT.1.AND.NI.EQ.NGINT) THEN
         CALL ADDBND(ZMA)
      ELSEIF(NI.EQ.NGINT) THEN
C
C V1 AT PLASMA BOUDNARY SET TO ZERO
C
         DO 250 I=NBG+1,NBG+2*MANZ,2
            DO 240 J=1,NZMA
               ZMA((I-1)*NZMA+J) = (0.0,0.0)
               ZMA(I+(J-1)*NZMA) = (0.0,0.0)
  240       CONTINUE
            ZMA((I-1)*NZMA+I) = ZBIG
  250    CONTINUE
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
C
      INTEGER  INDHG(*), NGL, NBG, NZMA, IND,INDO,INDU,INDN,MZ,MS,IANZ,L
      COMPLEX  ZMA(*), FACT(*)
      REAL     H1(*), H2(*), GEW
C
      NGL=NBG/(2*L)
C
      DO 5 J=1,IANZ
         FACT(J)=FACT(J)*GEW
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
         ZMA(IND)         = ZMA(IND) + H1(2) * FACT(I) * H2(2)
         ZMA(IND+NZMA)    = ZMA(IND+NZMA) + H1(2) * FACT(I) * H2(4)
         ZMA(IND+1)       = ZMA(IND+1) + H1(4) * FACT(I) * H2(2)
         ZMA(IND+1+NZMA)  = ZMA(IND+1+NZMA) + H1(4) * FACT(I) * H2(4)
C
         ZMA(INDO)        = ZMA(INDO) + H1(2) * FACT(I) * H2(1)
         ZMA(INDO+NZMA)   = ZMA(INDO+NZMA) + H1(2) * FACT(I) * H2(3)
         ZMA(INDO+1)      = ZMA(INDO+1)+ H1(4) * FACT(I) * H2(1)
         ZMA(INDO+1+NZMA) = ZMA(INDO+1+NZMA) + H1(4) * FACT(I) * H2(3)
C
         ZMA(INDU)        = ZMA(INDU) + H1(1) * FACT(I) * H2(2)
         ZMA(INDU+NZMA)   = ZMA(INDU+NZMA) + H1(1) * FACT(I) * H2(4)
         ZMA(INDU+1)      = ZMA(INDU+1) + H1(3) * FACT(I) * H2(2)
         ZMA(INDU+1+NZMA) = ZMA(INDU+1+NZMA)+ H1(3) * FACT(I) * H2(4)
C
         ZMA(INDN)        = ZMA(INDN) + H1(1) * FACT(I) * H2(1)
         ZMA(INDN+NZMA)   = ZMA(INDN+NZMA) + H1(1) * FACT(I) * H2(3)
         ZMA(INDN+1)      = ZMA(INDN+1) + H1(3) * FACT(I) * H2(1)
         ZMA(INDN+1+NZMA) = ZMA(INDN+1+NZMA) + H1(3) * FACT(I) * H2(3)
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
*CALL COMEQUI
*CALL COMIOD
*CALL COMSPL
*CALL COMVRSP
C
      COMPLEX  ZMA(*),FACT(4),R2
      REAL     ZSR,ZRHO,ZQ,MSNQ,MZNQ,FKDUMMY,
     >         ZT,ZETA,T0,QOT,TOQ,T2OQ,DOQDOT,SPS2,
     >         DUMMY(3)
      INTEGER  NI,MS,MZ,I,K,
     >         INDBM(1)
C
      DATA INDBM /  1 /
C
      ZSR  = 1.
      ZQ   = Q1(NPSI)
      ZT   = RBP1(NPSI)
      ZETA = ETA
      ZRHO = 1.
      T0   = P1(NPSI)
C
      QOT  = ZQ/ZT
      TOQ  = ZT/ZQ
      SPS2 = 2.*CPSURF
C
      DO 20 MS = 1, MANZ
         SMS = RFOUR(MS)
         DO 10 MZ = 1, MANZ
            SMZ = RFOUR(MZ)
C-----------------------------------------------------------------------
C           A(1,1)      MOMENTUM EQ.
C-----------------------------------------------------------------------
            FACT(1)= -(0.,1.)*(SMZ+ZNKWEL*ZQ)*(SMS+ZNKWEL*ZQ)/ZNKWEL
     >              /ZQ**2  * B3B1(MS,MZ) 
            CALL FBOUND(MZ,MS,MANZ,1,INDBM,NBG,NZMA,ZMA,FACT,0)
   10    CONTINUE
   20 CONTINUE
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
        DS2=(SU-SL)**2
        SM=(SU+SL)/2.
        H(1)= 4.*(S-SL)*(SU-S)/DS2
        H(2)= 0.0
        H(3)= 2.*(S-SM)*(S-SL)/DS2
        H(4)= 2.*(S-SM)*(S-SU)/DS2
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
        DS2=(SU-SL)**2
        H(1)= 4.*(-2.*S+SU+SL)/DS2
        H(2)= 0.0
        H(3)= (4.*S-SU-3.*SL)/DS2
        H(4)= (4.*S-SL-3.*SU)/DS2
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
*CALL COMMAX
*CALL COMPAR
*CALL COMPIO
*CALL COMDIM
*CALL CORE1
C
      INTEGER  IWORK(257)
      REAL     AMATR(NDIM1,NDIM1), AMATI(NDIM1,NDIM1), DUMMY(NDIM1)
      COMPLEX  HVEC(NDIM1)
C
      EQUIVALENCE (BMAT(1,1), AMATR(1,1))
      EQUIVALENCE (BMAT(1,NDIM1/2+1), AMATI(1,1))
      EQUIVALENCE (HVEC(1), WR(1))
      EQUIVALENCE (HVEC(NDIM1/2+1), WI(1))
C
C ... QR - ALGORITHM ...
C
      CALL CPOCO(BMAT,NDIM1,NDIM,RCOND,HVEC,INFO)
C
      WRITE(NOUT,1) RCOND
C
      IF(INFO.NE.0) THEN
         WRITE(NOUT,3)
         STOP
      ENDIF
C
      DO 10 K=1,NDIM
         CALL CPOSL(BMAT,NDIM1,NDIM,AMAT(1,K))
   10 CONTINUE
C
C
      DO 20 J=1,NDIM
      DO 20 I=1,NDIM
         AMATR(I,J)=REAL(AMAT(I,J))
         WRITE(*,*) AMATR(I,J)
   20 CONTINUE
      DO 30 J=1,NDIM
      DO 30 I=1,NDIM
         AMATI(I,J)=AIMAG(AMAT(I,J))
   30 CONTINUE
      CALL CBAL(NDIM1,NDIM,AMATR,AMATI,IS1,IS2,WI)
      CALL CORTH(NDIM1,NDIM,IS1,IS2,AMATR,AMATI,WR,WI)
      CALL COMQR(NDIM1,NDIM,IS1,IS2,AMATR,AMATI,WR,WI,IERR)
C
      IF(IERR.NE.0) THEN
         WRITE(NOUT,31) IERR
         STOP
      ENDIF
C
      DO 40 I=1,NDIM
         EVMAG(1,I) = CABS(CMPLX(WR(I),WI(I)))
C
C     TEST HORUS
C
C         INDEX(I) = I
   40 CONTINUE
C
*IF CRAY
      CALL ORDERS(2,IWORK,EVMAG,INDEX,NDIM,1,8,1)
*ELSE
C
C     TEST HORUS
C
C      CALL DSORTX(EVMAG,+1,NDIM,INDEX)
C      CALL DSVRGP(NDIM,EVMAG,DUMMY,INDEX)
*ENDIF
C
      WRITE(NOUT,41)(I,WR(INDEX(I)),WI(INDEX(I)),I=1,NDIM)
*IF IBM
      WRITE(NOUTI,43)(I,WR(INDEX(I)),WI(INDEX(I)),I=1,NDIM)
*ENDIF
C
      RETURN
C
    1 FORMAT('   KONDITION = ',1P,E16.6,0P)
    3 FORMAT(///5X,'MATRIX BMAT1 NICHT POSITIV DEFINIT')
   11 FORMAT('   TIME = ',1P,E16.6,0P)
   31 FORMAT(//' HQR/HQZ :  IERR=',I5)
   41 FORMAT(1X,I3,'.-EIGENWERT:',1P,2E16.6,0P)
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
**                     (icamax)                                      **
**                     (cscal)                                       **
**                     (caxpy)                                       **
**                   (cscal)                                         **
**                   CGBSL                                            **
**                     (caxpy)                                       **
**                     (ccopy)                                       **
**                     (cdotc)                                       **
**                   (cdotc)                                         **
**                   (ccopy)                                         **
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
      COMPLEX  cdotc, PRD1, PRD2
C
      DATA INC /1/
      DATA QQ /1.E-4/
C
C ... R(0) = B * X(0) ...
C
      CALL MATVER(ZMA,NDIM,NZMA,NGINT,X0,X1,Y0,Y1)
      CALL CSHIFT(AMAT,LDA,NDIM,EWSHIFT,ZMA,NZMA)
C
C ... ABSCHAETZEN START-EIGENWERT ...
C
      EWALT = QQ * EWSHIFT
C
C
C ... ZERLEGEN AMAT : AMAT = L * R ...
C
      CALL CGBFA(AMAT,LDA,NDIM,ML,MU,IPVT,INFO)
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
      CALL cscal(NDIM,EWALT,X1,INC)
      CALL cscal(NDIM,CONJG(EWALT),Y1,INC)
C
C ... LOESUNG GL.SYST  ( A - LMBDA * B ) * X(I) = X(I-1) ...
C
C
      CALL CGBSL(AMAT,LDA,NDIM,ML,MU,IPVT,X1,0,X0)
      CALL CGBSL(AMAT,LDA,NDIM,ML,MU,IPVT,Y1,1,Y0)
C
C ... PRD1 = HERM(Y) * L * R * X ...
C
      PRD1 = cdotc(NDIM,Y0,INC,X0,INC)
C
C ... KOPIEREN ITERIERTE WERTE ...
C
      CALL ccopy(NDIM,X1,INC,X0,INC)
      CALL ccopy(NDIM,Y1,INC,Y0,INC)
C
C ... R(I) = B * X(I) ...
C
      CALL MATVER(ZMA,NDIM,NZMA,NGINT,X0,X1,Y0,Y1)
C
C ... PRD2 = HERM(Y) * B * X ...
C
      PRD2 = cdotc(NDIM,Y0,INC,X1,INC)
      WRITE(NOUT,11) PRD1,PRD2
C
C
      EWNEU = PRD1/PRD2
C
      WRITE(NOUT,13) IT
      EW = EWSHIFT + EWNEU

      WRITE(NOUT,15) EW
C
C ... RELATIVE AENDERUNG DES EIGENWERTES ...
C
      DE = ABS(CABS(EWNEU/EWALT)-1.0)
      WRITE(NOUT,17) EWNEU,DE
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
   11 FORMAT(///' PRD1 = HERM(Y) * L * R * X  PRD2 = HERM(Y) * B * X',
     >       1P,2E12.4,2X,2E12.4,0P)
   13 FORMAT(' ITERATION =',I5)
   15 FORMAT(' EIGENWERT =',1P,2E14.5)
   17 FORMAT(' DELT(EW) =',1P,E21.11,E20.11,/' DE =',E20.11,0P)
   21 FORMAT(' NACH',I4,' ITERATIONEN ABGEBROCHEN')
   31 FORMAT(' EIGENVEKTOR:'/(5(1X,1P,2E12.4,0P)))
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
C     BLAS caxpy=C(Z)AXPY,cscal=C(Z)SCAL,icamax=IC(Z)AMAX
C     FORTRAN MAX0,MIN0,ABS,AIMAG,REAL
C-----------------------------------------------------------------------
C
      INTEGER  I, icamax, I0, J, JU, JZ, J0, J1, K, KP1, L, LM,M,MM,NM1
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
         L = icamax(LM+1,ABD(M,K),1) + M - 1
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
         CALL cscal(LM,T,ABD(M+1,K),1)
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
               CALL caxpy(LM,T,ABD(M+1,K),1,ABD(MM+1,J),1)
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
C     BLAS caxpy=C(Z)AXPY,cdotc=C(Z)DOTC
C     FORTRAN MIN0,CONJG
C
C-----------------------------------------------------------------------
C
      INTEGER  LDA, N, ML, MU, IPVT(*), JOB
      INTEGER  K, KB, L, LA, LB, LM, M, NM1
      COMPLEX  ABD(LDA,*), B(*), Q(*)
      COMPLEX  cdotc, T
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
            CALL caxpy(LM,T,ABD(M+1,K),1,B(K+1),1)
   20    CONTINUE
   30 CONTINUE
      CALL ccopy(N,B,1,Q,1)
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
         T = cdotc(LM,ABD(LA,K),1,B(LB),1)
         B(K) = (B(K) - T)/CONJG(ABD(M,K))
   70 CONTINUE
      CALL ccopy(N,B,1,Q,1)
C
C ... NOW SOLVE HERM(L)*X = Y ...
C
      IF(ML.EQ.0) GOTO 100
      IF(NM1.LT.1) GOTO 100
         DO 90 KB=1,NM1
            K = N - KB
            LM = MIN0(ML,N-K)
            B(K) = B(K) + cdotc(LM,ABD(M+1,K),1,B(K+1),1)
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
*DECK SOLV4
      SUBROUTINE SOLV4(EWOUT,NVI)
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
**                       (cdotu)                                     **
**                     CGEFAP                                         **
**                       ICMAXP                                       **
**                   VITERIC                                          **
**                     (cscal)                                       **
**                     (cdotu)                                       **
**                     CGES2P                                         **
**                       (ccopy)                                     **
**                     (cdotc)                                       **
**                     (ccopy)                                       **
**                   RITERIC                                          **
**                     (cdotu)                                       **
**                     CGES2P                                         **
**                       (ccopy)                                     **
**                     (cdotc)                                       **
**                     (ccopy)                                       **
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
      IF(ABS(CABS(DLI/DLIM1)-1.0).LE.EPS ) GOTO 30
      GOTO 10
C
   20 WRITE(NOUT,21) IT-1
C
   30 CONTINUE
C
      EWOUT = EW
      NVI = IT-1
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
      COMPLEX  SUM, ZZ, cdotu
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
               SUM = cdotu(NBG,APR(K1,1,I),NBG,HVX(1,1),1)
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
      COMPLEX  SUM, cdotu, cdotc, QP, DLIALT
      COMPLEX  APR(NBG,NB3,*), X(NBG,NCV,*), HVX(NBG,*), HVX2(NBG,*)
C
      NBG1 = NBG+1
      QP = (0.0,0.0)
C
      DO 50 I=1,NG
C
         CALL cscal(NBG,DLIALT,X(1,I,1),1)
         CALL cscal(NBG,CONJG(DLIALT),X(1,I,2),1)
C
         IF(I.EQ.1) GOTO 20
         DO 10 K1=1,NBG
            SUM = cdotu(NBG,APR(K1,1,I),NBG,HVX(1,2),1)
            X(K1,I,1) = X(K1,I,1)-SUM
            X(K1,I,2) = X(K1,I,2)-HVX(K1,1)
   10    CONTINUE
   20    CALL CGES2P(APR(1,NBG1,I),NBG,NBG,X(1,I,1),
     >               X(1,I,2),IPVT(1,I),HVX(1,3),0)
C
         DO 30 K1=1,NBG
            SUM = cdotc(NBG,APR(1,K1+2*NBG,I),1,X(1,I,2),1)
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
         CALL ccopy(NBG,X(1,I,1),1,HVX(1,2),1)
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
      COMPLEX  SUM, cdotu, cdotc, YR
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
         SUM = cdotu(NBG,APR(IPVK1,2*NBG+1,I),NBG,HVX(1,2),1)
         X(K1,I,1) = X(K1,I,1)-SUM
         X(IPVK1,I,2) = X(IPVK1,I,2)-HVX(K1,1)
   10 CONTINUE
   20 CALL CGES2P(APR(1,N1,I),NBG,NBG,X(1,I,1),
     >            X(1,I,2),IPVT(1,I),HVX(1,2),1)
      DO 30 K1=1,NBG
         SUM = cdotc(NBG,APR(1,K1,I),1,X(1,I,2),1)
         HVX(K1,1) = SUM
   30 CONTINUE
      CALL ccopy(NBG,X(1,I,1),1,HVX(1,2),1)
      CALL ccopy(NBG,X(1,I,1),1,EV(1,I),1)
C
   40 CONTINUE
C     --------
C
      CALL ccopy(NBG,X(1,NG,1),1,HVX(1,1),1)
      CALL ccopy(NBG,X(1,NG,2),1,HVX(1,2),1)
C
      CALL CONBMAT(NGINT,NZMA,ZMA)
C
      DO 50 K=1,NBG
         X(K,NG,1) = cdotu(NBG,ZMA(NBG+K,N1),NZMA,HVX(1,1),1)
         X(K,NG,2) = cdotu(NBG,ZMA(NBG+K,N1),NZMA,HVX(1,2),1)
   50 CONTINUE
C
      DO 90 I=NGINT,1,-1
C     ----------------------
C
      DO 60 K=1,NBG
         X(K,I+1,1) =X(K,I+1,1)+cdotu(NBG,ZMA(NBG+K,1),NZMA,X(1,I,1),1)
         X(K,I+1,2) =X(K,I+1,2)+cdotu(NBG,ZMA(NBG+K,1),NZMA,X(1,I,2),1)
   60 CONTINUE
C
      YR = YR+cdotc(NBG,HVX(1,2),1,X(1,I+1,1),1)
C
      CALL ccopy(NBG,HVX(1,1),1,HVX2(1,1),1)
      CALL ccopy(NBG,HVX(1,2),1,HVX2(1,2),1)
      CALL ccopy(NBG,X(1,I,1),1,HVX(1,1),1)
      CALL ccopy(NBG,X(1,I,2),1,HVX(1,2),1)
C
      DO 70 K=1,NBG
         X(K,I,1) = cdotu(NBG,ZMA(K,1),NZMA,HVX(1,1),1)
     >            + cdotu(NBG,ZMA(K,N1),NZMA,HVX2(1,1),1)
         X(K,I,2) = cdotu(NBG,ZMA(K,1),NZMA,HVX(1,2),1)
     >            + cdotu(NBG,ZMA(K,N1),NZMA,HVX2(1,2),1)
   70 CONTINUE
C
      IF(I.GT.1) THEN
C
         CALL CONBMAT(I-1,NZMA,ZMA)
C
         DO 80 K=1,NBG
            X(K,I,1) = X(K,I,1) +
     >                 cdotu(NBG,ZMA(NBG+K,N1),NZMA,HVX(1,1),1)
            X(K,I,2) = X(K,I,2) +
     >                 cdotu(NBG,ZMA(NBG+K,N1),NZMA,HVX(1,2),1)
   80    CONTINUE
C
      ENDIF
C
   90 CONTINUE
C     --------
C
      YR = YR+cdotc(NBG,HVX(1,2),1,X(1,1,1),1)
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
      COMPLEX  cdotu, T
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
            T = cdotu(K-1,HVX,1,B(J,1),LDA)
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
      CALL ccopy(N,HVX,1,R,1)
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
      CALL ccopy(N,HVX,1,S,1)
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
**                               (cdotu)                             **
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
C     TEILSPECTRUM
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
     >        (LOR,1,1201,X,Y,L,1,'TEILSPECTRUM',12,XNAME,10,YNAME,10)
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
      write(22,12) ng,manz,ngl
      write(22,11) (rfour(m),m=1,manz)
      write(22,11) (sgrid(i),i=1,ng)
      do 5 i=1,ng
      do 5 j=1,nbg
        rev = real(ev(j,i))
        zev = imag(ev(j,i))
        if (abs(rev).lt.1.e-20) rev=0.
        if (abs(zev).lt.1.e-20) zev=0.
        ev(j,i) = rev + (0.,1.)*zev
    5 continue    
      do 10 i=1,ng
        write(22,11) (ev(j,i),j=1,nbg)
   10 continue
   11 format(4e16.8)
   12 format(3i8)


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
      INTEGER  NG, NPDIM, NBG, IEIG, IK(2)
      CHARACTER*3 KOMP(2)
      DATA IK  / 1, 2/
      DATA KOMP/'V1 ','V2 '/
C
      EXTERNAL QUAFCT, CUBFCT
C
      NP = 2 * NG - 1
      WRITE(NOUTE) LABEL(1:3), EQNAME
C
      DO 30 IEIG=1,2
         DO 10 IM=1,MANZ
         DO 10 I=1,NPDIM
            Y(I,1,IM) = 0.0
            Y(I,2,IM) = 0.0
   10    CONTINUE
C
         IF (IEIG.EQ.1) THEN
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
      MZ = MANZ          !      MZ = MIN0(MANZ,7)
C
      WRITE(YNAMER,'(''RE'',A5,22X)') YNAME
      WRITE(YNAMEI,'(''IM'',A5,22X)') YNAME
C
C ... REALTEIL ...
C
      IOMU = IPN + 1
      ILR = 2
C
      YMIN = 0.0
      YMAX = 0.0
      DO 20 I  = 1,MZ
      DO 20 J  = 1,NPOINT
         IF (Y(J,1,I).GT.YMAX) YMAX = Y(J,1,I)
         IF (Y(J,1,I).LT.YMIN) YMIN = Y(J,1,I)
   20 CONTINUE
      YMIN = YMIN * 1.1
      YMAX = YMAX * 1.1
C-----------------------------------------------------------------------
C PLOT THE EIGENFUNKTION 
C-----------------------------------------------------------------------
      CALL NFRAME(ILR,IOMU,1,XMIN,XMAX,YMIN,YMAX,YNAMER,30,'S',1,' ',1)
      DO 30 I = 1,MZ
         CALL LINCOL(INT(ABS(RFOUR(I)-1)))
         CALL LPLOT(ILR,IOMU,1,X,Y(1,1,I),-NPOINT,1,YNAMER,30,
     >              'S',1,' ',1)
   30 CONTINUE
      CALL LINCOL(0)
C
C----------------------------------------------------------------------
C IMAGINARY PART
C----------------------------------------------------------------------
      ILR = 3
C
      YMIN = 0.0
      YMAX = 0.0
      DO 50 I = 1,MZ
      DO 50 J = 1,NPOINT
         IF (Y(J,2,I).GT.YMAX) YMAX = Y(J,2,I)
         IF (Y(J,2,I).LT.YMIN) YMIN = Y(J,2,I)
   50 CONTINUE
      YMIN = YMIN * 1.1
      YMAX = YMAX * 1.1
C-----------------------------------------------------------------------
C  PLOT THE EIGENFUNCTION 
C-----------------------------------------------------------------------
      CALL NFRAME(ILR,IOMU,1,XMIN,XMAX,YMIN,YMAX,YNAMEI,30,'S',1,' ',1)
      DO 60 I = 1,MZ
         CALL LINCOL(INT(ABS(RFOUR(I)-2)))
         CALL LPLOT(ILR,IOMU,1,X,Y(1,2,I),-NPOINT,1,YNAMEI,30,
     >              'S',1,' ',1)
   60 CONTINUE
      CALL LINCOL(0)
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
      COMPLEX  cdotu,CSUM1,CSUM2,CSUM3,CSUM
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
            CSUM1 = cdotu(KANZ,BUFF(J,KA),      NBG,X(IXM1+KA),1)
            CSUM2 = cdotu(KANZ,BUFF(J,NBG+KA),  NBG,X(IX+KA),  1)
            CSUM3 = cdotu(KANZ,BUFF(J,2*NBG+KA),NBG,X(IXP1+KA),1)
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
            CSUM1 = cdotu(KANZ,BUFF(J,KA),    NBG,X(IXM1+KA),1)
            CSUM2 = cdotu(KANZ,BUFF(J,NBG+KA),NBG,X(IX+KA),  1)
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





