*COMDECK COMVER
      CHARACTER  VERSION*(*),   DD*(*)
      PARAMETER (VERSION = '11', DD = '26 APRIL 2006')
*COMDECK COMMAX
      PARAMETER (LANZ=11, MANZ=LANZ, MDIF=1, LMAX=32)
      PARAMETER (NGMAX=401)
      PARAMETER (NPSIMAX=201, NCHIMAX=513)
      PARAMETER (NPNC=NPSIMAX*NCHIMAX, NP4=4*NPSIMAX)
      PARAMETER (NVPSIMX=101, NVCHIMX=513)
      PARAMETER (NVPNVC=NVPSIMX*NVCHIMX,NVP4=4*NVPSIMX)
*COMDECK COMPAR
      PARAMETER (NGL=8, NBG=NGL*MANZ, NZMA=NBG, NB3=3*NBG)
      PARAMETER (KILWOR=3600000, NDIM1=NZMA)
      PARAMETER (NGPL1=4*(NGMAX-1)*NGL*MANZ)
*COMDECK COMPARE                                                              
      PARAMETER (MXNINT=1000)                                                 
      PARAMETER (NDEQ=MXNINT)    
*COMDECK COMPIO
      PARAMETER (NIN=10, NIN2=11, NOUT=20, NOUTI=21, NOUT2=21, NOUT3=23)
      PARAMETER (NOUTP=24, NOUTV=25, NOUTE=26, NOUTVB = 27, NOUTB=28)
      PARAMETER (NMAP=12)
      PARAMETER (ND3=15, ND4=16, ND5=17, ND6=18)
*COMDECK COMPCON
      REAL       PI, ZERO, ONE
      COMPLEX    ZEROC, ONEC, CHALF, CTWO
      PARAMETER (NMAX=201)
      PARAMETER (PI=3.141592653589793)
      PARAMETER (ZERO=0.E0, ONE=1.E0)
      PARAMETER (ZEROC=(0.E0,0.E0), ONEC=(1.E0,0.E0))
      PARAMETER (CHALF=(.5E0,0.E0), CTWO=(2.E0,0.E0))
C-----------------------------------------------------------------------
*COMDECK CORE1
      COMMON / CORE /
     C      AMAT(NZMA,NZMA), BMAT(NZMA,NZMA),
     C      WRI(NZMA),VRI(1,1), 
     R      SR(NGPL1), WIG(NGPL1),XV(NGPL1), YV(NGPL1)                   
 
      COMPLEX AMAT, BMAT, WRI, VRI
      REAL    WR, WI, SR, WIG              
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
      COMMON / COMPLOT /  XMIN, XMAX, YMIN, YMAX, NPLOT                       
      INTEGER             NPLOT                                               
      REAL                XMIN, XMAX, YMIN, YMAX                              
C-----------------------------------------------------------------------
*COMDECK COMGRID
      COMMON / COMGRID / SGRID(NGMAX), SBEG, SEND, NG
      REAL               SGRID, SBEG, SEND
      INTEGER            NG
C-----------------------------------------------------------------------
*COMDECK COMWEL
      COMMON / COMWEL  / RFOUR(MANZ), ZNKWEL, NTOR
      REAL               RFOUR, ZNKWEL
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
     R         DSURF2, DSURF3,
     I         IDPOW, IEQ, IAS
C
      REAL     GAMMA, ETA, PSIS, Q0ZYL, DSURF, DSURF1, ALPHIN
      REAL     DSURF2, DSURF3
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
     R         RAD(NPSIMAX),
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
     R         RR2(NP4,LANZ), RR4(NP4,LANZ), RGPSI(NP4,LANZ),
     R         RSOGPSI(NP4,LANZ), RGPGT(NP4,LANZ), RSGGG(NP4,LANZ),
     R         RSR2GGG(NP4,LANZ), RR2GPSI(NP4,LANZ), RSOR2GP(NP4,LANZ),
     R         RSR2OGP(NP4,LANZ), RR2GPGT(NP4,LANZ), RR4GPSI(NP4,LANZ),
     R         RR4GPGT(NP4,LANZ), RDSR2(NP4,LANZ), RDSGPSI(NP4,LANZ),
     R         RSR4GGG(NP4,LANZ),
     R         IR2(NP4,LANZ), IR4(NP4,LANZ), IGPSI(NP4,LANZ),
     R         ISOGPSI(NP4,LANZ), IGPGT(NP4,LANZ), ISGGG(NP4,LANZ),
     R         ISR2GGG(NP4,LANZ), IR2GPSI(NP4,LANZ), ISOR2GP(NP4,LANZ),
     R         ISR2OGP(NP4,LANZ), IR2GPGT(NP4,LANZ), IR4GPSI(NP4,LANZ),
     R         IR4GPGT(NP4,LANZ), IDSR2(NP4,LANZ), IDSGPSI(NP4,LANZ),
     R         ISR4GGG(NP4,LANZ),
     I         NP1, N2P1, N3P1
C
      REAL     RR2, RR4, RGPSI, RSOGPSI, RGPGT, RSGGG, RSR2GGG,
     >         RR2GPSI, RSOR2GP, RSR2OGP, RR2GPGT, RR4GPSI, RR4GPGT,
     >         RDSR2, RDSGPSI, RSR4GGG, IR2, IR4, IGPSI, ISOGPSI,
     >         IGPGT, ISGGG, ISR2GGG, IR2GPSI, ISOR2GP, ISR2OGP,
     >         IR2GPGT, IR4GPSI, IR4GPGT, IDSR2, IDSGPSI, ISR4GGG
      INTEGER  NP1, N2P1, N3P1
*COMDECK COMNIP                                                               
      COMMON  / COMNIP /  NIP                                                 
      INTEGER             NIP     
C
*DECK CSCAS
      PROGRAM CSCAS
C
************************************************************************
************************************************************************
**                                                                    **
**  M A S T E R F I L E  :  CASTOR-9D8                                **
**  ------------------------------                                    **
**                                                                    **
**  AUTHORS :         W. KERNER, J.P. GOEDBLOED,                      **
**                    G.T.A. HUYSMANS, S. POEDTS, E. SCHWARZ          **
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
*CALL COMPARE
*CALL COMPIO
*CALL COMMOD
*CALL COMDIM
*CALL COMLAB
*CALL COMINT
*CALL COMBX
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
*CALL CORE1
*CALL COMNIP
*CALL COMPLOT

C
      CHARACTER*11  TXTPL(1)
C
      DATA TXTPL /'QR         '/
C
      NAMELIST / NEWRUN / MODE, EQNAME, NLTORE, NG,
     >                    RFOUR, NTOR, ETA,
     >                    ASPECT, Q0ZYL, SBEG, SEND, GAMMA,
     >                    NVPSI, NGV, SIGV, DSURF, IDPOW, NDIAGFK,
     >                    VSHIFT, NSHIFT, NRS, NIS, DRS, DIS, EPS,
     >                    NPLOT, XMIN, YMIN, XMAX, YMAX,
     >                    DSURF1,ALPHIN,IEQ,IAS,
     >                    DSURF2, DSURF3
C
      CALL PRESET
C
      WRITE(EQNAME,'(A10)') ' '
      LABEL(1:3) = VERSION
C
C     OPENING PLOT FILE :
C     =================
      CALL BEGPLT('CASPLOT')
C
      IBEGIN = 0
   10 READ(NIN,NEWRUN)
      IF(IBEGIN.NE.0) WRITE(NOUT,'(''1'')')
      IBEGIN = 1 
      IF(MODE.EQ.0) GOTO 1000
      IF(NG.GT.NGMAX) STOP 'NG > NGMAX'
C
      RFOUR1 = RFOUR(1)
      DO 20 JJ=1,MANZ
         RFOUR(JJ) = RFOUR1 + FLOAT((JJ-1)*MDIF)
   20 CONTINUE
C
      NDIM = NZMA
      ZNKWEL = FLOAT(NTOR)
      IF(.NOT.NLTORE) ZNKWEL = ZNKWEL / ASPECT
C
      WRITE(NOUT,53) NDIM
      WRITE(NOUT,55) NG
      WRITE(NOUT,57) NZMA
      WRITE(NOUT,59) ZNKWEL
C------------------------------------------------------------------------
C READ EQUILIBRIUM  FROM DISK
C------------------------------------------------------------------------
      CALL EQUIL
C-----------------------------------------------------------------------
C QR ALGORITHM
C-----------------------------------------------------------------------
      WRITE(NOUT,101)
C
      DO 300 NI=1,NG                                                          
         NIP = NI                                                             
         CALL MAT1                                                            
         CALL SOLV1                                                           
  300 CONTINUE           
      NDIMP=NG*NBG                                                        
      CALL QRPLOT(SR,WIG,NDIMP,XV,YV)
C
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
   53 FORMAT(/5X,'DIMENSION OF THE A-MATRIX :    NDIM  = ',I8)
   55 FORMAT(5X,'NUMBER OF GRID POINTS      :    NGINT = ',I8)
   57 FORMAT(5X,'DIMENSION OF ZMA SUBBLOCK  :    NZMA  = ',I4/)
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
*CALL COMGRID
*CALL COMWEL
*CALL COMGEO
*CALL COMEQUI
*CALL COMGEW
*CALL COMMEW
*CALL COMIOD
*CALL COMPLOT

C----------------------------------------------------------------------
C VARIABLES FOR PLOTTING 
C----------------------------------------------------------------------
      NPLOT     =    1
      XMIN      =    0.
      XMAX      =    1.
      YMIN      =    0.
      YMAX      =    1.
C-----------------------------------------------------------------------
C PHYSICAL VARIABLES 
C-----------------------------------------------------------------------
      RFOUR(1)  =  0.
      DO 20 I= 2,MANZ
   20 RFOUR(I)  =  0.
      NLTORE    = .TRUE.
      NTOR      = -1
      Q0ZYL     =  0.3
      ASPECT    =  1.
      DSURF     =  0.
      DSURF1    =  0.
      DSURF2    =  0.
      DSURF3    =  0.
      ALPHIN    =  1.
      IEQ       =  2
      IAS       =  0
      IDPOW     =  1
      RMIN      = 1.
C--------------------------------------------------------------------------
C NUMERICAL VARIABLES 
C--------------------------------------------------------------------------
      NG        =  11
      ITER      =  20
      EPS       = 1.E-6
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
*CALL COMPIO
*CALL COMGRID
*CALL COMIOD
C-------------------------------------------- READ EQUILIBRIUM FROM DISK 
      CALL IODSK
C------------------------------------------------- GRID (S - COORDINATES) 
      XWALL=1.0
      CALL GRID(XWALL)
C-------------------------------------------- DEFINE PLASMA COEFFICIENTS
      CALL EQUILV
      CALL FKEQ
C
      RETURN
C
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
C
      REAL     SCALEQ, RBPHI02, DJ0, DJE, DP0, DPE, DRBPHI0, DRBPHIE,
     >         WURZEL, DQEC, RAXIS
      REAL     VX(NCHIMAX),VY(NCHIMAX),XOUT(NPNC),YOUT(NPNC)
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

C----------------------------------------- ADDITIONAL DATA FOR VACUUM --
      READ(NMAP,*) (VX(JS),JS=1,NCHI)
      READ(NMAP,*) (VY(JS),JS=1,NCHI)
      READ(NMAP,*) EPS
c      READ(NMAP,*) (XOUT(JS),JS=NCHI+1,(JS0+1)*NCHI)
c      READ(NMAP,*) (YOUT(JS),JS=NCHI+1,(JS0+1)*NCHI)
      
c      DELTA = (1./radius - 1. /EPS)
c      DO J=1,NCHI
c        XOUT(J) = DELTA
c      ENDDO
c      DO I=1,NPSI
c        INDEX = (I-1)*NCHI + 1
c        RAD(I) = (XOUT(INDEX)-XOUT(1))/(XOUT((NPSI-1)*NCHI+1)-XOUT(1))
c      ENDDO
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
      CALL SPLINE(NPSI,CS,QS,DQ0,DQ1,2,Q1,Q2,Q3,Q4)
      CALL SPLINE(NPSI,CS,CURJ,DJ0,DJE,2,C1,C2,C3,C4)
      CALL SPLINE(NPSI,CS,P0,DP0,DPE,2,P1,P2,P3,P4)
      CALL SPLINE(NPSI,CS,RBPHI,DRBPHI0,DRBPHIE,2,RBP1,RBP2,RBP3,RBP4)
C
      CALL SGCOPY(NPSI,Q2,1,DQS,1)
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
      SUBROUTINE GRID(XWALL)                                                         
C            
*CALL COMMAX                                                                 
*CALL COMGRID                                                                 
C                                                                             
      IF(NG.GT.1) THEN                       
         DELS       = (SEND-SBEG) / (NG-1)                                 
      ELSE                                                                 
         DELS       = SBEG                                                 
      ENDIF                                                                
C                                                                             
      DO  200    N  = 1, NG                                                   
         SGRID(N)   =   SBEG + FLOAT(N-1)*DELS                                
  200 CONTINUE                                                                
C                                                                             
      RETURN                                                                  
      END                                                                     

************************************************************************
*DECK EQUILV
      SUBROUTINE EQUILV
C-----------------------------------------------------------------------
C CALCULATE EQUILIBRIUM QUANTITIES ON GAUSSIAN POINTS
C-----------------------------------------------------------------------
*CALL COMMAX
*CALL COMPARE
*CALL COMPIO
*CALL COMGRID
*CALL COMEQUI
*CALL COMEQV
*CALL COMIOD
*CALL COMSPL
      REAL ZS(4), ABLTG(3), ZA, ZB, ZC, ZDIF
C
      DO 200 NI = 1, NG                                                                                  
        SGI(NI) = SGRID(NI)                                               
  200 CONTINUE                   
      DO 10 J=1,NG
            Q(J)    = SPWERT(NPSI,SGI(J),Q1,Q2,Q3,Q4,CS,ABLTG)
            DQ(J)   = ABLTG(1)
            T(J)    = SPWERT(NPSI,SGI(J),RBP1,RBP2,RBP3,RBP4,CS,ABLTG)
            DT(J)   = ABLTG(1)
            ZP      = SPWERT(NPSI,SGI(J),P1,P2,P3,P4,CS,ABLTG)
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
     >                 +6.*DSURF2*SGI(J)**5+8.*DSURF3*SGI(J)**7
            ELSEIF(IEQ.EQ.4) THEN
               RHO(J) = 1.+DSURF*SGI(J)     + DSURF1*SGI(J)**2
     >                    +DSURF2*SGI(J)**3 + DSURF3*SGI(J)**4
               DRHO(J) =   DSURF            + 2*DSURF1*SGI(J)
     >                 + 3*DSURF2*SGI(J)**2 + 4*DSURF3*SGI(J)**3
            ELSEIF(IEQ.EQ.5) THEN
               RHO(J)  = (1.-DSURF)*ZP/P1(1) + DSURF
               DRHO(J) = (1.-DSURF)*ZDP/P1(1)
            ELSE
               RHO(J)  = DENS**IDPOW
               DRHO(J) = IDPOW*DENS**(IDPOW-1)*(-2*(1.-DSURF)*SGI(J))
            ENDIF
            ZT0(J)  = ZP/RHO(J)
            ZDT0(J) = ZDP/RHO(J) - ZP*DRHO(J)/RHO(J)**2
c$$$            ZT0(J) = ZP
c$$$            ZDT0(J) = ZDP
   10    CONTINUE
   20 CONTINUE

      WRITE(NOUT,21)
      DO 30 J=1,NG
        WRITE(NOUT,22) SGI(J),Q(J),DQ(J),T(J),DT(J),
     >                 RHO(J),DRHO(J),ZT0(J),ZDT0(J)
   30 CONTINUE   
   
      RETURN
   21 FORMAT(///7X,'S',11X,'Q',10X,'DQ',11X,'T',10X,'DT',10X,
     >       'RHO',8X,'DRHO',9X,'T0',10X,'DT0'/1X,132('-'))
   22 FORMAT(1X,1P,11E12.4)
   31 FORMAT(/' Q AT BOUNDARY :',E12.4)
      END

************************************************************************
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
C
C
*CALL COMMAX
*CALL COMPIO
*CALL COMPCON
*CALL COMGEM
*CALL COMIOD
*CALL COMEQUI
*CALL COMDIAG
*CALL COMFFT
 
      INTEGER  INDEX(2*(NCHIMAX-1))
      REAL     HV(NPNC), FWT(2*(NCHIMAX-1))
      REAL     H1(NPSIMAX), H2(NPSIMAX), H3(NPSIMAX), H4(NPSIMAX)
      REAL     DUMMY(3)
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
C
C     INITIALIZATION OF THE SINE AND COSINE TABLES
C     --------------------------------------------
C
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
C
C     FOURIER ANALYSIS AND SPLINE FOR EVERY FOURIER COEFFICIENT
C     ---------------------------------------------------------
C
C        R**2  -->  RR2,IR2
C
      WRITE(NOUT,31)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,GEM33,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RR2,IR2)
C
C        R**4  -->  RR4,IR4
C
      WRITE(NOUT,33)
      DO 40 I=1,NGES
         HV(I) = GEM33(I)**2
   40 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RR4,IR4)
C
C        GRAD.PSI**2  -->  RGPSI,IGPSI
C
      WRITE(NOUT,41)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,GEM11,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RGPSI,IGPSI)
C
C        S**2/GRAD.PSI**2  -->  RSOGPSI,ISOGPSI
C
      WRITE(NOUT,43)
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
C
C        GRAD.PSI * GRAD.THETA  -->  RGPGT,IGPGT
C
      WRITE(NOUT,61)
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,GEM12,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RGPGT,IGPGT)
C
C        S**2*(GRAD.PSI*GRAD.THETA)**2/GRAD.PSI**2  -->  RSGGG,ISGGG
C
      WRITE(NOUT,63)
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
C
C        S**2 * R**2 * (GRAD.PSI*GRAD.THETA)**2/GRAD.PSI**2  -->
C                                                     RSR2GGG,ISR2GGG
C
      WRITE(NOUT,81)
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
C
C        S**2 * R**4 * (GRAD.PSI*GRAD.THETA)**2/GRAD.PSI**2  -->
C                                                  RSR4GGG,ISR4GGG
C
      WRITE(NOUT,101)
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
C
C        R**2 * GRAD.PSI**2  -->  RR2GPSI,IR2GPSI
C
      WRITE(NOUT,121)
      DO 130 I=1,NGES
         HV(I) = GEM33(I)*GEM11(I)
  130 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RR2GPSI,IR2GPSI)
C
C        S**2 / (R**2*GRAD.PSI**2)  -->  RSOR2GP,ISOR2GP
C
      WRITE(NOUT,131)
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
C
C        S**2 * R**2 / GRAD.PSI**2  -->  RSR2OGP,ISR2OGP
C
      WRITE(NOUT,151)
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
C
C        R**2*GRAD.PSI*GRAD.THETA  -->  RR2GPGT,IR2GPGT
C
      WRITE(NOUT,171)
      DO 180 I=1,NGES
         HV(I) = GEM33(I)*GEM12(I)
  180 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RR2GPGT,IR2GPGT)
C
C        R**4*GRAD.PSI**2  -->  RR4GPSI,IR4GPSI
C
      WRITE(NOUT,181)
      DO 190 I=1,NGES
         HV(I) = GEM33(I)**2*GEM11(I)
  190 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RR4GPSI,IR4GPSI)
C
C        R**4*GRAD.PSI*GRAD.THETA  -->  RR4GPGT,IR4GPGT
C
      WRITE(NOUT,191)
      DO 200 I=1,NGES
         HV(I) = GEM33(I)**2*GEM12(I)
  200 CONTINUE
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,0)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RR4GPGT,IR4GPGT)
C
C        DS R**2  -->  RDSR2,IDSR2
C
      WRITE(NOUT,201)
      DO 210 J=1,NCHI
         CALL SGCOPY(NPSI,GEM33(J),NCHI,FWT,1)
         CALL SPLINE(NPSI,CS,FWT,0.0,0.0,3,H1,H2,H3,H4)
         CALL SGCOPY(NPSI,H2,1,HV(J),NCHI)
  210 CONTINUE
C
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RDSR2,IDSR2)
C
C        DS GRAD.PSI**2  -->  RDSGPSI,IDSGPSI
C
      WRITE(NOUT,211)
      DO 220 J=1,NCHI
         CALL SGCOPY(NPSI,GEM11(J),NCHI,FWT,1)
         CALL SPLINE(NPSI,CS,FWT,0.0,0.0,3,H1,H2,H3,H4)
         CALL SGCOPY(NPSI,H2,1,HV(J),NCHI)
  220 CONTINUE
C
      CALL FFTRAN(LMAX,NCHIMAX,NPSI,NCHI,HV,WORK,FWT,ZFK,INDEX,1)
      CALL SPLFK(NCHI,NPSI,NCHIMAX,NP4,LANZ,CS,HV,ZFK,RDSGPSI,IDSGPSI)
C
      RETURN
C
   31 FORMAT(///' FOURIER-KOEFFIZIENTEN AUS IODSK'//1X,'R**2')
   33 FORMAT(/1X,'R**4')
   41 FORMAT(/1X,'GRAD.PSI**2')
   43 FORMAT(/1X,'S**2/GRAD.PSI**2')
   61 FORMAT(/1X,'GRAD.PSI*GRAD.THETA')
   63 FORMAT(/1X,'S**2*(GRAD.PSI*GRAD.THETA)**2/GRAD.PSI**2')
   81 FORMAT(/1X,'S**2*R**2*(GRAD.PSI*GRAD.THETA)**2/GRAD.PSI**2')
  101 FORMAT(/1X,'S**2*R**4*(GRAD.PSI*GRAD.THETA)**2/GRAD.PSI**2')
  121 FORMAT(/1X,'R**2*GRAD.PSI**2')
  131 FORMAT(/1X,'S**2/(R**2*GRAD.PSI**2)')
  151 FORMAT(/1X,'S**2*R**2/GRAD.PSI**2')
  171 FORMAT(/1X,'R**2*GRAD.PSI*GRAD.THETA')
  181 FORMAT(/1X,'R**4*GRAD.PSI**2')
  191 FORMAT(/1X,'R**4*GRAD.PSI*GRAD.THETA')
  201 FORMAT(/1X,'DS R**2')
  211 FORMAT(/1X,'DS GRAD.PSI**2')
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
      CALL DGTSL(N-2,B,D,C,A,IERR)
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
**    MAT1                                                            **
**      CONBMAT                                                       **
**        CUBFCT                                                      **
**        QUAFCT                                                      **
**        SPWERT                                                      **
**        FKUBL                                                       **
**      CONAMAT                                                       **
**        CUBFCT                                                      **
**        QUAFCT                                                      **
**        DCUBF                                                       **
**        DQUAF                                                       **
**        SPWERT                                                      **
**        FKUBL                                                       **
**        ADDBOUND                                                    **
**          FBOUND                                                    **
**                                                                    **
************************************************************************
************************************************************************
C-----------------------------------------------------------------------
C     COMPUTATION MATRICES FOR QR
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPAR
*CALL CORE1
*CALL COMGRID
*CALL COMNIP
C
      DO 10 J=1,NZMA
          DO 10 I=1,NZMA
             AMAT(I,J) = (0.0,0.0)
             BMAT(I,J) = (0.0,0.0)
   10 CONTINUE
C
      CALL CONBMAT(NIP,BMAT)
C
      CALL CONAMAT(NIP,AMAT)

      RETURN
      END
C***********************************************************************
*DECK CONBMAT
      SUBROUTINE CONBMAT(NI,ZMA)
C-----------------------------------------------------------------------
C     COMPUTATION OF THE MATRIX BMAT (STORED IN ZMA)
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPAR
*CALL COMPARE
*CALL COMGRID
*CALL COMEQUI
*CALL COMEQV
*CALL COMWEL
*CALL COMGEW
*CALL COMIOD
*CALL COMFFT
C
      INTEGER  NI,MS,MZ,I,K,
     >         INDCC(3),INDQQ(7),INDCQ(3),INDQC(3)
      REAL     SL,SU,ZDIF,ZA,ZB,ZC,ZSR,ZQ,ZT,QOT,TOQ,
     >         DZQ,DZT,ZETA,DZETA,ZRHO,DZRHO,T0,DT0,FKDUMMY,
     >         ZS(4),HC(4),HQ(4),DUMMY(3)
      COMPLEX  R2,R4,R2GPSI,R4GPSI,R2GPGT,R4GPGT,SOGPSI,
     >         SR2OGP,SR2GGG,SR4GGG,
     >         ZMA(NZMA*NZMA),FACT(7)
C
      DATA INDQQ / 1, 19, 20, 27, 28, 37, 46 /
      DATA INDCC / 10, 55, 64 /
      DATA INDCQ / 18, 26, 47 /
      DATA INDQC / 11, 12, 54 /
C
C ... NULLSCHREIBEN DER MATRIX ZMA ...
C
      DO 10 I = 1, NZMA*NZMA
         ZMA(I) = (0.0,0.0)
   10 CONTINUE
C
      EPS = 1.0D-8                                                           
      DO 11 I = 1 , 4                                                         
         HC(I)= ALOG(EPS)                                                     
         HQ(I)= 1./EPS                                                        
   11 CONTINUE                                                                
C                                                                             
      ZSR            = SGI(NI)                                             
      ZQ             = Q(NI)                                               
      DZQ            = DQ(NI)                                              
      ZT             = T(NI)                                               
      DZT            = DT(NI)                                              
      ZETA           = ETAV(NI)                                            
      DZETA          = DETA(NI)                                            
      ZRHO           = RHO(NI)                                             
      DZRHO          = DRHO(NI)                                            
      T0             = ZT0(NI)                                             
      DT0            = ZDT0(NI)                                            
C
      QOT     = ZQ/ZT
      TOQ     = ZT/ZQ
      FKDUMMY =  1.0
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
        R4     = CMPLX(SPWERT(NPSI,ZSR,RR4(1,K),RR4(NP1,K),
     >                        RR4(N2P1,K),RR4(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IR4(1,K),IR4(NP1,K),
     >                        IR4(N2P1,K),IR4(N3P1,K),CS,DUMMY))
C
        R2GPSI = CMPLX(SPWERT(NPSI,ZSR,RR2GPSI(1,K),RR2GPSI(NP1,K),
     >                        RR2GPSI(N2P1,K),RR2GPSI(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IR2GPSI(1,K),IR2GPSI(NP1,K),
     >                        IR2GPSI(N2P1,K),IR2GPSI(N3P1,K),CS,DUMMY))
C
        R4GPSI = CMPLX(SPWERT(NPSI,ZSR,RR4GPSI(1,K),RR4GPSI(NP1,K),
     >                        RR4GPSI(N2P1,K),RR4GPSI(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IR4GPSI(1,K),IR4GPSI(NP1,K),
     >                        IR4GPSI(N2P1,K),IR4GPSI(N3P1,K),CS,DUMMY))
C
        R2GPGT = CMPLX(SPWERT(NPSI,ZSR,RR2GPGT(1,K),RR2GPGT(NP1,K),
     >                        RR2GPGT(N2P1,K),RR2GPGT(N3P1,K),CS,DUMMY),
     >                  SPWERT(NPSI,ZSR,IR2GPGT(1,K),IR2GPGT(NP1,K),
     >                        IR2GPGT(N2P1,K),IR2GPGT(N3P1,K),CS,DUMMY))
C
        R4GPGT = CMPLX(SPWERT(NPSI,ZSR,RR4GPGT(1,K),RR4GPGT(NP1,K),
     >                        RR4GPGT(N2P1,K),RR4GPGT(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IR4GPGT(1,K),IR4GPGT(NP1,K),
     >                        IR4GPGT(N2P1,K),IR4GPGT(N3P1,K),CS,DUMMY))
C
        SOGPSI = CMPLX(SPWERT(NPSI,ZSR,RSOGPSI(1,K),RSOGPSI(NP1,K),
     >                        RSOGPSI(N2P1,K),RSOGPSI(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,ISOGPSI(1,K),ISOGPSI(NP1,K),
     >                        ISOGPSI(N2P1,K),ISOGPSI(N3P1,K),CS,DUMMY))
C
        SR2OGP = CMPLX(SPWERT(NPSI,ZSR,RSR2OGP(1,K),RSR2OGP(NP1,K),
     >                        RSR2OGP(N2P1,K),RSR2OGP(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,ISR2OGP(1,K),ISR2OGP(NP1,K),
     >                        ISR2OGP(N2P1,K),ISR2OGP(N3P1,K),CS,DUMMY))
C
        SR2GGG = CMPLX(SPWERT(NPSI,ZSR,RSR2GGG(1,K),RSR2GGG(NP1,K),
     >                        RSR2GGG(N2P1,K),RSR2GGG(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,ISR2GGG(1,K),ISR2GGG(NP1,K),
     >                        ISR2GGG(N2P1,K),ISR2GGG(N3P1,K),CS,DUMMY))
C
        SR4GGG = CMPLX(SPWERT(NPSI,ZSR,RSR4GGG(1,K),RSR4GGG(NP1,K),
     >                        RSR4GGG(N2P1,K),RSR4GGG(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,ISR4GGG(1,K),ISR4GGG(NP1,K),
     >                        ISR4GGG(N2P1,K),ISR4GGG(N3P1,K),CS,DUMMY))
C
C
      DO 100  MS = 1 , MANZ - KF + 1
C     ------------------------------
C
      MZ = MS + KF - 1
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(QUA)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(1,1), B(3,3), B(4,3), B(3,4), B(4,4), B(5,5), B(6,6)
C
         FACT(1)  = 2.*CPSURF*ZQ*R2/(ZSR*ZT)
         FACT(2)  = ZRHO*R4GPSI*ZQ/(2.*ZSR*CPSURF*ZT)
         FACT(3)  = FACT(2)
         FACT(4)  = FACT(2)
         FACT(5)  = ZRHO*ZQ*(ZT**2*R4+R4GPSI)/(2.*ZSR*CPSURF*ZT)
         FACT(6)  = 2.*CPSURF*ZRHO*ZQ*R2/(ZSR*ZT)
         FACT(7)  = ZQ*R2GPSI/(2.*ZSR*CPSURF*ZT)
C
      CALL FKUBL(MZ,MS,MANZ,7,INDQQ,NBG,NZMA,ZMA,FACT,1.,HQ,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(CUB)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(2,2), B(7,7), B(8,8)
C
         FACT(1)  = 2.*CPSURF*ZRHO*(TOQ*SR2OGP+QOT*SR4GGG)/ZSR
         FACT(2)  = 2.*CPSURF*(TOQ*SOGPSI+QOT*SR2GGG)/ZSR
         FACT(3)  = 2.*ZSR*CPSURF*ZQ/ZT*FKDUMMY
C
      CALL FKUBL(MZ,MS,MANZ,3,INDCC,NBG,NZMA,ZMA,FACT,1.,HC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(QUA)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(2,3), B(2,4), B(7,6)
C
         FACT(1)  = ZRHO*QOT*(0.0,1.0)*R4GPGT
         FACT(2)  = FACT(1)
         FACT(3)  = QOT*R2GPGT/(0.0,1.0)
C
      CALL FKUBL(MZ,MS,MANZ,3,INDCQ,NBG,NZMA,ZMA,FACT,1.,HC,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(CUB)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(3,2), B(4,2), B(6,7)
C
         FACT(1)  = ZRHO*QOT*R4GPGT/(0.0,1.0)
         FACT(2)  = FACT(1)
         FACT(3)  = QOT*(0.0,1.0)*R2GPGT
C
      CALL FKUBL(MZ,MS,MANZ,3,INDQC,NBG,NZMA,ZMA,FACT,1.,HQ,HC)
C
      IF(MS.EQ.MZ)  GOTO 100
C     ----------------------
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(QUA)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(1,1), B(3,3), B(4,3), B(3,4), B(4,4), B(5,5), B(6,6)
C
         FACT(1)  = 2.*CPSURF*ZQ*CONJG(R2)/(ZSR*ZT)
         FACT(2)  = ZRHO*CONJG(R4GPSI)*ZQ/(2.*ZSR*CPSURF*ZT)
         FACT(3)  = FACT(2)
         FACT(4)  = FACT(2)
         FACT(5)  = ZRHO*ZQ*(ZT**2*CONJG(R4)+CONJG(R4GPSI))
     >              /(2.*ZSR*CPSURF*ZT)
         FACT(6)  = 2.*CPSURF*ZRHO*ZQ*CONJG(R2)/(ZSR*ZT)
         FACT(7)  = ZQ*CONJG(R2GPSI)/(2.*ZSR*CPSURF*ZT)
C
      CALL FKUBL(MS,MZ,MANZ,7,INDQQ,NBG,NZMA,ZMA,FACT,1.,HQ,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(CUB)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(2,2), B(7,7), B(8,8)
C
         FACT(1)  = 2.*CPSURF*ZRHO*(TOQ*CONJG(SR2OGP)+QOT
     >              *CONJG(SR4GGG))/ZSR
         FACT(2)  = 2.*CPSURF*(TOQ*CONJG(SOGPSI)+QOT*CONJG(SR2GGG))/ZSR
         FACT(3)  = 2.*ZSR*CPSURF*QOT*FKDUMMY
C
      CALL FKUBL(MS,MZ,MANZ,3,INDCC,NBG,NZMA,ZMA,FACT,1.,HC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(QUA)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(2,3), B(2,4), B(7,6)
C
C
         FACT(1)  = ZRHO*QOT*(0.0,1.0)*CONJG(R4GPGT)
         FACT(2)  = FACT(1)
         FACT(3)  = QOT*CONJG(R2GPGT)/(0.0,1.0)
C
      CALL FKUBL(MS,MZ,MANZ,3,INDCQ,NBG,NZMA,ZMA,FACT,1.,HC,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(CUB)  FUER DIE GLEICHUNGEN
C     -----------------------
C     B(3,2), B(4,2), B(6,7)
C
C
         FACT(1)  = ZRHO*QOT*CONJG(R4GPGT)/(0.0,1.0)
         FACT(2)  = FACT(1)
         FACT(3)  = QOT*(0.0,1.0)*CONJG(R2GPGT)
C
      CALL FKUBL(MS,MZ,MANZ,3,INDQC,NBG,NZMA,ZMA,FACT,1.,HQ,HC)
C
  100 CONTINUE
C     --------
C
      FKDUMMY =  0.0
C
  200 CONTINUE
C     --------
C
      RETURN
      END
************************************************************************
*DECK CONAMAT
      SUBROUTINE CONAMAT(NI,ZMA)
C-----------------------------------------------------------------------
C     COMPUTATION OF THE MATRIX AMAT (STORED IN ZMA)
C-----------------------------------------------------------------------
C
*CALL COMMAX
*CALL COMPAR
*CALL COMPARE
*CALL COMGRID
*CALL COMWEL
*CALL COMGEW
*CALL COMEQUI
*CALL COMEQV
*CALL COMIOD
*CALL COMFFT
C
      INTEGER  NI,MS,MZ,I,K,
     >         INDCC(8),INDCQ(6),INDQC(9),INDQQ(12),IDCDC(4),
     >         IDCQ(5),IQDC(8),IDCC(4),ICDC(4)
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
      DATA INDCC / 15, 16, 50, 55, 56, 58, 63, 64 /
      DATA INDCQ / 2, 23, 34, 42, 47, 48 /
      DATA INDQC / 9, 13, 14, 51, 52, 54, 59, 60, 62 /
      DATA INDQQ / 3, 4, 17, 21, 22, 25, 29, 35, 36, 43, 44, 46 /
      DATA IDCDC / 50, 55, 58, 64 /
      DATA IDCQ  / 2, 34, 42, 47, 48 /
      DATA IQDC  / 9, 13, 51, 52, 54, 59, 60, 62 /
      DATA IDCC  / 50, 56, 58, 64 /
      DATA ICDC  / 55, 58, 63, 64 /
C
C ... NULLSCHREIBEN DER MATRIX ZMA ...
C
C
      DO 10 I = 1, NZMA*NZMA
         ZMA(I) = (0.0,0.0)
   10 CONTINUE
      EPS = 1.D-8                                                           
      DO 11 I = 1 , 4                                                         
         HC(I)= ALOG(EPS)                                                     
         DHC(I)= 1./EPS                                                       
         HQ(I)= 1./EPS                                                        
         DHQ(I)= -1./EPS**2                                                   
   11 CONTINUE                                                                
C                                                                             
      ZSR            = SGI(NI)                                             
      ZQ             = Q(NI)                                               
      DZQ            = DQ(NI)                                              
      ZT             = T(NI)                                               
      DZT            = DT(NI)                                              
      ZETA           = ETAV(NI)                                            
      DZETA          = DETA(NI)                                            
      ZRHO           = RHO(NI)                                             
      DZRHO          = DRHO(NI)                                            
      T0             = ZT0(NI)                                             
      DT0            = ZDT0(NI)                                            

      QOT     = ZQ/ZT
      TOQ     = ZT/ZQ
      T2OQ    = ZT**2/ZQ
      DOQDOT  = DZQ/ZQ-DZT/ZT
      SPS2    = 2.*ZSR*CPSURF
      FKDUMMY =  1.0
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
        R2GPSI = CMPLX(SPWERT(NPSI,ZSR,RR2GPSI(1,K),RR2GPSI(NP1,K),
     >                        RR2GPSI(N2P1,K),RR2GPSI(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IR2GPSI(1,K),IR2GPSI(NP1,K),
     >                        IR2GPSI(N2P1,K),IR2GPSI(N3P1,K),CS,DUMMY))
C
        R2GPGT = CMPLX(SPWERT(NPSI,ZSR,RR2GPGT(1,K),RR2GPGT(NP1,K),
     >                        RR2GPGT(N2P1,K),RR2GPGT(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,IR2GPGT(1,K),IR2GPGT(NP1,K),
     >                        IR2GPGT(N2P1,K),IR2GPGT(N3P1,K),CS,DUMMY))
C
        SOGPSI = CMPLX(SPWERT(NPSI,ZSR,RSOGPSI(1,K),RSOGPSI(NP1,K),
     >                        RSOGPSI(N2P1,K),RSOGPSI(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,ISOGPSI(1,K),ISOGPSI(NP1,K),
     >                        ISOGPSI(N2P1,K),ISOGPSI(N3P1,K),CS,DUMMY))
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
        SR2GGG = CMPLX(SPWERT(NPSI,ZSR,RSR2GGG(1,K),RSR2GGG(NP1,K),
     >                        RSR2GGG(N2P1,K),RSR2GGG(N3P1,K),CS,DUMMY),
     >                 SPWERT(NPSI,ZSR,ISR2GGG(1,K),ISR2GGG(NP1,K),
     >                        ISR2GGG(N2P1,K),ISR2GGG(N3P1,K),CS,DUMMY))
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
        DTR2I    = -1.*(K-1)*R2
        DTR2OI   = (K-1)*R2
        DTGPSII  = -1.*(K-1)*GPSI
        DTGPSIOI = (K-1)*GPSI
        DTGPGT   = (0.0,1.0)*(K-1)*GPGT
        DTSGGGOI = (K-1)*SGGG
C
        DTR2IM   = (K-1)*CONJG(R2)
        DTR2OIM  = -1.*(K-1)*CONJG(R2)
        DTGPSIIM = (K-1)*CONJG(GPSI)
        DTGPSOIM = -1.*(K-1)*CONJG(GPSI)
        DTGPGTM  = -1.*(0.0,1.0)*(K-1)*CONJG(GPGT)
        DTGGGOIM = -1.*(K-1)*CONJG(SGGG)
C
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
C     A(7,2),A(8,2),A(2,7),A(7,7),A(8,7),A(2,8),A(7,8),A(8,8)
C
C
         FACT(1) = -2.*CPSURF/ZSR*(T2OQ*SOGPSI+ZQ*SR2GGG)
         FACT(2) = SPS2*FKDUMMY
         FACT(3) = 2.*CPSURF/ZSR*ZNKWEL*(MZNQ*TOQ**2*SOR2GP+MSNQ*SGGG
     >             +DTSGGGOI+1./(0.0,1.0)*ZSR/(2.*CPSURF)
     >             *DOQDOT*GPGT)
         FACT(4) = -ZNKWEL**2*2.*CPSURF/ZSR*ZETA*(TOQ*SOR2GP+QOT*SGGG)
         FACT(5) = ZNKWEL*(QOT*DZETA*(0.0,1.0)*GPGT+ZETA*2.*CPSURF
     >             /ZSR*(SMZ*TOQ*SOR2GP+SMS*QOT*SGGG+QOT*DTSGGGOI))
         FACT(6) = -2.*CPSURF/ZSR*SMS*(MZNQ*TOQ**2*SOR2GP+MSNQ*SGGG
     >             +DTSGGGOI+1./(0.0,1.0)*ZSR/(2.*CPSURF)
     >             *DOQDOT*GPGT)
         FACT(7) = ZNKWEL*SMS*2.*CPSURF/ZSR*ZETA*(TOQ*SOR2GP+QOT*SGGG)
         FACT(8) = -SMS*(QOT*DZETA*(0.0,1.0)*GPGT+ZETA*2.*CPSURF
     >             /ZSR*(SMZ*TOQ*SOR2GP+SMS*QOT*SGGG+QOT*DTSGGGOI))
C
      CALL FKUBL(MZ,MS,MANZ,8,INDCC,NBG,NZMA,ZMA,FACT,1.,HC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     ----------------------
C     A(2,1),A(7,3),A(2,5),A(2,6),A(7,6),A(8,6)
C
C
         FACT(1) = T0/ZSR*DSR2
         FACT(2) = ZQ/(0.0,1.0)*R2GPGT
         FACT(3) = ZRHO/ZSR*DSR2
         FACT(4) = -ZNKWEL*(MSNQ*(0.0,1.0)*GPGT+2.*DTGPGT
     >             +1./SPS2*(DSGPSI+2.*GPSI*DOQDOT))
         FACT(5) = ZNKWEL**2*ZETA*QOT*(0.0,1.0)*GPGT+SMS/SPS2*TOQ*DZETA
     >             *FKDUMMY
         FACT(6) = ZNKWEL*QOT*(SMS*ZETA/(0.0,1.0)*GPGT-ZETA*DTGPGT
     >             +1./SPS2*DZETA*GPSI)
C
      CALL FKUBL(MZ,MS,MANZ,6,INDCQ,NBG,NZMA,ZMA,FACT,1.,HC,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(CUB)   FUER DIE GLEICHUNGEN
C     ----------------------
C     A(1,2),A(5,2),A(6,2),A(3,7),A(4,7),A(6,7),A(3,8),A(4,8),A(6,8)
C
C
         FACT(1) = -1./ZSR*(DZRHO*R2+ZRHO*DSR2)
         FACT(2) = ZRHO/ZSR*((1.-GAMMA)*T0*DSR2-DT0*R2)
         FACT(3) = ZQ/(0.0,1.0)*R2GPGT
         FACT(4) = ZNKWEL/SPS2*(SPS2*(SMZ-MSNQ)*(0.0,1.0)*GPGT+
     >             DSGPSI+GPSI*DOQDOT)
         FACT(5) = ZNKWEL/SPS2*(ZT*DZT*FKDUMMY+SPS2*(SMZ-SMS)
     >             *(0.0,1.0)*GPGT+DSGPSI+GPSI*DOQDOT)
         FACT(6) = ZETA*QOT*ZNKWEL**2/(0.0,1.0)*GPGT
         FACT(7) = -SMS/SPS2*(SPS2*(SMZ-MSNQ)*(0.0,1.0)*GPGT+
     >             DSGPSI+GPSI*DOQDOT)
         FACT(8) = -SMS/SPS2*(ZT*DZT*FKDUMMY+SPS2*(SMZ-SMS)*(0.0,1.0)
     >             *GPGT+DSGPSI+GPSI*DOQDOT)
         FACT(9) = ZNKWEL*SMS*ZETA*QOT*(0.0,1.0)*GPGT
C
      CALL FKUBL(MZ,MS,MANZ,9,INDQC,NBG,NZMA,ZMA,FACT,1.,HQ,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     ----------------------
C     A(3,1),A(4,1),A(1,3),A(5,3),A(6,3),A(1,4),A(5,4),
C     A(3,5),A(4,5),A(3,6),A(4,6),A(6,6)
C
C
         FACT(1) = T0/ZSR*(SMZ*R2+DTR2I)
         FACT(2) = T0/ZSR*(MZNQ*R2+DTR2I)
         FACT(3) = ZRHO/ZSR*(DTR2I-SMS*R2)
         FACT(4) = ZRHO*T0/ZSR*(1.-GAMMA)*(SMS*R2+DTR2OI)
         FACT(5) = 1./SPS2*ZQ*R2GPSI
         FACT(6) = ZRHO/ZSR*(DTR2I-MSNQ*R2)
         FACT(7) = ZRHO*T0/ZSR*(1.-GAMMA)*(MSNQ*R2+DTR2OI)
         FACT(8) = ZRHO/ZSR*(SMZ*R2+DTR2I)
         FACT(9) = ZRHO/ZSR*(MZNQ*R2+DTR2I)
         FACT(10)= 1./SPS2*((SMZ-MSNQ)*ZNKWEL*GPSI+ZNKWEL*DTGPSII
     >               -SMS*SMZ*T2OQ*FKDUMMY)
         FACT(11)= 1./SPS2*((SMZ-SMS)*(ZNKWEL*GPSI-SMS*T2OQ*FKDUMMY)
     >               +ZNKWEL*DTGPSII)
         FACT(12)= -1./SPS2*ZETA*(SMS**2*TOQ*FKDUMMY+ZNKWEL**2*QOT*GPSI)
C
      CALL FKUBL(MZ,MS,MANZ,12,INDQQ,NBG,NZMA,ZMA,FACT,1.,HQ,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H'(CUB)   FUER DIE GLEICHUNG
C     ------------------------
C     A(2,7),A(7,7),A(2,8),A(8,8)
C
C
         FACT(1) = 1./SPS2*T2OQ*FKDUMMY
         FACT(2) = -1./SPS2*TOQ*ZETA*FKDUMMY
         FACT(3) = -1./SPS2*GPSI
         FACT(4) = -ZETA*1./SPS2*QOT*GPSI
C
      CALL FKUBL(MZ,MS,MANZ,4,IDCDC,NBG,NZMA,ZMA,FACT,1.,DHC,DHC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(2,1),A(2,5),A(2,6),A(7,6),A(8,6)
C
C
         FACT(1) = T0/ZSR*R2
         FACT(2) = ZRHO/ZSR*R2
         FACT(3) = 1./SPS2*(ZNKWEL*GPSI-SMS*T2OQ*FKDUMMY)
         FACT(4) = SMS/SPS2*ZETA*TOQ*FKDUMMY
         FACT(5) = 1./SPS2*ZNKWEL*ZETA*QOT*GPSI
C
      CALL FKUBL(MZ,MS,MANZ,5,IDCQ,NBG,NZMA,ZMA,FACT,1.,DHC,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H'(CUB)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(1,2),A(5,2),A(3,7),A(4,7),A(6,7),A(3,8),A(4,8),A(6,8)
C
C
         FACT(1) = -1./ZSR*ZRHO*R2
         FACT(2) = (1.-GAMMA)*ZRHO*T0/ZSR*R2
         FACT(3) = SMZ/SPS2*T2OQ*FKDUMMY
         FACT(4) = 1./SPS2*(SMZ-SMS)*T2OQ*FKDUMMY
         FACT(5) = SMS/SPS2*ZETA*TOQ*FKDUMMY
         FACT(6) = 1./SPS2*((MSNQ-SMZ)*GPSI+DTGPSIOI)
         FACT(7) = 1./SPS2*((SMS-SMZ)*GPSI+DTGPSIOI)
         FACT(8) = ZNKWEL/SPS2*ZETA*QOT*GPSI
C
      CALL FKUBL(MZ,MS,MANZ,8,IQDC,NBG,NZMA,ZMA,FACT,1.,HQ,DHC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H(CUB)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(2,7),A(8,7),A(2,8),A(8,8)
C
C
         FACT(1) = ZNKWEL*(0.0,1.0)*GPGT
         FACT(2) = ZNKWEL*ZETA*QOT*(0.0,1.0)*GPGT
         FACT(3) = SMS/(0.0,1.0)*GPGT
         FACT(4) = SMS*ZETA*QOT/(0.0,1.0)*GPGT
C
      CALL FKUBL(MZ,MS,MANZ,4,IDCC,NBG,NZMA,ZMA,FACT,1.,DHC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H'(CUB)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(7,7),A(2,8),A(7,8),A(8,8)
C
C
         FACT(1) = -1./SPS2*TOQ*DZETA*FKDUMMY
         FACT(2) = 1./SPS2*(DSGPSI+2.*GPSI*DOQDOT)+2.*DTGPGT
     >             +MSNQ*(0.0,1.0)*GPGT
         FACT(3) = ZNKWEL*ZETA*QOT/(0.0,1.0)*GPGT
         FACT(4) = QOT*(ZETA*SMS*(0.0,1.0)*GPGT+ZETA*DTGPGT
     >             -1./SPS2*DZETA*GPSI)
C
      CALL FKUBL(MZ,MS,MANZ,4,ICDC,NBG,NZMA,ZMA,FACT,1.,HC,DHC)
C
C
      IF(MS.EQ.MZ) GOTO 100
C     ---------------------
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(CUB)   FUER DIE GLEICHUNGEN
C     ----------------------
C     A(7,2),A(8,2),A(2,7),A(7,7),A(8,7),A(2,8),A(7,8),A(8,8)
C
C
         FACT(1) = -2.*CPSURF/ZSR*(T2OQ*CONJG(SOGPSI)+ZQ*CONJG(SR2GGG))
         FACT(2) = SPS2*FKDUMMY
         FACT(3) = 2.*CPSURF/ZSR*ZNKWEL*(MSNQ*TOQ**2*CONJG(SOR2GP)
     >             +MZNQ*CONJG(SGGG)+DTGGGOIM+ZSR/(0.0,1.0)
     >             *1./(2.*CPSURF)*DOQDOT*CONJG(GPGT))
         FACT(4) = -ZNKWEL**2*2.*CPSURF/ZSR*ZETA*(TOQ*CONJG(SOR2GP)
     >             +QOT*CONJG(SGGG))
         FACT(5) = ZNKWEL*(QOT*DZETA*(0.0,1.0)*CONJG(GPGT)+ZETA*2.
     >            *CPSURF/ZSR*(SMS*TOQ*CONJG(SOR2GP)+SMZ*QOT*CONJG(SGGG)
     >             +QOT*DTGGGOIM))
         FACT(6) = -2.*CPSURF/ZSR*SMZ*(MSNQ*TOQ**2*CONJG(SOR2GP)+MZNQ*
     >             CONJG(SGGG)+DTGGGOIM+1./(0.0,1.0)*ZSR/(2.*CPSURF)
     >             *DOQDOT*CONJG(GPGT))
         FACT(7) = ZNKWEL*SMZ*2.*CPSURF/ZSR*ZETA*(TOQ*CONJG(SOR2GP)
     >             +QOT*CONJG(SGGG))
         FACT(8) = -SMZ*(QOT*DZETA*(0.0,1.0)*CONJG(GPGT)+ZETA*2.*CPSURF
     >             /ZSR*(SMS*TOQ*CONJG(SOR2GP)+SMZ*QOT*CONJG(SGGG)
     >             +QOT*DTGGGOIM))
C
      CALL FKUBL(MS,MZ,MANZ,8,INDCC,NBG,NZMA,ZMA,FACT,1.,HC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     ----------------------
C     A(2,1),A(7,3),A(2,5),A(2,6),A(7,6),A(8,6)
C
C
         FACT(1) = T0/ZSR*CONJG(DSR2)
         FACT(2) = ZQ/(0.0,1.0)*CONJG(R2GPGT)
         FACT(3) = ZRHO/ZSR*CONJG(DSR2)
         FACT(4) = -ZNKWEL*(MZNQ*(0.0,1.0)*CONJG(GPGT)+2.*DTGPGTM
     >             +1./SPS2*(CONJG(DSGPSI)+2.*CONJG(GPSI)*DOQDOT))
         FACT(5) = ZNKWEL**2*ZETA*QOT*(0.0,1.0)*CONJG(GPGT)+SMZ/SPS2*TOQ
     >             *DZETA*FKDUMMY
         FACT(6) = ZNKWEL*QOT*(SMZ*ZETA/(0.0,1.0)*CONJG(GPGT)-ZETA
     >             *DTGPGTM+1./SPS2*DZETA*CONJG(GPSI))
C
      CALL FKUBL(MS,MZ,MANZ,6,INDCQ,NBG,NZMA,ZMA,FACT,1.,HC,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(CUB)   FUER DIE GLEICHUNGEN
C     ----------------------
C     A(1,2),A(5,2),A(6,2),A(3,7),A(4,7),A(6,7),A(3,8),A(4,8),A(6,8)
C
C
         FACT(1) = -1./ZSR*(DZRHO*CONJG(R2)+ZRHO*CONJG(DSR2))
         FACT(2) = ZRHO/ZSR*((1.-GAMMA)*T0*CONJG(DSR2)-DT0*CONJG(R2))
         FACT(3) = ZQ/(0.0,1.0)*CONJG(R2GPGT)
         FACT(4) = ZNKWEL/SPS2*(SPS2*(SMS-MZNQ)*(0.0,1.0)*CONJG(GPGT)+
     >             CONJG(DSGPSI)+CONJG(GPSI)*DOQDOT)
         FACT(5) = ZNKWEL/SPS2*(ZT*DZT*FKDUMMY+SPS2*(SMS-SMZ)*(0.0,1.0)
     >             *CONJG(GPGT)+CONJG(DSGPSI)+CONJG(GPSI)*DOQDOT)
         FACT(6) = ZETA*QOT*ZNKWEL**2/(0.0,1.0)*CONJG(GPGT)
         FACT(7) = -SMZ/SPS2*(SPS2*(SMS-MZNQ)*(0.0,1.0)*CONJG(GPGT)+
     >             CONJG(DSGPSI)+CONJG(GPSI)*DOQDOT)
         FACT(8) =-SMZ/SPS2*(ZT*DZT*FKDUMMY+SPS2*(SMS-SMZ)*(0.0,1.0)
     >             *CONJG(GPGT)+CONJG(DSGPSI)+CONJG(GPSI)*DOQDOT)
         FACT(9) = ZNKWEL*SMZ*ZETA*QOT*(0.0,1.0)*CONJG(GPGT)
C
      CALL FKUBL(MS,MZ,MANZ,9,INDQC,NBG,NZMA,ZMA,FACT,1.,HQ,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     ----------------------
C     A(3,1),A(4,1),A(1,3),A(5,3),A(6,3),A(1,4),A(5,4),
C     A(3,5),A(4,5),A(3,6),A(4,6),A(6,6)
C
C
         FACT(1) = T0/ZSR*(SMS*CONJG(R2)+DTR2IM)
         FACT(2) = T0/ZSR*(MSNQ*CONJG(R2)+DTR2IM)
         FACT(3) = ZRHO/ZSR*(DTR2IM-SMZ*CONJG(R2))
         FACT(4) = ZRHO*T0/ZSR*(1.-GAMMA)*(SMZ*CONJG(R2)+DTR2OIM)
         FACT(5) = 1./SPS2*ZQ*CONJG(R2GPSI)
         FACT(6) = ZRHO/ZSR*(DTR2IM-MZNQ*CONJG(R2))
         FACT(7) = ZRHO*T0/ZSR*(1.-GAMMA)*(MZNQ*CONJG(R2)+DTR2OIM)
         FACT(8) = ZRHO/ZSR*(SMS*CONJG(R2)+DTR2IM)
         FACT(9) = ZRHO/ZSR*(MSNQ*CONJG(R2)+DTR2IM)
         FACT(10)= 1./SPS2*((SMS-MZNQ)*ZNKWEL*CONJG(GPSI)+ZNKWEL
     >             *DTGPSIIM-SMZ*SMS*T2OQ*FKDUMMY)
         FACT(11)= 1./SPS2*((SMS-SMZ)*(ZNKWEL*CONJG(GPSI)
     >             -SMZ*T2OQ*FKDUMMY)+ZNKWEL*DTGPSIIM)
         FACT(12)= -1./SPS2*ZETA*(SMZ**2*TOQ*FKDUMMY+ZNKWEL**2*QOT
     >             *CONJG(GPSI))
C
      CALL FKUBL(MS,MZ,MANZ,12,INDQQ,NBG,NZMA,ZMA,FACT,1.,HQ,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H'(CUB)   FUER DIE GLEICHUNG
C     ------------------------
C     A(2,7),A(7,7),A(2,8),A(8,8)
C
C
         FACT(1) = 1./SPS2*T2OQ*FKDUMMY
         FACT(2) = -1./SPS2*TOQ*ZETA*FKDUMMY
         FACT(3) = -1./SPS2*CONJG(GPSI)
         FACT(4) = -ZETA*1./SPS2*QOT*CONJG(GPSI)
C
      CALL FKUBL(MS,MZ,MANZ,4,IDCDC,NBG,NZMA,ZMA,FACT,1.,DHC,DHC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H(QUA)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(2,1),A(2,5),A(2,6),A(7,6),A(8,6)
C
C
         FACT(1) = T0/ZSR*CONJG(R2)
         FACT(2) = ZRHO/ZSR*CONJG(R2)
         FACT(3) = 1./SPS2*(ZNKWEL*CONJG(GPSI)-SMZ*T2OQ*FKDUMMY)
         FACT(4) = SMZ/SPS2*ZETA*TOQ*FKDUMMY
         FACT(5) = 1./SPS2*ZNKWEL*ZETA*QOT*CONJG(GPSI)
C
      CALL FKUBL(MS,MZ,MANZ,5,IDCQ,NBG,NZMA,ZMA,FACT,1.,DHC,HQ)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(QUA) * FACT * H'(CUB)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(1,2),A(5,2),A(3,7),A(4,7),A(6,7),A(3,8),A(4,8),A(6,8)
C
C
         FACT(1) = -1./ZSR*ZRHO*CONJG(R2)
         FACT(2) = (1.-GAMMA)*ZRHO*T0/ZSR*CONJG(R2)
         FACT(3) = SMS/SPS2*T2OQ*FKDUMMY
         FACT(4) = 1./SPS2*(SMS-SMZ)*T2OQ*FKDUMMY
         FACT(5) = SMZ/SPS2*ZETA*TOQ*FKDUMMY
         FACT(6) = 1./SPS2*((MZNQ-SMS)*CONJG(GPSI)+DTGPSOIM)
         FACT(7) = 1./SPS2*((SMZ-SMS)*CONJG(GPSI)+DTGPSOIM)
         FACT(8) = ZNKWEL/SPS2*ZETA*QOT*CONJG(GPSI)
C
      CALL FKUBL(MS,MZ,MANZ,8,IQDC,NBG,NZMA,ZMA,FACT,1.,HQ,DHC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H'(CUB) * FACT * H(CUB)   FUER DIE GLEICHUNGEN
C     -----------------------
C     A(2,7),A(8,7),A(2,8),A(8,8)
C
C
         FACT(1) = ZNKWEL*(0.0,1.0)*CONJG(GPGT)
         FACT(2) = ZNKWEL*ZETA*QOT*(0.0,1.0)*CONJG(GPGT)
         FACT(3) = SMZ/(0.0,1.0)*CONJG(GPGT)
         FACT(4) = SMZ*ZETA*QOT/(0.0,1.0)*CONJG(GPGT)
C
      CALL FKUBL(MS,MZ,MANZ,4,IDCC,NBG,NZMA,ZMA,FACT,1.,DHC,HC)
C
C
C     FAKTOREN FUER DIE FINITEN ELEMENTE DER ANTEILE:
C     H(CUB) * FACT * H'(CUB)   FUER DIE GLEICHUNGEN
C     -----------------------
C      A(7,7),A(2,8),A(7,8),A(8,8)
C
C
         FACT(1) = -1./SPS2*TOQ*DZETA*FKDUMMY
         FACT(2) = 1./SPS2*(CONJG(DSGPSI)+2.*CONJG(GPSI)*DOQDOT)
     >             +2.*DTGPGTM+MZNQ*(0.0,1.0)*CONJG(GPGT)
         FACT(3) = ZNKWEL*ZETA*QOT/(0.0,1.0)*CONJG(GPGT)
         FACT(4) = QOT*(ZETA*SMZ*(0.0,1.0)*CONJG(GPGT)+ZETA*DTGPGTM
     >             -1./SPS2*DZETA*CONJG(GPSI))
C
      CALL FKUBL(MS,MZ,MANZ,4,ICDC,NBG,NZMA,ZMA,FACT,1.,HC,DHC)
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
      RETURN
      END

************************************************************************      
*DECK FKUBL                                                                   
      SUBROUTINE FKUBL(MZ,MS,L,IANZ,INDHG,NBG,NZMA,ZMA,FACT,GEW,H1,H2)        
C                                                                             
C     BESETZT DIE ZUSAMMENGEHOERENDEN UNTERBLOECKE U(MZ,MS)                   
C     DER FOUR.KOEFF. MZ,MS DER UNTERBLOECKE A(IZ,IS).                        
C                (IZ=MOD(INDHG(I)-1,NGL)+1)                                   
C                (IS=   (INDHG(I)-1)/NGL+1)                                   
C                                                                             
      INTEGER INDHG(*),NGL,NBG,NZMA,IND,INDO,INDU,INDN,MZ,MS,IANZ,L           
      COMPLEX ZMA(*),FACT(*)                                                  
      REAL H1(*),H2(*),GEW                                                    
C                                                                             
        NGL=NBG/L                                                             
C                                                                             
      DO 5 J=1,IANZ                                                           
        FACT(J)=FACT(J)*GEW                                                   
    5 CONTINUE                                                                
C                                                                             
      DO 10 I=1,IANZ                                                          
        IND    = ((INDHG(I)-1)/NGL  * L + (MS-1)  ) * NZMA                    
     >           + MOD(INDHG(I)-1,NGL)  * L + (MZ - 1)  + 1                   
                 IZ=MOD(INDHG(I)-1,NGL)+1                                     
                 IS=   (INDHG(I)-1)/NGL+1                                     
C                                                                             
        ZMA(IND)          = ZMA(IND) + H1(2) * FACT(I) * H2(2)                
C                                                                             
   10 CONTINUE                                                                
C                                                                             
      RETURN                                                                  
      END                                                                     
************************************************************************ 
************************************************************************
*DECK SOLV1
      SUBROUTINE SOLV1
C
************************************************************************
**                                                                    **
**    SOLV1 : QR-SOLVER (COMPLEX)                                     **
**            VERSION C,  4.7.91                                      **
**                                                                    **
**    STRUCTURE :                                                     **
**                 SOLV1           (using lapack routines)            **
**                   (ZPOTRF)                                         **
**                   (ZPOTRS)                                         **
**                   (ZGEEV)                                          **
************************************************************************
************************************************************************
C
*CALL COMMAX
*CALL COMPAR
*CALL COMPARE
*CALL COMEQV
*CALL COMPIO
*CALL COMDIM
*CALL CORE1
*CALL COMNIP
C
      INTEGER  LWORK,IPIV(NDIM1)
      REAL     RWORK(2*NDIM1)
      COMPLEX  WORK(2*NDIM1)
C
C
C ... QR - ALGORITHM ...
C
C-------------------------------------------- using lapack routines

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
      DO 200 I=1,NDIM
         IND=(NIP-1)*NBG+I
         SR(IND)=SGI(NIP)
         WIG(IND)=IMAG(WRI(I))
ccc         WRITE(20,*)'SR(IND),WIG(IND),IND=',IND,SR(IND),WIG(IND)
  200 CONTINUE
      RETURN
C
    1 FORMAT('   CONDITION = ',1P,E16.6,0P)
    3 FORMAT(///5X,'MATRIX BMAT1 NOT POSITIVE DEFINIT')
    4 FORMAT(' INFO = ',I4)
   11 FORMAT('   TIME = ',1P,E16.6,0P)
   31 FORMAT(//' HQR/HQZ :  IERR=',I5)
   41 FORMAT(1X,I3,'.-EIGENVALUE:',1P,2E16.6,0P)
   43 FORMAT(3X,'VSHIFT(',I3,') = (',1P,E16.6,',',E16.6,'),')


      END


************************************************************************      
*DECK QRPLOT                                                                  
      SUBROUTINE QRPLOT(WR,WI,NDIM,X,Y)                                       
C           
*CALL COMMAX    
*CALL COMPARE                                                                                                                          
*CALL COMPLOT                                                                                                                                
*CALL COMEQV
*CALL COMGRID                                                                  
*CALL COMEQUI
*CALL COMIOD
      INTEGER  NDIM                                                           
      REAL  WR(*), WI(*)                                                      
      REAL  X(*), Y(*)                                                        
      REAL  GTAE(NDEQ), GEAE(NDEQ), GAM(NDEQ), SMOL(NDEQ)
      INTEGER L,LOR                                                           
      CHARACTER*10 XNAME,YNAME                                                
C                      
      CALL LBLBOT('CSCAS (Version 11) : QR-PLOT',28)                          
      XNAME = 'S'                                                             
      YNAME = 'IM(LAMBDA)'                                                    
C                                                                             
      L = 1                                                                   
      DO  10  I = 1,NDIM 
         IF ((WI(I) .GE. YMIN)  .AND. (WI(I) .LE. YMAX ) .AND.                
     &      (WR(I) .GE. XMIN ) .AND. (WR(I) .LE. XMAX ))  THEN                
               Y(L) = WI(I)                                                   
               X(L) = WR(I)                                                   
               L    = L + 1                                                   
         ENDIF                                                                
   10 CONTINUE                                                                
      L = L - 1                                                               
      WRITE(*,1000)L,XMIN,XMAX,YMIN,YMAX                                     
C      IF (L .LE. 1) RETURN                                                    
C                      
  
      WRITE(*,*) ' PLOT SPECTRUM'  
      CALL NFRAME(1,1,1,XMIN,XMAX,YMIN,YMAX,
     &           'CONTINUOUS SPECTRUM',19,XNAME,1,YNAME,10)                                           
      CALL LPLOT(1,1,1201,X,Y,-L,1,'CONTINUOUS SPECTRUM',19,                   
     &           XNAME,1,YNAME,10)                                            
C                                                                             
      WRITE(*,*) ' PLOT Q-profile'     
      CALL LINCOL(1)                                                
      CALL LPLOT(1,1,1,SGI,Q,-NG,1,' ',1,' ',1,' ',1)                         
C                                                                             
      WRITE(*,*) ' PLOT DENSITY'  
      CALL LINCOL(2)                                                   
      CALL LPLOT(1,1,1,SGI,RHO,-NG,1,' ',1,' ',1,' ',1)
C                                                                             
      DO 20 I = 1,NG
         GTAE(I) = 1./(2.*Q(I)*SQRT(RHO(I)))
	 GAM(I)  = SQRT( GAMMA *  ZT0(I) / (RHO(I)) * (2. + 1./Q(I)) )
         SMOL(I) = SQRT( 2. * GAMMA *  ZT0(I) / RHO(I) + 1. / Q(I)**2)
	 WRITE(*,'(i5,f8.4,6e12.4)') 
     >           I,SGI(I),Q(I),RHO(I),ZT0(I),GTAE(I),GAM(I),SMOL(I) 
 20   CONTINUE
C
      CALL LINCOL(3)
      CALL LPLOT(1,1,1,SGI,GTAE,-NG,1,' ',1,' ',1,' ',1)
      CALL LINCOL(1)
      CALL LPLOT(1,1,1,SGI,GAM,-NG,1,' ',1,' ',1,' ',1)
      CALL LINCOL(0)
C                                                                             
      DO 30 I = 1,NG
         GEAE(I) = 1./(Q(I)*SQRT(RHO(I)))
 30   CONTINUE
C
      CALL LPLOT(1,1,1,SGI,GEAE,-NG,1,' ',1,' ',1,' ',1)
C                                                                             
      RETURN                                                                  
 1000 FORMAT(1X,I5,' VALUES IN WINDOW ',1P,2E12.4,2X,                          
     >       2E12.4,' PLOTTED')                                             
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
