
c file ThEcoli.f
c 2013-05-29 millard@insa-toulouse.fr
c Copyright 2013, INRA
c License: GPL v2





!      subroutine initmod(odeparms)
!       external odeparms
!       double precision parms(446)
!       common /myparms/parms
!       call odeparms(446, parms)
!       return
!      end
!     subroutine derivs (neq, t, y, y2, ydot, yout, ip)
      subroutine derivs (y, y2, ydot)
!      double precision, intent(in), t
       double precision, dimension(0:75), intent(in) :: y
	   double precision, dimension(0:446), intent(in) :: y2
       double precision, dimension(0:75), intent(out) :: ydot
	   
!      double precision yout(*)
       double precision MgADP
       double precision MgATP
       double precision MgFDP
       double precision v_XCH_P
       double precision v_XCH_GLC
       double precision v_XCH_ACE
       double precision v_ACE_OUT
       double precision v_PGI
       double precision v_PFK
       double precision v_FBA
       double precision v_TPI
       double precision v_GDH
       double precision v_PGK
       double precision v_GPM
       double precision v_ENO
       double precision v_PYK
       double precision v_ZWF
       double precision v_PGL
       double precision v_GND
       double precision v_RPE
       double precision v_RPI
       double precision v_X5P_GAP_TKT
       double precision v_F6P_E4P_TKT
       double precision v_S7P_R5P_TKT
       double precision v_F6P_GAP_TAL
       double precision v_S7P_E4P_TAL
       double precision v_FBP
       double precision v_PPC
       double precision v_PCK
       double precision v_PPS
       double precision v_MAD
       double precision v_PDH
       double precision v_GLT
       double precision v_ACN_1
       double precision v_ACN_2
       double precision v_ICD
       double precision v_LPD
       double precision v_SK
       double precision v_SDH
       double precision v_FUMA
       double precision v_MQO
       double precision v_MDH
       double precision v_ACEA
       double precision v_ACEB
       double precision v_ACEK_1
       double precision v_ACEK_2
       double precision v_EDD
       double precision v_EDA
       double precision v_NADPH_req
       double precision v_PNT
       double precision v_ADK
       double precision v_ATP_NGAM
       double precision v_ATP_SYN
       double precision v_CYTBO
       double precision v_NDHI
       double precision v_NDHII
       double precision v_SQR
       double precision v_CYA
       double precision v_DOS
       double precision v_GROWTH
       double precision v_PIT
       double precision v_ACK
       double precision v_ACS
       double precision v_PTA
       double precision v_PTS_0
       double precision v_PTS_4
       double precision v_GLC_feed
       double precision v_XCH_PYR
       double precision v_XCH_SUC
       double precision v_PTS_1
       double precision v_PTS_2
       double precision v_PTS_3
!       integer neq, ip(*)
       double precision ACCOA
       double precision ACEx
       double precision ACO
       double precision ACP
       double precision ADP
       double precision AKG
       double precision AMP
       double precision ASP
       double precision ATP
       double precision BPG
       double precision CAMP
       double precision CIT
       double precision COA
       double precision CYS
       double precision DAP
       double precision E4P
       double precision ei
       double precision eiia
       double precision eiiaP
       double precision eiicb
       double precision eiicbP
       double precision eiP
       double precision F6P
       double precision FDP
       double precision FUM
       double precision G6P
       double precision GAP
       double precision GL6P
       double precision GLCx
       double precision GLCp
       double precision ACE
       double precision ACEp
       double precision Px
       double precision Pp
       double precision GLX
       double precision Hc
       double precision Hp
       double precision FAD
       double precision FADH2
       double precision O2
       double precision HCO3
       double precision hpr
       double precision hprP
       double precision icd
       double precision icdP
       double precision ICIT
       double precision KDPG
       double precision MAL
       double precision MG
       double precision MN
       double precision NAD
       double precision NADH
       double precision NADP
       double precision NADPH
       double precision OAA
       double precision P
       double precision PEP
       double precision PGA2
       double precision PGA3
       double precision PGN
       double precision PYR
       double precision PYRx
       double precision Q
       double precision QH2
       double precision R5P
       double precision RU5P
       double precision S7P
       double precision SUC
       double precision SUCCOA
       double precision SUCx
       double precision tal
       double precision talC3
       double precision tkt
       double precision tktC2
       double precision X5P
       double precision FEED
	   
       double precision v_XCH_P_P
       double precision v_XCH_GLC_P
       double precision v_XCH_ACE_P
       double precision v_XCH_A
       double precision v_ACE_OUT_k
       double precision v_ACE_OUT_Keq
       double precision v_PGI_Keq
       double precision v_PGI_KmF6P
       double precision v_PGI_KmG6P
       double precision v_PGI_KmPEP
       double precision v_PGI_KmPGN
       double precision v_PGI_Vmax
       double precision v_PFK_KefrADP
       double precision v_PFK_KefrPEP
       double precision v_PFK_KeftADP
       double precision v_PFK_KeftPEP
       double precision v_PFK_Keq
       double precision v_PFK_KirADP
       double precision v_PFK_KirATP
       double precision v_PFK_KirF6P
       double precision v_PFK_KirFDP
       double precision v_PFK_KitADP
       double precision v_PFK_KitATP
       double precision v_PFK_KitF6P
       double precision v_PFK_KitFDP
       double precision v_PFK_KmrADP
       double precision v_PFK_KmrATPMg
       double precision v_PFK_KmrF6P
       double precision v_PFK_KmrFDP
       double precision v_PFK_KmtADP
       double precision v_PFK_KmtATPMg
       double precision v_PFK_KmtF6P
       double precision v_PFK_KmtFDP
       double precision v_PFK_L0
       double precision v_PFK_Vmax
       double precision v_PFK_Wr
       double precision v_PFK_Wt
       double precision v_PFK_n
       double precision v_FBA_Keq
       double precision v_FBA_KmDAP
       double precision v_FBA_KmFDP
       double precision v_FBA_KmGAP
       double precision v_FBA_KmPEP
       double precision v_FBA_Vmax
       double precision v_TPI_Keq
       double precision v_TPI_KmDAP
       double precision v_TPI_KmGAP
       double precision v_TPI_Vmax
       double precision v_GDH_Keq
       double precision v_GDH_KmBPG
       double precision v_GDH_KmGAP
       double precision v_GDH_KmNAD
       double precision v_GDH_KmNADH
       double precision v_GDH_KmP
       double precision v_GDH_Vmax
       double precision v_PGK_Keq
       double precision v_PGK_KmADPMg
       double precision v_PGK_KmATPMg
       double precision v_PGK_KmBPG
       double precision v_PGK_KmPGA3
       double precision v_PGK_Vmax
       double precision v_GPM_Keq
       double precision v_GPM_KmPGA2
       double precision v_GPM_KmPGA3
       double precision v_GPM_Vmax
       double precision v_ENO_Keq
       double precision v_ENO_KmPEP
       double precision v_ENO_KmPGA2
       double precision v_ENO_Vmax
       double precision v_PYK_KefrFDP
       double precision v_PYK_KefrG6P
       double precision v_PYK_KefrGL6P
       double precision v_PYK_KefrR5P
       double precision v_PYK_KefrRU5P
       double precision v_PYK_KefrS7P
       double precision v_PYK_KefrX5P
       double precision v_PYK_KeftATP
       double precision v_PYK_KeftSUCCOA
       double precision v_PYK_KirADP
       double precision v_PYK_KirATP
       double precision v_PYK_KirPEP
       double precision v_PYK_KirPYR
       double precision v_PYK_KirPyrATP
       double precision v_PYK_KitADP
       double precision v_PYK_KitATP
       double precision v_PYK_KitPEP
       double precision v_PYK_KitPYR
       double precision v_PYK_KitPyrATP
       double precision v_PYK_KmrADPMg
       double precision v_PYK_KmrPEP
       double precision v_PYK_KmtADPMg
       double precision v_PYK_KmtPEP
       double precision v_PYK_L0
       double precision v_PYK_Vmax
       double precision v_PYK_n
       double precision v_ZWF_KdG6P
       double precision v_ZWF_KdGL6P
       double precision v_ZWF_Keq
       double precision v_ZWF_KmG6P
       double precision v_ZWF_KmGL6P
       double precision v_ZWF_KmNADP
       double precision v_ZWF_KmNADPH
       double precision v_ZWF_Vmax
       double precision v_PGL_Keq
       double precision v_PGL_KiG6P
       double precision v_PGL_KmGL6P
       double precision v_PGL_KmPGN
       double precision v_PGL_Vmax
       double precision v_GND_KdHCO3
       double precision v_GND_KdHCO3NADPH
       double precision v_GND_KdNADP
       double precision v_GND_KdNADPH
       double precision v_GND_KdRu5P
       double precision v_GND_KefATP
       double precision v_GND_KefFbP
       double precision v_GND_KefNADPATP
       double precision v_GND_KefNADPFbP
       double precision v_GND_Keq
       double precision v_GND_KmHCO3
       double precision v_GND_KmNADP
       double precision v_GND_KmNADPH
       double precision v_GND_KmPGN
       double precision v_GND_KmRU5P
       double precision v_GND_Vmax
       double precision v_RPE_Keq
       double precision v_RPE_KmRU5P
       double precision v_RPE_KmX5P
       double precision v_RPE_Vmax
       double precision v_RPI_Keq
       double precision v_RPI_KmE4P
       double precision v_RPI_KmR5P
       double precision v_RPI_KmRU5P
       double precision v_RPI_Vmax
       double precision v_X5P_GAP_TKT_Keq
       double precision v_X5P_GAP_TKT_kcat
       double precision v_F6P_E4P_TKT_Keq
       double precision v_F6P_E4P_TKT_kcat
       double precision v_S7P_R5P_TKT_Keq
       double precision v_S7P_R5P_TKT_kcat
       double precision v_F6P_GAP_TAL_Keq
       double precision v_F6P_GAP_TAL_kcat
       double precision v_S7P_E4P_TAL_Keq
       double precision v_S7P_E4P_TAL_kcat
       double precision v_FBP_KirAMP
       double precision v_FBP_KirAMPFDP
       double precision v_FBP_KirF6P
       double precision v_FBP_KirF6PMg
       double precision v_FBP_KirFDP
       double precision v_FBP_KirFDPMg
       double precision v_FBP_KirFDPMgMg
       double precision v_FBP_KirP
       double precision v_FBP_KirPF6P
       double precision v_FBP_KirPF6PMg
       double precision v_FBP_KirPMg
       double precision v_FBP_KitAMP
       double precision v_FBP_KitAMPFDP
       double precision v_FBP_KitF6P
       double precision v_FBP_KitF6PMg
       double precision v_FBP_KitFDP
       double precision v_FBP_KitFDPMg
       double precision v_FBP_KitFDPMgMg
       double precision v_FBP_KitP
       double precision v_FBP_KitPF6P
       double precision v_FBP_KitPF6PMg
       double precision v_FBP_KitPMg
       double precision v_FBP_KmrFDP
       double precision v_FBP_KmrMg
       double precision v_FBP_KmtFDP
       double precision v_FBP_KmtMg
       double precision v_FBP_L0
       double precision v_FBP_Vmax
       double precision v_FBP_n
       double precision v_PPC_KdrOAA
       double precision v_PPC_KdrPEP
       double precision v_PPC_KdtOAA
       double precision v_PPC_KdtPEP
       double precision v_PPC_KefrACCOA
       double precision v_PPC_KefrASP
       double precision v_PPC_KefrCIT
       double precision v_PPC_KefrCYS
       double precision v_PPC_KefrFDP
       double precision v_PPC_KefrFDPACCOA
       double precision v_PPC_KefrFUM
       double precision v_PPC_KefrMAL
       double precision v_PPC_KefrSUC
       double precision v_PPC_KeftACCOA
       double precision v_PPC_KeftASP
       double precision v_PPC_KeftCIT
       double precision v_PPC_KeftCYS
       double precision v_PPC_KeftFDP
       double precision v_PPC_KeftFDPACCOA
       double precision v_PPC_KeftFUM
       double precision v_PPC_KeftMAL
       double precision v_PPC_KeftSUC
       double precision v_PPC_Keq
       double precision v_PPC_KmrHCO3
       double precision v_PPC_KmrOAA
       double precision v_PPC_KmrP
       double precision v_PPC_KmrPEP
       double precision v_PPC_KmtHCO3
       double precision v_PPC_KmtOAA
       double precision v_PPC_KmtP
       double precision v_PPC_KmtPEP
       double precision v_PPC_L0
       double precision v_PPC_Vmax
       double precision v_PPC_n
       double precision v_PCK_Keq
       double precision v_PCK_KmADP
       double precision v_PCK_KmATP
       double precision v_PCK_KmHCO3
       double precision v_PCK_KmOAA
       double precision v_PCK_KmPEP
       double precision v_PCK_Vmax
       double precision v_PPS_KdAMP
       double precision v_PPS_KdATPMgPPS
       double precision v_PPS_KdMg
       double precision v_PPS_KdP
       double precision v_PPS_KdPEP
       double precision v_PPS_KdPYR
       double precision v_PPS_KefADP
       double precision v_PPS_KefAKG
       double precision v_PPS_KefATP
       double precision v_PPS_KefOAA
       double precision v_PPS_Keq
       double precision v_PPS_KmAMP
       double precision v_PPS_KmATPMg
       double precision v_PPS_KmP
       double precision v_PPS_KmPEP
       double precision v_PPS_KmPYR
       double precision v_PPS_Vmax
       double precision v_PPS_W
       double precision v_PPS_alpha
       double precision v_MAD_KefrACCOA
       double precision v_MAD_KefrASP
       double precision v_MAD_KefrATP
       double precision v_MAD_KefrCOA
       double precision v_MAD_KeftACCOA
       double precision v_MAD_KeftASP
       double precision v_MAD_KeftATP
       double precision v_MAD_KeftCOA
       double precision v_MAD_KirNAD
       double precision v_MAD_KitNAD
       double precision v_MAD_KmrMAL
       double precision v_MAD_KmrMg
       double precision v_MAD_KmrMn
       double precision v_MAD_KmrNAD
       double precision v_MAD_KmtMAL
       double precision v_MAD_KmtMg
       double precision v_MAD_KmtMn
       double precision v_MAD_KmtNAD
       double precision v_MAD_L0
       double precision v_MAD_Vmax
       double precision v_MAD_n
       double precision v_PDH_Keq
       double precision v_PDH_KmACCOA
       double precision v_PDH_KmCOA
       double precision v_PDH_KmHCO3
       double precision v_PDH_KmNAD
       double precision v_PDH_KmNADH
       double precision v_PDH_KmPYR
       double precision v_PDH_Vmax
       double precision v_GLT_KdACCOA0
       double precision v_GLT_KdcsCIT
       double precision v_GLT_KdcsCOA
       double precision v_GLT_KdcsOAA
       double precision v_GLT_Keq
       double precision v_GLT_Ki1AKG
       double precision v_GLT_Ki1NADH
       double precision v_GLT_Ki2AKG
       double precision v_GLT_Ki2NADH
       double precision v_GLT_KiATP
       double precision v_GLT_KmACCOA0
       double precision v_GLT_KmOAA0
       double precision v_GLT_KmcsCIT
       double precision v_GLT_KmcsCOA
       double precision v_GLT_Vmax
       double precision v_ACN_1_Keq
       double precision v_ACN_1_KmACO
       double precision v_ACN_1_KmCIT
       double precision v_ACN_1_KmICIT
       double precision v_ACN_1_Vmax
       double precision v_ACN_2_Keq
       double precision v_ACN_2_KmACO
       double precision v_ACN_2_KmCIT
       double precision v_ACN_2_KmICIT
       double precision v_ACN_2_Vmax
       double precision v_ICD_Keq
       double precision v_ICD_KmAKG
       double precision v_ICD_KmICIT
       double precision v_ICD_KmNADP
       double precision v_ICD_KmNADPH
       double precision v_ICD_kcat
       double precision v_LPD_KdAKG
       double precision v_LPD_KmAKG
       double precision v_LPD_KmCOA
       double precision v_LPD_KmNAD
       double precision v_LPD_Vmax
       double precision v_LPD_alpha
       double precision v_SK_Keq
       double precision v_SK_KmADP
       double precision v_SK_KmATP
       double precision v_SK_KmCOA
       double precision v_SK_KmP
       double precision v_SK_KmSUC
       double precision v_SK_KmSUCCOA
       double precision v_SK_Vmax
       double precision v_SDH_KefFUM
       double precision v_SDH_KefSUC
       double precision v_SDH_Keq
       double precision v_SDH_KmFUM
       double precision v_SDH_KmQ
       double precision v_SDH_KmQH2
       double precision v_SDH_KmSUC
       double precision v_SDH_Vmax
       double precision v_FUMA_Keq
       double precision v_FUMA_KmFUM
       double precision v_FUMA_KmMAL
       double precision v_FUMA_Vmax
       double precision v_MQO_Keq
       double precision v_MQO_KmMAL
       double precision v_MQO_KmOAA
       double precision v_MQO_KmQ
       double precision v_MQO_KmQH2
       double precision v_MQO_Vmax
       double precision v_MDH_Keq
       double precision v_MDH_KiNAD
       double precision v_MDH_KiNADH
       double precision v_MDH_KiOAA
       double precision v_MDH_KmMAL
       double precision v_MDH_KmNAD
       double precision v_MDH_KmNADH
       double precision v_MDH_KmOAA
       double precision v_MDH_Vmax
       double precision v_ACEA_KdICITsuc
       double precision v_ACEA_KdPEP
       double precision v_ACEA_KdPEPglx
       double precision v_ACEA_KdPEPicit
       double precision v_ACEA_KdPGA3
       double precision v_ACEA_KdSUC
       double precision v_ACEA_Keq
       double precision v_ACEA_KmGLX
       double precision v_ACEA_KmICIT
       double precision v_ACEA_KmSUC
       double precision v_ACEA_Vmax
       double precision v_ACEB_Keq
       double precision v_ACEB_KmACCOA
       double precision v_ACEB_KmCOA
       double precision v_ACEB_KmGLX
       double precision v_ACEB_KmMAL
       double precision v_ACEB_Vmax
       double precision v_ACEK_1_Keq
       double precision v_ACEK_1_k
       double precision v_ACEK_2_Keq
       double precision v_ACEK_2_k
       double precision v_EDD_Keq
       double precision v_EDD_KmKDPG
       double precision v_EDD_KmPGN
       double precision v_EDD_Vmax
       double precision v_EDA_Keq
       double precision v_EDA_KmGAP
       double precision v_EDA_KmKDPG
       double precision v_EDA_KmPYR
       double precision v_EDA_Vmax
       double precision v_NDHI_Keq
       double precision v_NDHI_k
       double precision v_NDHII_Keq
       double precision v_NDHII_k
       double precision v_NADPH_req_Keq
       double precision v_NADPH_req_k
       double precision v_PNT_Keq
       double precision v_PNT_k
       double precision v_SQR_Keq
       double precision v_SQR_k
       double precision v_CYTBO_Keq
       double precision v_CYTBO_k
       double precision v_ADK_Keq
       double precision v_ADK_k
       double precision v_ATP_SYN_Keq
       double precision v_ATP_SYN_k
       double precision v_ATP_NGAM_Keq
       double precision v_ATP_NGAM_k
       double precision v_CYA_k
       double precision v_CYA_Keq
       double precision v_CYA_KaeiiaP
       double precision v_DOS_k
       double precision v_DOS_Keq
       double precision v_ACK_Keq
       double precision v_ACK_KmACE
       double precision v_ACK_KmACP
       double precision v_ACK_KmADP
       double precision v_ACK_KmATP
       double precision v_ACK_Vmax
       double precision v_ACS_KmACE
       double precision v_ACS_KmATP
       double precision v_ACS_KmCOA
       double precision v_ACS_Vmax
       double precision v_PTA_Keq
       double precision v_PTA_KiACCOA
       double precision v_PTA_KiACP
       double precision v_PTA_KiCOA
       double precision v_PTA_KiP
       double precision v_PTA_KmACP
       double precision v_PTA_KmP
       double precision v_PTA_Vmax
       double precision v_PTS_0_KmPEP
       double precision v_PTS_0_KmPYR
       double precision v_PTS_0_kF
       double precision v_PTS_0_kR
       double precision v_PTS_1_k1
       double precision v_PTS_1_k2
       double precision v_PTS_2_k1
       double precision v_PTS_2_k2
       double precision v_PTS_3_k1
       double precision v_PTS_3_k2
       double precision v_PTS_4_KmG6P
       double precision v_PTS_4_KmGLC
       double precision v_PTS_4_kF
       double precision v_PTS_4_kR
       double precision v_XCH_PYR_KmPYRx
       double precision v_XCH_PYR_Vmax
       double precision v_XCH_SUC_KmSUCx
       double precision v_XCH_SUC_Vmax
       double precision r_KdADPMg
       double precision r_KdATPMg
       double precision r_KdFDPMg
       double precision ENV_vol
       double precision PER_vol
       double precision v_GROWTH_KmG6P
       double precision v_GROWTH_KmF6P
       double precision v_GROWTH_KmGAP
       double precision v_GROWTH_KmR5P
       double precision v_GROWTH_KmE4P
       double precision v_GROWTH_KmPGA3
       double precision v_GROWTH_KmPEP
       double precision v_GROWTH_KmPYR
       double precision v_GROWTH_KmOAA
       double precision v_GROWTH_KmAKG
       double precision v_GROWTH_KmACCOA
       double precision v_GROWTH_KmNADPH
       double precision v_GROWTH_KmNAD
       double precision v_GROWTH_KmATP
       double precision v_GROWTH_Vmax
       double precision v_PIT_k
       double precision v_PIT_Kr
       double precision v_PIT_KmPp
       double precision v_PIT_KmP
	   
       ACCOA = y(0)
       ACEx = y(1)
       ACO = y(2)
       ACP = y(3)
       ADP = y(4)
       AKG = y(5)
       AMP = y(6)
       ASP = y(7)
       ATP = y(8)
       BPG = y(9)
       CAMP = y(10)
       CIT = y(11)
       COA = y(12)
       CYS = y(13)
       DAP = y(14)
       E4P = y(15)
       ei = y(16)
       eiia = y(17)
       eiiaP = y(18)
       eiicb = y(19)
       eiicbP = y(20)
       eiP = y(21)
       F6P = y(22)
       FDP = y(23)
       FUM = y(24)
       G6P = y(25)
       GAP = y(26)
       GL6P = y(27)
       GLCx = y(28)
       GLX = y(29)
       HCO3 = y(30)
       hpr = y(31)
       hprP = y(32)
       icd = y(33)
       icdP = y(34)
       ICIT = y(35)
       KDPG = y(36)
       MAL = y(37)
       MG = y(38)
       MN = y(39)
       NAD = y(40)
       NADH = y(41)
       NADP = y(42)
       NADPH = y(43)
       OAA = y(44)
       P = y(45)
       PEP = y(46)
       PGA2 = y(47)
       PGA3 = y(48)
       PGN = y(49)
       PYR = y(50)
       PYRx = y(51)
       Q = y(52)
       QH2 = y(53)
       R5P = y(54)
       RU5P = y(55)
       S7P = y(56)
       SUC = y(57)
       SUCCOA = y(58)
       SUCx = y(59)
       tal = y(60)
       talC3 = y(61)
       tkt = y(62)
       tktC2 = y(63)
       X5P = y(64)
       Px = y(65)
       Pp = y(66)
       GLCp = y(67)
       ACEp = y(68)
       ACE = y(69)
       Hc = y(70)
       Hp = y(71)
       FAD = y(72)
       FADH2 = y(73)
       O2 = y(74)
       FEED = y(75)
	   
	   v_PGI_Keq=y2(1)
       v_PGI_KmF6P=y2(2)
       v_PGI_KmG6P=y2(3)
       v_PGI_KmPEP=y2(4)
       v_PGI_KmPGN=y2(5)
       v_PGI_Vmax=y2(6)
       v_PFK_KefrADP=y2(7)
       v_PFK_KefrPEP=y2(8)
       v_PFK_KeftADP=y2(9)
       v_PFK_KeftPEP=y2(10)
       v_PFK_Keq=y2(11)
       v_PFK_KirADP=y2(12)
       v_PFK_KirATP=y2(13)
       v_PFK_KirF6P=y2(14)
       v_PFK_KirFDP=y2(15)
       v_PFK_KitADP=y2(16)
       v_PFK_KitATP=y2(17)
       v_PFK_KitF6P=y2(18)
       v_PFK_KitFDP=y2(19)
       v_PFK_KmrADP=y2(20)
       v_PFK_KmrATPMg=y2(21)
       v_PFK_KmrF6P=y2(22)
       v_PFK_KmrFDP=y2(23)
       v_PFK_KmtADP=y2(24)
       v_PFK_KmtATPMg=y2(25)
       v_PFK_KmtF6P=y2(26)
       v_PFK_KmtFDP=y2(27)
       v_PFK_L0=y2(28)
       v_PFK_Vmax=y2(29)
       v_PFK_Wr=y2(30)
       v_PFK_Wt=y2(31)
       v_PFK_n=y2(32)
       v_FBA_Keq=y2(33)
       v_FBA_KmDAP=y2(34)
       v_FBA_KmFDP=y2(35)
       v_FBA_KmGAP=y2(36)
       v_FBA_KmPEP=y2(37)
       v_FBA_Vmax=y2(38)
       v_TPI_Keq=y2(39)
       v_TPI_KmDAP=y2(40)
       v_TPI_KmGAP=y2(41)
       v_TPI_Vmax=y2(42)
       v_GDH_Keq=y2(43)
       v_GDH_KmBPG=y2(44)
       v_GDH_KmGAP=y2(45)
       v_GDH_KmNAD=y2(46)
       v_GDH_KmNADH=y2(47)
       v_GDH_KmP=y2(48)
       v_GDH_Vmax=y2(49)
       v_PGK_Keq=y2(50)
       v_PGK_KmADPMg=y2(51)
       v_PGK_KmATPMg=y2(52)
       v_PGK_KmBPG=y2(53)
       v_PGK_KmPGA3=y2(54)
       v_PGK_Vmax=y2(55)
       v_GPM_Keq=y2(56)
       v_GPM_KmPGA2=y2(57)
       v_GPM_KmPGA3=y2(58)
       v_GPM_Vmax=y2(59)
       v_ENO_Keq=y2(60)
       v_ENO_KmPEP=y2(61)
       v_ENO_KmPGA2=y2(62)
       v_ENO_Vmax=y2(63)
       v_PYK_KefrFDP=y2(64)
       v_PYK_KefrG6P=y2(65)
       v_PYK_KefrGL6P=y2(66)
       v_PYK_KefrR5P=y2(67)
       v_PYK_KefrRU5P=y2(68)
       v_PYK_KefrS7P=y2(69)
       v_PYK_KefrX5P=y2(70)
       v_PYK_KeftATP=y2(71)
       v_PYK_KeftSUCCOA=y2(72)
       v_PYK_KirADP=y2(73)
       v_PYK_KirATP=y2(74)
       v_PYK_KirPEP=y2(75)
       v_PYK_KirPYR=y2(76)
       v_PYK_KirPyrATP=y2(77)
       v_PYK_KitADP=y2(78)
       v_PYK_KitATP=y2(79)
       v_PYK_KitPEP=y2(80)
       v_PYK_KitPYR=y2(81)
       v_PYK_KitPyrATP=y2(82)
       v_PYK_KmrADPMg=y2(83)
       v_PYK_KmrPEP=y2(84)
       v_PYK_KmtADPMg=y2(85)
       v_PYK_KmtPEP=y2(86)
       v_PYK_L0=y2(87)
       v_PYK_Vmax=y2(88)
       v_PYK_n=y2(89)
       v_ZWF_KdG6P=y2(90)
       v_ZWF_KdGL6P=y2(91)
       v_ZWF_Keq=y2(92)
       v_ZWF_KmG6P=y2(93)
       v_ZWF_KmGL6P=y2(94)
       v_ZWF_KmNADP=y2(95)
       v_ZWF_KmNADPH=y2(96)
       v_ZWF_Vmax=y2(97)
       v_PGL_Keq=y2(98)
       v_PGL_KiG6P=y2(99)
       v_PGL_KmGL6P=y2(100)
       v_PGL_KmPGN=y2(101)
       v_PGL_Vmax=y2(102)
       v_GND_KdHCO3=y2(103)
       v_GND_KdHCO3NADPH=y2(104)
       v_GND_KdNADP=y2(105)
       v_GND_KdNADPH=y2(106)
       v_GND_KdRu5P=y2(107)
       v_GND_KefATP=y2(108)
       v_GND_KefFbP=y2(109)
       v_GND_KefNADPATP=y2(110)
       v_GND_KefNADPFbP=y2(111)
       v_GND_Keq=y2(112)
       v_GND_KmHCO3=y2(113)
       v_GND_KmNADP=y2(114)
       v_GND_KmNADPH=y2(115)
       v_GND_KmPGN=y2(116)
       v_GND_KmRU5P=y2(117)
       v_GND_Vmax=y2(118)
       v_RPE_Keq=y2(119)
       v_RPE_KmRU5P=y2(120)
       v_RPE_KmX5P=y2(121)
       v_RPE_Vmax=y2(122)
       v_RPI_Keq=y2(123)
       v_RPI_KmE4P=y2(124)
       v_RPI_KmR5P=y2(125)
       v_RPI_KmRU5P=y2(126)
       v_RPI_Vmax=y2(127)
       v_X5P_GAP_TKT_Keq=y2(128)
       v_X5P_GAP_TKT_kcat=y2(129)
       v_F6P_E4P_TKT_Keq=y2(130)
       v_F6P_E4P_TKT_kcat=y2(131)
       v_S7P_R5P_TKT_Keq=y2(132)
       v_S7P_R5P_TKT_kcat=y2(133)
       v_F6P_GAP_TAL_Keq=y2(134)
       v_F6P_GAP_TAL_kcat=y2(135)
       v_S7P_E4P_TAL_Keq=y2(136)
       v_S7P_E4P_TAL_kcat=y2(137)
       v_FBP_KirAMP=y2(138)
       v_FBP_KirAMPFDP=y2(139)
       v_FBP_KirF6P=y2(140)
       v_FBP_KirF6PMg=y2(141)
       v_FBP_KirFDP=y2(142)
       v_FBP_KirFDPMg=y2(143)
       v_FBP_KirFDPMgMg=y2(144)
       v_FBP_KirP=y2(145)
       v_FBP_KirPF6P=y2(146)
       v_FBP_KirPF6PMg=y2(147)
       v_FBP_KirPMg=y2(148)
       v_FBP_KitAMP=y2(149)
       v_FBP_KitAMPFDP=y2(150)
       v_FBP_KitF6P=y2(151)
       v_FBP_KitF6PMg=y2(152)
       v_FBP_KitFDP=y2(153)
       v_FBP_KitFDPMg=y2(154)
       v_FBP_KitFDPMgMg=y2(155)
       v_FBP_KitP=y2(156)
       v_FBP_KitPF6P=y2(157)
       v_FBP_KitPF6PMg=y2(158)
       v_FBP_KitPMg=y2(159)
       v_FBP_KmrFDP=y2(160)
       v_FBP_KmrMg=y2(161)
       v_FBP_KmtFDP=y2(162)
       v_FBP_KmtMg=y2(163)
       v_FBP_L0=y2(164)
       v_FBP_Vmax=y2(165)
       v_FBP_n=y2(166)
       v_PPC_KdrOAA=y2(167)
       v_PPC_KdrPEP=y2(168)
       v_PPC_KdtOAA=y2(169)
       v_PPC_KdtPEP=y2(170)
       v_PPC_KefrACCOA=y2(171)
       v_PPC_KefrASP=y2(172)
       v_PPC_KefrCIT=y2(173)
       v_PPC_KefrCYS=y2(174)
       v_PPC_KefrFDP=y2(175)
       v_PPC_KefrFDPACCOA=y2(176)
       v_PPC_KefrFUM=y2(177)
       v_PPC_KefrMAL=y2(178)
       v_PPC_KefrSUC=y2(179)
       v_PPC_KeftACCOA=y2(180)
       v_PPC_KeftASP=y2(181)
       v_PPC_KeftCIT=y2(182)
       v_PPC_KeftCYS=y2(183)
       v_PPC_KeftFDP=y2(184)
       v_PPC_KeftFDPACCOA=y2(185)
       v_PPC_KeftFUM=y2(186)
       v_PPC_KeftMAL=y2(187)
       v_PPC_KeftSUC=y2(188)
       v_PPC_Keq=y2(189)
       v_PPC_KmrHCO3=y2(190)
       v_PPC_KmrOAA=y2(191)
       v_PPC_KmrP=y2(192)
       v_PPC_KmrPEP=y2(193)
       v_PPC_KmtHCO3=y2(194)
       v_PPC_KmtOAA=y2(195)
       v_PPC_KmtP=y2(196)
       v_PPC_KmtPEP=y2(197)
       v_PPC_L0=y2(198)
       v_PPC_Vmax=y2(199)
       v_PPC_n=y2(200)
       v_PCK_Keq=y2(201)
       v_PCK_KmADP=y2(202)
       v_PCK_KmATP=y2(203)
       v_PCK_KmHCO3=y2(204)
       v_PCK_KmOAA=y2(205)
       v_PCK_KmPEP=y2(206)
       v_PCK_Vmax=y2(207)
       v_PPS_KdAMP=y2(208)
       v_PPS_KdATPMgPPS=y2(209)
       v_PPS_KdMg=y2(210)
       v_PPS_KdP=y2(211)
       v_PPS_KdPEP=y2(212)
       v_PPS_KdPYR=y2(213)
       v_PPS_KefADP=y2(214)
       v_PPS_KefAKG=y2(215)
       v_PPS_KefATP=y2(216)
       v_PPS_KefOAA=y2(217)
       v_PPS_Keq=y2(218)
       v_PPS_KmAMP=y2(219)
       v_PPS_KmATPMg=y2(220)
       v_PPS_KmP=y2(221)
       v_PPS_KmPEP=y2(222)
       v_PPS_KmPYR=y2(223)
       v_PPS_Vmax=y2(224)
       v_PPS_W=y2(225)
       v_PPS_alpha=y2(226)
       v_MAD_KefrACCOA=y2(227)
       v_MAD_KefrASP=y2(228)
       v_MAD_KefrATP=y2(229)
       v_MAD_KefrCOA=y2(230)
       v_MAD_KeftACCOA=y2(231)
       v_MAD_KeftASP=y2(232)
       v_MAD_KeftATP=y2(233)
       v_MAD_KeftCOA=y2(234)
       v_MAD_KirNAD=y2(235)
       v_MAD_KitNAD=y2(236)
       v_MAD_KmrMAL=y2(237)
       v_MAD_KmrMg=y2(238)
       v_MAD_KmrMn=y2(239)
       v_MAD_KmrNAD=y2(240)
       v_MAD_KmtMAL=y2(241)
       v_MAD_KmtMg=y2(242)
       v_MAD_KmtMn=y2(243)
       v_MAD_KmtNAD=y2(244)
       v_MAD_L0=y2(245)
       v_MAD_Vmax=y2(246)
       v_MAD_n=y2(247)
       v_PDH_Keq=y2(248)
       v_PDH_KmACCOA=y2(249)
       v_PDH_KmCOA=y2(250)
       v_PDH_KmHCO3=y2(251)
       v_PDH_KmNAD=y2(252)
       v_PDH_KmNADH=y2(253)
       v_PDH_KmPYR=y2(254)
       v_PDH_Vmax=y2(255)
       v_GLT_KdACCOA0=y2(256)
       v_GLT_KdcsCIT=y2(257)
       v_GLT_KdcsCOA=y2(258)
       v_GLT_KdcsOAA=y2(259)
       v_GLT_Keq=y2(260)
       v_GLT_Ki1AKG=y2(261)
       v_GLT_Ki1NADH=y2(262)
       v_GLT_Ki2AKG=y2(263)
       v_GLT_Ki2NADH=y2(264)
       v_GLT_KiATP=y2(265)
       v_GLT_KmACCOA0=y2(266)
       v_GLT_KmOAA0=y2(267)
       v_GLT_KmcsCIT=y2(268)
       v_GLT_KmcsCOA=y2(269)
       v_GLT_Vmax=y2(270)
       v_ACN_1_Keq=y2(271)
       v_ACN_1_KmACO=y2(272)
       v_ACN_1_KmCIT=y2(273)
       v_ACN_1_KmICIT=y2(274)
       v_ACN_1_Vmax=y2(275)
       v_ACN_2_Keq=y2(276)
       v_ACN_2_KmACO=y2(277)
       v_ACN_2_KmCIT=y2(278)
       v_ACN_2_KmICIT=y2(279)
       v_ACN_2_Vmax=y2(280)
       v_ICD_Keq=y2(281)
       v_ICD_KmAKG=y2(282)
       v_ICD_KmICIT=y2(283)
       v_ICD_KmNADP=y2(284)
       v_ICD_KmNADPH=y2(285)
       v_ICD_kcat=y2(286)
       v_LPD_KdAKG=y2(287)
       v_LPD_KmAKG=y2(288)
       v_LPD_KmCOA=y2(289)
       v_LPD_KmNAD=y2(290)
       v_LPD_Vmax=y2(291)
       v_LPD_alpha=y2(292)
       v_SK_Keq=y2(293)
       v_SK_KmADP=y2(294)
       v_SK_KmATP=y2(295)
       v_SK_KmCOA=y2(296)
       v_SK_KmP=y2(297)
       v_SK_KmSUC=y2(298)
       v_SK_KmSUCCOA=y2(299)
       v_SK_Vmax=y2(300)
       v_SDH_KefFUM=y2(301)
       v_SDH_KefSUC=y2(302)
       v_SDH_Keq=y2(303)
       v_SDH_KmFUM=y2(304)
       v_SDH_KmQ=y2(305)
       v_SDH_KmQH2=y2(306)
       v_SDH_KmSUC=y2(307)
       v_SDH_Vmax=y2(308)
       v_FUMA_Keq=y2(309)
       v_FUMA_KmFUM=y2(310)
       v_FUMA_KmMAL=y2(311)
       v_FUMA_Vmax=y2(312)
       v_MQO_Keq=y2(313)
       v_MQO_KmMAL=y2(314)
       v_MQO_KmOAA=y2(315)
       v_MQO_KmQ=y2(316)
       v_MQO_KmQH2=y2(317)
       v_MQO_Vmax=y2(318)
       v_MDH_Keq=y2(319)
       v_MDH_KiNAD=y2(320)
       v_MDH_KiNADH=y2(321)
       v_MDH_KiOAA=y2(322)
       v_MDH_KmMAL=y2(323)
       v_MDH_KmNAD=y2(324)
       v_MDH_KmNADH=y2(325)
       v_MDH_KmOAA=y2(326)
       v_MDH_Vmax=y2(327)
       v_ACEA_KdICITsuc=y2(328)
       v_ACEA_KdPEP=y2(329)
       v_ACEA_KdPEPglx=y2(330)
       v_ACEA_KdPEPicit=y2(331)
       v_ACEA_KdPGA3=y2(332)
       v_ACEA_KdSUC=y2(333)
       v_ACEA_Keq=y2(334)
       v_ACEA_KmGLX=y2(335)
       v_ACEA_KmICIT=y2(336)
       v_ACEA_KmSUC=y2(337)
       v_ACEA_Vmax=y2(338)
       v_ACEB_Keq=y2(339)
       v_ACEB_KmACCOA=y2(340)
       v_ACEB_KmCOA=y2(341)
       v_ACEB_KmGLX=y2(342)
       v_ACEB_KmMAL=y2(343)
       v_ACEB_Vmax=y2(344)
       v_ACEK_1_Keq=y2(345)
       v_ACEK_1_k=y2(346)
       v_ACEK_2_Keq=y2(347)
       v_ACEK_2_k=y2(348)
       v_EDD_Keq=y2(349)
       v_EDD_KmKDPG=y2(350)
       v_EDD_KmPGN=y2(351)
       v_EDD_Vmax=y2(352)
       v_EDA_Keq=y2(353)
       v_EDA_KmGAP=y2(354)
       v_EDA_KmKDPG=y2(355)
       v_EDA_KmPYR=y2(356)
       v_EDA_Vmax=y2(357)
       v_NDHI_Keq=y2(358)
       v_NDHI_k=y2(359)
       v_NDHII_Keq=y2(360)
       v_NDHII_k=y2(361)
       v_NADPH_req_Keq=y2(362)
       v_NADPH_req_k=y2(363)
       v_PNT_Keq=y2(364)
       v_PNT_k=y2(365)
       v_CYTBO_Keq=y2(366)
       v_CYTBO_k=y2(367)
       v_SQR_Keq=y2(368)
       v_SQR_k=y2(369)
       v_ADK_Keq=y2(370)
       v_ADK_k=y2(371)
       v_ATP_NGAM_Keq=y2(372)
       v_ATP_NGAM_k=y2(373)
       v_ATP_SYN_Keq=y2(374)
       v_ATP_SYN_k=y2(375)
       v_CYA_k=y2(376)
       v_CYA_Keq=y2(377)
       v_CYA_KaeiiaP=y2(378)
       v_DOS_k=y2(379)
       v_DOS_Keq=y2(380)
       v_ACK_Keq=y2(381)
       v_ACK_KmACE=y2(382)
       v_ACK_KmACP=y2(383)
       v_ACK_KmADP=y2(384)
       v_ACK_KmATP=y2(385)
       v_ACK_Vmax=y2(386)
       v_ACS_KmACE=y2(387)
       v_ACS_KmATP=y2(388)
       v_ACS_KmCOA=y2(389)
       v_ACS_Vmax=y2(390)
       v_PTA_Keq=y2(391)
       v_PTA_KiACCOA=y2(392)
       v_PTA_KiACP=y2(393)
       v_PTA_KiCOA=y2(394)
       v_PTA_KiP=y2(395)
       v_PTA_KmACP=y2(396)
       v_PTA_KmP=y2(397)
       v_PTA_Vmax=y2(398)
       v_PTS_0_KmPEP=y2(399)
       v_PTS_0_KmPYR=y2(400)
       v_PTS_0_kF=y2(401)
       v_PTS_0_kR=y2(402)
       v_PTS_1_k1=y2(403)
       v_PTS_1_k2=y2(404)
       v_PTS_2_k1=y2(405)
       v_PTS_2_k2=y2(406)
       v_PTS_3_k1=y2(407)
       v_PTS_3_k2=y2(408)
       v_PTS_4_KmG6P=y2(409)
       v_PTS_4_KmGLC=y2(410)
       v_PTS_4_kF=y2(411)
       v_PTS_4_kR=y2(412)
       v_XCH_PYR_KmPYRx=y2(413)
       v_XCH_PYR_Vmax=y2(414)
       v_XCH_SUC_KmSUCx=y2(415)
       v_XCH_SUC_Vmax=y2(416)
       r_KdADPMg=y2(417)
       r_KdATPMg=y2(418)
       r_KdFDPMg=y2(419)
       v_XCH_GLC_P=y2(420)
       v_XCH_P_P=y2(421)
       v_XCH_ACE_P=y2(422)
       v_XCH_A=y2(423)
       v_GROWTH_KmG6P=y2(424)
       v_GROWTH_KmF6P=y2(425)
       v_GROWTH_KmGAP=y2(426)
       v_GROWTH_KmR5P=y2(427)
       v_GROWTH_KmE4P=y2(428)
       v_GROWTH_KmPGA3=y2(429)
       v_GROWTH_KmPEP=y2(430)
       v_GROWTH_KmPYR=y2(431)
       v_GROWTH_KmOAA=y2(432)
       v_GROWTH_KmAKG=y2(433)
       v_GROWTH_KmACCOA=y2(434)
       v_GROWTH_KmNADPH=y2(435)
       v_GROWTH_KmNAD=y2(436)
       v_GROWTH_KmATP=y2(437)
       v_GROWTH_Vmax=y2(438)
       v_PIT_k=y2(439)
       v_PIT_Kr=y2(440)
       v_PIT_KmPp=y2(441)
       v_PIT_KmP=y2(442)
       v_ACE_OUT_k=y2(443)
       v_ACE_OUT_Keq=y2(444)
       PER_vol=y2(445)
       ENV_vol=y2(446)

       MgADP=(MG*ADP)/(r_KdADPMg+MG)
       MgATP=(MG*ATP)/(r_KdATPMg+MG)
       MgFDP=(MG*FDP)/(r_KdFDPMg+MG)
       v_PGI=v_PGI_Vmax*(G6P-F6P/v_PGI_Keq)/v_PGI_KmG6P/
     * (1+F6P/v_PGI_KmF6P+G6P/
     * v_PGI_KmG6P+PGN/v_PGI_KmPGN+PEP/v_PGI_KmPEP)
       v_PFK=v_PFK_Vmax*v_PFK_n*(MgATP*F6P-MgADP*FDP/
     * v_PFK_Keq)/(v_PFK_KirF6P*v_PFK_KmrATPMg)/(1+
     * v_PFK_KmrFDP/v_PFK_KirFDP*(MgADP/v_PFK_KmrADP)+
     * v_PFK_KmrF6P/v_PFK_KirF6P*(MgATP/v_PFK_KmrATPMg)+
     * v_PFK_KmrFDP/v_PFK_KirFDP*(MgADP/v_PFK_KmrADP)*
     * (F6P/v_PFK_KirF6P)+MgATP/v_PFK_KmrATPMg*(F6P/
     * v_PFK_KirF6P)+MgADP/v_PFK_KirADP*(MgATP/
     * v_PFK_KmrATPMg)*(F6P/v_PFK_KirF6P)+(1+(ATP-MgATP)/
     * v_PFK_KirATP)*(F6P/v_PFK_KirF6P)+FDP/v_PFK_KirFDP+
     * MgADP/v_PFK_KmrADP*(FDP/v_PFK_KirFDP)+v_PFK_KmrF6P/
     * v_PFK_KirF6P*(MgATP/v_PFK_KmrATPMg)*(FDP/
     * v_PFK_KirFDP)+v_PFK_Wr*(v_PFK_KmrF6P/v_PFK_KirF6P)*
     * (MgADP/v_PFK_KirADP)*(MgATP/v_PFK_KmrATPMg)*
     * (FDP/v_PFK_KmrFDP))/(1+v_PFK_L0*((1+v_PFK_KmtFDP/
     * v_PFK_KitFDP*(MgADP/v_PFK_KmtADP)+v_PFK_KmtF6P/
     * v_PFK_KitF6P*(MgATP/v_PFK_KmtATPMg)+v_PFK_KmtFDP/
     * v_PFK_KitFDP*(MgADP/v_PFK_KmtADP)*(F6P/v_PFK_KitF6P)+
     * MgATP/v_PFK_KmtATPMg*(F6P/v_PFK_KitF6P)+MgADP/
     * v_PFK_KitADP*(MgATP/v_PFK_KmtATPMg)*(F6P/
     * v_PFK_KitF6P)+(1+(ATP-MgATP)/v_PFK_KitATP)*(F6P/
     * v_PFK_KitF6P)+FDP/v_PFK_KitFDP+MgADP/v_PFK_KmtADP*
     * (FDP/v_PFK_KitFDP)+v_PFK_KmtF6P/v_PFK_KitF6P*(MgATP/
     * v_PFK_KmtATPMg)*(FDP/v_PFK_KitFDP)+v_PFK_Wt*
     * (v_PFK_KmtF6P/v_PFK_KitF6P)*(MgADP/v_PFK_KitADP)*
     * (MgATP/v_PFK_KmtATPMg)*(FDP/v_PFK_KmtFDP))*(1+MgADP/
     * v_PFK_KeftADP+PEP/v_PFK_KeftPEP+MgADP/v_PFK_KeftADP*
     * (PEP/v_PFK_KeftPEP))/((1+v_PFK_KmrFDP/v_PFK_KirFDP*
     * (MgADP/v_PFK_KmrADP)+v_PFK_KmrF6P*MgATP/
     * (v_PFK_KirF6P*v_PFK_KmrATPMg)+v_PFK_KmrFDP/
     * v_PFK_KirFDP*(MgADP/v_PFK_KmrADP)*(F6P/v_PFK_KirF6P)+
     * MgATP/v_PFK_KmrATPMg*(F6P/v_PFK_KirF6P)+MgADP/
     * v_PFK_KirADP*(MgATP/v_PFK_KmrATPMg)*(F6P/
     * v_PFK_KirF6P)+(1+(ATP-MgATP)/v_PFK_KirATP)*(F6P/
     * v_PFK_KirF6P)+FDP/v_PFK_KirFDP+MgADP/v_PFK_KmrADP*
     * (FDP/v_PFK_KirFDP)+v_PFK_KmrF6P/v_PFK_KirF6P*(MgATP/
     * v_PFK_KmrATPMg)*(FDP/v_PFK_KirFDP)+v_PFK_Wr*
     * (v_PFK_KmrF6P/v_PFK_KirF6P)*(MgADP/v_PFK_KirADP)*
     * (MgATP/v_PFK_KmrATPMg)*(FDP/v_PFK_KmrFDP))*(1+MgADP/
     * v_PFK_KefrADP+PEP/v_PFK_KefrPEP+MgADP/v_PFK_KefrADP*
     * (PEP/v_PFK_KefrPEP))))**v_PFK_n)
       v_FBA=v_FBA_Vmax*(FDP-DAP*GAP/v_FBA_Keq)/v_FBA_KmFDP/
     * (1+FDP/v_FBA_KmFDP+DAP/v_FBA_KmDAP+DAP/v_FBA_KmDAP*
     * (GAP/v_FBA_KmGAP)+PEP/v_FBA_KmPEP)
       v_TPI=v_TPI_Vmax*(DAP-GAP/v_TPI_Keq)/v_TPI_KmDAP/(1+
     * DAP/v_TPI_KmDAP+GAP/v_TPI_KmGAP)
       v_GDH=v_GDH_Vmax*(P*GAP*NAD-BPG*NADH/v_GDH_Keq)/
     * (v_GDH_KmP*v_GDH_KmGAP*v_GDH_KmNAD)/((1+P/v_GDH_KmP)*
     * (1+GAP/v_GDH_KmGAP)*(1+NAD/v_GDH_KmNAD)+(1+BPG/
     * v_GDH_KmBPG)*(1+NADH/v_GDH_KmNADH)-1)
       v_PGK=v_PGK_Vmax*(MgADP*BPG-MgATP*PGA3/v_PGK_Keq)/
     * (v_PGK_KmADPMg*v_PGK_KmBPG)/(1+MgADP/v_PGK_KmADPMg+
     * BPG/v_PGK_KmBPG+MgADP/v_PGK_KmADPMg*BPG/v_PGK_KmBPG+
     * MgATP/v_PGK_KmATPMg+PGA3/v_PGK_KmPGA3+MgATP/
     * v_PGK_KmATPMg*PGA3/v_PGK_KmPGA3)
       v_GPM=v_GPM_Vmax*(PGA3-PGA2/v_GPM_Keq)/v_GPM_KmPGA3/
     * (1+PGA3/v_GPM_KmPGA3+PGA2/v_GPM_KmPGA2)
       v_ENO=v_ENO_Vmax*(PGA2-PEP/v_ENO_Keq)/v_ENO_KmPGA2/
     * (1+PGA2/v_ENO_KmPGA2+PEP/v_ENO_KmPEP)
       v_PYK=v_PYK_Vmax*v_PYK_n*PEP*MgADP/(v_PYK_KirPEP*
     * v_PYK_KmrADPMg)/(1+v_PYK_KmrPEP/v_PYK_KirPEP*(MgADP/
     * v_PYK_KmrADPMg)+MgATP/v_PYK_KirATP+MgADP/
     * v_PYK_KmrADPMg*(PEP/v_PYK_KirPEP)+v_PYK_KmrADPMg/
     * v_PYK_KmrADPMg*(1+(ADP-MgADP)/v_PYK_KirADP)*(PEP/
     * v_PYK_KirPEP)+PYR/v_PYK_KirPYR+MgATP/v_PYK_KirPyrATP*
     * (PYR/v_PYK_KirPYR))/(1+v_PYK_L0*((1+v_PYK_KmtPEP/
     * v_PYK_KitPEP*(MgADP/v_PYK_KmtADPMg)+MgATP/
     * v_PYK_KitATP+MgADP*PEP/(v_PYK_KitPEP*v_PYK_KmtADPMg)+
     * (1+(ADP-MgADP)/v_PYK_KitADP)*(PEP/v_PYK_KitPEP)+PYR/
     * v_PYK_KitPYR+MgATP/v_PYK_KitPyrATP*(PYR/
     * v_PYK_KitPYR))*(1+SUCCOA/v_PYK_KeftSUCCOA+MgATP*
     * SUCCOA/(v_PYK_KeftATP*v_PYK_KeftSUCCOA))/((1+
     * v_PYK_KmrPEP/v_PYK_KirPEP*(MgADP/v_PYK_KmrADPMg)+
     * MgATP/v_PYK_KirATP+MgADP/v_PYK_KmrADPMg*(PEP/
     * v_PYK_KirPEP)+(1+(ADP-MgADP)/v_PYK_KirADP)*(PEP/
     * v_PYK_KirPEP)+PYR/v_PYK_KirPYR+MgATP/v_PYK_KirPyrATP*
     * (PYR/v_PYK_KirPYR))*(1+FDP/v_PYK_KefrFDP+G6P/
     * v_PYK_KefrG6P+GL6P/v_PYK_KefrGL6P+R5P/v_PYK_KefrR5P+
     * RU5P/v_PYK_KefrRU5P+S7P/v_PYK_KefrS7P+X5P/
     * v_PYK_KefrX5P)))**v_PYK_n)
       v_ZWF=v_ZWF_Vmax*(G6P*NADP-GL6P*NADPH/v_ZWF_Keq)/
     * (v_ZWF_KdG6P*v_ZWF_KmNADP)/(1+G6P/v_ZWF_KdG6P+
     * v_ZWF_KmG6P/v_ZWF_KdG6P*(NADP/v_ZWF_KmNADP)+G6P/
     * v_ZWF_KdG6P*(NADP/v_ZWF_KmNADP)+v_ZWF_KmGL6P/
     * v_ZWF_KdGL6P*(NADPH/v_ZWF_KmNADPH)+GL6P/
     * v_ZWF_KdGL6P*(NADPH/v_ZWF_KmNADPH))
       v_PGL=v_PGL_Vmax*(GL6P-PGN/v_PGL_Keq)/v_PGL_KmGL6P/
     * (1+GL6P/v_PGL_KmGL6P+PGN/v_PGL_KmPGN+G6P/v_PGL_KiG6P)
       v_GND=v_GND_Vmax*(NADP*PGN-NADPH*RU5P*HCO3/v_GND_Keq)/
     * (v_GND_KdNADP*v_GND_KmPGN)/(1+NADP/v_GND_KdNADP+FDP/
     * v_GND_KefFbP+NADP/v_GND_KdNADP*(FDP/
     * v_GND_KefNADPFbP)+v_GND_KmNADP/v_GND_KdNADP*(PGN/
     * v_GND_KmPGN)+NADP/v_GND_KdNADP*(PGN/v_GND_KmPGN)+
     * ATP/v_GND_KefATP+ATP/v_GND_KefNADPATP*(v_GND_KmNADP/
     * v_GND_KdNADP)*(PGN/v_GND_KmPGN)+HCO3/v_GND_KdHCO3+
     * NADPH/v_GND_KdNADPH+RU5P/v_GND_KdRu5P+HCO3/
     * v_GND_KdHCO3*(NADPH/v_GND_KdHCO3NADPH)+HCO3/
     * v_GND_KdHCO3*(v_GND_KmNADPH/v_GND_KdHCO3NADPH)*
     * (RU5P/v_GND_KmRU5P)+HCO3/v_GND_KdHCO3*(NADPH/
     * v_GND_KdHCO3NADPH)*(RU5P/v_GND_KmRU5P)+v_GND_KmHCO3/
     * v_GND_KdHCO3*(NADPH/v_GND_KdHCO3NADPH)*(RU5P/
     * v_GND_KmRU5P))
       v_RPE=v_RPE_Vmax*(RU5P-X5P/v_RPE_Keq)/v_RPE_KmRU5P/(1+
     * RU5P/v_RPE_KmRU5P+X5P/v_RPE_KmX5P)
       v_RPI=v_RPI_Vmax*(RU5P-R5P/v_RPI_Keq)/v_RPI_KmRU5P/(1+
     * RU5P/v_RPI_KmRU5P+R5P/v_RPI_KmR5P+E4P/v_RPI_KmE4P)
       v_X5P_GAP_TKT=v_X5P_GAP_TKT_kcat*(tkt*X5P-GAP*tktC2/
     * v_X5P_GAP_TKT_Keq)
       v_F6P_E4P_TKT=v_F6P_E4P_TKT_kcat*(E4P*tktC2-F6P*tkt/
     * v_F6P_E4P_TKT_Keq)
       v_S7P_R5P_TKT=v_S7P_R5P_TKT_kcat*(R5P*tktC2-S7P*tkt/
     * v_S7P_R5P_TKT_Keq)
       v_F6P_GAP_TAL=v_F6P_GAP_TAL_kcat*(GAP*talC3-F6P*tal/
     * v_F6P_GAP_TAL_Keq)
       v_S7P_E4P_TAL=v_S7P_E4P_TAL_kcat*(S7P*tal-E4P*talC3/
     * v_S7P_E4P_TAL_Keq)
       v_FBP=v_FBP_Vmax*v_FBP_n*MgFDP/v_FBP_KirFDPMg/(1+
     * v_FBP_KmrFDP/v_FBP_KirFDP*(MG/v_FBP_KmrMg)+P/
     * v_FBP_KirP+P/v_FBP_KirP*(MG/v_FBP_KirPMg)+F6P/
     * v_FBP_KirF6P+F6P/v_FBP_KirF6P*(MG/v_FBP_KirF6PMg)+
     * P/v_FBP_KirP*(F6P/v_FBP_KirPF6P)+P/v_FBP_KirP*(F6P/
     * v_FBP_KirPF6P)*(MG/v_FBP_KirPF6PMg)+(FDP-MgFDP)/
     * v_FBP_KirFDP+r_KdFDPMg/v_FBP_KmrMg*(MgFDP/
     * v_FBP_KirFDP)+AMP/v_FBP_KirAMP+MgFDP/
     * v_FBP_KirFDPMg+MgFDP/v_FBP_KirFDPMg*(MG/
     * v_FBP_KirFDPMgMg)+AMP/v_FBP_KirAMP*((FDP-MgFDP)/
     * v_FBP_KirAMPFDP))/(1+v_FBP_L0*((1+v_FBP_KmtFDP/
     * v_FBP_KitFDP*(MG/v_FBP_KmtMg)+P/v_FBP_KitP+P/
     * v_FBP_KitP*(MG/v_FBP_KitPMg)+F6P/v_FBP_KitF6P+
     * F6P/v_FBP_KitF6P*(MG/v_FBP_KitF6PMg)+P/
     * v_FBP_KitP*(F6P/v_FBP_KitPF6P)+P/v_FBP_KitP*
     * (F6P/v_FBP_KitPF6P)*(MG/v_FBP_KitPF6PMg)+(FDP-
     * MgFDP)/v_FBP_KitFDP+r_KdFDPMg/v_FBP_KmtMg*(MgFDP/
     * v_FBP_KitFDP)+AMP/v_FBP_KitAMP+MgFDP/v_FBP_KitFDPMg+
     * MgFDP/v_FBP_KitFDPMg*(MG/v_FBP_KitFDPMgMg)+AMP/
     * v_FBP_KitAMP*((FDP-MgFDP)/v_FBP_KitAMPFDP))/(1+
     * v_FBP_KmrFDP/v_FBP_KirFDP*(MG/v_FBP_KmrMg)+P/
     * v_FBP_KirP+P/v_FBP_KirP*(MG/v_FBP_KirPMg)+F6P/
     * v_FBP_KirF6P+F6P/v_FBP_KirF6P*(MG/v_FBP_KirF6PMg)+
     * P/v_FBP_KirP*(F6P/v_FBP_KirPF6P)+P/v_FBP_KirP*(F6P/
     * v_FBP_KirPF6P)*(MG/v_FBP_KirPF6PMg)+(FDP-MgFDP)/
     * v_FBP_KirFDP+r_KdFDPMg/v_FBP_KmrMg*(MgFDP/
     * v_FBP_KirFDP)+AMP/v_FBP_KirAMP+MgFDP/v_FBP_KirFDPMg+
     * MgFDP/v_FBP_KirFDPMg*(MG/v_FBP_KirFDPMgMg)+AMP/
     * v_FBP_KirAMP*((FDP-MgFDP)/v_FBP_KirAMPFDP)))**v_FBP_n)
       v_PPC=v_PPC_Vmax*v_PPC_n*(PEP*HCO3-OAA*P/v_PPC_Keq)/
     * (v_PPC_KdrPEP*v_PPC_KmrHCO3)/(1+v_PPC_KmrPEP/
     * v_PPC_KdrPEP*(HCO3/v_PPC_KmrHCO3)+v_PPC_KmrOAA/
     * v_PPC_KdrOAA*(P/v_PPC_KmrP)+OAA/v_PPC_KdrOAA+P/
     * v_PPC_KmrP*(OAA/v_PPC_KdrOAA)+HCO3/v_PPC_KmrHCO3*
     * (PEP/v_PPC_KdrPEP)+PEP/v_PPC_KdrPEP)/(1+v_PPC_L0*
     * ((1+ACCOA/v_PPC_KeftACCOA+FDP/v_PPC_KeftFDP+FDP/
     * v_PPC_KeftFDP*(ACCOA/v_PPC_KeftFDPACCOA))*(1+
     * v_PPC_KmtPEP/v_PPC_KdtPEP*(HCO3/v_PPC_KmtHCO3)+
     * v_PPC_KmtOAA/v_PPC_KdtOAA*(P/v_PPC_KmtP)+OAA/
     * v_PPC_KdtOAA+P/v_PPC_KmtP*(OAA/v_PPC_KdtOAA)+
     * HCO3/v_PPC_KmtHCO3*(PEP/v_PPC_KdtPEP)+PEP/
     * v_PPC_KdtPEP)*(1+ASP/v_PPC_KeftASP+CYS/
     * v_PPC_KeftCYS+CIT/v_PPC_KeftCIT+FUM/v_PPC_KeftFUM+
     * MAL/v_PPC_KeftMAL+SUC/v_PPC_KeftSUC)/((1+ACCOA/
     * v_PPC_KefrACCOA+FDP/v_PPC_KefrFDP+FDP/v_PPC_KefrFDP*
     * (ACCOA/v_PPC_KefrFDPACCOA))*(1+v_PPC_KmrPEP/
     * v_PPC_KdrPEP*(HCO3/v_PPC_KmrHCO3)+v_PPC_KmrOAA/
     * v_PPC_KdrOAA*(P/v_PPC_KmrP)+OAA/v_PPC_KdrOAA+P/
     * v_PPC_KmrP*(OAA/v_PPC_KdrOAA)+HCO3/v_PPC_KmrHCO3*
     * (PEP/v_PPC_KdrPEP)+PEP/v_PPC_KdrPEP)*(1+ASP/
     * v_PPC_KefrASP+CYS/v_PPC_KefrCYS+CIT/v_PPC_KefrCIT+
     * FUM/v_PPC_KefrFUM+MAL/v_PPC_KefrMAL+SUC/
     * v_PPC_KefrSUC)))**v_PPC_n)
       v_PCK=v_PCK_Vmax*(MgATP*OAA-HCO3*MgADP*PEP/v_PCK_Keq)/
     * (v_PCK_KmATP*v_PCK_KmOAA)/(1+HCO3/v_PCK_KmHCO3+HCO3/
     * v_PCK_KmHCO3*(ADP/v_PCK_KmADP)+MgADP/v_PCK_KmADP+
     * MgATP/v_PCK_KmATP+OAA/v_PCK_KmOAA+MgATP/v_PCK_KmATP*
     * (OAA/v_PCK_KmOAA)+HCO3/v_PCK_KmHCO3*(PEP/v_PCK_KmPEP)+
     * PEP/v_PCK_KmPEP+HCO3/v_PCK_KmHCO3*(MgADP/v_PCK_KmADP)*
     * (PEP/v_PCK_KmPEP)+MgADP/v_PCK_KmADP*(PEP/v_PCK_KmPEP))
       v_PPS=v_PPS_Vmax*(MgATP*PYR-AMP*PEP*P*MG/v_PPS_Keq)/
     * (v_PPS_KmATPMg*v_PPS_KmPYR)/(MgATP/v_PPS_KmATPMg+
     * v_PPS_alpha*(P/v_PPS_KdP)*(MgATP/v_PPS_KmATPMg)+
     * v_PPS_alpha*(AMP/v_PPS_KdAMP)*(MgATP/v_PPS_KmATPMg)+
     * v_PPS_alpha*(P/v_PPS_KdP)*(AMP/v_PPS_KdAMP)*(MgATP/
     * v_PPS_KmATPMg)+v_PPS_alpha*(MG/v_PPS_KdMg)*(P/
     * v_PPS_KmP)*(AMP/v_PPS_KdAMP)*(MgATP/v_PPS_KdATPMgPPS)/
     * (v_PPS_W*(1+MG/v_PPS_KdMg))+MgATP/v_PPS_KmATPMg*(AKG/
     * v_PPS_KefAKG)+(1+MG/v_PPS_KdMg)*(AKG/v_PPS_KefAKG)*
     * (PEP/v_PPS_KmPEP)/v_PPS_W+MgATP/v_PPS_KmATPMg*(OAA/
     * v_PPS_KefOAA)+(1+MG/v_PPS_KdMg)*(OAA/v_PPS_KefOAA)*
     * (PEP/v_PPS_KmPEP)/v_PPS_W+MG/v_PPS_KdMg*(P/v_PPS_KmP)*
     * (AMP/v_PPS_KdAMP)/v_PPS_W+v_PPS_alpha*(P/v_PPS_KdP)*
     * (AMP/v_PPS_KdAMP)*(PEP/v_PPS_KmPEP)/v_PPS_W+
     * v_PPS_alpha*(MG/v_PPS_KdMg)*(P/v_PPS_KmP)*(AMP/
     * v_PPS_KdAMP)*(PEP/v_PPS_KmPEP)/v_PPS_W+v_PPS_alpha*
     * (1+MG/v_PPS_KdMg)*(v_PPS_KmAMP/v_PPS_KdAMP*
     * (P/v_PPS_KmP)*(PEP/v_PPS_KmPEP)+AMP/v_PPS_KdAMP*
     * (PEP/v_PPS_KmPEP))/v_PPS_W+(1+MG/v_PPS_KdMg)*(PYR/
     * v_PPS_KmPYR)+MgATP/v_PPS_KmATPMg*(PYR/v_PPS_KmPYR)+
     * r_KdADPMg/v_PPS_KdMg*(P/v_PPS_KmP)*(MgADP/
     * v_PPS_KefADP)*(AMP/v_PPS_KdAMP)/(v_PPS_W*(1+MG/
     * v_PPS_KdMg))+(ADP-MgADP)/v_PPS_KefADP*(PYR/
     * v_PPS_KmPYR)+r_KdATPMg/v_PPS_KdMg*(P/v_PPS_KmP)*
     * (AMP/v_PPS_KdAMP)*(MgATP/v_PPS_KefATP)/(v_PPS_W*
     * (1+MG/v_PPS_KdMg))+(ATP-MgATP)/v_PPS_KefATP*
     * (PYR/v_PPS_KmPYR)+(1+MG/v_PPS_KdMg)*(PEP/
     * v_PPS_KmPEP)/v_PPS_W+v_PPS_alpha*(1+MG/
     * v_PPS_KdMg)*(PEP/v_PPS_KdPEP)*(PYR/v_PPS_KmPYR)+
     * (1+MG/v_PPS_KdMg)*(PYR/v_PPS_KdPYR)*(PEP/
     * v_PPS_KmPEP)/v_PPS_W)
       v_MAD=v_MAD_Vmax*v_MAD_n*MAL*NAD/(v_MAD_KmrMAL*
     * v_MAD_KirNAD)/(1+v_MAD_KmrNAD/v_MAD_KirNAD*(MAL/
     * v_MAD_KmrMAL)+NAD/v_MAD_KirNAD+MAL/v_MAD_KmrMAL*
     * (NAD/v_MAD_KirNAD))*((MG/v_MAD_KmrMg+MN/v_MAD_KmrMn)/
     * (1+MG/v_MAD_KmrMg+MN/v_MAD_KmrMn))/(1+v_MAD_L0*((1+
     * ASP/v_MAD_KeftASP)*(1+MG/v_MAD_KmtMg+MN/v_MAD_KmtMn)*
     * (1+ATP/v_MAD_KeftATP)*(1+ACCOA/v_MAD_KeftACCOA+COA/
     * v_MAD_KeftCOA)*(1+v_MAD_KmtNAD/v_MAD_KitNAD*(MAL/
     * v_MAD_KmtMAL)+NAD/v_MAD_KitNAD+MAL/v_MAD_KmtMAL*(NAD/
     * v_MAD_KitNAD))/((1+ASP/v_MAD_KefrASP)*(1+MG/
     * v_MAD_KmrMg+MN/v_MAD_KmrMn)*(1+ATP/v_MAD_KefrATP)*
     * (1+ACCOA/v_MAD_KefrACCOA+COA/v_MAD_KefrCOA)*(1+
     * v_MAD_KmrNAD/v_MAD_KirNAD*(MAL/v_MAD_KmrMAL)+NAD/
     * v_MAD_KirNAD+MAL/v_MAD_KmrMAL*(NAD/
     * v_MAD_KirNAD))))**v_MAD_n)
       v_PDH=v_PDH_Vmax*(COA*NAD*PYR-ACCOA*NADH*HCO3/v_PDH_Keq)/
     * (v_PDH_KmCOA*v_PDH_KmNAD*v_PDH_KmPYR)/(ACCOA/
     * v_PDH_KmACCOA+NADH/v_PDH_KmNADH+ACCOA/v_PDH_KmACCOA*
     * (NADH/v_PDH_KmNADH)+COA/v_PDH_KmCOA*(NADH/v_PDH_KmNADH)+
     * ACCOA/v_PDH_KmACCOA*(COA/v_PDH_KmCOA)*(NADH/
     * v_PDH_KmNADH)+NAD/v_PDH_KmNAD*(NADH/v_PDH_KmNADH)+
     * COA/v_PDH_KmCOA*(NAD/v_PDH_KmNAD)*(NADH/v_PDH_KmNADH)+
     * ACCOA/v_PDH_KmACCOA*(PYR/v_PDH_KmPYR)+ACCOA/
     * v_PDH_KmACCOA*(COA/v_PDH_KmCOA)*(PYR/v_PDH_KmPYR)+
     * COA/v_PDH_KmCOA*(1+NAD/v_PDH_KmNAD)*(PYR/v_PDH_KmPYR)+
     * NAD/v_PDH_KmNAD*(1+COA/v_PDH_KmCOA+PYR/v_PDH_KmPYR))/
     * (1+HCO3/v_PDH_KmHCO3)
       v_GLT=v_GLT_Vmax*(ACCOA*OAA-CIT*COA/v_GLT_Keq)/
     * (v_GLT_KdACCOA0*v_GLT_KmOAA0)/(ACCOA/v_GLT_KdACCOA0*
     * (1+AKG/v_GLT_Ki1AKG+NADH/v_GLT_Ki1NADH)+ACCOA/
     * v_GLT_KdACCOA0*(OAA/v_GLT_KmOAA0)*(1+AKG/v_GLT_Ki2AKG+NADH/
     * v_GLT_Ki2NADH)+(1+ATP/v_GLT_KiATP)*(1+v_GLT_KmACCOA0/
     * v_GLT_KdACCOA0*(OAA/v_GLT_KmOAA0))+v_GLT_KmcsCOA/
     * v_GLT_KdcsCOA*(CIT/v_GLT_KmcsCIT)+v_GLT_KmcsCOA/
     * v_GLT_KdcsCOA*(ACCOA/v_GLT_KdACCOA0)*(CIT/v_GLT_KmcsCIT)+
     * COA/v_GLT_KdcsCOA+CIT/v_GLT_KmcsCIT*(OAA/v_GLT_KmOAA0)+
     * ACCOA/v_GLT_KdACCOA0*(CIT/v_GLT_KdcsCIT)*(OAA/
     * v_GLT_KmOAA0)+v_GLT_KmACCOA0/v_GLT_KdACCOA0*(COA/
     * v_GLT_KdcsCOA)*(OAA/v_GLT_KmOAA0)+CIT/v_GLT_KmcsCIT*
     * (COA/v_GLT_KdcsCOA)*(OAA/v_GLT_KdcsOAA))
       v_ACN_1=v_ACN_1_Vmax*(CIT-ACO/v_ACN_1_Keq)/v_ACN_1_KmCIT/
     * (1+CIT/v_ACN_1_KmCIT+ACO/v_ACN_1_KmACO+ICIT/v_ACN_1_KmICIT)
       v_ACN_2=v_ACN_2_Vmax*(ACO-ICIT/v_ACN_2_Keq)/v_ACN_2_KmACO/
     * (1+ACO/v_ACN_2_KmACO+ICIT/v_ACN_2_KmICIT+CIT/v_ACN_2_KmCIT)
       v_ICD=icd*v_ICD_kcat*(ICIT*NADP-AKG*NADPH/v_ICD_Keq)/
     * (v_ICD_KmICIT*v_ICD_KmNADP)/((1+ICIT/v_ICD_KmICIT)*(1+
     * NADP/v_ICD_KmNADP)+(1+AKG/v_ICD_KmAKG)*(1+NADPH/
     * v_ICD_KmNADPH)-1)
       v_LPD=v_LPD_Vmax*COA*AKG*NAD*(1-AKG/v_LPD_KdAKG)/(v_LPD_KmCOA*
     * v_LPD_KmAKG*v_LPD_KmNAD)/(COA/v_LPD_KmCOA*(AKG/v_LPD_KmAKG)+
     * COA/v_LPD_KmCOA*(NAD/v_LPD_KmNAD)+AKG/v_LPD_KmAKG*(NAD/
     * v_LPD_KmNAD)+COA/v_LPD_KmCOA*(AKG/v_LPD_KmAKG)*(NAD/
     * v_LPD_KmNAD)-AKG/v_LPD_KdAKG*(COA/v_LPD_KmCOA*(AKG/
     * v_LPD_KmAKG)+AKG/v_LPD_KmAKG*(NAD/v_LPD_KmNAD)+v_LPD_alpha*
     * (COA/v_LPD_KmCOA)*(AKG/v_LPD_KmAKG)*(NAD/v_LPD_KmNAD)))
       v_SK=v_SK_Vmax*(ADP*SUCCOA*P-ATP*COA*SUC/v_SK_Keq)/(v_SK_KmADP*
     * v_SK_KmSUCCOA*v_SK_KmP)/((1+ADP/v_SK_KmADP)*(1+SUCCOA/
     * v_SK_KmSUCCOA)*(1+P/v_SK_KmP)+(1+ATP/v_SK_KmATP)*(1+COA/
     * v_SK_KmCOA)*(1+SUC/v_SK_KmSUC)-1)
       v_SDH=v_SDH_Vmax*(SUC*FAD-FUM*FADH2/v_SDH_Keq)/(v_SDH_KefSUC*
     * v_SDH_KmQ)/(1+FUM/v_SDH_KefFUM+v_SDH_KmSUC/v_SDH_KefSUC*(FAD/
     * v_SDH_KmQ)+v_SDH_KmFUM/v_SDH_KefFUM*(FADH2/v_SDH_KmQH2)+FUM/
     * v_SDH_KefFUM*(FADH2/v_SDH_KmQH2)+SUC/v_SDH_KefSUC+SUC/
     * v_SDH_KefSUC*(FAD/v_SDH_KmQ))
       v_FUMA=v_FUMA_Vmax*(FUM-MAL/v_FUMA_Keq)/v_FUMA_KmFUM/(1+FUM/
     * v_FUMA_KmFUM+MAL/v_FUMA_KmMAL)
       v_MQO=v_MQO_Vmax*(MAL*Q-OAA*QH2/v_MQO_Keq)/(v_MQO_KmMAL*
     * v_MQO_KmQ)/((1+MAL/v_MQO_KmMAL)*(1+Q/v_MQO_KmQ)+(1+OAA/
     * v_MQO_KmOAA)*(1+QH2/v_MQO_KmQH2)-1)
       v_MDH=v_MDH_Vmax*(NADH*OAA-MAL*NAD/v_MDH_Keq)/
     * (v_MDH_KiNADH*v_MDH_KmOAA)/(1+v_MDH_KmNAD/
     * v_MDH_KiNAD*(MAL/v_MDH_KmMAL)+NAD/v_MDH_KiNAD+
     * MAL/v_MDH_KmMAL*(NAD/v_MDH_KiNAD)+NADH/
     * v_MDH_KiNADH+v_MDH_KmNAD/v_MDH_KiNAD*(MAL/
     * v_MDH_KmMAL)*(NADH/v_MDH_KiNADH)+v_MDH_KmNADH/
     * v_MDH_KiNADH*(OAA/v_MDH_KmOAA)+v_MDH_KmNADH/
     * v_MDH_KiNADH*(NAD/v_MDH_KiNAD)*(OAA/v_MDH_KmOAA)+
     * MAL*NAD*OAA/(v_MDH_KiNAD*v_MDH_KiOAA*v_MDH_KmMAL)+
     * NADH/v_MDH_KiNADH*(OAA/v_MDH_KmOAA)+v_MDH_Keq*
     * (v_MDH_KiNADH*v_MDH_KmOAA/(v_MDH_KiNAD*
     * v_MDH_KmMAL))*(v_MDH_KmNAD/v_MDH_KiNAD)*(MAL/
     * v_MDH_KmMAL)*(NADH/v_MDH_KmNADH)*(OAA/v_MDH_KiOAA))
       v_ACEA=v_ACEA_Vmax*(ICIT-GLX*SUC/v_ACEA_Keq)/
     * v_ACEA_KmICIT/(1+ICIT/v_ACEA_KmICIT*(1+PEP/
     * v_ACEA_KdPEPicit)+SUC/v_ACEA_KdSUC*(1+ICIT/
     * v_ACEA_KdICITsuc)+v_ACEA_KmSUC/v_ACEA_KdSUC*
     * (GLX/v_ACEA_KmGLX)*(1+PEP/v_ACEA_KdPEPglx)+
     * GLX/v_ACEA_KmGLX*(SUC/v_ACEA_KdSUC)+PEP/
     * v_ACEA_KdPEP+PGA3/v_ACEA_KdPGA3)
       v_ACEB=v_ACEB_Vmax*(ACCOA*GLX-COA*MAL/v_ACEB_Keq)/
     * (v_ACEB_KmACCOA*v_ACEB_KmGLX)/((1+ACCOA/
     * v_ACEB_KmACCOA)*(1+GLX/v_ACEB_KmGLX)+(1+COA/
     * v_ACEB_KmCOA)*(1+MAL/v_ACEB_KmMAL)-1)
       v_ACEK_1=v_ACEK_1_k*(ATP*icd-ADP*icdP/v_ACEK_1_Keq)
       v_ACEK_2=v_ACEK_2_k*(icdP-icd*P/v_ACEK_2_Keq)
       v_EDD=v_EDD_Vmax*(PGN-KDPG/v_EDD_Keq)/v_EDD_KmPGN/
     * (1+PGN/v_EDD_KmPGN+KDPG/v_EDD_KmKDPG)
       v_EDA=v_EDA_Vmax*(KDPG-GAP*PYR/v_EDA_Keq)/
     * v_EDA_KmKDPG/(1+KDPG/v_EDA_KmKDPG+(1+GAP/
     * v_EDA_KmGAP)*(1+PYR/v_EDA_KmPYR)-1)
       v_NDHI=v_NDHI_k/(1+(log10(Hp/Hc))**2)*
     * (NADH*Q-NAD*QH2/v_NDHI_Keq)
       v_NDHII=v_NDHII_k*(NADH*Q-NAD*QH2/v_NDHII_Keq)
       v_NADPH_req=v_NADPH_req_k*(NADPH-NADP/v_NADPH_req_Keq)
       v_SQR=v_SQR_k*(FADH2*Q-FAD*QH2/v_SQR_Keq)
       v_PNT=v_PNT_k*(NAD*NADPH-NADH*NADP/v_PNT_Keq)
       v_CYTBO=v_CYTBO_k/(1+(log10(Hp/Hc))**2)*
     * (O2*QH2**2-Q**2/v_CYTBO_Keq)
       v_ADK=v_ADK_k*(AMP*ATP-ADP**2/v_ADK_Keq)
       v_ATP_SYN=v_ATP_SYN_k*(log10(Hp/Hc))**4/
     * (1+(log10(Hp/Hc))**4)*(ADP*P-ATP/v_ATP_SYN_Keq)
       v_CYA=v_CYA_k*(ATP-CAMP*P**2/v_CYA_Keq)*
     * eiiaP/(eiiaP+v_CYA_KaeiiaP)
       v_ATP_NGAM=v_ATP_NGAM_k*(ATP-ADP*P/v_ATP_NGAM_Keq)
       v_DOS=v_DOS_k*(CAMP-AMP/v_DOS_Keq)
       v_ACK=v_ACK_Vmax*(ACP*ADP-ACE*ATP/v_ACK_Keq)/
     * (v_ACK_KmACP*v_ACK_KmADP)/((1+ACP/
     * v_ACK_KmACP+ACE/v_ACK_KmACE)*(1+ADP/v_ACK_KmADP+
     * ATP/v_ACK_KmATP))
       v_ACS=v_ACS_Vmax*ACE*ATP*COA/(v_ACS_KmACE*
     * v_ACS_KmATP*v_ACS_KmCOA)/((1+ACE/
     * v_ACS_KmACE)*(1+ATP/v_ACS_KmATP)*(1+COA/
     * v_ACS_KmCOA))
       v_PTA=v_PTA_Vmax*(ACCOA*P-ACP*COA/v_PTA_Keq)/
     * (v_PTA_KiACCOA*v_PTA_KmP)/(1+ACCOA/v_PTA_KiACCOA+P/
     * v_PTA_KiP+ACP/v_PTA_KiACP+COA/v_PTA_KiCOA+ACCOA*P/
     * (v_PTA_KiACCOA*v_PTA_KmP)+ACP*COA/(v_PTA_KmACP*
     * v_PTA_KiCOA))
       v_PTS_0=v_PTS_0_kF*(ei*PEP**2/(v_PTS_0_KmPEP**2+PEP**2)-
     * v_PTS_0_kR*eiP*PYR**2/(v_PTS_0_KmPYR**2+PYR**2))
       v_PTS_4=v_PTS_4_kF*(eiicbP*GLCp/(v_PTS_4_KmGLC+GLCp)-
     * v_PTS_4_kR*eiicb*G6P/(v_PTS_4_KmG6P+G6P))
       v_GLC_feed=FEED
       v_ACE_OUT=v_ACE_OUT_k*(ACE-ACEp/v_ACE_OUT_Keq)
       v_XCH_PYR=v_XCH_PYR_Vmax*PYRx/(v_XCH_PYR_KmPYRx+PYRx)
       v_XCH_SUC=v_XCH_PYR_Vmax*SUCx/(v_XCH_SUC_KmSUCx+SUCx)
       v_PTS_1=v_PTS_1_k1*(hpr*eiP-v_PTS_1_k2*hprP*ei)
       v_PTS_2=v_PTS_2_k1*(hprP*eiia-v_PTS_2_k2*hpr*eiiaP)
       v_PTS_3=v_PTS_3_k1*(eiicb*eiiaP-v_PTS_3_k2*eiicbP*eiia)
       v_XCH_GLC=v_XCH_A*v_XCH_GLC_P*(GLCx-GLCp)
       v_XCH_ACE=v_XCH_A*v_XCH_ACE_P*(ACEp-ACEx)
       v_XCH_P=v_XCH_A*v_XCH_P_P*(Px-Pp)
       v_PIT=v_PIT_k*(((log10(Hp/(Hc)))**2/(1+
     * (log10(Hp/(Hc)))**2))*(Pp/(v_PIT_KmPp+Pp))-
     * (v_PIT_Kr/(1+(log10(Hp/(Hc)))**2))*(P/(v_PIT_KmP+P)))
       v_GROWTH=v_GROWTH_Vmax*G6P*E4P*PGA3*OAA*AKG*PYR*R5P*
     * PEP*GAP*F6P*NADPH*ACCOA*NAD*ATP/(v_GROWTH_KmG6P*
     * v_GROWTH_KmE4P*v_GROWTH_KmPGA3*v_GROWTH_KmOAA*
     * v_GROWTH_KmAKG*v_GROWTH_KmPYR*v_GROWTH_KmR5P*
     * v_GROWTH_KmPEP*v_GROWTH_KmGAP*v_GROWTH_KmF6P*
     * v_GROWTH_KmNADPH*v_GROWTH_KmACCOA*v_GROWTH_KmNAD*
     * v_GROWTH_KmATP)/((1+G6P/v_GROWTH_KmG6P)*(1+E4P/
     * v_GROWTH_KmE4P)*(1+PGA3/v_GROWTH_KmPGA3)*(1+OAA/
     * v_GROWTH_KmOAA)*(1+AKG/v_GROWTH_KmAKG)*(1+PYR/
     * v_GROWTH_KmPYR)*(1+R5P/v_GROWTH_KmR5P)*(1+PEP/
     * v_GROWTH_KmPEP)*(1+GAP/v_GROWTH_KmGAP)*(1+F6P/
     * v_GROWTH_KmF6P)*(1+NADPH/v_GROWTH_KmNADPH)*(1+
     * ACCOA/v_GROWTH_KmACCOA)*(1+NAD/
     * v_GROWTH_KmNAD)*(1+ATP/v_GROWTH_KmATP))

c d_ACCOA_dt
       ydot(0)=v_PDH-v_GLT-v_ACEB-2118*v_GROWTH+v_ACS-v_PTA
c d_ACEx_dt
       ydot(1)=(v_XCH_ACE-ACEx*0.1/3600)/ENV_vol
c d_ACO_dt
       ydot(2)=v_ACN_1-v_ACN_2
c d_ACP_dt
       ydot(3)=v_PTA-v_ACK
c d_ADP_dt
       ydot(4)=-v_PGK-v_PYK+v_PFK+v_PCK-v_SK+v_ACEK_1+
     * 2*v_ADK-v_ATP_SYN-v_ACK+v_ATP_NGAM+30508*v_GROWTH
c d_AKG_dt
       ydot(5)=v_ICD-v_LPD-610*v_GROWTH
c d_AMP_dt
       ydot(6)=v_PPS-v_ADK+v_DOS+v_ACS
c d_ASP_dt
       ydot(7)=0
c d_ATP_dt
       ydot(8)=v_PGK+v_PYK-v_PFK-v_PCK-v_PPS+v_SK-
     * v_ACEK_1-v_ADK+v_ATP_SYN-v_CYA+v_ACK-v_ACS-
     * v_ATP_NGAM-30508*v_GROWTH
c d_BPG_dt
       ydot(9)=v_GDH-v_PGK
c d_CAMP_dt
       ydot(10)=v_CYA-v_DOS
c d_CIT_dt
       ydot(11)=v_GLT-v_ACN_1
c d_COA_dt
       ydot(12)=0
c d_CYS_dt
       ydot(13)=0
c d_DAP_dt
       ydot(14)=v_FBA-v_TPI
c d_E4P_dt
       ydot(15)=-v_F6P_E4P_TKT+v_S7P_E4P_TAL-204*v_GROWTH
c d_ei_dt
       ydot(16)=v_PTS_1-v_PTS_0
c d_eiia_dt
       ydot(17)=v_PTS_3-v_PTS_2
c d_eiiaP_dt
       ydot(18)=v_PTS_2-v_PTS_3
c d_eiicb_dt
       ydot(19)=v_PTS_4-v_PTS_3
c d_eiicbP_dt
       ydot(20)=v_PTS_3-v_PTS_4
c d_eiP_dt
       ydot(21)=v_PTS_0-v_PTS_1
c d_F6P_dt
       ydot(22)=v_PGI-v_PFK+v_F6P_E4P_TKT+
     * v_F6P_GAP_TAL+v_FBP-40*v_GROWTH
c d_FDP_dt
       ydot(23)=-v_FBA+v_PFK-v_FBP
c d_FUM_dt
       ydot(24)=v_SDH-v_FUMA
c d_G6P_dt
       ydot(25)=-v_PGI-v_ZWF-116*v_GROWTH+v_PTS_4
c d_GAP_dt
       ydot(26)=v_FBA+v_TPI-v_GDH+v_X5P_GAP_TKT-
     * v_F6P_GAP_TAL+v_EDA-73*v_GROWTH
c d_GL6P_dt
       ydot(27)=-v_PGL+v_ZWF
c d_GLCx_dt
       ydot(28)=(v_GLC_feed-v_XCH_GLC)/ENV_vol
c d_GLX_dt
       ydot(29)=v_ACEA-v_ACEB
c d_HCO3_dt
       ydot(30)=0
c d_hpr_dt
       ydot(31)=v_PTS_2-v_PTS_1
c d_hprP_dt
       ydot(32)=v_PTS_1-v_PTS_2
c d_icd_dt
       ydot(33)=v_ACEK_2-v_ACEK_1
c d_icdP_dt
       ydot(34)=v_ACEK_1-v_ACEK_2
c d_ICIT_dt
       ydot(35)=v_ACN_2-v_ICD-v_ACEA
c d_KDPG_dt
       ydot(36)=v_EDD-v_EDA
c d_MAL_dt
       ydot(37)=-v_MAD+v_FUMA+v_MDH-v_MQO+v_ACEB
c d_MG_dt
       ydot(38)=0
c d_MN_dt
       ydot(39)=0
c d_NAD_dt
       ydot(40)=-v_GDH-v_MAD-v_PDH-v_LPD+
     * v_NDHI+v_NDHII-v_PNT-2004*v_GROWTH
c d_NADH_dt
       ydot(41)=v_GDH+v_MAD+v_PDH+v_LPD-
     * v_NDHI-v_NDHII+v_PNT+2004*v_GROWTH
c d_NADP_dt
       ydot(42)=-v_GND-v_ZWF-v_ICD+v_NADPH_req+
     * v_PNT+10169*v_GROWTH
c d_NADPH_dt
       ydot(43)=v_GND+v_ZWF+v_ICD-v_NADPH_req-
     * v_PNT-10169*v_GROWTH
c d_OAA_dt
       ydot(44)=v_PPC-v_PCK-v_GLT+v_MQO-v_MDH-1010*v_GROWTH
c d_P_dt
       ydot(45)=-v_GDH+v_FBP+v_PPC+v_PPS-v_SK+v_ACEK_2-
     * v_ATP_SYN+2*v_CYA+2*v_ACS-v_PTA+v_ATP_NGAM+
     * v_PIT+30508*v_GROWTH
c d_PEP_dt
       ydot(46)=v_ENO-v_PYK-v_PPC+v_PCK+v_PPS-
     * 293*v_GROWTH-v_PTS_0
c d_PGA2_dt
       ydot(47)=v_GPM-v_ENO
c d_PGA3_dt
       ydot(48)=v_PGK-v_GPM-845*v_GROWTH
c d_PGN_dt
       ydot(49)=v_PGL-v_GND-v_EDD
c d_PYR_dt
       ydot(50)=v_PYK-v_PPS+v_MAD-v_PDH+v_EDA-
     * 1601*v_GROWTH+v_PTS_0+v_XCH_PYR
c d_PYRx_dt
       ydot(51)=-v_XCH_PYR
c d_Q_dt
       ydot(52)=-v_MQO+v_MDH-v_NDHI+2*v_CYTBO-v_SQR-v_NDHII
c d_QH2_dt
       ydot(53)=v_MQO-v_MDH+v_NDHI-2*v_CYTBO+v_SQR+v_NDHII
c d_R5P_dt
       ydot(54)=v_RPI-v_S7P_R5P_TKT-507*v_GROWTH
c d_RU5P_dt
       ydot(55)=v_GND-v_RPE-v_RPI
c d_S7P_dt
       ydot(56)=-v_S7P_E4P_TAL+v_S7P_R5P_TKT
c d_SUC_dt
       ydot(57)=v_SK-v_SDH+v_ACEA+v_XCH_SUC
c d_SUCCOA_dt
       ydot(58)=v_LPD-v_SK
c d_SUCx_dt
       ydot(59)=-v_XCH_SUC
c d_tal_dt
       ydot(60)=v_F6P_GAP_TAL-v_S7P_E4P_TAL
c d_talC3_dt
       ydot(61)=-v_F6P_GAP_TAL+v_S7P_E4P_TAL
c d_tkt_dt
       ydot(62)=-v_X5P_GAP_TKT+v_F6P_E4P_TKT+v_S7P_R5P_TKT
c d_tktC2_dt
       ydot(63)=v_X5P_GAP_TKT-v_F6P_E4P_TKT-v_S7P_R5P_TKT
c d_X5P_dt
       ydot(64)=v_RPE-v_X5P_GAP_TKT
c d_Px_dt
       ydot(65)=0
c d_Pp_dt
       ydot(66)=(v_XCH_P-v_PIT)/PER_vol
c d_GLCp_dt
       ydot(67)=(-v_PTS_4+v_XCH_GLC)/PER_vol
c d_ACEp_dt
       ydot(68)=(v_ACE_OUT-v_XCH_ACE)/PER_vol
c d_ACE_dt
       ydot(69)=v_ACK-v_ACS-v_ACE_OUT
c d_Hc_dt
       ydot(70)=0
c d_Hp_dt
       ydot(71)=(v_NDHI*4-v_ATP_SYN*4+v_CYTBO*8-v_PIT)/PER_vol
c d_FAD_dt
       ydot(72)=v_SQR-v_SDH
c d_FADH2_dt
       ydot(73)=v_SDH-v_SQR
c d_O2_dt
       ydot(74)=0
c just to enable events to change FEED value
       ydot(75)=0
       return
      end
c end of file ThEcoli.f
