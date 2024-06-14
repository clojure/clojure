
(ns clojure.test-clojure.generated-all-fi-adapters-in-let
  (:use clojure.test)
  (:require [clojure.string :as str])
  (:import (clojure.test AdapterExerciser  AdapterExerciser$L
AdapterExerciser$I
AdapterExerciser$S
AdapterExerciser$B
AdapterExerciser$D
AdapterExerciser$F
AdapterExerciser$O
AdapterExerciser$LL
AdapterExerciser$DL
AdapterExerciser$OL
AdapterExerciser$LI
AdapterExerciser$DI
AdapterExerciser$OI
AdapterExerciser$LS
AdapterExerciser$DS
AdapterExerciser$OS
AdapterExerciser$LB
AdapterExerciser$DB
AdapterExerciser$OB
AdapterExerciser$LD
AdapterExerciser$DD
AdapterExerciser$OD
AdapterExerciser$LF
AdapterExerciser$DF
AdapterExerciser$OF
AdapterExerciser$LO
AdapterExerciser$DO
AdapterExerciser$OO
AdapterExerciser$LLL
AdapterExerciser$LOL
AdapterExerciser$OLL
AdapterExerciser$DDL
AdapterExerciser$LDL
AdapterExerciser$DLL
AdapterExerciser$OOL
AdapterExerciser$ODL
AdapterExerciser$DOL
AdapterExerciser$LLI
AdapterExerciser$LOI
AdapterExerciser$OLI
AdapterExerciser$DDI
AdapterExerciser$LDI
AdapterExerciser$DLI
AdapterExerciser$OOI
AdapterExerciser$ODI
AdapterExerciser$DOI
AdapterExerciser$LLS
AdapterExerciser$LOS
AdapterExerciser$OLS
AdapterExerciser$DDS
AdapterExerciser$LDS
AdapterExerciser$DLS
AdapterExerciser$OOS
AdapterExerciser$ODS
AdapterExerciser$DOS
AdapterExerciser$LLB
AdapterExerciser$LOB
AdapterExerciser$OLB
AdapterExerciser$DDB
AdapterExerciser$LDB
AdapterExerciser$DLB
AdapterExerciser$OOB
AdapterExerciser$ODB
AdapterExerciser$DOB
AdapterExerciser$LLD
AdapterExerciser$LOD
AdapterExerciser$OLD
AdapterExerciser$DDD
AdapterExerciser$LDD
AdapterExerciser$DLD
AdapterExerciser$OOD
AdapterExerciser$ODD
AdapterExerciser$DOD
AdapterExerciser$LLF
AdapterExerciser$LOF
AdapterExerciser$OLF
AdapterExerciser$DDF
AdapterExerciser$LDF
AdapterExerciser$DLF
AdapterExerciser$OOF
AdapterExerciser$ODF
AdapterExerciser$DOF
AdapterExerciser$LLO
AdapterExerciser$LOO
AdapterExerciser$OLO
AdapterExerciser$DDO
AdapterExerciser$LDO
AdapterExerciser$DLO
AdapterExerciser$OOO
AdapterExerciser$ODO
AdapterExerciser$DOO
AdapterExerciser$OOOO
AdapterExerciser$OOOOO
AdapterExerciser$OOOOOO
AdapterExerciser$OOOOOOO
AdapterExerciser$OOOOOOOO
AdapterExerciser$OOOOOOOOO
AdapterExerciser$OOOOOOOOOO
AdapterExerciser$OOOOOOOOOOO
)))

  (deftest test-all-fi-adapters-in-let
    (let [^AdapterExerciser exerciser (AdapterExerciser.)
          ^AdapterExerciser$L Ladapter (fn [] (long 1))
          ^AdapterExerciser$I Iadapter (fn [] 1)
          ^AdapterExerciser$S Sadapter (fn [] (short 1))
          ^AdapterExerciser$B Badapter (fn [] (byte 1))
          ^AdapterExerciser$D Dadapter (fn [] (double 1))
          ^AdapterExerciser$F Fadapter (fn [] (float 1))
          ^AdapterExerciser$O Oadapter (fn [] exerciser)
          ^AdapterExerciser$LL LLadapter (fn [^long a] (long 1))
          ^AdapterExerciser$DL DLadapter (fn [^double a] (long 1))
          ^AdapterExerciser$OL OLadapter (fn [^AdapterExerciser a] (long 1))
          ^AdapterExerciser$LI LIadapter (fn [^long a] 1)
          ^AdapterExerciser$DI DIadapter (fn [^double a] 1)
          ^AdapterExerciser$OI OIadapter (fn [^AdapterExerciser a] 1)
          ^AdapterExerciser$LS LSadapter (fn [^long a] (short 1))
          ^AdapterExerciser$DS DSadapter (fn [^double a] (short 1))
          ^AdapterExerciser$OS OSadapter (fn [^AdapterExerciser a] (short 1))
          ^AdapterExerciser$LB LBadapter (fn [^long a] (byte 1))
          ^AdapterExerciser$DB DBadapter (fn [^double a] (byte 1))
          ^AdapterExerciser$OB OBadapter (fn [^AdapterExerciser a] (byte 1))
          ^AdapterExerciser$LD LDadapter (fn [^long a] (double 1))
          ^AdapterExerciser$DD DDadapter (fn [^double a] (double 1))
          ^AdapterExerciser$OD ODadapter (fn [^AdapterExerciser a] (double 1))
          ^AdapterExerciser$LF LFadapter (fn [^long a] (float 1))
          ^AdapterExerciser$DF DFadapter (fn [^double a] (float 1))
          ^AdapterExerciser$OF OFadapter (fn [^AdapterExerciser a] (float 1))
          ^AdapterExerciser$LO LOadapter (fn [^long a] exerciser)
          ^AdapterExerciser$DO DOadapter (fn [^double a] exerciser)
          ^AdapterExerciser$OO OOadapter (fn [^AdapterExerciser a] exerciser)
          ^AdapterExerciser$LLL LLLadapter (fn [^long a ^long b] (long 1))
          ^AdapterExerciser$LOL LOLadapter (fn [^long a ^AdapterExerciser b] (long 1))
          ^AdapterExerciser$OLL OLLadapter (fn [^AdapterExerciser a ^long b] (long 1))
          ^AdapterExerciser$DDL DDLadapter (fn [^double a ^double b] (long 1))
          ^AdapterExerciser$LDL LDLadapter (fn [^long a ^double b] (long 1))
          ^AdapterExerciser$DLL DLLadapter (fn [^double a ^long b] (long 1))
          ^AdapterExerciser$OOL OOLadapter (fn [^AdapterExerciser a ^AdapterExerciser b] (long 1))
          ^AdapterExerciser$ODL ODLadapter (fn [^AdapterExerciser a ^double b] (long 1))
          ^AdapterExerciser$DOL DOLadapter (fn [^double a ^AdapterExerciser b] (long 1))
          ^AdapterExerciser$LLI LLIadapter (fn [^long a ^long b] 1)
          ^AdapterExerciser$LOI LOIadapter (fn [^long a ^AdapterExerciser b] 1)
          ^AdapterExerciser$OLI OLIadapter (fn [^AdapterExerciser a ^long b] 1)
          ^AdapterExerciser$DDI DDIadapter (fn [^double a ^double b] 1)
          ^AdapterExerciser$LDI LDIadapter (fn [^long a ^double b] 1)
          ^AdapterExerciser$DLI DLIadapter (fn [^double a ^long b] 1)
          ^AdapterExerciser$OOI OOIadapter (fn [^AdapterExerciser a ^AdapterExerciser b] 1)
          ^AdapterExerciser$ODI ODIadapter (fn [^AdapterExerciser a ^double b] 1)
          ^AdapterExerciser$DOI DOIadapter (fn [^double a ^AdapterExerciser b] 1)
          ^AdapterExerciser$LLS LLSadapter (fn [^long a ^long b] (short 1))
          ^AdapterExerciser$LOS LOSadapter (fn [^long a ^AdapterExerciser b] (short 1))
          ^AdapterExerciser$OLS OLSadapter (fn [^AdapterExerciser a ^long b] (short 1))
          ^AdapterExerciser$DDS DDSadapter (fn [^double a ^double b] (short 1))
          ^AdapterExerciser$LDS LDSadapter (fn [^long a ^double b] (short 1))
          ^AdapterExerciser$DLS DLSadapter (fn [^double a ^long b] (short 1))
          ^AdapterExerciser$OOS OOSadapter (fn [^AdapterExerciser a ^AdapterExerciser b] (short 1))
          ^AdapterExerciser$ODS ODSadapter (fn [^AdapterExerciser a ^double b] (short 1))
          ^AdapterExerciser$DOS DOSadapter (fn [^double a ^AdapterExerciser b] (short 1))
          ^AdapterExerciser$LLB LLBadapter (fn [^long a ^long b] (byte 1))
          ^AdapterExerciser$LOB LOBadapter (fn [^long a ^AdapterExerciser b] (byte 1))
          ^AdapterExerciser$OLB OLBadapter (fn [^AdapterExerciser a ^long b] (byte 1))
          ^AdapterExerciser$DDB DDBadapter (fn [^double a ^double b] (byte 1))
          ^AdapterExerciser$LDB LDBadapter (fn [^long a ^double b] (byte 1))
          ^AdapterExerciser$DLB DLBadapter (fn [^double a ^long b] (byte 1))
          ^AdapterExerciser$OOB OOBadapter (fn [^AdapterExerciser a ^AdapterExerciser b] (byte 1))
          ^AdapterExerciser$ODB ODBadapter (fn [^AdapterExerciser a ^double b] (byte 1))
          ^AdapterExerciser$DOB DOBadapter (fn [^double a ^AdapterExerciser b] (byte 1))
          ^AdapterExerciser$LLD LLDadapter (fn [^long a ^long b] (double 1))
          ^AdapterExerciser$LOD LODadapter (fn [^long a ^AdapterExerciser b] (double 1))
          ^AdapterExerciser$OLD OLDadapter (fn [^AdapterExerciser a ^long b] (double 1))
          ^AdapterExerciser$DDD DDDadapter (fn [^double a ^double b] (double 1))
          ^AdapterExerciser$LDD LDDadapter (fn [^long a ^double b] (double 1))
          ^AdapterExerciser$DLD DLDadapter (fn [^double a ^long b] (double 1))
          ^AdapterExerciser$OOD OODadapter (fn [^AdapterExerciser a ^AdapterExerciser b] (double 1))
          ^AdapterExerciser$ODD ODDadapter (fn [^AdapterExerciser a ^double b] (double 1))
          ^AdapterExerciser$DOD DODadapter (fn [^double a ^AdapterExerciser b] (double 1))
          ^AdapterExerciser$LLF LLFadapter (fn [^long a ^long b] (float 1))
          ^AdapterExerciser$LOF LOFadapter (fn [^long a ^AdapterExerciser b] (float 1))
          ^AdapterExerciser$OLF OLFadapter (fn [^AdapterExerciser a ^long b] (float 1))
          ^AdapterExerciser$DDF DDFadapter (fn [^double a ^double b] (float 1))
          ^AdapterExerciser$LDF LDFadapter (fn [^long a ^double b] (float 1))
          ^AdapterExerciser$DLF DLFadapter (fn [^double a ^long b] (float 1))
          ^AdapterExerciser$OOF OOFadapter (fn [^AdapterExerciser a ^AdapterExerciser b] (float 1))
          ^AdapterExerciser$ODF ODFadapter (fn [^AdapterExerciser a ^double b] (float 1))
          ^AdapterExerciser$DOF DOFadapter (fn [^double a ^AdapterExerciser b] (float 1))
          ^AdapterExerciser$LLO LLOadapter (fn [^long a ^long b] exerciser)
          ^AdapterExerciser$LOO LOOadapter (fn [^long a ^AdapterExerciser b] exerciser)
          ^AdapterExerciser$OLO OLOadapter (fn [^AdapterExerciser a ^long b] exerciser)
          ^AdapterExerciser$DDO DDOadapter (fn [^double a ^double b] exerciser)
          ^AdapterExerciser$LDO LDOadapter (fn [^long a ^double b] exerciser)
          ^AdapterExerciser$DLO DLOadapter (fn [^double a ^long b] exerciser)
          ^AdapterExerciser$OOO OOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b] exerciser)
          ^AdapterExerciser$ODO ODOadapter (fn [^AdapterExerciser a ^double b] exerciser)
          ^AdapterExerciser$DOO DOOadapter (fn [^double a ^AdapterExerciser b] exerciser)
          ^AdapterExerciser$OOOO OOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c] exerciser)
          ^AdapterExerciser$OOOOO OOOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d] exerciser)
          ^AdapterExerciser$OOOOOO OOOOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e] exerciser)
          ^AdapterExerciser$OOOOOOO OOOOOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f] exerciser)
          ^AdapterExerciser$OOOOOOOO OOOOOOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f ^AdapterExerciser g] exerciser)
          ^AdapterExerciser$OOOOOOOOO OOOOOOOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f ^AdapterExerciser g ^AdapterExerciser h] exerciser)
          ^AdapterExerciser$OOOOOOOOOO OOOOOOOOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f ^AdapterExerciser g ^AdapterExerciser h ^AdapterExerciser i] exerciser)
          ^AdapterExerciser$OOOOOOOOOOO OOOOOOOOOOOadapter (fn [^AdapterExerciser a ^AdapterExerciser b ^AdapterExerciser c ^AdapterExerciser d ^AdapterExerciser e ^AdapterExerciser f ^AdapterExerciser g ^AdapterExerciser h ^AdapterExerciser i ^AdapterExerciser j] exerciser)]
      (is (= (.takesRetL Ladapter ) (long 1)))
      (is (= (.takesRetI Iadapter ) 1))
      (is (= (.takesRetS Sadapter ) (short 1)))
      (is (= (.takesRetB Badapter ) (byte 1)))
      (is (= (.takesRetD Dadapter ) (double 1)))
      (is (= (.takesRetF Fadapter ) (float 1)))
      (is (= (.takesRetO Oadapter ) exerciser))
      (is (= (.takesLRetL LLadapter (long 1)) (long 1)))
      (is (= (.takesDRetL DLadapter (double 1)) (long 1)))
      (is (= (.takesORetL OLadapter exerciser) (long 1)))
      (is (= (.takesLRetI LIadapter (long 1)) 1))
      (is (= (.takesDRetI DIadapter (double 1)) 1))
      (is (= (.takesORetI OIadapter exerciser) 1))
      (is (= (.takesLRetS LSadapter (long 1)) (short 1)))
      (is (= (.takesDRetS DSadapter (double 1)) (short 1)))
      (is (= (.takesORetS OSadapter exerciser) (short 1)))
      (is (= (.takesLRetB LBadapter (long 1)) (byte 1)))
      (is (= (.takesDRetB DBadapter (double 1)) (byte 1)))
      (is (= (.takesORetB OBadapter exerciser) (byte 1)))
      (is (= (.takesLRetD LDadapter (long 1)) (double 1)))
      (is (= (.takesDRetD DDadapter (double 1)) (double 1)))
      (is (= (.takesORetD ODadapter exerciser) (double 1)))
      (is (= (.takesLRetF LFadapter (long 1)) (float 1)))
      (is (= (.takesDRetF DFadapter (double 1)) (float 1)))
      (is (= (.takesORetF OFadapter exerciser) (float 1)))
      (is (= (.takesLRetO LOadapter (long 1)) exerciser))
      (is (= (.takesDRetO DOadapter (double 1)) exerciser))
      (is (= (.takesORetO OOadapter exerciser) exerciser))
      (is (= (.takesLLRetL LLLadapter (long 1) (long 1)) (long 1)))
      (is (= (.takesLORetL LOLadapter (long 1) exerciser) (long 1)))
      (is (= (.takesOLRetL OLLadapter exerciser (long 1)) (long 1)))
      (is (= (.takesDDRetL DDLadapter (double 1) (double 1)) (long 1)))
      (is (= (.takesLDRetL LDLadapter (long 1) (double 1)) (long 1)))
      (is (= (.takesDLRetL DLLadapter (double 1) (long 1)) (long 1)))
      (is (= (.takesOORetL OOLadapter exerciser exerciser) (long 1)))
      (is (= (.takesODRetL ODLadapter exerciser (double 1)) (long 1)))
      (is (= (.takesDORetL DOLadapter (double 1) exerciser) (long 1)))
      (is (= (.takesLLRetI LLIadapter (long 1) (long 1)) 1))
      (is (= (.takesLORetI LOIadapter (long 1) exerciser) 1))
      (is (= (.takesOLRetI OLIadapter exerciser (long 1)) 1))
      (is (= (.takesDDRetI DDIadapter (double 1) (double 1)) 1))
      (is (= (.takesLDRetI LDIadapter (long 1) (double 1)) 1))
      (is (= (.takesDLRetI DLIadapter (double 1) (long 1)) 1))
      (is (= (.takesOORetI OOIadapter exerciser exerciser) 1))
      (is (= (.takesODRetI ODIadapter exerciser (double 1)) 1))
      (is (= (.takesDORetI DOIadapter (double 1) exerciser) 1))
      (is (= (.takesLLRetS LLSadapter (long 1) (long 1)) (short 1)))
      (is (= (.takesLORetS LOSadapter (long 1) exerciser) (short 1)))
      (is (= (.takesOLRetS OLSadapter exerciser (long 1)) (short 1)))
      (is (= (.takesDDRetS DDSadapter (double 1) (double 1)) (short 1)))
      (is (= (.takesLDRetS LDSadapter (long 1) (double 1)) (short 1)))
      (is (= (.takesDLRetS DLSadapter (double 1) (long 1)) (short 1)))
      (is (= (.takesOORetS OOSadapter exerciser exerciser) (short 1)))
      (is (= (.takesODRetS ODSadapter exerciser (double 1)) (short 1)))
      (is (= (.takesDORetS DOSadapter (double 1) exerciser) (short 1)))
      (is (= (.takesLLRetB LLBadapter (long 1) (long 1)) (byte 1)))
      (is (= (.takesLORetB LOBadapter (long 1) exerciser) (byte 1)))
      (is (= (.takesOLRetB OLBadapter exerciser (long 1)) (byte 1)))
      (is (= (.takesDDRetB DDBadapter (double 1) (double 1)) (byte 1)))
      (is (= (.takesLDRetB LDBadapter (long 1) (double 1)) (byte 1)))
      (is (= (.takesDLRetB DLBadapter (double 1) (long 1)) (byte 1)))
      (is (= (.takesOORetB OOBadapter exerciser exerciser) (byte 1)))
      (is (= (.takesODRetB ODBadapter exerciser (double 1)) (byte 1)))
      (is (= (.takesDORetB DOBadapter (double 1) exerciser) (byte 1)))
      (is (= (.takesLLRetD LLDadapter (long 1) (long 1)) (double 1)))
      (is (= (.takesLORetD LODadapter (long 1) exerciser) (double 1)))
      (is (= (.takesOLRetD OLDadapter exerciser (long 1)) (double 1)))
      (is (= (.takesDDRetD DDDadapter (double 1) (double 1)) (double 1)))
      (is (= (.takesLDRetD LDDadapter (long 1) (double 1)) (double 1)))
      (is (= (.takesDLRetD DLDadapter (double 1) (long 1)) (double 1)))
      (is (= (.takesOORetD OODadapter exerciser exerciser) (double 1)))
      (is (= (.takesODRetD ODDadapter exerciser (double 1)) (double 1)))
      (is (= (.takesDORetD DODadapter (double 1) exerciser) (double 1)))
      (is (= (.takesLLRetF LLFadapter (long 1) (long 1)) (float 1)))
      (is (= (.takesLORetF LOFadapter (long 1) exerciser) (float 1)))
      (is (= (.takesOLRetF OLFadapter exerciser (long 1)) (float 1)))
      (is (= (.takesDDRetF DDFadapter (double 1) (double 1)) (float 1)))
      (is (= (.takesLDRetF LDFadapter (long 1) (double 1)) (float 1)))
      (is (= (.takesDLRetF DLFadapter (double 1) (long 1)) (float 1)))
      (is (= (.takesOORetF OOFadapter exerciser exerciser) (float 1)))
      (is (= (.takesODRetF ODFadapter exerciser (double 1)) (float 1)))
      (is (= (.takesDORetF DOFadapter (double 1) exerciser) (float 1)))
      (is (= (.takesLLRetO LLOadapter (long 1) (long 1)) exerciser))
      (is (= (.takesLORetO LOOadapter (long 1) exerciser) exerciser))
      (is (= (.takesOLRetO OLOadapter exerciser (long 1)) exerciser))
      (is (= (.takesDDRetO DDOadapter (double 1) (double 1)) exerciser))
      (is (= (.takesLDRetO LDOadapter (long 1) (double 1)) exerciser))
      (is (= (.takesDLRetO DLOadapter (double 1) (long 1)) exerciser))
      (is (= (.takesOORetO OOOadapter exerciser exerciser) exerciser))
      (is (= (.takesODRetO ODOadapter exerciser (double 1)) exerciser))
      (is (= (.takesDORetO DOOadapter (double 1) exerciser) exerciser))
      (is (= (.takesOOORetO OOOOadapter exerciser exerciser exerciser) exerciser))
      (is (= (.takesOOOORetO OOOOOadapter exerciser exerciser exerciser exerciser) exerciser))
      (is (= (.takesOOOOORetO OOOOOOadapter exerciser exerciser exerciser exerciser exerciser) exerciser))
      (is (= (.takesOOOOOORetO OOOOOOOadapter exerciser exerciser exerciser exerciser exerciser exerciser) exerciser))
      (is (= (.takesOOOOOOORetO OOOOOOOOadapter exerciser exerciser exerciser exerciser exerciser exerciser exerciser) exerciser))
      (is (= (.takesOOOOOOOORetO OOOOOOOOOadapter exerciser exerciser exerciser exerciser exerciser exerciser exerciser exerciser) exerciser))
      (is (= (.takesOOOOOOOOORetO OOOOOOOOOOadapter exerciser exerciser exerciser exerciser exerciser exerciser exerciser exerciser exerciser) exerciser))
      (is (= (.takesOOOOOOOOOORetO OOOOOOOOOOOadapter exerciser exerciser exerciser exerciser exerciser exerciser exerciser exerciser exerciser exerciser) exerciser))))