/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 25, 2006 3:54:03 PM */

package clojure.lang;

import java.util.concurrent.Callable;

public interface IFn extends Callable, Runnable{

public Object invoke() ;

public Object invoke(Object arg1) ;

public Object invoke(Object arg1, Object arg2) ;

public Object invoke(Object arg1, Object arg2, Object arg3) ;

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) ;

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) ;

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6) ;

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7)
		;

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8) ;

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9) ;

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10) ;

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11) ;

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12) ;

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13) ;

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14)
		;

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15) ;

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16) ;

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17) ;

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18) ;

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19) ;

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20)
		;

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20,
                     Object... args)
		;

public Object applyTo(ISeq arglist) ;

static public interface L{long invokePrim();}
static public interface D{double invokePrim();}
static public interface OL{long invokePrim(Object arg0);}
static public interface OD{double invokePrim(Object arg0);}
static public interface LO{Object invokePrim(long arg0);}
static public interface LL{long invokePrim(long arg0);}
static public interface LD{double invokePrim(long arg0);}
static public interface DO{Object invokePrim(double arg0);}
static public interface DL{long invokePrim(double arg0);}
static public interface DD{double invokePrim(double arg0);}
static public interface OOL{long invokePrim(Object arg0, Object arg1);}
static public interface OOD{double invokePrim(Object arg0, Object arg1);}
static public interface OLO{Object invokePrim(Object arg0, long arg1);}
static public interface OLL{long invokePrim(Object arg0, long arg1);}
static public interface OLD{double invokePrim(Object arg0, long arg1);}
static public interface ODO{Object invokePrim(Object arg0, double arg1);}
static public interface ODL{long invokePrim(Object arg0, double arg1);}
static public interface ODD{double invokePrim(Object arg0, double arg1);}
static public interface LOO{Object invokePrim(long arg0, Object arg1);}
static public interface LOL{long invokePrim(long arg0, Object arg1);}
static public interface LOD{double invokePrim(long arg0, Object arg1);}
static public interface LLO{Object invokePrim(long arg0, long arg1);}
static public interface LLL{long invokePrim(long arg0, long arg1);}
static public interface LLD{double invokePrim(long arg0, long arg1);}
static public interface LDO{Object invokePrim(long arg0, double arg1);}
static public interface LDL{long invokePrim(long arg0, double arg1);}
static public interface LDD{double invokePrim(long arg0, double arg1);}
static public interface DOO{Object invokePrim(double arg0, Object arg1);}
static public interface DOL{long invokePrim(double arg0, Object arg1);}
static public interface DOD{double invokePrim(double arg0, Object arg1);}
static public interface DLO{Object invokePrim(double arg0, long arg1);}
static public interface DLL{long invokePrim(double arg0, long arg1);}
static public interface DLD{double invokePrim(double arg0, long arg1);}
static public interface DDO{Object invokePrim(double arg0, double arg1);}
static public interface DDL{long invokePrim(double arg0, double arg1);}
static public interface DDD{double invokePrim(double arg0, double arg1);}
static public interface OOOL{long invokePrim(Object arg0, Object arg1, Object arg2);}
static public interface OOOD{double invokePrim(Object arg0, Object arg1, Object arg2);}
static public interface OOLO{Object invokePrim(Object arg0, Object arg1, long arg2);}
static public interface OOLL{long invokePrim(Object arg0, Object arg1, long arg2);}
static public interface OOLD{double invokePrim(Object arg0, Object arg1, long arg2);}
static public interface OODO{Object invokePrim(Object arg0, Object arg1, double arg2);}
static public interface OODL{long invokePrim(Object arg0, Object arg1, double arg2);}
static public interface OODD{double invokePrim(Object arg0, Object arg1, double arg2);}
static public interface OLOO{Object invokePrim(Object arg0, long arg1, Object arg2);}
static public interface OLOL{long invokePrim(Object arg0, long arg1, Object arg2);}
static public interface OLOD{double invokePrim(Object arg0, long arg1, Object arg2);}
static public interface OLLO{Object invokePrim(Object arg0, long arg1, long arg2);}
static public interface OLLL{long invokePrim(Object arg0, long arg1, long arg2);}
static public interface OLLD{double invokePrim(Object arg0, long arg1, long arg2);}
static public interface OLDO{Object invokePrim(Object arg0, long arg1, double arg2);}
static public interface OLDL{long invokePrim(Object arg0, long arg1, double arg2);}
static public interface OLDD{double invokePrim(Object arg0, long arg1, double arg2);}
static public interface ODOO{Object invokePrim(Object arg0, double arg1, Object arg2);}
static public interface ODOL{long invokePrim(Object arg0, double arg1, Object arg2);}
static public interface ODOD{double invokePrim(Object arg0, double arg1, Object arg2);}
static public interface ODLO{Object invokePrim(Object arg0, double arg1, long arg2);}
static public interface ODLL{long invokePrim(Object arg0, double arg1, long arg2);}
static public interface ODLD{double invokePrim(Object arg0, double arg1, long arg2);}
static public interface ODDO{Object invokePrim(Object arg0, double arg1, double arg2);}
static public interface ODDL{long invokePrim(Object arg0, double arg1, double arg2);}
static public interface ODDD{double invokePrim(Object arg0, double arg1, double arg2);}
static public interface LOOO{Object invokePrim(long arg0, Object arg1, Object arg2);}
static public interface LOOL{long invokePrim(long arg0, Object arg1, Object arg2);}
static public interface LOOD{double invokePrim(long arg0, Object arg1, Object arg2);}
static public interface LOLO{Object invokePrim(long arg0, Object arg1, long arg2);}
static public interface LOLL{long invokePrim(long arg0, Object arg1, long arg2);}
static public interface LOLD{double invokePrim(long arg0, Object arg1, long arg2);}
static public interface LODO{Object invokePrim(long arg0, Object arg1, double arg2);}
static public interface LODL{long invokePrim(long arg0, Object arg1, double arg2);}
static public interface LODD{double invokePrim(long arg0, Object arg1, double arg2);}
static public interface LLOO{Object invokePrim(long arg0, long arg1, Object arg2);}
static public interface LLOL{long invokePrim(long arg0, long arg1, Object arg2);}
static public interface LLOD{double invokePrim(long arg0, long arg1, Object arg2);}
static public interface LLLO{Object invokePrim(long arg0, long arg1, long arg2);}
static public interface LLLL{long invokePrim(long arg0, long arg1, long arg2);}
static public interface LLLD{double invokePrim(long arg0, long arg1, long arg2);}
static public interface LLDO{Object invokePrim(long arg0, long arg1, double arg2);}
static public interface LLDL{long invokePrim(long arg0, long arg1, double arg2);}
static public interface LLDD{double invokePrim(long arg0, long arg1, double arg2);}
static public interface LDOO{Object invokePrim(long arg0, double arg1, Object arg2);}
static public interface LDOL{long invokePrim(long arg0, double arg1, Object arg2);}
static public interface LDOD{double invokePrim(long arg0, double arg1, Object arg2);}
static public interface LDLO{Object invokePrim(long arg0, double arg1, long arg2);}
static public interface LDLL{long invokePrim(long arg0, double arg1, long arg2);}
static public interface LDLD{double invokePrim(long arg0, double arg1, long arg2);}
static public interface LDDO{Object invokePrim(long arg0, double arg1, double arg2);}
static public interface LDDL{long invokePrim(long arg0, double arg1, double arg2);}
static public interface LDDD{double invokePrim(long arg0, double arg1, double arg2);}
static public interface DOOO{Object invokePrim(double arg0, Object arg1, Object arg2);}
static public interface DOOL{long invokePrim(double arg0, Object arg1, Object arg2);}
static public interface DOOD{double invokePrim(double arg0, Object arg1, Object arg2);}
static public interface DOLO{Object invokePrim(double arg0, Object arg1, long arg2);}
static public interface DOLL{long invokePrim(double arg0, Object arg1, long arg2);}
static public interface DOLD{double invokePrim(double arg0, Object arg1, long arg2);}
static public interface DODO{Object invokePrim(double arg0, Object arg1, double arg2);}
static public interface DODL{long invokePrim(double arg0, Object arg1, double arg2);}
static public interface DODD{double invokePrim(double arg0, Object arg1, double arg2);}
static public interface DLOO{Object invokePrim(double arg0, long arg1, Object arg2);}
static public interface DLOL{long invokePrim(double arg0, long arg1, Object arg2);}
static public interface DLOD{double invokePrim(double arg0, long arg1, Object arg2);}
static public interface DLLO{Object invokePrim(double arg0, long arg1, long arg2);}
static public interface DLLL{long invokePrim(double arg0, long arg1, long arg2);}
static public interface DLLD{double invokePrim(double arg0, long arg1, long arg2);}
static public interface DLDO{Object invokePrim(double arg0, long arg1, double arg2);}
static public interface DLDL{long invokePrim(double arg0, long arg1, double arg2);}
static public interface DLDD{double invokePrim(double arg0, long arg1, double arg2);}
static public interface DDOO{Object invokePrim(double arg0, double arg1, Object arg2);}
static public interface DDOL{long invokePrim(double arg0, double arg1, Object arg2);}
static public interface DDOD{double invokePrim(double arg0, double arg1, Object arg2);}
static public interface DDLO{Object invokePrim(double arg0, double arg1, long arg2);}
static public interface DDLL{long invokePrim(double arg0, double arg1, long arg2);}
static public interface DDLD{double invokePrim(double arg0, double arg1, long arg2);}
static public interface DDDO{Object invokePrim(double arg0, double arg1, double arg2);}
static public interface DDDL{long invokePrim(double arg0, double arg1, double arg2);}
static public interface DDDD{double invokePrim(double arg0, double arg1, double arg2);}
static public interface OOOOL{long invokePrim(Object arg0, Object arg1, Object arg2, Object arg3);}
static public interface OOOOD{double invokePrim(Object arg0, Object arg1, Object arg2, Object arg3);}
static public interface OOOLO{Object invokePrim(Object arg0, Object arg1, Object arg2, long arg3);}
static public interface OOOLL{long invokePrim(Object arg0, Object arg1, Object arg2, long arg3);}
static public interface OOOLD{double invokePrim(Object arg0, Object arg1, Object arg2, long arg3);}
static public interface OOODO{Object invokePrim(Object arg0, Object arg1, Object arg2, double arg3);}
static public interface OOODL{long invokePrim(Object arg0, Object arg1, Object arg2, double arg3);}
static public interface OOODD{double invokePrim(Object arg0, Object arg1, Object arg2, double arg3);}
static public interface OOLOO{Object invokePrim(Object arg0, Object arg1, long arg2, Object arg3);}
static public interface OOLOL{long invokePrim(Object arg0, Object arg1, long arg2, Object arg3);}
static public interface OOLOD{double invokePrim(Object arg0, Object arg1, long arg2, Object arg3);}
static public interface OOLLO{Object invokePrim(Object arg0, Object arg1, long arg2, long arg3);}
static public interface OOLLL{long invokePrim(Object arg0, Object arg1, long arg2, long arg3);}
static public interface OOLLD{double invokePrim(Object arg0, Object arg1, long arg2, long arg3);}
static public interface OOLDO{Object invokePrim(Object arg0, Object arg1, long arg2, double arg3);}
static public interface OOLDL{long invokePrim(Object arg0, Object arg1, long arg2, double arg3);}
static public interface OOLDD{double invokePrim(Object arg0, Object arg1, long arg2, double arg3);}
static public interface OODOO{Object invokePrim(Object arg0, Object arg1, double arg2, Object arg3);}
static public interface OODOL{long invokePrim(Object arg0, Object arg1, double arg2, Object arg3);}
static public interface OODOD{double invokePrim(Object arg0, Object arg1, double arg2, Object arg3);}
static public interface OODLO{Object invokePrim(Object arg0, Object arg1, double arg2, long arg3);}
static public interface OODLL{long invokePrim(Object arg0, Object arg1, double arg2, long arg3);}
static public interface OODLD{double invokePrim(Object arg0, Object arg1, double arg2, long arg3);}
static public interface OODDO{Object invokePrim(Object arg0, Object arg1, double arg2, double arg3);}
static public interface OODDL{long invokePrim(Object arg0, Object arg1, double arg2, double arg3);}
static public interface OODDD{double invokePrim(Object arg0, Object arg1, double arg2, double arg3);}
static public interface OLOOO{Object invokePrim(Object arg0, long arg1, Object arg2, Object arg3);}
static public interface OLOOL{long invokePrim(Object arg0, long arg1, Object arg2, Object arg3);}
static public interface OLOOD{double invokePrim(Object arg0, long arg1, Object arg2, Object arg3);}
static public interface OLOLO{Object invokePrim(Object arg0, long arg1, Object arg2, long arg3);}
static public interface OLOLL{long invokePrim(Object arg0, long arg1, Object arg2, long arg3);}
static public interface OLOLD{double invokePrim(Object arg0, long arg1, Object arg2, long arg3);}
static public interface OLODO{Object invokePrim(Object arg0, long arg1, Object arg2, double arg3);}
static public interface OLODL{long invokePrim(Object arg0, long arg1, Object arg2, double arg3);}
static public interface OLODD{double invokePrim(Object arg0, long arg1, Object arg2, double arg3);}
static public interface OLLOO{Object invokePrim(Object arg0, long arg1, long arg2, Object arg3);}
static public interface OLLOL{long invokePrim(Object arg0, long arg1, long arg2, Object arg3);}
static public interface OLLOD{double invokePrim(Object arg0, long arg1, long arg2, Object arg3);}
static public interface OLLLO{Object invokePrim(Object arg0, long arg1, long arg2, long arg3);}
static public interface OLLLL{long invokePrim(Object arg0, long arg1, long arg2, long arg3);}
static public interface OLLLD{double invokePrim(Object arg0, long arg1, long arg2, long arg3);}
static public interface OLLDO{Object invokePrim(Object arg0, long arg1, long arg2, double arg3);}
static public interface OLLDL{long invokePrim(Object arg0, long arg1, long arg2, double arg3);}
static public interface OLLDD{double invokePrim(Object arg0, long arg1, long arg2, double arg3);}
static public interface OLDOO{Object invokePrim(Object arg0, long arg1, double arg2, Object arg3);}
static public interface OLDOL{long invokePrim(Object arg0, long arg1, double arg2, Object arg3);}
static public interface OLDOD{double invokePrim(Object arg0, long arg1, double arg2, Object arg3);}
static public interface OLDLO{Object invokePrim(Object arg0, long arg1, double arg2, long arg3);}
static public interface OLDLL{long invokePrim(Object arg0, long arg1, double arg2, long arg3);}
static public interface OLDLD{double invokePrim(Object arg0, long arg1, double arg2, long arg3);}
static public interface OLDDO{Object invokePrim(Object arg0, long arg1, double arg2, double arg3);}
static public interface OLDDL{long invokePrim(Object arg0, long arg1, double arg2, double arg3);}
static public interface OLDDD{double invokePrim(Object arg0, long arg1, double arg2, double arg3);}
static public interface ODOOO{Object invokePrim(Object arg0, double arg1, Object arg2, Object arg3);}
static public interface ODOOL{long invokePrim(Object arg0, double arg1, Object arg2, Object arg3);}
static public interface ODOOD{double invokePrim(Object arg0, double arg1, Object arg2, Object arg3);}
static public interface ODOLO{Object invokePrim(Object arg0, double arg1, Object arg2, long arg3);}
static public interface ODOLL{long invokePrim(Object arg0, double arg1, Object arg2, long arg3);}
static public interface ODOLD{double invokePrim(Object arg0, double arg1, Object arg2, long arg3);}
static public interface ODODO{Object invokePrim(Object arg0, double arg1, Object arg2, double arg3);}
static public interface ODODL{long invokePrim(Object arg0, double arg1, Object arg2, double arg3);}
static public interface ODODD{double invokePrim(Object arg0, double arg1, Object arg2, double arg3);}
static public interface ODLOO{Object invokePrim(Object arg0, double arg1, long arg2, Object arg3);}
static public interface ODLOL{long invokePrim(Object arg0, double arg1, long arg2, Object arg3);}
static public interface ODLOD{double invokePrim(Object arg0, double arg1, long arg2, Object arg3);}
static public interface ODLLO{Object invokePrim(Object arg0, double arg1, long arg2, long arg3);}
static public interface ODLLL{long invokePrim(Object arg0, double arg1, long arg2, long arg3);}
static public interface ODLLD{double invokePrim(Object arg0, double arg1, long arg2, long arg3);}
static public interface ODLDO{Object invokePrim(Object arg0, double arg1, long arg2, double arg3);}
static public interface ODLDL{long invokePrim(Object arg0, double arg1, long arg2, double arg3);}
static public interface ODLDD{double invokePrim(Object arg0, double arg1, long arg2, double arg3);}
static public interface ODDOO{Object invokePrim(Object arg0, double arg1, double arg2, Object arg3);}
static public interface ODDOL{long invokePrim(Object arg0, double arg1, double arg2, Object arg3);}
static public interface ODDOD{double invokePrim(Object arg0, double arg1, double arg2, Object arg3);}
static public interface ODDLO{Object invokePrim(Object arg0, double arg1, double arg2, long arg3);}
static public interface ODDLL{long invokePrim(Object arg0, double arg1, double arg2, long arg3);}
static public interface ODDLD{double invokePrim(Object arg0, double arg1, double arg2, long arg3);}
static public interface ODDDO{Object invokePrim(Object arg0, double arg1, double arg2, double arg3);}
static public interface ODDDL{long invokePrim(Object arg0, double arg1, double arg2, double arg3);}
static public interface ODDDD{double invokePrim(Object arg0, double arg1, double arg2, double arg3);}
static public interface LOOOO{Object invokePrim(long arg0, Object arg1, Object arg2, Object arg3);}
static public interface LOOOL{long invokePrim(long arg0, Object arg1, Object arg2, Object arg3);}
static public interface LOOOD{double invokePrim(long arg0, Object arg1, Object arg2, Object arg3);}
static public interface LOOLO{Object invokePrim(long arg0, Object arg1, Object arg2, long arg3);}
static public interface LOOLL{long invokePrim(long arg0, Object arg1, Object arg2, long arg3);}
static public interface LOOLD{double invokePrim(long arg0, Object arg1, Object arg2, long arg3);}
static public interface LOODO{Object invokePrim(long arg0, Object arg1, Object arg2, double arg3);}
static public interface LOODL{long invokePrim(long arg0, Object arg1, Object arg2, double arg3);}
static public interface LOODD{double invokePrim(long arg0, Object arg1, Object arg2, double arg3);}
static public interface LOLOO{Object invokePrim(long arg0, Object arg1, long arg2, Object arg3);}
static public interface LOLOL{long invokePrim(long arg0, Object arg1, long arg2, Object arg3);}
static public interface LOLOD{double invokePrim(long arg0, Object arg1, long arg2, Object arg3);}
static public interface LOLLO{Object invokePrim(long arg0, Object arg1, long arg2, long arg3);}
static public interface LOLLL{long invokePrim(long arg0, Object arg1, long arg2, long arg3);}
static public interface LOLLD{double invokePrim(long arg0, Object arg1, long arg2, long arg3);}
static public interface LOLDO{Object invokePrim(long arg0, Object arg1, long arg2, double arg3);}
static public interface LOLDL{long invokePrim(long arg0, Object arg1, long arg2, double arg3);}
static public interface LOLDD{double invokePrim(long arg0, Object arg1, long arg2, double arg3);}
static public interface LODOO{Object invokePrim(long arg0, Object arg1, double arg2, Object arg3);}
static public interface LODOL{long invokePrim(long arg0, Object arg1, double arg2, Object arg3);}
static public interface LODOD{double invokePrim(long arg0, Object arg1, double arg2, Object arg3);}
static public interface LODLO{Object invokePrim(long arg0, Object arg1, double arg2, long arg3);}
static public interface LODLL{long invokePrim(long arg0, Object arg1, double arg2, long arg3);}
static public interface LODLD{double invokePrim(long arg0, Object arg1, double arg2, long arg3);}
static public interface LODDO{Object invokePrim(long arg0, Object arg1, double arg2, double arg3);}
static public interface LODDL{long invokePrim(long arg0, Object arg1, double arg2, double arg3);}
static public interface LODDD{double invokePrim(long arg0, Object arg1, double arg2, double arg3);}
static public interface LLOOO{Object invokePrim(long arg0, long arg1, Object arg2, Object arg3);}
static public interface LLOOL{long invokePrim(long arg0, long arg1, Object arg2, Object arg3);}
static public interface LLOOD{double invokePrim(long arg0, long arg1, Object arg2, Object arg3);}
static public interface LLOLO{Object invokePrim(long arg0, long arg1, Object arg2, long arg3);}
static public interface LLOLL{long invokePrim(long arg0, long arg1, Object arg2, long arg3);}
static public interface LLOLD{double invokePrim(long arg0, long arg1, Object arg2, long arg3);}
static public interface LLODO{Object invokePrim(long arg0, long arg1, Object arg2, double arg3);}
static public interface LLODL{long invokePrim(long arg0, long arg1, Object arg2, double arg3);}
static public interface LLODD{double invokePrim(long arg0, long arg1, Object arg2, double arg3);}
static public interface LLLOO{Object invokePrim(long arg0, long arg1, long arg2, Object arg3);}
static public interface LLLOL{long invokePrim(long arg0, long arg1, long arg2, Object arg3);}
static public interface LLLOD{double invokePrim(long arg0, long arg1, long arg2, Object arg3);}
static public interface LLLLO{Object invokePrim(long arg0, long arg1, long arg2, long arg3);}
static public interface LLLLL{long invokePrim(long arg0, long arg1, long arg2, long arg3);}
static public interface LLLLD{double invokePrim(long arg0, long arg1, long arg2, long arg3);}
static public interface LLLDO{Object invokePrim(long arg0, long arg1, long arg2, double arg3);}
static public interface LLLDL{long invokePrim(long arg0, long arg1, long arg2, double arg3);}
static public interface LLLDD{double invokePrim(long arg0, long arg1, long arg2, double arg3);}
static public interface LLDOO{Object invokePrim(long arg0, long arg1, double arg2, Object arg3);}
static public interface LLDOL{long invokePrim(long arg0, long arg1, double arg2, Object arg3);}
static public interface LLDOD{double invokePrim(long arg0, long arg1, double arg2, Object arg3);}
static public interface LLDLO{Object invokePrim(long arg0, long arg1, double arg2, long arg3);}
static public interface LLDLL{long invokePrim(long arg0, long arg1, double arg2, long arg3);}
static public interface LLDLD{double invokePrim(long arg0, long arg1, double arg2, long arg3);}
static public interface LLDDO{Object invokePrim(long arg0, long arg1, double arg2, double arg3);}
static public interface LLDDL{long invokePrim(long arg0, long arg1, double arg2, double arg3);}
static public interface LLDDD{double invokePrim(long arg0, long arg1, double arg2, double arg3);}
static public interface LDOOO{Object invokePrim(long arg0, double arg1, Object arg2, Object arg3);}
static public interface LDOOL{long invokePrim(long arg0, double arg1, Object arg2, Object arg3);}
static public interface LDOOD{double invokePrim(long arg0, double arg1, Object arg2, Object arg3);}
static public interface LDOLO{Object invokePrim(long arg0, double arg1, Object arg2, long arg3);}
static public interface LDOLL{long invokePrim(long arg0, double arg1, Object arg2, long arg3);}
static public interface LDOLD{double invokePrim(long arg0, double arg1, Object arg2, long arg3);}
static public interface LDODO{Object invokePrim(long arg0, double arg1, Object arg2, double arg3);}
static public interface LDODL{long invokePrim(long arg0, double arg1, Object arg2, double arg3);}
static public interface LDODD{double invokePrim(long arg0, double arg1, Object arg2, double arg3);}
static public interface LDLOO{Object invokePrim(long arg0, double arg1, long arg2, Object arg3);}
static public interface LDLOL{long invokePrim(long arg0, double arg1, long arg2, Object arg3);}
static public interface LDLOD{double invokePrim(long arg0, double arg1, long arg2, Object arg3);}
static public interface LDLLO{Object invokePrim(long arg0, double arg1, long arg2, long arg3);}
static public interface LDLLL{long invokePrim(long arg0, double arg1, long arg2, long arg3);}
static public interface LDLLD{double invokePrim(long arg0, double arg1, long arg2, long arg3);}
static public interface LDLDO{Object invokePrim(long arg0, double arg1, long arg2, double arg3);}
static public interface LDLDL{long invokePrim(long arg0, double arg1, long arg2, double arg3);}
static public interface LDLDD{double invokePrim(long arg0, double arg1, long arg2, double arg3);}
static public interface LDDOO{Object invokePrim(long arg0, double arg1, double arg2, Object arg3);}
static public interface LDDOL{long invokePrim(long arg0, double arg1, double arg2, Object arg3);}
static public interface LDDOD{double invokePrim(long arg0, double arg1, double arg2, Object arg3);}
static public interface LDDLO{Object invokePrim(long arg0, double arg1, double arg2, long arg3);}
static public interface LDDLL{long invokePrim(long arg0, double arg1, double arg2, long arg3);}
static public interface LDDLD{double invokePrim(long arg0, double arg1, double arg2, long arg3);}
static public interface LDDDO{Object invokePrim(long arg0, double arg1, double arg2, double arg3);}
static public interface LDDDL{long invokePrim(long arg0, double arg1, double arg2, double arg3);}
static public interface LDDDD{double invokePrim(long arg0, double arg1, double arg2, double arg3);}
static public interface DOOOO{Object invokePrim(double arg0, Object arg1, Object arg2, Object arg3);}
static public interface DOOOL{long invokePrim(double arg0, Object arg1, Object arg2, Object arg3);}
static public interface DOOOD{double invokePrim(double arg0, Object arg1, Object arg2, Object arg3);}
static public interface DOOLO{Object invokePrim(double arg0, Object arg1, Object arg2, long arg3);}
static public interface DOOLL{long invokePrim(double arg0, Object arg1, Object arg2, long arg3);}
static public interface DOOLD{double invokePrim(double arg0, Object arg1, Object arg2, long arg3);}
static public interface DOODO{Object invokePrim(double arg0, Object arg1, Object arg2, double arg3);}
static public interface DOODL{long invokePrim(double arg0, Object arg1, Object arg2, double arg3);}
static public interface DOODD{double invokePrim(double arg0, Object arg1, Object arg2, double arg3);}
static public interface DOLOO{Object invokePrim(double arg0, Object arg1, long arg2, Object arg3);}
static public interface DOLOL{long invokePrim(double arg0, Object arg1, long arg2, Object arg3);}
static public interface DOLOD{double invokePrim(double arg0, Object arg1, long arg2, Object arg3);}
static public interface DOLLO{Object invokePrim(double arg0, Object arg1, long arg2, long arg3);}
static public interface DOLLL{long invokePrim(double arg0, Object arg1, long arg2, long arg3);}
static public interface DOLLD{double invokePrim(double arg0, Object arg1, long arg2, long arg3);}
static public interface DOLDO{Object invokePrim(double arg0, Object arg1, long arg2, double arg3);}
static public interface DOLDL{long invokePrim(double arg0, Object arg1, long arg2, double arg3);}
static public interface DOLDD{double invokePrim(double arg0, Object arg1, long arg2, double arg3);}
static public interface DODOO{Object invokePrim(double arg0, Object arg1, double arg2, Object arg3);}
static public interface DODOL{long invokePrim(double arg0, Object arg1, double arg2, Object arg3);}
static public interface DODOD{double invokePrim(double arg0, Object arg1, double arg2, Object arg3);}
static public interface DODLO{Object invokePrim(double arg0, Object arg1, double arg2, long arg3);}
static public interface DODLL{long invokePrim(double arg0, Object arg1, double arg2, long arg3);}
static public interface DODLD{double invokePrim(double arg0, Object arg1, double arg2, long arg3);}
static public interface DODDO{Object invokePrim(double arg0, Object arg1, double arg2, double arg3);}
static public interface DODDL{long invokePrim(double arg0, Object arg1, double arg2, double arg3);}
static public interface DODDD{double invokePrim(double arg0, Object arg1, double arg2, double arg3);}
static public interface DLOOO{Object invokePrim(double arg0, long arg1, Object arg2, Object arg3);}
static public interface DLOOL{long invokePrim(double arg0, long arg1, Object arg2, Object arg3);}
static public interface DLOOD{double invokePrim(double arg0, long arg1, Object arg2, Object arg3);}
static public interface DLOLO{Object invokePrim(double arg0, long arg1, Object arg2, long arg3);}
static public interface DLOLL{long invokePrim(double arg0, long arg1, Object arg2, long arg3);}
static public interface DLOLD{double invokePrim(double arg0, long arg1, Object arg2, long arg3);}
static public interface DLODO{Object invokePrim(double arg0, long arg1, Object arg2, double arg3);}
static public interface DLODL{long invokePrim(double arg0, long arg1, Object arg2, double arg3);}
static public interface DLODD{double invokePrim(double arg0, long arg1, Object arg2, double arg3);}
static public interface DLLOO{Object invokePrim(double arg0, long arg1, long arg2, Object arg3);}
static public interface DLLOL{long invokePrim(double arg0, long arg1, long arg2, Object arg3);}
static public interface DLLOD{double invokePrim(double arg0, long arg1, long arg2, Object arg3);}
static public interface DLLLO{Object invokePrim(double arg0, long arg1, long arg2, long arg3);}
static public interface DLLLL{long invokePrim(double arg0, long arg1, long arg2, long arg3);}
static public interface DLLLD{double invokePrim(double arg0, long arg1, long arg2, long arg3);}
static public interface DLLDO{Object invokePrim(double arg0, long arg1, long arg2, double arg3);}
static public interface DLLDL{long invokePrim(double arg0, long arg1, long arg2, double arg3);}
static public interface DLLDD{double invokePrim(double arg0, long arg1, long arg2, double arg3);}
static public interface DLDOO{Object invokePrim(double arg0, long arg1, double arg2, Object arg3);}
static public interface DLDOL{long invokePrim(double arg0, long arg1, double arg2, Object arg3);}
static public interface DLDOD{double invokePrim(double arg0, long arg1, double arg2, Object arg3);}
static public interface DLDLO{Object invokePrim(double arg0, long arg1, double arg2, long arg3);}
static public interface DLDLL{long invokePrim(double arg0, long arg1, double arg2, long arg3);}
static public interface DLDLD{double invokePrim(double arg0, long arg1, double arg2, long arg3);}
static public interface DLDDO{Object invokePrim(double arg0, long arg1, double arg2, double arg3);}
static public interface DLDDL{long invokePrim(double arg0, long arg1, double arg2, double arg3);}
static public interface DLDDD{double invokePrim(double arg0, long arg1, double arg2, double arg3);}
static public interface DDOOO{Object invokePrim(double arg0, double arg1, Object arg2, Object arg3);}
static public interface DDOOL{long invokePrim(double arg0, double arg1, Object arg2, Object arg3);}
static public interface DDOOD{double invokePrim(double arg0, double arg1, Object arg2, Object arg3);}
static public interface DDOLO{Object invokePrim(double arg0, double arg1, Object arg2, long arg3);}
static public interface DDOLL{long invokePrim(double arg0, double arg1, Object arg2, long arg3);}
static public interface DDOLD{double invokePrim(double arg0, double arg1, Object arg2, long arg3);}
static public interface DDODO{Object invokePrim(double arg0, double arg1, Object arg2, double arg3);}
static public interface DDODL{long invokePrim(double arg0, double arg1, Object arg2, double arg3);}
static public interface DDODD{double invokePrim(double arg0, double arg1, Object arg2, double arg3);}
static public interface DDLOO{Object invokePrim(double arg0, double arg1, long arg2, Object arg3);}
static public interface DDLOL{long invokePrim(double arg0, double arg1, long arg2, Object arg3);}
static public interface DDLOD{double invokePrim(double arg0, double arg1, long arg2, Object arg3);}
static public interface DDLLO{Object invokePrim(double arg0, double arg1, long arg2, long arg3);}
static public interface DDLLL{long invokePrim(double arg0, double arg1, long arg2, long arg3);}
static public interface DDLLD{double invokePrim(double arg0, double arg1, long arg2, long arg3);}
static public interface DDLDO{Object invokePrim(double arg0, double arg1, long arg2, double arg3);}
static public interface DDLDL{long invokePrim(double arg0, double arg1, long arg2, double arg3);}
static public interface DDLDD{double invokePrim(double arg0, double arg1, long arg2, double arg3);}
static public interface DDDOO{Object invokePrim(double arg0, double arg1, double arg2, Object arg3);}
static public interface DDDOL{long invokePrim(double arg0, double arg1, double arg2, Object arg3);}
static public interface DDDOD{double invokePrim(double arg0, double arg1, double arg2, Object arg3);}
static public interface DDDLO{Object invokePrim(double arg0, double arg1, double arg2, long arg3);}
static public interface DDDLL{long invokePrim(double arg0, double arg1, double arg2, long arg3);}
static public interface DDDLD{double invokePrim(double arg0, double arg1, double arg2, long arg3);}
static public interface DDDDO{Object invokePrim(double arg0, double arg1, double arg2, double arg3);}
static public interface DDDDL{long invokePrim(double arg0, double arg1, double arg2, double arg3);}
static public interface DDDDD{double invokePrim(double arg0, double arg1, double arg2, double arg3);}
}
