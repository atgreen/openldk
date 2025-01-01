// Tags: JDK1.2

/* This code was derived from test code from IBM's ICU project, which
   uses the following license...

Copyright (c) 1995-2001 International Business Machines Corporation and others
   
All rights reserved.

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, and/or sell copies of the Software, and to permit persons
to whom the Software is furnished to do so, provided that the above
copyright notice(s) and this permission notice appear in all copies of
the Software and that both the above copyright notice(s) and this
permission notice appear in supporting documentation.  

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT
OF THIRD PARTY RIGHTS. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
HOLDERS INCLUDED IN THIS NOTICE BE LIABLE FOR ANY CLAIM, OR ANY
SPECIAL INDIRECT OR CONSEQUENTIAL DAMAGES, OR ANY DAMAGES WHATSOEVER
RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

Except as contained in this notice, the name of a copyright holder
shall not be used in advertising or otherwise to promote the sale, use
or other dealings in this Software without prior written authorization
of the copyright holder. 

All trademarks and registered trademarks mentioned herein are the
property of their respective owners.

*/


/* Generated from 'DiagBigDecimal.nrx' 27 Mar 2000 22:38:44 [v1.162] */
/* Options: Binary Comments Crossref Format Java Logo Trace1 Verbose3 */
package gnu.testlet.java.math.BigDecimal;

import java.math.*;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/* ------------------------------------------------------------------ */
/* Decimal diagnostic tests                                       mfc */
/* Copyright (c) IBM Corporation 1996, 2000.  All Rights Reserved.    */
/* ------------------------------------------------------------------ */
/* DiagBigDecimal                                                     */
/*                                                                    */
/*   A class that tests the BigDecimal and MathContext classes.       */
/*                                                                    */
/*   The tests here are derived from or cover the same paths as:      */
/*      -- ANSI X3-274 testcases                                      */
/*      -- Java JCK testcases                                         */
/*      -- NetRexx testcases                                          */
/*      -- VM/CMS S/370 REXX implementation testcases [1981+]         */
/*      -- IBM Vienna Laboratory Rexx compiler testcases [1988+]      */
/*      -- New testcases                                              */
/*                                                                    */
/*   The authoritative sources for how the underlying technology      */
/*   (arithmetic) should work are:                                    */
/*      -- for digits=0 (fixed point): java.math.BigDecimal           */
/*      -- for digits>0 (floating point): ANSI X3.274-1996 + errata   */
/*                                                                    */
/* ------------------------------------------------------------------ */
/* Change list                                                        */
/* 1997.09.05 Initial implementation, from DiagRexx [NetRexx tests]   */
/* 1998.05.02 0.07 changes (e.g., compareTo)                          */
/* 1998.06.06 Rounding modes and format additions                     */
/* 1998.06.25 Rename from DiagDecimal; make stand-alone [add          */
/*            DiagException as a Minor class]                         */
/* 1998.06.27 Start adding testcases for DIGITS=0/FORM=PLAIN cases    */
/*            Reorganize for faster trace compilation                 */
/* 1998.06.28 new: valueof, scale, movePointX, unscaledValue, etc.    */
/* 1998.07.07 Scaled divide                                           */
/* 1998.07.08 setScale                                                */
/* 1998.07.15 new scaffolding (Minor Test class) -- see diagabs       */
/* 1998.12.14 add toBigDecimal and BigDecimal(java.math.BigDecimal)   */
/* 1999.02.04 number preparation rounds instead of digits+1 trunc     */
/* 1999.02.09 format method now only has two signatures               */
/* 1999.02.27 no longer use Rexx class or RexxIO class                */
/* 1999.03.05 add MathContext tests                                   */
/* 1999.03.05 update for 0.96 [no null settings, etc.]                */
/*            drop sundry constructors; no blanks; char[] gets ints   */
/*            drop sundry converters, add Exact converters            */
/* 1999.05.27 additional tests for scaled arithmetic                  */
/* 1999.06.29 additional tests for exponent overflows                 */
/* 1999.07.03 add 'continue' option                                   */
/* 1999.07.10 additional tests for scaled arithmetic                  */
/* 1999.07.18 randomly-generated tests added for base operators       */
/* 1999.10.28 weird intValueExact bad cases                           */
/* 1999.12.21 multiplication fast path failure and edge cases         */
/* 2000.01.01 copyright update                                        */
/* 2000.03.26 cosmetic updates; add extra format() testcases          */
/* 2000.03.27 1.00 move to com.ibm.icu.math package; open source release; */
/*                 change to javadoc comments                         */
/* ------------------------------------------------------------------ */

// note BINARY for conversions checking




/**
 * The <code>DiagBigDecimal</code> class forms a standalone test suite
 * for the <code>com.ibm.icu.math.BigDecimal</code> and
 * <code>com.ibm.icu.math.MathContext</code> classes (or, by changing the
 * <code>package</code> statement, other classes of the same names and
 * definition in other packages).  It may also be used as a constructed
 * object to embed the tests in an external test harness.
 * <p>
 * The tests are collected into <i>groups</i>, each corresponding to a
 * tested method or a more general grouping.  By default, when run from
 * the static {@link #main(java.lang.String[])} method, the run will end
 * if any test fails in a group.  The <code>continue</code> argument may
 * be specified to force the tests to run to completion.
 * <p>
 * Two minor (inner) classes are used; {@link
 * DiagBigDecimal.DiagException} is used to signal the failure of a test
 * group, and {@link DiagBigDecimal.Test}, a dependent minor class, is
 * used to register tests so that a summary of failures (or success) can be
 * presented as each group is completed.
 *
 * @see     com.ibm.icu.math.BigDecimal
 * @see     com.ibm.icu.math.MathContext
 * @version 1.00 2000.03.27
 * @author  Mike Cowlishaw
 */

public class DiagBigDecimal implements Testlet {
 private final boolean CHECK_EXCEPTION_MESSAGES = false;

 private static final java.lang.String xx0="DiagBigDecimal.nrx";
 
 /* properties shared */
 java.util.Vector Tests=new java.util.Vector(100); // scaffolding
 
 /* properties private */
 private int totalcount=0; // counts tests run
 
 /* properties constant private */
 
 /* Count of test groups */
 private static final int testcount=38;
 
 private static final BigDecimal zero=new BigDecimal (BigInteger.valueOf (0), 0);
 private static final BigDecimal one=new BigDecimal (BigInteger.valueOf (1), 0);
 private static final BigDecimal two=new BigDecimal (BigInteger.valueOf (2), 0);
 private static final BigDecimal ten=new BigDecimal (BigInteger.valueOf (10), 0);
 private static final BigDecimal tenlong=new BigDecimal((long)1234554321); // 10-digiter
 
 /* boundary primitive values */
 private static final byte bmin=-128;
 private static final byte bmax=127;
 private static final byte bzer=0;
 private static final byte bneg=-1;
 private static final byte bpos=1;
 private static final int imin=-2147483648;
 private static final int imax=2147483647;
 private static final int izer=0;
 private static final int ineg=-1;
 private static final int ipos=1;
 private static final long lmin=-9223372036854775808L;
 private static final long lmax=9223372036854775807L;
 private static final String lminString="-9223372036854775808";
 private static final String lmaxString="9223372036854775807";
 private static final long lzer=(long)0;
 private static final long lneg=(long)-1;
 private static final long lpos=(long)1;
 private static final short smin=-32768;
 private static final short smax=32767;
 private static final short szer=(short)0;
 private static final short sneg=(short)(-1);
 private static final short spos=(short)1;
 
 
 /* properties constant private unused */ // present but not referenced
 private static final java.lang.String copyright=" Copyright (c) IBM Corporation 1996, 2000.  All rights reserved. ";

 
 /** Constructs a <code>DiagBigDecimal</code> test suite.
   * <p>
   * Invoke its {@link #diagrun} method to run the tests.
   */
 
 public DiagBigDecimal(){super();
  return;
  }

 /** Run the tests in the test suite.
  *
  * @param isContinue The <code>boolean</code> which determines whether
  *                   to stop running after a group fails.  If 1 (true)
  *                   then the tests should be run to completion if
  *                   possible; if 0 (false) then the run will end if a
  *                   group fails.
  * @return an <code>int</code> which is 0 if all tests were
  * successful, >0 (the count of failures) if some failures were
  * detected, or <0 if an unexpected Exception was signalled.
  */
 
 public void diagrun(TestHarness harness){
  int num=0;
  RuntimeException de=null;
  java.lang.RuntimeException e=null;
  java.lang.String rest=null;
  
  try{num=1;num:for(;num<=testcount;num++){ // [testcount is constant set above]
   try{
    dotest(harness, num);
   }
   catch (RuntimeException xx1){de=xx1;
    say(harness);
    harness.verbose("**** Failed:"+" "+de.getMessage()+" "+"****");
    say(harness);
   }
  }
  }
  catch (java.lang.RuntimeException xx2){e=xx2; // any other exception is total failure; just show trace and quit
   say(harness);
   harness.verbose("**** Failed: unexpected exception ****");
   e.printStackTrace();
   return;
  }/*num*/
  
  return;
 }

 /* Run test by number -- method for development/private switching */
 
 private void dotest(TestHarness harness, int num){
  {/*select*/switch(num){
   /* -------------------------------------------------------------- */
   /* MathContext                                                    */
   /* -------------------------------------------------------------- */
  case 1:
   break;
   
   /* -------------------------------------------------------------- */
   /* Constructors                                                   */
   /* -------------------------------------------------------------- */
  case 2:
    /* diagconstructors(harness); */ break;
   
   /* -------------------------------------------------------------- */
   /* Operator methods                                               */
   /* -------------------------------------------------------------- */
  case 3:
   diagabs(harness);break;
  case 4:
   diagadd(harness);break;
  case 5:
   diagcompareto(harness);break;
  case 6:
   diagdivide(harness);break;
  case 7:
    break;
  case 8:
   diagmax(harness);break;
  case 9:
   diagmin(harness);break;
  case 10:
   diagmultiply(harness);break;
  case 11:
   diagnegate(harness);break;
  case 12:
    break;
  case 13:
    break;
  case 14:
    break;
  case 15:
   diagsubtract(harness);break;
  case 16:
    break;
   
   /* -------------------------------------------------------------- */
   /* Other methods                                                  */
   /* -------------------------------------------------------------- */
  case 17:
   diagbyteValue(harness);break;
  case 18:
   diagcomparetoObj(harness);break;
  case 19:
   diagdoublevalue(harness);break;
  case 20:
   diagequals(harness);break;
  case 21:
   diagfloatvalue(harness);break;
  case 22:
    break;
  case 23:
   diaghashcode(harness);break;
  case 24:
   diagintvalue(harness);break;
  case 25:
   diaglongvalue(harness);break;
  case 26:
   diagmovepointleft(harness);break;
  case 27:
   diagmovepointright(harness);break;
  case 28:
   diagscale(harness);break;
  case 29:
   diagsetscale(harness);break;
  case 30:
   diagshortvalue(harness);break;
  case 31:
   diagsignum(harness);break;
  case 32:
    break;
  case 33:
   diagtobiginteger(harness);break;
  case 34:
    break;
  case 35:
   diagtostring(harness);break;
  case 36:
    break;
  case 37:
   diagvalueof(harness);break;
   
   /* -------------------------------------------------------------- */
   /* Mutation test [must be the last test]                          */
   /* -------------------------------------------------------------- */
  case 38:
   diagmutation(harness);break;
   // if any more, increase testcount above
  default:{
   say("*** dotest case not found:"+" "+num+" "+"***", harness);
  }}
  }
  return;
  }

 /*--------------------------------------------------------------------*/
 /* Diagnostic group methods                                           */
 /*--------------------------------------------------------------------*/
 
 /** Test constructors (and {@link #toString()} for equalities). */
 
 public void diagconstructors(TestHarness harness){
  boolean flag=false;
  java.lang.String num;
  java.math.BigInteger bip;
  java.math.BigInteger biz;
  java.math.BigInteger bin;
  BigDecimal bda;
  BigDecimal bdb;
  BigDecimal bmc;
  BigDecimal bmd;
  BigDecimal bme;
  java.lang.RuntimeException e=null;
  char ca[];
  double dzer;
  double dpos;
  double dneg;
  double dpos5;
  double dneg5;
  double dmin;
  double dmax;
  double d;
  java.lang.String badstrings[];
  int i=0;
  
  // constants [statically-called constructors]
  harness.check ((zero.toString()).equals("0"), "con001");
  harness.check ((one.toString()).equals("1"), "con002");
  harness.check ((ten.toString()).equals("10"), "con003");
  harness.check ((zero.intValue())==0, "con004");
  harness.check ((one.intValue())==1, "con005");
  harness.check ((ten.intValue())==10, "con006");
  
  // [java.math.] BigDecimal
  harness.check (((new BigDecimal(new BigDecimal("0").toString())).toString()).equals("0"), "cbd001");
  harness.check (((new BigDecimal(new BigDecimal("1").toString())).toString()).equals("1"), "cbd002");
  harness.check (((new BigDecimal(new BigDecimal("10").toString())).toString()).equals("10"), "cbd003");
  harness.check (((new BigDecimal(new BigDecimal("1000").toString())).toString()).equals("1000"), "cbd004");
  harness.check (((new BigDecimal(new BigDecimal("10.0").toString())).toString()).equals("10.0"), "cbd005");
  harness.check (((new BigDecimal(new BigDecimal("10.1").toString())).toString()).equals("10.1"), "cbd006");
  harness.check (((new BigDecimal(new BigDecimal("-1.1").toString())).toString()).equals("-1.1"), "cbd007");
  harness.check (((new BigDecimal(new BigDecimal("-9.0").toString())).toString()).equals("-9.0"), "cbd008");
  harness.check (((new BigDecimal(new BigDecimal("0.9").toString())).toString()).equals("0.9"), "cbd009");
  
  num="123456789.123456789";
  harness.check (((new BigDecimal(new BigDecimal(num).toString())).toString()).equals(num), "cbd010");
  num="123456789.000000000";
  harness.check (((new BigDecimal(new BigDecimal(num).toString())).toString()).equals(num), "cbd011");
  num="123456789000000000";
  harness.check (((new BigDecimal(new BigDecimal(num).toString())).toString()).equals(num), "cbd012");
  num="0.00000123456789";
  harness.check (((new BigDecimal(new BigDecimal(num).toString())).toString()).equals(num), "cbd013");
  num="0.000000123456789";
  harness.check (((new BigDecimal(new BigDecimal(num).toString())).toString()).equals(num), "cbd014");
  
  // BigInteger
  bip=new BigInteger("987654321987654321987654321"); // biggie +ve
  biz=new BigInteger("0"); // biggie 0
  bin=new BigInteger("-12345678998765432112345678"); // biggie -ve
  harness.check (((new BigDecimal(bip)).toString()).equals(bip.toString()), "cbi001");
  harness.check (((new BigDecimal(biz)).toString()).equals("0"), "cbi002");
  harness.check (((new BigDecimal(bin)).toString()).equals(bin.toString()), "cbi003");
  try{checknull:do{
   new BigDecimal((java.math.BigInteger)null);
   flag=false;
  }while(false);}
  catch (java.lang.NullPointerException xx4){
   flag=true;
  }/*checknull*/
  harness.check (flag, "cbi004");
  
  // BigInteger with scale
  bip=new BigInteger("123456789"); // bigish
  bda=new BigDecimal(bip);
  bdb=new BigDecimal(bip,5);
  bmc=new BigDecimal(bip,15);
  harness.check ((bda.toString()).equals("123456789"), "cbs001");
  harness.check ((bdb.toString()).equals("1234.56789"), "cbs002");
  harness.check ((bmc.toString()).equals("0.000000123456789"), "cbs003");
  bip=new BigInteger("123456789123456789123456789"); // biggie
  bda=new BigDecimal(bip);
  bdb=new BigDecimal(bip,7);
  bmc=new BigDecimal(bip,13);
  bmd=new BigDecimal(bip,19);
  bme=new BigDecimal(bip,29);
  harness.check ((bda.toString()).equals("123456789123456789123456789"), "cbs011");
  harness.check ((bdb.toString()).equals("12345678912345678912.3456789"), "cbs012");
  harness.check ((bmc.toString()).equals("12345678912345.6789123456789"), "cbs013");
  harness.check ((bmd.toString()).equals("12345678.9123456789123456789"), "cbs014");
  harness.check ((bme.toString()).equals("0.00123456789123456789123456789"), "cbs015");
  try{checknull:do{
   new BigDecimal((java.math.BigInteger)null,1);
   flag=false;
  }while(false);}
  catch (java.lang.NullPointerException xx5){
   flag=true;
  }/*checknull*/
  harness.check (flag, "cbs004");
  try{checkscale:do{
   new BigDecimal(bip,-8);
   flag=false;
  }while(false);}
  catch (java.lang.RuntimeException xx6){e=xx6;
   flag=checkMessage(e, "Negative scale: -8");
  }/*checkscale*/
  harness.check (flag, "cbs005");
  
  // double [deprecated]
  // Note that many of these differ from the valueOf(double) results.
  dzer=(double)0;
  dpos=(double)1;
  dpos=dpos/((double)10);
  dneg=(double)-dpos;
  harness.check (((new BigDecimal(dneg)).toString()).equals("-0.1000000000000000055511151231257827021181583404541015625"), "cdo001");
  
  harness.check (((new BigDecimal(dzer)).toString()).equals("0"), "cdo002"); // NB, not '0.0'
  harness.check (((new BigDecimal(dpos)).toString()).equals("0.1000000000000000055511151231257827021181583404541015625"), "cdo003");
  
  dpos5=(double)0.5D;
  dneg5=(double)-dpos5;
  harness.check (((new BigDecimal(dneg5)).toString()).equals("-0.5"), "cdo004");
  harness.check (((new BigDecimal(dpos5)).toString()).equals("0.5"), "cdo005");
  dmin=java.lang.Double.MIN_VALUE;
  dmax=java.lang.Double.MAX_VALUE;
  harness.check (((new BigDecimal(dmin)).toString()).equals("0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004940656458412465441765687928682213723650598026143247644255856825006755072702087518652998363616359923797965646954457177309266567103559397963987747960107818781263007131903114045278458171678489821036887186360569987307230500063874091535649843873124733972731696151400317153853980741262385655911710266585566867681870395603106249319452715914924553293054565444011274801297099995419319894090804165633245247571478690147267801593552386115501348035264934720193790268107107491703332226844753335720832431936092382893458368060106011506169809753078342277318329247904982524730776375927247874656084778203734469699533647017972677717585125660551199131504891101451037862738167250955837389733598993664809941164205702637090279242767544565229087538682506419718265533447265625"), "cdo006");
  
  harness.check (((new BigDecimal(dmax)).toString()).equals("179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368"), "cdo007");
  
  
  // nasties
  d=(double)9;
  d=d/((double)10);
  harness.check (((new BigDecimal(d)).toString()).equals("0.90000000000000002220446049250313080847263336181640625"), "cdo010");
  
  d=d/((double)10);
  harness.check (((new BigDecimal(d)).toString()).equals("0.0899999999999999966693309261245303787291049957275390625"), "cdo011");
  
  d=d/((double)10);
  harness.check (((new BigDecimal(d)).toString()).equals("0.00899999999999999931998839741709161899052560329437255859375"), "cdo012");
  
  d=d/((double)10);
  harness.check (((new BigDecimal(d)).toString()).equals("0.00089999999999999997536692664112933925935067236423492431640625"), "cdo013");
  
  d=d/((double)10);
  harness.check (((new BigDecimal(d)).toString()).equals("0.00008999999999999999211568180168541175589780323207378387451171875"), "cdo014");
  
  d=d/((double)10);
  harness.check (((new BigDecimal(d)).toString()).equals("0.00000899999999999999853394182236510090433512232266366481781005859375"), "cdo015");
  
  d=d/((double)10);
  harness.check (((new BigDecimal(d)).toString()).equals("0.000000899999999999999853394182236510090433512232266366481781005859375"), "cdo016");
  
  d=d/((double)10);
  harness.check (((new BigDecimal(d)).toString()).equals("0.0000000899999999999999853394182236510090433512232266366481781005859375"), "cdo017");
  
  d=d/((double)10);
  harness.check (((new BigDecimal(d)).toString()).equals("0.000000008999999999999997872197332322678764437995369007694534957408905029296875"), "cdo018");
  
  
  try{checkpin:do{
   new BigDecimal(java.lang.Double.POSITIVE_INFINITY);
   flag=false;
  }while(false);}
  catch (java.lang.NumberFormatException xx13){
   flag=true;
  }/*checkpin*/
  harness.check (flag, "cdo101");
  try{checknin:do{
   new BigDecimal(java.lang.Double.NEGATIVE_INFINITY);
   flag=false;
  }while(false);}
  catch (java.lang.NumberFormatException xx14){
   flag=true;
  }/*checknin*/
  harness.check (flag, "cdo102");
  try{checknan:do{
   new BigDecimal(java.lang.Double.NaN);
   flag=false;
  }while(false);}
  catch (java.lang.NumberFormatException xx15){
   flag=true;
  }/*checknan*/
  harness.check (flag, "cdo103");
  
  // int
  harness.check (((new BigDecimal(imin)).toString()).equals("-2147483648"), "cin001");
  harness.check (((new BigDecimal(imax)).toString()).equals("2147483647"), "cin002");
  harness.check (((new BigDecimal(ineg)).toString()).equals("-1"), "cin003");
  harness.check (((new BigDecimal(izer)).toString()).equals("0"), "cin004");
  harness.check (((new BigDecimal(ipos)).toString()).equals("1"), "cin005");
  harness.check (((new BigDecimal(10)).toString()).equals("10"), "cin006");
  harness.check (((new BigDecimal(9)).toString()).equals("9"), "cin007");
  harness.check (((new BigDecimal(5)).toString()).equals("5"), "cin008");
  harness.check (((new BigDecimal(2)).toString()).equals("2"), "cin009");
  harness.check (((new BigDecimal(-2)).toString()).equals("-2"), "cin010");
  harness.check (((new BigDecimal(-5)).toString()).equals("-5"), "cin011");
  harness.check (((new BigDecimal(-9)).toString()).equals("-9"), "cin012");
  harness.check (((new BigDecimal(-10)).toString()).equals("-10"), "cin013");
  harness.check (((new BigDecimal(-11)).toString()).equals("-11"), "cin014");
  harness.check (((new BigDecimal(-99)).toString()).equals("-99"), "cin015");
  harness.check (((new BigDecimal(-100)).toString()).equals("-100"), "cin016");
  harness.check (((new BigDecimal(-999)).toString()).equals("-999"), "cin017");
  harness.check (((new BigDecimal(-1000)).toString()).equals("-1000"), "cin018");
  
  harness.check (((new BigDecimal(11)).toString()).equals("11"), "cin019");
  harness.check (((new BigDecimal(99)).toString()).equals("99"), "cin020");
  harness.check (((new BigDecimal(100)).toString()).equals("100"), "cin021");
  harness.check (((new BigDecimal(999)).toString()).equals("999"), "cin022");
  harness.check (((new BigDecimal(1000)).toString()).equals("1000"), "cin023");
  
  // long
  harness.check (((new BigDecimal(lmin)).toString()).equals("-9223372036854775808"), "clo001");
  harness.check (((new BigDecimal(lmax)).toString()).equals("9223372036854775807"), "clo002");
  harness.check (((new BigDecimal(lneg)).toString()).equals("-1"), "clo003");
  harness.check (((new BigDecimal(lzer)).toString()).equals("0"), "clo004");
  harness.check (((new BigDecimal(lpos)).toString()).equals("1"), "clo005");
  
  // String [many more examples are elsewhere]
  // strings without E cannot generate E in result
  harness.check (((new BigDecimal("12")).toString()).equals("12"), "cst001");
  harness.check (((new BigDecimal("-76")).toString()).equals("-76"), "cst002");
  harness.check (((new BigDecimal("12.76")).toString()).equals("12.76"), "cst003");
  harness.check (((new BigDecimal("+12.76")).toString()).equals("12.76"), "cst004");
  harness.check (((new BigDecimal("012.76")).toString()).equals("12.76"), "cst005");
  harness.check (((new BigDecimal("+0.003")).toString()).equals("0.003"), "cst006");
  harness.check (((new BigDecimal("17.")).toString()).equals("17"), "cst007");
  harness.check (((new BigDecimal(".5")).toString()).equals("0.5"), "cst008");
  harness.check (((new BigDecimal("044")).toString()).equals("44"), "cst009");
  harness.check (((new BigDecimal("0044")).toString()).equals("44"), "cst010");
  harness.check (((new BigDecimal("0.0005")).toString()).equals("0.0005"), "cst011");
  harness.check (((new BigDecimal("00.00005")).toString()).equals("0.00005"), "cst012");
  harness.check (((new BigDecimal("0.000005")).toString()).equals("0.000005"), "cst013");
  harness.check (((new BigDecimal("0.0000005")).toString()).equals("0.0000005"), "cst014");
  harness.check (((new BigDecimal("0.00000005")).toString()).equals("0.00000005"), "cst015");
  harness.check (((new BigDecimal("12345678.876543210")).toString()).equals("12345678.876543210"), "cst016");
  harness.check (((new BigDecimal("2345678.876543210")).toString()).equals("2345678.876543210"), "cst017");
  harness.check (((new BigDecimal("345678.876543210")).toString()).equals("345678.876543210"), "cst018");
  harness.check (((new BigDecimal("0345678.87654321")).toString()).equals("345678.87654321"), "cst019");
  harness.check (((new BigDecimal("345678.8765432")).toString()).equals("345678.8765432"), "cst020");
  harness.check (((new BigDecimal("+345678.8765432")).toString()).equals("345678.8765432"), "cst021");
  harness.check (((new BigDecimal("+0345678.8765432")).toString()).equals("345678.8765432"), "cst022");
  harness.check (((new BigDecimal("+00345678.8765432")).toString()).equals("345678.8765432"), "cst023");
  harness.check (((new BigDecimal("-345678.8765432")).toString()).equals("-345678.8765432"), "cst024");
  harness.check (((new BigDecimal("-0345678.8765432")).toString()).equals("-345678.8765432"), "cst025");
  harness.check (((new BigDecimal("-00345678.8765432")).toString()).equals("-345678.8765432"), "cst026");
  
  // exotics --
  harness.check (((new BigDecimal("\u0e57.\u0e50")).toString()).equals("7.0"), "cst035");
  harness.check (((new BigDecimal("\u0b66.\u0b67")).toString()).equals("0.1"), "cst036");
  harness.check (((new BigDecimal("\u0b66\u0b66")).toString()).equals("0"), "cst037");
  harness.check (((new BigDecimal("\u0b6a\u0b66")).toString()).equals("40"), "cst038");
  
  // strings with E
  // Some implementations throw a NumberFormatException here.
  try {
    harness.check (((new BigDecimal("1E+9")).toString()).equals("1E+9"), "cst040");
  } catch (Exception ecst040) {
    harness.check (false, "cst040");
  }
  try {
    harness.check (((new BigDecimal("1e+09")).toString()).equals("1E+9"), "cst041");
  } catch (Exception ecst041) {
    harness.check (false, "cst041");
  }
  try {
    harness.check (((new BigDecimal("1E+90")).toString()).equals("1E+90"), "cst042");
  } catch (Exception ecst042) {
    harness.check (false, "cst042");
  }
  try {
    harness.check (((new BigDecimal("+1E+009")).toString()).equals("1E+9"), "cst043");
  } catch (Exception ecst043) {
    harness.check (false, "cst043");
  }
  try {
    harness.check (((new BigDecimal("0E+9")).toString()).equals("0"), "cst044");
  } catch (Exception ecst044) {
    harness.check (false, "cst044");
  }
  try {
    harness.check (((new BigDecimal("1E+9")).toString()).equals("1E+9"), "cst045");
  } catch (Exception ecst045) {
    harness.check (false, "cst045");
  }
  try {
    harness.check (((new BigDecimal("1E+09")).toString()).equals("1E+9"), "cst046");
  } catch (Exception ecst046) {
    harness.check (false, "cst046");
  }
  try {
    harness.check (((new BigDecimal("1e+90")).toString()).equals("1E+90"), "cst047");
  } catch (Exception ecst047) {
    harness.check (false, "cst047");
  }
  try {
    harness.check (((new BigDecimal("1E+009")).toString()).equals("1E+9"), "cst048");
  } catch (Exception ecst048) {
    harness.check (false, "cst048");
  }
  try {
    harness.check (((new BigDecimal("0E+9")).toString()).equals("0"), "cst049");
  } catch (Exception ecst049) {
    harness.check (false, "cst049");
  }
  try {
    harness.check (((new BigDecimal("1E9")).toString()).equals("1E+9"), "cst050");
  } catch (Exception ecst050) {
    harness.check (false, "cst050");
  }
  try {
    harness.check (((new BigDecimal("1e09")).toString()).equals("1E+9"), "cst051");
  } catch (Exception ecst051) {
    harness.check (false, "cst051");
  }
  try {
    harness.check (((new BigDecimal("1E90")).toString()).equals("1E+90"), "cst052");
  } catch (Exception ecst052) {
    harness.check (false, "cst052");
  }
  try {
    harness.check (((new BigDecimal("1E009")).toString()).equals("1E+9"), "cst053");
  } catch (Exception ecst053) {
    harness.check (false, "cst053");
  }
  try {
    harness.check (((new BigDecimal("0E9")).toString()).equals("0"), "cst054");
  } catch (Exception ecst054) {
    harness.check (false, "cst054");
  }
  try {
    harness.check (((new BigDecimal("0.000e+0")).toString()).equals("0"), "cst055");
  } catch (Exception ecst055) {
    harness.check (false, "cst055");
  }
  try {
    harness.check (((new BigDecimal("0.000E-1")).toString()).equals("0"), "cst056");
  } catch (Exception ecst056) {
    harness.check (false, "cst056");
  }
  try {
    harness.check (((new BigDecimal("4E+9")).toString()).equals("4E+9"), "cst057");
  } catch (Exception ecst057) {
    harness.check (false, "cst057");
  }
  try {
    harness.check (((new BigDecimal("44E+9")).toString()).equals("4.4E+10"), "cst058");
  } catch (Exception ecst058) {
    harness.check (false, "cst058");
  }
  try {
    harness.check (((new BigDecimal("0.73e-7")).toString()).equals("7.3E-8"), "cst059");
  } catch (Exception ecst059) {
    harness.check (false, "cst059");
  }
  try {
    harness.check (((new BigDecimal("00E+9")).toString()).equals("0"), "cst060");
  } catch (Exception ecst060) {
    harness.check (false, "cst060");
  }
  try {
    harness.check (((new BigDecimal("00E-9")).toString()).equals("0"), "cst061");
  } catch (Exception ecst061) {
    harness.check (false, "cst061");
  }
  try {
    harness.check (((new BigDecimal("10E+9")).toString()).equals("1.0E+10"), "cst062");
  } catch (Exception ecst062) {
    harness.check (false, "cst062");
  }
  try {
    harness.check (((new BigDecimal("10E+09")).toString()).equals("1.0E+10"), "cst063");
  } catch (Exception ecst063) {
    harness.check (false, "cst063");
  }
  try {
    harness.check (((new BigDecimal("10e+90")).toString()).equals("1.0E+91"), "cst064");
  } catch (Exception ecst064) {
    harness.check (false, "cst064");
  }
  try {
    harness.check (((new BigDecimal("10E+009")).toString()).equals("1.0E+10"), "cst065");
  } catch (Exception ecst065) {
    harness.check (false, "cst065");
  }
  try {
    harness.check (((new BigDecimal("100e+9")).toString()).equals("1.00E+11"), "cst066");
  } catch (Exception ecst066) {
    harness.check (false, "cst066");
  }
  try {
    harness.check (((new BigDecimal("100e+09")).toString()).equals("1.00E+11"), "cst067");
  } catch (Exception ecst067) {
    harness.check (false, "cst067");
  }
  try {
    harness.check (((new BigDecimal("100E+90")).toString()).equals("1.00E+92"), "cst068");
  } catch (Exception ecst068) {
    harness.check (false, "cst068");
  }
  try {
    harness.check (((new BigDecimal("100e+009")).toString()).equals("1.00E+11"), "cst069");
  } catch (Exception ecst069) {
    harness.check (false, "cst069");
  }    

  try {
    harness.check (((new BigDecimal("1.265")).toString()).equals("1.265"), "cst070");
  } catch (Exception ecst070) {
    harness.check (false, "cst070");
  }
  try {
    harness.check (((new BigDecimal("1.265E-20")).toString()).equals("1.265E-20"), "cst071");
  } catch (Exception ecst071) {
    harness.check (false, "cst071");
  }
  try {
    harness.check (((new BigDecimal("1.265E-8")).toString()).equals("1.265E-8"), "cst072");
  } catch (Exception ecst072) {
    harness.check (false, "cst072");
  }
  try {
    harness.check (((new BigDecimal("1.265E-4")).toString()).equals("1.265E-4"), "cst073");
  } catch (Exception ecst073) {
    harness.check (false, "cst073");
  }
  try {
    harness.check (((new BigDecimal("1.265E-3")).toString()).equals("1.265E-3"), "cst074");
  } catch (Exception ecst074) {
    harness.check (false, "cst074");
  }
  try {
    harness.check (((new BigDecimal("1.265E-2")).toString()).equals("1.265E-2"), "cst075");
  } catch (Exception ecst075) {
    harness.check (false, "cst075");
  }
  try {
    harness.check (((new BigDecimal("1.265E-1")).toString()).equals("1.265E-1"), "cst076");
  } catch (Exception ecst076) {
    harness.check (false, "cst076");
  }
  try {
    harness.check (((new BigDecimal("1.265E-0")).toString()).equals("1.265"), "cst077");
  } catch (Exception ecst077) {
    harness.check (false, "cst077");
  }
  try {
    harness.check (((new BigDecimal("1.265E+1")).toString()).equals("1.265E+1"), "cst078");
  } catch (Exception ecst078) {
    harness.check (false, "cst078");
  }
  try {
    harness.check (((new BigDecimal("1.265E+2")).toString()).equals("1.265E+2"), "cst079");
  } catch (Exception ecst079) {
    harness.check (false, "cst079");
  }
  try {
    harness.check (((new BigDecimal("1.265E+3")).toString()).equals("1.265E+3"), "cst080");
  } catch (Exception ecst080) {
    harness.check (false, "cst080");
  }
  try {
    harness.check (((new BigDecimal("1.265E+4")).toString()).equals("1.265E+4"), "cst081");
  } catch (Exception ecst081) {
    harness.check (false, "cst081");
  }
  try {
    harness.check (((new BigDecimal("1.265E+8")).toString()).equals("1.265E+8"), "cst082");
  } catch (Exception ecst082) {
    harness.check (false, "cst082");
  }
  try {
    harness.check (((new BigDecimal("1.265E+20")).toString()).equals("1.265E+20"), "cst083");
  } catch (Exception ecst083) {
    harness.check (false, "cst083");
  }
  
  try {
    harness.check (((new BigDecimal("12.65")).toString()).equals("12.65"), "cst090");
  } catch (Exception ecst090) {
    harness.check (false, "cst090");
  }
  try {
    harness.check (((new BigDecimal("12.65E-20")).toString()).equals("1.265E-19"), "cst091");
  } catch (Exception ecst091) {
    harness.check (false, "cst091");
  }
  try {
    harness.check (((new BigDecimal("12.65E-8")).toString()).equals("1.265E-7"), "cst092");
  } catch (Exception ecst092) {
    harness.check (false, "cst092");
  }
  try {
    harness.check (((new BigDecimal("12.65E-4")).toString()).equals("1.265E-3"), "cst093");
  } catch (Exception ecst093) {
    harness.check (false, "cst093");
  }
  try {
    harness.check (((new BigDecimal("12.65E-3")).toString()).equals("1.265E-2"), "cst094");
  } catch (Exception ecst094) {
    harness.check (false, "cst094");
  }
  try {
    harness.check (((new BigDecimal("12.65E-2")).toString()).equals("1.265E-1"), "cst095");
  } catch (Exception ecst095) {
    harness.check (false, "cst095");
  }
  try {
    harness.check (((new BigDecimal("12.65E-1")).toString()).equals("1.265"), "cst096");
  } catch (Exception ecst096) {
    harness.check (false, "cst096");
  }
  try {
    harness.check (((new BigDecimal("12.65E-0")).toString()).equals("1.265E+1"), "cst097");
  } catch (Exception ecst097) {
    harness.check (false, "cst097");
  }
  try {
    harness.check (((new BigDecimal("12.65E+1")).toString()).equals("1.265E+2"), "cst098");
  } catch (Exception ecst098) {
    harness.check (false, "cst098");
  }
  try {
    harness.check (((new BigDecimal("12.65E+2")).toString()).equals("1.265E+3"), "cst099");
  } catch (Exception ecst099) {
    harness.check (false, "cst099");
  }
  try {
    harness.check (((new BigDecimal("12.65E+3")).toString()).equals("1.265E+4"), "cst100");
  } catch (Exception ecst100) {
    harness.check (false, "cst100");
  }
  try {
    harness.check (((new BigDecimal("12.65E+4")).toString()).equals("1.265E+5"), "cst101");
  } catch (Exception ecst101) {
    harness.check (false, "cst101");
  }
  try {
    harness.check (((new BigDecimal("12.65E+8")).toString()).equals("1.265E+9"), "cst102");
  } catch (Exception ecst102) {
    harness.check (false, "cst102");
  }
  try {
    harness.check (((new BigDecimal("12.65E+20")).toString()).equals("1.265E+21"), "cst103");
  } catch (Exception ecst103) {
    harness.check (false, "cst103");
  }
  
  try {
    harness.check (((new BigDecimal("126.5")).toString()).equals("126.5"), "cst110");
  } catch (Exception ecst110) {
    harness.check (false, "cst110");
  }
  try {
    harness.check (((new BigDecimal("126.5E-20")).toString()).equals("1.265E-18"), "cst111");
  } catch (Exception ecst111) {
    harness.check (false, "cst111");
  }
  try {
    harness.check (((new BigDecimal("126.5E-8")).toString()).equals("1.265E-6"), "cst112");
  } catch (Exception ecst112) {
    harness.check (false, "cst112");
  }
  try {
    harness.check (((new BigDecimal("126.5E-4")).toString()).equals("1.265E-2"), "cst113");
  } catch (Exception ecst113) {
    harness.check (false, "cst113");
  }
  try {
    harness.check (((new BigDecimal("126.5E-3")).toString()).equals("1.265E-1"), "cst114");
  } catch (Exception ecst114) {
    harness.check (false, "cst114");
  }
  try {
    harness.check (((new BigDecimal("126.5E-2")).toString()).equals("1.265"), "cst115");
  } catch (Exception ecst115) {
    harness.check (false, "cst115");
  }
  try {
    harness.check (((new BigDecimal("126.5E-1")).toString()).equals("1.265E+1"), "cst116");
  } catch (Exception ecst116) {
    harness.check (false, "cst116");
  }
  try {
    harness.check (((new BigDecimal("126.5E-0")).toString()).equals("1.265E+2"), "cst117");
  } catch (Exception ecst117) {
    harness.check (false, "cst117");
  }
  try {
    harness.check (((new BigDecimal("126.5E+1")).toString()).equals("1.265E+3"), "cst118");
  } catch (Exception ecst118) {
    harness.check (false, "cst118");
  }
  try {
    harness.check (((new BigDecimal("126.5E+2")).toString()).equals("1.265E+4"), "cst119");
  } catch (Exception ecst119) {
    harness.check (false, "cst119");
  }
  try {
    harness.check (((new BigDecimal("126.5E+3")).toString()).equals("1.265E+5"), "cst120");
  } catch (Exception ecst120) {
    harness.check (false, "cst120");
  }
  try {
    harness.check (((new BigDecimal("126.5E+4")).toString()).equals("1.265E+6"), "cst121");
  } catch (Exception ecst121) {
    harness.check (false, "cst121");
  }
  try {
    harness.check (((new BigDecimal("126.5E+8")).toString()).equals("1.265E+10"), "cst122");
  } catch (Exception ecst122) {
    harness.check (false, "cst122");
  }
  try {
    harness.check (((new BigDecimal("126.5E+20")).toString()).equals("1.265E+22"), "cst123");
  } catch (Exception ecst123) {
    harness.check (false, "cst123");
  }
  
  try {
    harness.check (((new BigDecimal("1265")).toString()).equals("1265"), "cst130");
  } catch (Exception ecst130) {
    harness.check (false, "cst130");
  }
  try {
    harness.check (((new BigDecimal("1265E-20")).toString()).equals("1.265E-17"), "cst131");
  } catch (Exception ecst131) {
    harness.check (false, "cst131");
  }
  try {
    harness.check (((new BigDecimal("1265E-8")).toString()).equals("1.265E-5"), "cst132");
  } catch (Exception ecst132) {
    harness.check (false, "cst132");
  }
  try {
    harness.check (((new BigDecimal("1265E-4")).toString()).equals("1.265E-1"), "cst133");
  } catch (Exception ecst133) {
    harness.check (false, "cst133");
  }
  try {
    harness.check (((new BigDecimal("1265E-3")).toString()).equals("1.265"), "cst134");
  } catch (Exception ecst134) {
    harness.check (false, "cst134");
  }
  try {
    harness.check (((new BigDecimal("1265E-2")).toString()).equals("1.265E+1"), "cst135");
  } catch (Exception ecst135) {
    harness.check (false, "cst135");
  }
  try {
    harness.check (((new BigDecimal("1265E-1")).toString()).equals("1.265E+2"), "cst136");
  } catch (Exception ecst136) {
    harness.check (false, "cst136");
  }
  try {
    harness.check (((new BigDecimal("1265E-0")).toString()).equals("1.265E+3"), "cst137");
  } catch (Exception ecst137) {
    harness.check (false, "cst137");
  }
  try {
    harness.check (((new BigDecimal("1265E+1")).toString()).equals("1.265E+4"), "cst138");
  } catch (Exception ecst138) {
    harness.check (false, "cst138");
  }
  try {
    harness.check (((new BigDecimal("1265E+2")).toString()).equals("1.265E+5"), "cst139");
  } catch (Exception ecst139) {
    harness.check (false, "cst139");
  }
  try {
    harness.check (((new BigDecimal("1265E+3")).toString()).equals("1.265E+6"), "cst140");
  } catch (Exception ecst140) {
    harness.check (false, "cst140");
  }
  try {
    harness.check (((new BigDecimal("1265E+4")).toString()).equals("1.265E+7"), "cst141");
  } catch (Exception ecst141) {
    harness.check (false, "cst141");
  }
  try {
    harness.check (((new BigDecimal("1265E+8")).toString()).equals("1.265E+11"), "cst142");
  } catch (Exception ecst142) {
    harness.check (false, "cst142");
  }
  try {
    harness.check (((new BigDecimal("1265E+20")).toString()).equals("1.265E+23"), "cst143");
  } catch (Exception ecst143) {
    harness.check (false, "cst143");
  }
  try {
    harness.check (((new BigDecimal("0.1265")).toString()).equals("0.1265"), "cst150");
  } catch (Exception ecst150) {
    harness.check (false, "cst150");
  }
  try {
    harness.check (((new BigDecimal("0.1265E-20")).toString()).equals("1.265E-21"), "cst151");
  } catch (Exception ecst151) {
    harness.check (false, "cst151");
  }
  try {
    harness.check (((new BigDecimal("0.1265E-8")).toString()).equals("1.265E-9"), "cst152");
  } catch (Exception ecst152) {
    harness.check (false, "cst152");
  }
  try {
    harness.check (((new BigDecimal("0.1265E-4")).toString()).equals("1.265E-5"), "cst153");
  } catch (Exception ecst153) {
    harness.check (false, "cst153");
  }
  try {
    harness.check (((new BigDecimal("0.1265E-3")).toString()).equals("1.265E-4"), "cst154");
  } catch (Exception ecst154) {
    harness.check (false, "cst154");
  }
  try {
    harness.check (((new BigDecimal("0.1265E-2")).toString()).equals("1.265E-3"), "cst155");
  } catch (Exception ecst155) {
    harness.check (false, "cst155");
  }
  try {
    harness.check (((new BigDecimal("0.1265E-1")).toString()).equals("1.265E-2"), "cst156");
  } catch (Exception ecst156) {
    harness.check (false, "cst156");
  }
  try {
    harness.check (((new BigDecimal("0.1265E-0")).toString()).equals("1.265E-1"), "cst157");
  } catch (Exception ecst157) {
    harness.check (false, "cst157");
  }
  try {
    harness.check (((new BigDecimal("0.1265E+1")).toString()).equals("1.265"), "cst158");
  } catch (Exception ecst158) {
    harness.check (false, "cst158");
  }
  try {
    harness.check (((new BigDecimal("0.1265E+2")).toString()).equals("1.265E+1"), "cst159");
  } catch (Exception ecst159) {
    harness.check (false, "cst159");
  }
  try {
    harness.check (((new BigDecimal("0.1265E+3")).toString()).equals("1.265E+2"), "cst160");
  } catch (Exception ecst160) {
    harness.check (false, "cst160");
  }
  try {
    harness.check (((new BigDecimal("0.1265E+4")).toString()).equals("1.265E+3"), "cst161");
  } catch (Exception ecst161) {
    harness.check (false, "cst161");
  }
  try {
    harness.check (((new BigDecimal("0.1265E+8")).toString()).equals("1.265E+7"), "cst162");
  } catch (Exception ecst162) {
    harness.check (false, "cst162");
  }
  try {
    harness.check (((new BigDecimal("0.1265E+20")).toString()).equals("1.265E+19"), "cst163");
  } catch (Exception ecst163) {
    harness.check (false, "cst163");
  }
  try {
    harness.check (((new BigDecimal("0.09e999999999")).toString()).equals("9E+999999997"), "cst170");
  } catch (Exception ecst170) {
    harness.check (false, "cst170");
  }
  try {
    harness.check (((new BigDecimal("0.9e999999999")).toString()).equals("9E+999999998"), "cst171");
  } catch (Exception ecst171) {
    harness.check (false, "cst171");
  }
  try {
    harness.check (((new BigDecimal("9e999999999")).toString()).equals("9E+999999999"), "cst172");
  } catch (Exception ecst172) {
    harness.check (false, "cst172");
  }
  try {
    harness.check (((new BigDecimal("9.9e999999999")).toString()).equals("9.9E+999999999"), "cst173");
  } catch (Exception ecst173) {
    harness.check (false, "cst173");
  }
  try {
    harness.check (((new BigDecimal("9.99e999999999")).toString()).equals("9.99E+999999999"), "cst174");
  } catch (Exception ecst174) {
    harness.check (false, "cst174");
  }
  try {
    harness.check (((new BigDecimal("9.99e-999999999")).toString()).equals("9.99E-999999999"), "cst175");
  } catch (Exception ecst175) {
    harness.check (false, "cst175");
  }
  try {
    harness.check (((new BigDecimal("9.9e-999999999")).toString()).equals("9.9E-999999999"), "cst176");
  } catch (Exception ecst176) {
    harness.check (false, "cst176");
  }
  try {
    harness.check (((new BigDecimal("9e-999999999")).toString()).equals("9E-999999999"), "cst177");
  } catch (Exception ecst177) {
    harness.check (false, "cst177");
  }
  try {
    harness.check (((new BigDecimal("99e-999999999")).toString()).equals("9.9E-999999998"), "cst179");
  } catch (Exception ecst179) {
    harness.check (false, "cst179");
  }
  try {
    harness.check (((new BigDecimal("999e-999999999")).toString()).equals("9.99E-999999997"), "cst180");
  } catch (Exception ecst180) {
    harness.check (false, "cst180");
  }
  
  // baddies --
  badstrings=new java.lang.String[]{"1..2",".","..","++1","--1","-+1","+-1","12e","12e++","12f4"," +1","+ 1","12 "," + 1"," - 1 ","x","-1-","12-","3+","","1e-","7e1000000000","","e100","\u0e5a","\u0b65","99e999999999","999e999999999","0.9e-999999999","0.09e-999999999","0.1e1000000000","10e-1000000000","0.9e9999999999","99e-9999999999","111e9999999999","1111e-9999999999"+" "+"111e*123","111e123-","111e+12+","111e1-3-","111e1*23","111e1e+3","1e1.0","1e123e","ten","ONE","1e.1","1e1.","1ee","e+1"}; // 200-203
  // 204-207
  // 208-211
  // 211-214
  // 215-219
  // 220-222
  // 223-224
  // 225-226
  // 227-228
  // 229-230
  // 231-232
  // 233-234
  // 235-237
  // 238-240
  // 241-244
  // 245-248
  
  // watch out for commas on continuation lines
  
  {int xx16=badstrings.length;i=0;i:for(;xx16>0;xx16--,i++){
   try{
    new BigDecimal(badstrings[i]);
    say(">>> cst"+(200+i)+":"+" "+badstrings[i]+" "+(new BigDecimal(badstrings[i])).toString(), harness);
    flag=false;
   }
   catch (java.lang.NumberFormatException xx17){
    flag=true;
   }
   harness.check (flag, "cst"+(200+i));
   }
  }/*i*/
  
  try{checknull:do{
   new BigDecimal((java.lang.String)null);
   flag=false;
  }while(false);}
  catch (java.lang.NullPointerException xx18){
   flag=true;
  }/*checknull*/
  harness.check (flag, "cst301");
  
  return;
  }

 /** Mutation tests (checks that contents of constant objects are unchanged). */
 
 public void diagmutation(TestHarness harness){
  /* ---------------------------------------------------------------- */
  /* Final tests -- check constants haven't mutated                   */
  /*             -- also that MC objects haven't mutated              */
  /* ---------------------------------------------------------------- */
   harness.check ((zero.toString()).equals("0"), "cuc001");
   harness.check ((one.toString()).equals("1"), "cuc002");
   harness.check ((ten.toString()).equals("10"), "cuc003");
  
  return;}

 
 /* ----------------------------------------------------------------- */
 /* Operator test methods                                             */
 /* ----------------------------------------------------------------- */
 // The use of context in these tests are primarily to show that they
 // are correctly passed to the methods, except that we check that
 // each method checks for lostDigits.
 
 /** Test the {@link BigDecimal#abs} method. */
 
 public void diagabs(TestHarness harness){
  boolean flag=false;
  java.lang.ArithmeticException ae=null;
  
  // most of the function of this is tested by add
  harness.check (((new BigDecimal("2")).abs().toString()).equals("2"), "abs001");
  harness.check (((new BigDecimal("-2")).abs().toString()).equals("2"), "abs002");
  harness.check (((new BigDecimal("+0.000")).abs().toString()).equals("0.000"), "abs003");
  harness.check (((new BigDecimal("00.000")).abs().toString()).equals("0.000"), "abs004");
  harness.check (((new BigDecimal("-0.000")).abs().toString()).equals("0.000"), "abs005");
  harness.check (((new BigDecimal("-2000000")).abs().toString()).equals("2000000"), "abs009");
  harness.check (((new BigDecimal("0.2")).abs().toString()).equals("0.2"), "abs013");
  harness.check (((new BigDecimal("-0.2")).abs().toString()).equals("0.2"), "abs014");
  harness.check (((new BigDecimal("0.01")).abs().toString()).equals("0.01"), "abs015");
  harness.check (((new BigDecimal("-0.01")).abs().toString()).equals("0.01"), "abs016");

  return;}
/* ----------------------------------------------------------------- */
 
 /** Test the {@link BigDecimal#add} method. */
 
 public void diagadd(TestHarness harness){
  boolean flag=false;
  BigDecimal alhs;
  BigDecimal arhs;
  java.lang.ArithmeticException ae=null;
  
  // [Now the same group with fixed arithmetic]
  harness.check (((new BigDecimal(2)).add(new BigDecimal(3)).toString()).equals("5"), "add030");
  harness.check (((new BigDecimal("5.75")).add(new BigDecimal("3.3")).toString()).equals("9.05"), "add031");
  harness.check (((new BigDecimal("5")).add(new BigDecimal("-3")).toString()).equals("2"), "add032");
  harness.check (((new BigDecimal("-5")).add(new BigDecimal("-3")).toString()).equals("-8"), "add033");
  harness.check (((new BigDecimal("-7")).add(new BigDecimal("2.5")).toString()).equals("-4.5"), "add034");
  harness.check (((new BigDecimal("0.7")).add(new BigDecimal("0.3")).toString()).equals("1.0"), "add035");
  harness.check (((new BigDecimal("1.25")).add(new BigDecimal("1.25")).toString()).equals("2.50"), "add036");
  harness.check (((new BigDecimal("1.23456789")).add(new BigDecimal("1.00000000")).toString()).equals("2.23456789"), "add037");
  
  harness.check (((new BigDecimal("1.23456789")).add(new BigDecimal("1.00000011")).toString()).equals("2.23456800"), "add038");
  
  
  harness.check (((new BigDecimal("0.4444444444")).add(new BigDecimal("0.5555555555")).toString()).equals("0.9999999999"), "add039");
  
  harness.check (((new BigDecimal("0.4444444440")).add(new BigDecimal("0.5555555555")).toString()).equals("0.9999999995"), "add040");
  
  harness.check (((new BigDecimal("0.4444444444")).add(new BigDecimal("0.5555555550")).toString()).equals("0.9999999994"), "add041");
  
  harness.check (((new BigDecimal("0.4444444444999")).add(new BigDecimal("0")).toString()).equals("0.4444444444999"), "add042");
  
  harness.check (((new BigDecimal("0.4444444445000")).add(new BigDecimal("0")).toString()).equals("0.4444444445000"), "add043");
  
  
  harness.check (((new BigDecimal("70")).add(new BigDecimal("10000e+9")).toString()).equals("10000000000070"), "add044");
  
  harness.check (((new BigDecimal("700")).add(new BigDecimal("10000e+9")).toString()).equals("10000000000700"), "add045");
  
  harness.check (((new BigDecimal("7000")).add(new BigDecimal("10000e+9")).toString()).equals("10000000007000"), "add046");
  
  harness.check (((new BigDecimal("70000")).add(new BigDecimal("10000e+9")).toString()).equals("10000000070000"), "add047");
  
  harness.check (((new BigDecimal("700000")).add(new BigDecimal("10000e+9")).toString()).equals("10000000700000"), "add048");
  
  harness.check (((new BigDecimal("10000e+9")).add(new BigDecimal("70")).toString()).equals("10000000000070"), "add054");
  
  harness.check (((new BigDecimal("10000e+9")).add(new BigDecimal("700")).toString()).equals("10000000000700"), "add055");
  
  harness.check (((new BigDecimal("10000e+9")).add(new BigDecimal("7000")).toString()).equals("10000000007000"), "add056");
  
  harness.check (((new BigDecimal("10000e+9")).add(new BigDecimal("70000")).toString()).equals("10000000070000"), "add057");
  
  harness.check (((new BigDecimal("10000e+9")).add(new BigDecimal("700000")).toString()).equals("10000000700000"), "add058");
  
  // some rounding effects
  harness.check (((new BigDecimal("0.9998")).add(new BigDecimal("0.0000")).toString()).equals("0.9998"), "add059");
  
  harness.check (((new BigDecimal("0.9998")).add(new BigDecimal("0.0001")).toString()).equals("0.9999"), "add060");
  
  harness.check (((new BigDecimal("0.9998")).add(new BigDecimal("0.0002")).toString()).equals("1.0000"), "add061");
  
  harness.check (((new BigDecimal("0.9998")).add(new BigDecimal("0.0003")).toString()).equals("1.0001"), "add062");
  
  // more fixed, LHS swaps
  harness.check (((new BigDecimal("-56267E-10")).add(zero).toString()).equals("-0.0000056267"), "add090");
  harness.check (((new BigDecimal("-56267E-6")).add(zero).toString()).equals("-0.056267"), "add091");
  harness.check (((new BigDecimal("-56267E-5")).add(zero).toString()).equals("-0.56267"), "add092");
  harness.check (((new BigDecimal("-56267E-4")).add(zero).toString()).equals("-5.6267"), "add093");
  harness.check (((new BigDecimal("-56267E-3")).add(zero).toString()).equals("-56.267"), "add094");
  harness.check (((new BigDecimal("-56267E-2")).add(zero).toString()).equals("-562.67"), "add095");
  harness.check (((new BigDecimal("-56267E-1")).add(zero).toString()).equals("-5626.7"), "add096");
  harness.check (((new BigDecimal("-56267E-0")).add(zero).toString()).equals("-56267"), "add097");
  harness.check (((new BigDecimal("-5E-10")).add(zero).toString()).equals("-5E-10"), "add098");
  harness.check (((new BigDecimal("-5E-5")).add(zero).toString()).equals("-0.00005"), "add099");
  harness.check (((new BigDecimal("-5E-1")).add(zero).toString()).equals("-0.5"), "add100");
  harness.check (((new BigDecimal("-5E-10")).add(zero).toString()).equals("-5E-10"), "add101");
  harness.check (((new BigDecimal("-5E-5")).add(zero).toString()).equals("-0.00005"), "add102");
  harness.check (((new BigDecimal("-5E-1")).add(zero).toString()).equals("-0.5"), "add103");
  harness.check (((new BigDecimal("-5E10")).add(zero).toString()).equals("-50000000000"), "add104");
  harness.check (((new BigDecimal("-5E5")).add(zero).toString()).equals("-500000"), "add105");
  harness.check (((new BigDecimal("-5E1")).add(zero).toString()).equals("-50"), "add106");
  harness.check (((new BigDecimal("-5E0")).add(zero).toString()).equals("-5"), "add107");
  
  // more fixed, RHS swaps
  harness.check ((zero.add(new BigDecimal("-56267E-10")).toString()).equals("-0.0000056267"), "add108");
  harness.check ((zero.add(new BigDecimal("-56267E-6")).toString()).equals("-0.056267"), "add109");
  harness.check ((zero.add(new BigDecimal("-56267E-5")).toString()).equals("-0.56267"), "add110");
  harness.check ((zero.add(new BigDecimal("-56267E-4")).toString()).equals("-5.6267"), "add111");
  harness.check ((zero.add(new BigDecimal("-56267E-3")).toString()).equals("-56.267"), "add112");
  harness.check ((zero.add(new BigDecimal("-56267E-2")).toString()).equals("-562.67"), "add113");
  harness.check ((zero.add(new BigDecimal("-56267E-1")).toString()).equals("-5626.7"), "add114");
  harness.check ((zero.add(new BigDecimal("-56267E-0")).toString()).equals("-56267"), "add115");
  harness.check ((zero.add(new BigDecimal("-5E-10")).toString()).equals("-5E-10"), "add116");
  harness.check ((zero.add(new BigDecimal("-5E-5")).toString()).equals("-0.00005"), "add117");
  harness.check ((zero.add(new BigDecimal("-5E-1")).toString()).equals("-0.5"), "add118");
  harness.check ((zero.add(new BigDecimal("-5E-10")).toString()).equals("-5E-10"), "add129");
  harness.check ((zero.add(new BigDecimal("-5E-5")).toString()).equals("-0.00005"), "add130");
  harness.check ((zero.add(new BigDecimal("-5E-1")).toString()).equals("-0.5"), "add131");
  harness.check ((zero.add(new BigDecimal("-5E10")).toString()).equals("-50000000000"), "add132");
  harness.check ((zero.add(new BigDecimal("-5E5")).toString()).equals("-500000"), "add133");
  harness.check ((zero.add(new BigDecimal("-5E1")).toString()).equals("-50"), "add134");
  harness.check ((zero.add(new BigDecimal("-5E0")).toString()).equals("-5"), "add135");
  
  harness.check (((new BigDecimal("00.0")).add(new BigDecimal("0.00")).toString()).equals("0.00"), "add150");
  harness.check (((new BigDecimal("0.00")).add(new BigDecimal("00.0")).toString()).equals("0.00"), "add151");
  harness.check (((new BigDecimal("3")).add(new BigDecimal(".3")).toString()).equals("3.3"), "add152");
  harness.check (((new BigDecimal("3.")).add(new BigDecimal(".3")).toString()).equals("3.3"), "add153");
  harness.check (((new BigDecimal("3.0")).add(new BigDecimal(".3")).toString()).equals("3.3"), "add154");
  harness.check (((new BigDecimal("3.00")).add(new BigDecimal(".3")).toString()).equals("3.30"), "add155");
  harness.check (((new BigDecimal("3")).add(new BigDecimal("3")).toString()).equals("6"), "add156");
  harness.check (((new BigDecimal("3")).add(new BigDecimal("+3")).toString()).equals("6"), "add157");
  harness.check (((new BigDecimal("3")).add(new BigDecimal("-3")).toString()).equals("0"), "add158");
  harness.check (((new BigDecimal("0.3")).add(new BigDecimal("-0.3")).toString()).equals("0.0"), "add159");
  harness.check (((new BigDecimal("0.03")).add(new BigDecimal("-0.03")).toString()).equals("0.00"), "add160");
  
  // input preparation tests
  alhs=new BigDecimal("12345678900000");
  arhs=new BigDecimal("9999999999999");
  
  try{checknull:do{
   ten.add((BigDecimal)null);
   flag=false;
  }while(false);}
  catch (java.lang.NullPointerException xx22){
   flag=true;
  }/*checknull*/
  harness.check (flag, "add200");
  
  return;}

 /* ----------------------------------------------------------------- */
 /** Test the {@link BigDecimal#compareTo(BigDecimal)} method. */
 
 public void diagcompareto(TestHarness harness){
  boolean flag=false;
  java.lang.ArithmeticException ae=null;
  // we assume add/subtract test function; this just
  // tests existence, exceptions, and possible results
  
  harness.check (((new BigDecimal("5")).compareTo(new BigDecimal("2")))==1, "cpt001");
  harness.check (((new BigDecimal("5")).compareTo(new BigDecimal("5")))==0, "cpt002");
  harness.check (((new BigDecimal("5")).compareTo(new BigDecimal("5.00")))==0, "cpt003");
  harness.check (((new BigDecimal("0.5")).compareTo(new BigDecimal("0.5")))==0, "cpt004");
  harness.check (((new BigDecimal("2")).compareTo(new BigDecimal("5")))==(-1), "cpt005");
  try{checknull:do{
   ten.compareTo((BigDecimal)null);
   flag=false;
  }while(false);}
  catch (java.lang.NullPointerException xx28){
   flag=true;
  }/*checknull*/
  harness.check (flag, "cpt100");
  
  return;}

 /* ----------------------------------------------------------------- */
 
 /** Test the {@link BigDecimal#divide} method. */
 
 public void diagdivide(TestHarness harness){
  boolean flag=false;
  int rhu;
  int rd;
  int ru;
  java.lang.RuntimeException e=null;
  java.lang.ArithmeticException ae=null;
  
  // fixed point...
  harness.check (((new BigDecimal("1")).divide(new BigDecimal("3"), BigDecimal.ROUND_HALF_UP).toString()).equals("0"), "div350");
  harness.check (((new BigDecimal("2")).divide(new BigDecimal("3"), BigDecimal.ROUND_HALF_UP).toString()).equals("1"), "div351");
  harness.check (((new BigDecimal("2.4")).divide(new BigDecimal("1"), BigDecimal.ROUND_HALF_UP).toString()).equals("2.4"), "div352");
  harness.check (((new BigDecimal("2.4")).divide(new BigDecimal("-1"), BigDecimal.ROUND_HALF_UP).toString()).equals("-2.4"), "div353");
  harness.check (((new BigDecimal("-2.4")).divide(new BigDecimal("1"), BigDecimal.ROUND_HALF_UP).toString()).equals("-2.4"), "div354");
  harness.check (((new BigDecimal("-2.4")).divide(new BigDecimal("-1"), BigDecimal.ROUND_HALF_UP).toString()).equals("2.4"), "div355");
  harness.check (((new BigDecimal("2.40")).divide(new BigDecimal("1"), BigDecimal.ROUND_HALF_UP).toString()).equals("2.40"), "div356");
  harness.check (((new BigDecimal("2.400")).divide(new BigDecimal("1"), BigDecimal.ROUND_HALF_UP).toString()).equals("2.400"), "div357");
  harness.check (((new BigDecimal("2.4")).divide(new BigDecimal("2"), BigDecimal.ROUND_HALF_UP).toString()).equals("1.2"), "div358");
  harness.check (((new BigDecimal("2.400")).divide(new BigDecimal("2"), BigDecimal.ROUND_HALF_UP).toString()).equals("1.200"), "div359");
  harness.check (((new BigDecimal("2.")).divide(new BigDecimal("2"), BigDecimal.ROUND_HALF_UP).toString()).equals("1"), "div360");
  harness.check (((new BigDecimal("20")).divide(new BigDecimal("20"), BigDecimal.ROUND_HALF_UP).toString()).equals("1"), "div361");
  harness.check (((new BigDecimal("187")).divide(new BigDecimal("187"), BigDecimal.ROUND_HALF_UP).toString()).equals("1"), "div362");
  harness.check (((new BigDecimal("5")).divide(new BigDecimal("2"), BigDecimal.ROUND_HALF_UP).toString()).equals("3"), "div363");
  harness.check (((new BigDecimal("5")).divide(new BigDecimal("2.0"), BigDecimal.ROUND_HALF_UP).toString()).equals("3"), "div364");
  harness.check (((new BigDecimal("5")).divide(new BigDecimal("2.000"), BigDecimal.ROUND_HALF_UP).toString()).equals("3"), "div365");
  harness.check (((new BigDecimal("5")).divide(new BigDecimal("0.200"), BigDecimal.ROUND_HALF_UP).toString()).equals("25"), "div366");
  harness.check (((new BigDecimal("5.0")).divide(new BigDecimal("2"), BigDecimal.ROUND_HALF_UP).toString()).equals("2.5"), "div367");
  harness.check (((new BigDecimal("5.0")).divide(new BigDecimal("2.0"), BigDecimal.ROUND_HALF_UP).toString()).equals("2.5"), "div368");
  harness.check (((new BigDecimal("5.0")).divide(new BigDecimal("2.000"), BigDecimal.ROUND_HALF_UP).toString()).equals("2.5"), "div369");
  harness.check (((new BigDecimal("5.0")).divide(new BigDecimal("0.200"), BigDecimal.ROUND_HALF_UP).toString()).equals("25.0"), "div370");
  harness.check (((new BigDecimal("999999999")).divide(new BigDecimal("1"), BigDecimal.ROUND_HALF_UP).toString()).equals("999999999"), "div371");
  harness.check (((new BigDecimal("999999999.4")).divide(new BigDecimal("1"), BigDecimal.ROUND_HALF_UP).toString()).equals("999999999.4"), "div372");
  harness.check (((new BigDecimal("999999999.5")).divide(new BigDecimal("1"), BigDecimal.ROUND_HALF_UP).toString()).equals("999999999.5"), "div373");
  harness.check (((new BigDecimal("999999999.9")).divide(new BigDecimal("1"), BigDecimal.ROUND_HALF_UP).toString()).equals("999999999.9"), "div374");
  harness.check (((new BigDecimal("999999999.999")).divide(new BigDecimal("1"), BigDecimal.ROUND_HALF_UP).toString()).equals("999999999.999"), "div375");
  harness.check (((new BigDecimal("0.0000E-5")).divide(new BigDecimal("1"), BigDecimal.ROUND_HALF_UP).toString()).equals("0E-9"), "div376");
  harness.check (((new BigDecimal("0.000000000")).divide(new BigDecimal("1"), BigDecimal.ROUND_HALF_UP).toString()).equals("0E-9"), "div377");
  
  //- Fixed point; explicit scales & rounds [old BigDecimal divides]
  harness.check (((new BigDecimal("0")).divide(new BigDecimal("3"), BigDecimal.ROUND_HALF_UP).toString()).equals("0"), "div001");
  harness.check (((new BigDecimal("1")).divide(new BigDecimal("3"), BigDecimal.ROUND_HALF_UP).toString()).equals("0"), "div008");
  harness.check (((new BigDecimal("2")).divide(new BigDecimal("3"), BigDecimal.ROUND_HALF_UP).toString()).equals("1"), "div015");
  
  try{div0:do{
   (new BigDecimal("5")).divide(new BigDecimal("0.00"), BigDecimal.ROUND_HALF_UP);
   flag=false;
  }while(false);}
  catch (java.lang.ArithmeticException xx40){ae=xx40;
   flag=checkMessage(ae, "Divide by 0");
  }/*div0*/
  harness.check (flag, "div204");

  return;}
/* ----------------------------------------------------------------- */
 
 /** Test the {@link BigDecimal#max} method. */
 
 public void diagmax(TestHarness harness){
  boolean flag=false;
  java.lang.ArithmeticException ae=null;
  
  // we assume add/subtract test function; this and min just
  // test existence and test the truth table
  harness.check (((new BigDecimal("5")).max(new BigDecimal("2")).toString()).equals("5"), "max001");
  harness.check (((new BigDecimal("5")).max(new BigDecimal("5")).toString()).equals("5"), "max002");
  harness.check (((new BigDecimal("2")).max(new BigDecimal("7")).toString()).equals("7"), "max003");
  harness.check (((new BigDecimal("2E+3")).max(new BigDecimal("7")).toString()).equals("2E+3"), "max006");
  harness.check (((new BigDecimal("7")).max(new BigDecimal("2E+3")).toString()).equals("2E+3"), "max008");

  try{checknull:do{
   ten.max((BigDecimal)null);
   flag=false;
  }while(false);}
  catch (java.lang.NullPointerException xx47){
   flag=true;
  }/*checknull*/
  harness.check (flag, "max010");
  return;}

 /** Test the {@link BigDecimal#min} method. */
 
 public void diagmin(TestHarness harness){
  boolean flag=false;
  BigDecimal minx=null;
  java.lang.ArithmeticException ae=null;
  // we assume add/subtract test function; this and max just
  // test existence and test the truth table
  
  harness.check (((new BigDecimal("5")).min(new BigDecimal("2")).toString()).equals("2"), "min001");
  harness.check (((new BigDecimal("5")).min(new BigDecimal("5")).toString()).equals("5"), "min002");
  harness.check (((new BigDecimal("2")).min(new BigDecimal("7")).toString()).equals("2"), "min003");
  harness.check (((new BigDecimal("-2E+3")).min(new BigDecimal("7")).toString()).equals("-2E+3"), "min006");
  harness.check (((new BigDecimal("7")).min(new BigDecimal("-2E+3")).toString()).equals("-2E+3"), "min008");
  try{checknull:do{
   minx=ten;
   minx.min((BigDecimal)null);
   flag=false;
  }while(false);}
  catch (java.lang.NullPointerException xx51){
   flag=true;
  }/*checknull*/
  harness.check (flag, "min010");
  
  return;}
/* ----------------------------------------------------------------- */
 
 /** Test the {@link BigDecimal#multiply} method. */
 
 public void diagmultiply(TestHarness harness){
  boolean flag=false;
  BigDecimal l9;
  BigDecimal l77e;
  BigDecimal l12345;
  BigDecimal edge;
  BigDecimal tenedge;
  BigDecimal hunedge;
  BigDecimal opo;
  BigDecimal d1=null;
  BigDecimal d2=null;
  java.lang.ArithmeticException oe=null;
  java.lang.ArithmeticException ae=null;
  
  harness.check (((new BigDecimal("2")).multiply(new BigDecimal("3")).toString()).equals("6"), "mul020");
  harness.check (((new BigDecimal("5")).multiply(new BigDecimal("1")).toString()).equals("5"), "mul021");
  harness.check (((new BigDecimal("5")).multiply(new BigDecimal("2")).toString()).equals("10"), "mul022");
  harness.check (((new BigDecimal("1.20")).multiply(new BigDecimal("2")).toString()).equals("2.40"), "mul023");
  harness.check (((new BigDecimal("1.20")).multiply(new BigDecimal("0")).toString()).equals("0.00"), "mul024");
  harness.check (((new BigDecimal("1.20")).multiply(new BigDecimal("-2")).toString()).equals("-2.40"), "mul025");
  harness.check (((new BigDecimal("-1.20")).multiply(new BigDecimal("2")).toString()).equals("-2.40"), "mul026");
  harness.check (((new BigDecimal("-1.20")).multiply(new BigDecimal("0")).toString()).equals("0.00"), "mul027");
  harness.check (((new BigDecimal("-1.20")).multiply(new BigDecimal("-2")).toString()).equals("2.40"), "mul028");
  harness.check (((new BigDecimal("5.09")).multiply(new BigDecimal("7.1")).toString()).equals("36.139"), "mul029");
  harness.check (((new BigDecimal("2.5")).multiply(new BigDecimal("4")).toString()).equals("10.0"), "mul030");
  harness.check (((new BigDecimal("2.50")).multiply(new BigDecimal("4")).toString()).equals("10.00"), "mul031");
  harness.check (((new BigDecimal("1.23456789")).multiply(new BigDecimal("1.00000000")).toString()).equals("1.2345678900000000"), "mul032");
  
  harness.check (((new BigDecimal("1234.56789")).multiply(new BigDecimal("-1000.00000")).toString()).equals("-1234567.8900000000"), "mul033");
  
  harness.check (((new BigDecimal("-1234.56789")).multiply(new BigDecimal("1000.00000")).toString()).equals("-1234567.8900000000"), "mul034");
  
  harness.check (((new BigDecimal("9.999999999")).multiply(new BigDecimal("9.999999999")).toString()).equals("99.999999980000000001"), "mul035");
  
  harness.check (((new BigDecimal("5.00")).multiply(new BigDecimal("1E-3")).toString()).equals("0.00500"), "mul036");
  harness.check (((new BigDecimal("00.00")).multiply(new BigDecimal("0.000")).toString()).equals("0.00000"), "mul037");
  harness.check (((new BigDecimal("00.00")).multiply(new BigDecimal("0E-3")).toString()).equals("0.00000"), "mul038");
  // 1999.12.21: next one is a edge case if intermediate longs are used
  harness.check (((new BigDecimal("999999999999")).multiply(new BigDecimal("9765625")).toString()).equals("9765624999990234375"), "mul039");
  
  l9=new BigDecimal("123456789E+10");
  l77e=new BigDecimal("77E-20");
  harness.check ((l9.multiply(new BigDecimal("3456757")).toString()).equals("4.26760119573273E+24"), "mul040");
  harness.check ((l9.multiply(l77e).toString()).equals("0.9506172753"), "mul042");
  
  // test some more edge cases and carries
  harness.check (((new BigDecimal("9")).multiply(new BigDecimal("9")).toString()).equals("81"), "mul101");
  harness.check (((new BigDecimal("9")).multiply(new BigDecimal("90")).toString()).equals("810"), "mul102");
  harness.check (((new BigDecimal("9")).multiply(new BigDecimal("900")).toString()).equals("8100"), "mul103");
  harness.check (((new BigDecimal("9")).multiply(new BigDecimal("9000")).toString()).equals("81000"), "mul104");
  harness.check (((new BigDecimal("9")).multiply(new BigDecimal("90000")).toString()).equals("810000"), "mul105");
  harness.check (((new BigDecimal("9")).multiply(new BigDecimal("900000")).toString()).equals("8100000"), "mul106");
  harness.check (((new BigDecimal("9")).multiply(new BigDecimal("9000000")).toString()).equals("81000000"), "mul107");
  harness.check (((new BigDecimal("9")).multiply(new BigDecimal("90000000")).toString()).equals("810000000"), "mul108");
  harness.check (((new BigDecimal("9")).multiply(new BigDecimal("900000000")).toString()).equals("8100000000"), "mul109");
  harness.check (((new BigDecimal("9")).multiply(new BigDecimal("9000000000")).toString()).equals("81000000000"), "mul110");
  harness.check (((new BigDecimal("9")).multiply(new BigDecimal("90000000000")).toString()).equals("810000000000"), "mul111");
  harness.check (((new BigDecimal("9")).multiply(new BigDecimal("900000000000")).toString()).equals("8100000000000"), "mul112");
  harness.check (((new BigDecimal("9")).multiply(new BigDecimal("9000000000000")).toString()).equals("81000000000000"), "mul113");
  harness.check (((new BigDecimal("9")).multiply(new BigDecimal("90000000000000")).toString()).equals("810000000000000"), "mul114");
  harness.check (((new BigDecimal("9")).multiply(new BigDecimal("900000000000000")).toString()).equals("8100000000000000"), "mul115");
  harness.check (((new BigDecimal("9")).multiply(new BigDecimal("9000000000000000")).toString()).equals("81000000000000000"), "mul116");
  harness.check (((new BigDecimal("9")).multiply(new BigDecimal("90000000000000000")).toString()).equals("810000000000000000"), "mul117");
  harness.check (((new BigDecimal("9")).multiply(new BigDecimal("900000000000000000")).toString()).equals("8100000000000000000"), "mul118");
  harness.check (((new BigDecimal("9")).multiply(new BigDecimal("9000000000000000000")).toString()).equals("81000000000000000000"), "mul119");
  harness.check (((new BigDecimal("9")).multiply(new BigDecimal("90000000000000000000")).toString()).equals("810000000000000000000"), "mul120");
  harness.check (((new BigDecimal("9")).multiply(new BigDecimal("900000000000000000000")).toString()).equals("8100000000000000000000"), "mul121");
  harness.check (((new BigDecimal("9")).multiply(new BigDecimal("9000000000000000000000")).toString()).equals("81000000000000000000000"), "mul122");
  harness.check (((new BigDecimal("9")).multiply(new BigDecimal("90000000000000000000000")).toString()).equals("810000000000000000000000"), "mul123");
  // test some more edge cases without carries
  harness.check (((new BigDecimal("3")).multiply(new BigDecimal("3")).toString()).equals("9"), "mul131");
  harness.check (((new BigDecimal("3")).multiply(new BigDecimal("30")).toString()).equals("90"), "mul132");
  harness.check (((new BigDecimal("3")).multiply(new BigDecimal("300")).toString()).equals("900"), "mul133");
  harness.check (((new BigDecimal("3")).multiply(new BigDecimal("3000")).toString()).equals("9000"), "mul134");
  harness.check (((new BigDecimal("3")).multiply(new BigDecimal("30000")).toString()).equals("90000"), "mul135");
  harness.check (((new BigDecimal("3")).multiply(new BigDecimal("300000")).toString()).equals("900000"), "mul136");
  harness.check (((new BigDecimal("3")).multiply(new BigDecimal("3000000")).toString()).equals("9000000"), "mul137");
  harness.check (((new BigDecimal("3")).multiply(new BigDecimal("30000000")).toString()).equals("90000000"), "mul138");
  harness.check (((new BigDecimal("3")).multiply(new BigDecimal("300000000")).toString()).equals("900000000"), "mul139");
  harness.check (((new BigDecimal("3")).multiply(new BigDecimal("3000000000")).toString()).equals("9000000000"), "mul140");
  harness.check (((new BigDecimal("3")).multiply(new BigDecimal("30000000000")).toString()).equals("90000000000"), "mul141");
  harness.check (((new BigDecimal("3")).multiply(new BigDecimal("300000000000")).toString()).equals("900000000000"), "mul142");
  harness.check (((new BigDecimal("3")).multiply(new BigDecimal("3000000000000")).toString()).equals("9000000000000"), "mul143");
  harness.check (((new BigDecimal("3")).multiply(new BigDecimal("30000000000000")).toString()).equals("90000000000000"), "mul144");
  harness.check (((new BigDecimal("3")).multiply(new BigDecimal("300000000000000")).toString()).equals("900000000000000"), "mul145");
  harness.check (((new BigDecimal("3")).multiply(new BigDecimal("3000000000000000")).toString()).equals("9000000000000000"), "mul146");
  harness.check (((new BigDecimal("3")).multiply(new BigDecimal("30000000000000000")).toString()).equals("90000000000000000"), "mul147");
  harness.check (((new BigDecimal("3")).multiply(new BigDecimal("300000000000000000")).toString()).equals("900000000000000000"), "mul148");
  harness.check (((new BigDecimal("3")).multiply(new BigDecimal("3000000000000000000")).toString()).equals("9000000000000000000"), "mul149");
  harness.check (((new BigDecimal("3")).multiply(new BigDecimal("30000000000000000000")).toString()).equals("90000000000000000000"), "mul150");
  harness.check (((new BigDecimal("3")).multiply(new BigDecimal("300000000000000000000")).toString()).equals("900000000000000000000"), "mul151");
  harness.check (((new BigDecimal("3")).multiply(new BigDecimal("3000000000000000000000")).toString()).equals("9000000000000000000000"), "mul152");
  harness.check (((new BigDecimal("3")).multiply(new BigDecimal("30000000000000000000000")).toString()).equals("90000000000000000000000"), "mul153");
  
  try{checknull:do{
   ten.multiply((BigDecimal)null);
   flag=false;
  }while(false);}
  catch (java.lang.NullPointerException xx55){
   flag=true;
  }/*checknull*/
  harness.check (flag, "mul200");
  
  return;}

 /* ----------------------------------------------------------------- */
 
 /** Test the {@link BigDecimal#negate} method. */
 
 public void diagnegate(TestHarness harness){
  boolean flag=false;
  java.lang.ArithmeticException ae=null;
  
  harness.check (((new BigDecimal("2")).negate().toString()).equals("-2"), "neg001");
  harness.check (((new BigDecimal("-2")).negate().toString()).equals("2"), "neg002");
  
  harness.check (((new BigDecimal("2.00")).negate().toString()).equals("-2.00"), "neg010");
  harness.check (((new BigDecimal("-2.00")).negate().toString()).equals("2.00"), "neg011");
  harness.check (((new BigDecimal("0")).negate().toString()).equals("0"), "neg012");
  harness.check (((new BigDecimal("0.00")).negate().toString()).equals("0.00"), "neg013");
  harness.check (((new BigDecimal("00.0")).negate().toString()).equals("0.0"), "neg014");
  harness.check (((new BigDecimal("00.00")).negate().toString()).equals("0.00"), "neg015");
  harness.check (((new BigDecimal("00")).negate().toString()).equals("0"), "neg016");
  
  harness.check (((new BigDecimal("-2000000")).negate().toString()).equals("2000000"), "neg020");
  
  return;}
/* ----------------------------------------------------------------- */
 
 /** Test the {@link BigDecimal#subtract} method. */
 
 public void diagsubtract(TestHarness harness){
  boolean flag=false;
  BigDecimal alhs;
  BigDecimal arhs;
  java.lang.ArithmeticException ae=null;
  
  harness.check (((new BigDecimal(2)).subtract(new BigDecimal(3)).toString()).equals("-1"), "sub001");
  harness.check (((new BigDecimal("5.75")).subtract(new BigDecimal("3.3")).toString()).equals("2.45"), "sub002");
  harness.check (((new BigDecimal("5")).subtract(new BigDecimal("-3")).toString()).equals("8"), "sub003");
  harness.check (((new BigDecimal("-5")).subtract(new BigDecimal("-3")).toString()).equals("-2"), "sub004");
  harness.check (((new BigDecimal("-7")).subtract(new BigDecimal("2.5")).toString()).equals("-9.5"), "sub005");
  harness.check (((new BigDecimal("0.7")).subtract(new BigDecimal("0.3")).toString()).equals("0.4"), "sub006");
  harness.check (((new BigDecimal("1.3")).subtract(new BigDecimal("0.3")).toString()).equals("1.0"), "sub007");
  harness.check (((new BigDecimal("1.25")).subtract(new BigDecimal("1.25")).toString()).equals("0.00"), "sub008");
  harness.check (((new BigDecimal("0.02")).subtract(new BigDecimal("0.02")).toString()).equals("0.00"), "sub009");
  
  harness.check (((new BigDecimal("1.23456789")).subtract(new BigDecimal("1.00000000")).toString()).equals("0.23456789"), "sub010");
  
  harness.check (((new BigDecimal("1.23456789")).subtract(new BigDecimal("1.00000089")).toString()).equals("0.23456700"), "sub011");
  
  harness.check (((new BigDecimal("0.5555555559")).subtract(new BigDecimal("0.0000000001")).toString()).equals("0.5555555558"), "sub012");
  
  harness.check (((new BigDecimal("0.5555555559")).subtract(new BigDecimal("0.0000000005")).toString()).equals("0.5555555554"), "sub013");
  
  harness.check (((new BigDecimal("0.4444444444")).subtract(new BigDecimal("0.1111111111")).toString()).equals("0.3333333333"), "sub014");
  
  harness.check (((new BigDecimal("1.0000000000")).subtract(new BigDecimal("0.00000001")).toString()).equals("0.9999999900"), "sub015");
  
  harness.check (((new BigDecimal("0.4444444444999")).subtract(new BigDecimal("0")).toString()).equals("0.4444444444999"), "sub016");
  
  harness.check (((new BigDecimal("0.4444444445000")).subtract(new BigDecimal("0")).toString()).equals("0.4444444445000"), "sub017");
  
  
  harness.check (((new BigDecimal("70")).subtract(new BigDecimal("10000e+9")).toString()).equals("-9999999999930"), "sub018");
  
  harness.check (((new BigDecimal("700")).subtract(new BigDecimal("10000e+9")).toString()).equals("-9999999999300"), "sub019");
  
  harness.check (((new BigDecimal("7000")).subtract(new BigDecimal("10000e+9")).toString()).equals("-9999999993000"), "sub020");
  
  harness.check (((new BigDecimal("70000")).subtract(new BigDecimal("10000e+9")).toString()).equals("-9999999930000"), "sub021");
  
  harness.check (((new BigDecimal("700000")).subtract(new BigDecimal("10000e+9")).toString()).equals("-9999999300000"), "sub022");
  
  // symmetry:
  harness.check (((new BigDecimal("10000e+9")).subtract(new BigDecimal("70")).toString()).equals("9999999999930"), "sub023");
  
  harness.check (((new BigDecimal("10000e+9")).subtract(new BigDecimal("700")).toString()).equals("9999999999300"), "sub024");
  
  harness.check (((new BigDecimal("10000e+9")).subtract(new BigDecimal("7000")).toString()).equals("9999999993000"), "sub025");
  
  harness.check (((new BigDecimal("10000e+9")).subtract(new BigDecimal("70000")).toString()).equals("9999999930000"), "sub026");
  
  harness.check (((new BigDecimal("10000e+9")).subtract(new BigDecimal("700000")).toString()).equals("9999999300000"), "sub027");
  
  // some of the next group are really constructor tests
  harness.check (((new BigDecimal("00.0")).subtract(new BigDecimal("0.0")).toString()).equals("0.0"), "sub040");
  harness.check (((new BigDecimal("00.0")).subtract(new BigDecimal("0.00")).toString()).equals("0.00"), "sub041");
  harness.check (((new BigDecimal("0.00")).subtract(new BigDecimal("00.0")).toString()).equals("0.00"), "sub042");
  harness.check (((new BigDecimal("3")).subtract(new BigDecimal(".3")).toString()).equals("2.7"), "sub052");
  harness.check (((new BigDecimal("3.")).subtract(new BigDecimal(".3")).toString()).equals("2.7"), "sub053");
  harness.check (((new BigDecimal("3.0")).subtract(new BigDecimal(".3")).toString()).equals("2.7"), "sub054");
  harness.check (((new BigDecimal("3.00")).subtract(new BigDecimal(".3")).toString()).equals("2.70"), "sub055");
  harness.check (((new BigDecimal("3")).subtract(new BigDecimal("3")).toString()).equals("0"), "sub056");
  harness.check (((new BigDecimal("3")).subtract(new BigDecimal("+3")).toString()).equals("0"), "sub057");
  harness.check (((new BigDecimal("3")).subtract(new BigDecimal("-3")).toString()).equals("6"), "sub058");
  
  
  alhs=new BigDecimal("12345678900000");
  arhs=new BigDecimal("9999999999999");
  harness.check ((alhs.subtract(arhs).toString()).equals("2345678900001"), "sub112");
  harness.check ((arhs.subtract(alhs).toString()).equals("-2345678900001"), "sub113");
  
  // additional scaled arithmetic tests [0.97 problem]
  harness.check (((new BigDecimal("0")).subtract(new BigDecimal(".1")).toString()).equals("-0.1"), "sub120");
  harness.check (((new BigDecimal("00")).subtract(new BigDecimal(".97983")).toString()).equals("-0.97983"), "sub121");
  harness.check (((new BigDecimal("0")).subtract(new BigDecimal(".9")).toString()).equals("-0.9"), "sub122");
  harness.check (((new BigDecimal("0")).subtract(new BigDecimal("0.102")).toString()).equals("-0.102"), "sub123");
  harness.check (((new BigDecimal("0")).subtract(new BigDecimal(".4")).toString()).equals("-0.4"), "sub124");
  harness.check (((new BigDecimal("0")).subtract(new BigDecimal(".307")).toString()).equals("-0.307"), "sub125");
  harness.check (((new BigDecimal("0")).subtract(new BigDecimal(".43822")).toString()).equals("-0.43822"), "sub126");
  harness.check (((new BigDecimal("0")).subtract(new BigDecimal(".911")).toString()).equals("-0.911"), "sub127");
  harness.check (((new BigDecimal(".0")).subtract(new BigDecimal(".02")).toString()).equals("-0.02"), "sub128");
  harness.check (((new BigDecimal("00")).subtract(new BigDecimal(".392")).toString()).equals("-0.392"), "sub129");
  harness.check (((new BigDecimal("0")).subtract(new BigDecimal(".26")).toString()).equals("-0.26"), "sub130");
  harness.check (((new BigDecimal("0")).subtract(new BigDecimal("0.51")).toString()).equals("-0.51"), "sub131");
  harness.check (((new BigDecimal("0")).subtract(new BigDecimal(".2234")).toString()).equals("-0.2234"), "sub132");
  harness.check (((new BigDecimal("0")).subtract(new BigDecimal(".2")).toString()).equals("-0.2"), "sub133");
  harness.check (((new BigDecimal(".0")).subtract(new BigDecimal(".0008")).toString()).equals("-0.0008"), "sub134");
  // 0. on left
  harness.check (((new BigDecimal("0.0")).subtract(new BigDecimal("-.1")).toString()).equals("0.1"), "sub140");
  harness.check (((new BigDecimal("0.00")).subtract(new BigDecimal("-.97983")).toString()).equals("0.97983"), "sub141");
  harness.check (((new BigDecimal("0.0")).subtract(new BigDecimal("-.9")).toString()).equals("0.9"), "sub142");
  harness.check (((new BigDecimal("0.0")).subtract(new BigDecimal("-0.102")).toString()).equals("0.102"), "sub143");
  harness.check (((new BigDecimal("0.0")).subtract(new BigDecimal("-.4")).toString()).equals("0.4"), "sub144");
  harness.check (((new BigDecimal("0.0")).subtract(new BigDecimal("-.307")).toString()).equals("0.307"), "sub145");
  harness.check (((new BigDecimal("0.0")).subtract(new BigDecimal("-.43822")).toString()).equals("0.43822"), "sub146");
  harness.check (((new BigDecimal("0.0")).subtract(new BigDecimal("-.911")).toString()).equals("0.911"), "sub147");
  harness.check (((new BigDecimal("0.0")).subtract(new BigDecimal("-.02")).toString()).equals("0.02"), "sub148");
  harness.check (((new BigDecimal("0.00")).subtract(new BigDecimal("-.392")).toString()).equals("0.392"), "sub149");
  harness.check (((new BigDecimal("0.0")).subtract(new BigDecimal("-.26")).toString()).equals("0.26"), "sub150");
  harness.check (((new BigDecimal("0.0")).subtract(new BigDecimal("-0.51")).toString()).equals("0.51"), "sub151");
  harness.check (((new BigDecimal("0.0")).subtract(new BigDecimal("-.2234")).toString()).equals("0.2234"), "sub152");
  harness.check (((new BigDecimal("0.0")).subtract(new BigDecimal("-.2")).toString()).equals("0.2"), "sub153");
  harness.check (((new BigDecimal("0.0")).subtract(new BigDecimal("-.0008")).toString()).equals("0.0008"), "sub154");
  // negatives of same
  harness.check (((new BigDecimal("0")).subtract(new BigDecimal("-.1")).toString()).equals("0.1"), "sub160");
  harness.check (((new BigDecimal("00")).subtract(new BigDecimal("-.97983")).toString()).equals("0.97983"), "sub161");
  harness.check (((new BigDecimal("0")).subtract(new BigDecimal("-.9")).toString()).equals("0.9"), "sub162");
  harness.check (((new BigDecimal("0")).subtract(new BigDecimal("-0.102")).toString()).equals("0.102"), "sub163");
  harness.check (((new BigDecimal("0")).subtract(new BigDecimal("-.4")).toString()).equals("0.4"), "sub164");
  harness.check (((new BigDecimal("0")).subtract(new BigDecimal("-.307")).toString()).equals("0.307"), "sub165");
  harness.check (((new BigDecimal("0")).subtract(new BigDecimal("-.43822")).toString()).equals("0.43822"), "sub166");
  harness.check (((new BigDecimal("0")).subtract(new BigDecimal("-.911")).toString()).equals("0.911"), "sub167");
  harness.check (((new BigDecimal(".0")).subtract(new BigDecimal("-.02")).toString()).equals("0.02"), "sub168");
  harness.check (((new BigDecimal("00")).subtract(new BigDecimal("-.392")).toString()).equals("0.392"), "sub169");
  harness.check (((new BigDecimal("0")).subtract(new BigDecimal("-.26")).toString()).equals("0.26"), "sub170");
  harness.check (((new BigDecimal("0")).subtract(new BigDecimal("-0.51")).toString()).equals("0.51"), "sub171");
  harness.check (((new BigDecimal("0")).subtract(new BigDecimal("-.2234")).toString()).equals("0.2234"), "sub172");
  harness.check (((new BigDecimal("0")).subtract(new BigDecimal("-.2")).toString()).equals("0.2"), "sub173");
  harness.check (((new BigDecimal(".0")).subtract(new BigDecimal("-.0008")).toString()).equals("0.0008"), "sub174");
  
  // more fixed, LHS swaps [really same as testcases under add]
  harness.check (((new BigDecimal("-56267E-10")).subtract(zero).toString()).equals("-0.0000056267"), "sub180");
  harness.check (((new BigDecimal("-56267E-5")).subtract(zero).toString()).equals("-0.56267"), "sub181");
  harness.check (((new BigDecimal("-56267E-2")).subtract(zero).toString()).equals("-562.67"), "sub182");
  harness.check (((new BigDecimal("-56267E-1")).subtract(zero).toString()).equals("-5626.7"), "sub183");
  harness.check (((new BigDecimal("-56267E-0")).subtract(zero).toString()).equals("-56267"), "sub185");
  
  try{checknull:do{
   ten.subtract((BigDecimal)null);
   flag=false;
  }while(false);}
  catch (java.lang.NullPointerException xx83){
   flag=true;
  }/*checknull*/
  harness.check (flag, "sub200");
  
  return;}
/* ----------------------------------------------------------------- */
 
 /* ----------------------------------------------------------------- */
 /* Other methods                                                     */
 /* ----------------------------------------------------------------- */
 
 /** Test the <code>BigDecimal.byteValue()</code> method. */
 
 public void diagbyteValue(TestHarness harness){
  boolean flag=false;
  java.lang.String v=null;
  java.lang.ArithmeticException ae=null;
  java.lang.String badstrings[];
  int i=0;
  java.lang.String norm=null;
  
  
  harness.check (((((byte)-128)))==((new BigDecimal("-128")).byteValue()), "byv001");
  harness.check (((0))==((new BigDecimal("0")).byteValue()), "byv002");
  harness.check (((1))==((new BigDecimal("1")).byteValue()), "byv003");
  harness.check (((99))==((new BigDecimal("99")).byteValue()), "byv004");
  harness.check (((127))==((new BigDecimal("127")).byteValue()), "byv005");
  harness.check (((-128))==((new BigDecimal("128")).byteValue()), "byv006");
  harness.check (((-127))==((new BigDecimal("129")).byteValue()), "byv007");
  harness.check (((127))==((new BigDecimal("-129")).byteValue()), "byv008");
  harness.check (((126))==((new BigDecimal("-130")).byteValue()), "byv009");
  harness.check (((bmax))==((new BigDecimal(bmax)).byteValue()), "byv010");
  harness.check (((bmin))==((new BigDecimal(bmin)).byteValue()), "byv011");
  harness.check (((bneg))==((new BigDecimal(bneg)).byteValue()), "byv012");
  harness.check (((bzer))==((new BigDecimal(bzer)).byteValue()), "byv013");
  harness.check (((bpos))==((new BigDecimal(bpos)).byteValue()), "byv014");
  harness.check (((bmin))==((new BigDecimal(bmax+1)).byteValue()), "byv015");
  harness.check (((bmax))==((new BigDecimal(bmin-1)).byteValue()), "byv016");
  
  badstrings=new java.lang.String[]{"1234",(new BigDecimal(bmax)).add(one).toString(),(new BigDecimal(bmin)).subtract(one).toString(),"170","270","370","470","570","670","770","870","970","-170","-270","-370","-470","-570","-670","-770","-870","-970",(new BigDecimal(bmin)).multiply(two).toString(),(new BigDecimal(bmax)).multiply(two).toString(),(new BigDecimal(bmin)).multiply(ten).toString(),(new BigDecimal(bmax)).multiply(ten).toString(),"-1234"}; // 220
  // 221
  // 222
  // 223
  // 224
  // 225
  // 226
  // 227
  // 228
  // 229
  // 230
  // 231
  // 232
  // 233
  // 234
  // 235
  // 236
  // 237
  // 238
  // 239
  // 240
  // 241
  // 242
  // 243
  // 244
  // 245
  
  return;}
/* ----------------------------------------------------------------- */
 
 /** Test the {@link BigDecimal#compareTo(java.lang.Object)} method. */
 
 public void diagcomparetoObj(TestHarness harness){
  boolean flag=false;
  BigDecimal d;
  BigDecimal long1;
  BigDecimal long2;
  
  d=new BigDecimal(17);
  harness.check ((((Comparable)d).compareTo((java.lang.Object)(new BigDecimal(66))))==(-1), "cto001");
  harness.check ((((Comparable)d).compareTo((java.lang.Object)((new BigDecimal(10)).add(new BigDecimal(7)))))==0, "cto002");
  harness.check ((((Comparable)d).compareTo((java.lang.Object)(new BigDecimal(10))))==1, "cto003");
  long1=new BigDecimal("12345678903");
  long2=new BigDecimal("12345678900");
  harness.check ((((Comparable)long1).compareTo((java.lang.Object)long2))==1, "cto004");
  harness.check ((((Comparable)long2).compareTo((java.lang.Object)long1))==(-1), "cto005");
  harness.check ((((Comparable)long2).compareTo((java.lang.Object)long2))==0, "cto006");
  try{
   ((Comparable)d).compareTo((java.lang.Object)null);
   flag=false;
  }
  catch (java.lang.NullPointerException xx92){
   flag=true; // should get here
  }
  harness.check (flag, "cto101");
  try{
   ((Comparable)d).compareTo((java.lang.Object)"foo");
   flag=false;
  }
  catch (java.lang.ClassCastException xx93){
   flag=true; // should get here
  }
  harness.check (flag, "cto102");

  return;}
/* ----------------------------------------------------------------- */
 
 /** Test the {@link BigDecimal#doubleValue} method. */
 
 public void diagdoublevalue(TestHarness harness){
  java.lang.String val;
  // 1999.03.07 Infinities no longer errors
  val="-1";
  harness.check (((new BigDecimal(val)).doubleValue())==((new java.lang.Double(val)).doubleValue()), "dov001");
  val="-0.1";
  harness.check (((new BigDecimal(val)).doubleValue())==((new java.lang.Double(val)).doubleValue()), "dov002");
  val="0";
  harness.check (((new BigDecimal(val)).doubleValue())==((new java.lang.Double(val)).doubleValue()), "dov003");
  val="0.1";
  harness.check (((new BigDecimal(val)).doubleValue())==((new java.lang.Double(val)).doubleValue()), "dov004");
  val="1";
  harness.check (((new BigDecimal(val)).doubleValue())==((new java.lang.Double(val)).doubleValue()), "dov005");
  val="1e1000";
  harness.check (((new BigDecimal(val)).doubleValue())==java.lang.Double.POSITIVE_INFINITY, "dov006");
  val="-1e1000";
  harness.check (((new BigDecimal(val)).doubleValue())==java.lang.Double.NEGATIVE_INFINITY, "dov007");
  return;}
/* ----------------------------------------------------------------- */
 
 /** Test the {@link BigDecimal#equals} method. */
 
 public void diagequals(TestHarness harness){
  BigDecimal d;
  d=new BigDecimal(17);
  harness.check ((!(d.equals((java.lang.Object)null))), "equ001");
  harness.check ((!(d.equals((java.lang.Object)"foo"))), "equ002");
  harness.check ((!(d.equals((java.lang.Object)(new BigDecimal(66))))), "equ003");
  harness.check (d.equals((java.lang.Object)d), "equ004");
  harness.check (d.equals((java.lang.Object)((new BigDecimal(10)).add(new BigDecimal(7)))), "equ005");
  return;}

 /* ----------------------------------------------------------------- */
 
 /** Test the {@link BigDecimal#floatValue} method. */
 
 public void diagfloatvalue(TestHarness harness){
  java.lang.String val;
  // 1999.03.07 Infinities no longer errors
  val="-1";
  harness.check (((new BigDecimal(val)).floatValue())==((new java.lang.Float(val)).floatValue()), "flv001");
  val="-0.1";
  harness.check (((new BigDecimal(val)).floatValue())==((new java.lang.Float(val)).floatValue()), "flv002");
  val="0";
  harness.check (((new BigDecimal(val)).floatValue())==((new java.lang.Float(val)).floatValue()), "flv003");
  val="0.1";
  harness.check (((new BigDecimal(val)).floatValue())==((new java.lang.Float(val)).floatValue()), "flv004");
  val="1";
  harness.check (((new BigDecimal(val)).floatValue())==((new java.lang.Float(val)).floatValue()), "flv005");
  val="1e200";
  harness.check (((new BigDecimal(val)).floatValue())==java.lang.Float.POSITIVE_INFINITY, "flv006");
  val="-1e200";
  harness.check (((new BigDecimal(val)).floatValue())==java.lang.Float.NEGATIVE_INFINITY, "flv007");
  val="1e1000";
  harness.check (((new BigDecimal(val)).floatValue())==java.lang.Float.POSITIVE_INFINITY, "flv008");
  val="-1e1000";
  harness.check (((new BigDecimal(val)).floatValue())==java.lang.Float.NEGATIVE_INFINITY, "flv009");
  return;}

 /* ----------------------------------------------------------------- */
 
 /** Test the {@link BigDecimal#hashCode} method. */
 
 public void diaghashcode(TestHarness harness){
  // These tests are all wrong.  The JDK API for BigDecimal.hashCode()
  // does not say how the hash values should be calculated.
  // 
  //  java.lang.String hs;
  //  BigDecimal d;
  //  hs="27827817";
  //  d=new BigDecimal(hs);
  //  harness.check ((d.hashCode())==(hs.hashCode()), "has001");
  //  hs="1.265E+200";
  //  d=new BigDecimal(hs);
  //  harness.check ((d.hashCode())==(hs.hashCode()), "has002");
  //  hs="126.5E+200";
  //  d=new BigDecimal(hs);
  //  harness.check ((d.hashCode())!=(hs.hashCode()), "has003");
  return;}
/* ----------------------------------------------------------------- */
 
 /** Test the {@link BigDecimal#intValue} method. */
 
 public void diagintvalue(TestHarness harness){
  boolean flag=false;
  java.lang.String v=null;
  java.lang.ArithmeticException ae=null;
  int i=0;
  java.lang.String norm=null;
  BigDecimal dimax;
  BigDecimal num=null;
  BigDecimal dv=null;
  BigDecimal dimin;
  
  
  // intValue --
  
  harness.check (imin==((new BigDecimal(imin)).intValue()), "inv001");
  harness.check (((99))==((new BigDecimal("99")).intValue()), "inv002");
  harness.check (((1))==((new BigDecimal("1")).intValue()), "inv003");
  harness.check (((0))==((new BigDecimal("0")).intValue()), "inv004");
  harness.check (((-1))==((new BigDecimal("-1")).intValue()), "inv005");
  harness.check (((-99))==((new BigDecimal("-99")).intValue()), "inv006");
  harness.check (imax==((new BigDecimal(imax)).intValue()), "inv007");
  harness.check (((5))==((new BigDecimal("5.0")).intValue()), "inv008");
  harness.check (((5))==((new BigDecimal("5.3")).intValue()), "inv009");
  harness.check (((5))==((new BigDecimal("5.5")).intValue()), "inv010");
  harness.check (((5))==((new BigDecimal("5.7")).intValue()), "inv011");
  harness.check (((5))==((new BigDecimal("5.9")).intValue()), "inv012");
  harness.check (((-5))==((new BigDecimal("-5.0")).intValue()), "inv013");
  harness.check (((-5))==((new BigDecimal("-5.3")).intValue()), "inv014");
  harness.check (((-5))==((new BigDecimal("-5.5")).intValue()), "inv015");
  harness.check (((-5))==((new BigDecimal("-5.7")).intValue()), "inv016");
  harness.check (((-5))==((new BigDecimal("-5.9")).intValue()), "inv017");
  harness.check (((new BigDecimal("88888888888")).intValue())==(-1305424328), "inv018"); // ugh
  harness.check (((new BigDecimal("-88888888888")).intValue())==1305424328, "inv019"); // ugh
  harness.check (((imin))==((new BigDecimal((((long)imax))+1)).intValue()), "inv020");
  harness.check (((imax))==((new BigDecimal((((long)imin))-1)).intValue()), "inv021");
  
  harness.check (((99))==((new BigDecimal("99")).intValue()), "inv102");
  harness.check (((1))==((new BigDecimal("1")).intValue()), "inv103");
  harness.check (((0))==((new BigDecimal("0")).intValue()), "inv104");
  harness.check (((-1))==((new BigDecimal("-1")).intValue()), "inv105");
  harness.check (((-99))==((new BigDecimal("-99")).intValue()), "inv106");
  harness.check (imax==((new BigDecimal(imax)).intValue()), "inv107");
  harness.check (((5))==((new BigDecimal("5.0")).intValue()), "inv108");
  harness.check (((-5))==((new BigDecimal("-5.0")).intValue()), "inv109");
  
  return;}

 /* ----------------------------------------------------------------- */
 
 /** Test the {@link BigDecimal#longValue} method. */
 
 public void diaglongvalue(TestHarness harness){
  boolean flag=false;
  java.lang.String v=null;
  java.lang.ArithmeticException ae=null;
  java.lang.String badstrings[];
  int i=0;
  java.lang.String norm=null;
  BigDecimal dlmax;
  BigDecimal num=null;
  BigDecimal dv=null;
  BigDecimal dlmin;
  
  
  // longValue --
  
  harness.check (lmin==((new BigDecimal(lmin)).longValue()), "lov001");
  harness.check (lmin==((new BigDecimal(lminString)).longValue()), "lov001a");
  harness.check ((((long)99))==((new BigDecimal("99")).longValue()), "lov002");
  harness.check ((((long)1))==((new BigDecimal("1")).longValue()), "lov003");
  harness.check ((((long)0))==((new BigDecimal("0")).longValue()), "lov004");
  harness.check ((((long)-1))==((new BigDecimal("-1")).longValue()), "lov005");
  harness.check ((((long)-99))==((new BigDecimal("-99")).longValue()), "lov006");
  // This version of the test uses the BigDecimal(double) constructor.  
  // The test fails because ((long)((double) lmax)) != lmax
  // harness.check (lmax==((new BigDecimal(lmax)).longValue()), "lov007");
  harness.check (lmax==((new BigDecimal(lmaxString)).longValue()), "lov007a");
  harness.check ((((long)5))==((new BigDecimal("5.0")).longValue()), "lov008");
  harness.check ((((long)5))==((new BigDecimal("5.3")).longValue()), "lov009");
  harness.check ((((long)5))==((new BigDecimal("5.5")).longValue()), "lov010");
  harness.check ((((long)5))==((new BigDecimal("5.7")).longValue()), "lov011");
  harness.check ((((long)5))==((new BigDecimal("5.9")).longValue()), "lov012");
  harness.check ((((long)-5))==((new BigDecimal("-5.0")).longValue()), "lov013");
  harness.check ((((long)-5))==((new BigDecimal("-5.3")).longValue()), "lov014");
  harness.check ((((long)-5))==((new BigDecimal("-5.5")).longValue()), "lov015");
  harness.check ((((long)-5))==((new BigDecimal("-5.7")).longValue()), "lov016");
  harness.check ((((long)-5))==((new BigDecimal("-5.9")).longValue()), "lov017");
  harness.check (((new BigDecimal("888888888899999999998")).longValue())==3445173361941522430L, "lov018"); // ugh
  harness.check (((new BigDecimal("-888888888899999999998")).longValue())==(-3445173361941522430L), "lov019"); // ugh
  
  harness.check (lmin==((new BigDecimal(lminString)).longValue()), "lov101");
  harness.check ((((long)99))==((new BigDecimal("99")).longValue()), "lov102");
  harness.check ((((long)1))==((new BigDecimal("1")).longValue()), "lov103");
  harness.check ((((long)0))==((new BigDecimal("0")).longValue()), "lov104");
  harness.check ((((long)-1))==((new BigDecimal("-1")).longValue()), "lov105");
  harness.check ((((long)-99))==((new BigDecimal("-99")).longValue()), "lov106");
  // This version of this test is incorrect: see 'lov007'
  // harness.check (lmax==((new BigDecimal(lmax)).longValue()), "lov107");
  harness.check (lmax==((new BigDecimal(lmaxString)).longValue()), "lov107a");
  harness.check ((((long)5))==((new BigDecimal("5.0")).longValue()), "lov108");
  harness.check ((((long)-5))==((new BigDecimal("-5.0")).longValue()), "lov109");
  
  return;}

 /* ----------------------------------------------------------------- */
 
 /** Test the {@link BigDecimal#movePointLeft} method. */
 
 public void diagmovepointleft(TestHarness harness){
  harness.check (((new BigDecimal("-1")).movePointLeft(-10).toString()).equals("-10000000000"), "mpl001");
  harness.check (((new BigDecimal("-1")).movePointLeft(-5).toString()).equals("-100000"), "mpl002");
  harness.check (((new BigDecimal("-1")).movePointLeft(-1).toString()).equals("-10"), "mpl003");
  harness.check (((new BigDecimal("-1")).movePointLeft(0).toString()).equals("-1"), "mpl004");
  harness.check (((new BigDecimal("-1")).movePointLeft(+1).toString()).equals("-0.1"), "mpl005");
  harness.check (((new BigDecimal("-1")).movePointLeft(+5).toString()).equals("-0.00001"), "mpl006");
  harness.check (((new BigDecimal("-1")).movePointLeft(+10).toString()).equals("-1E-10"), "mpl007");
  
  harness.check (((new BigDecimal("0")).movePointLeft(-10).toString()).equals("0"), "mpl010");
  harness.check (((new BigDecimal("0")).movePointLeft(-5).toString()).equals("0"), "mpl010");
  harness.check (((new BigDecimal("0")).movePointLeft(-1).toString()).equals("0"), "mpl010");
  harness.check (((new BigDecimal("0")).movePointLeft(0).toString()).equals("0"), "mpl010");
  harness.check (((new BigDecimal("0")).movePointLeft(+1).toString()).equals("0.0"), "mpl010");
  harness.check (((new BigDecimal("0")).movePointLeft(+5).toString()).equals("0.00000"), "mpl010");
  harness.check (((new BigDecimal("0")).movePointLeft(+10).toString()).equals("0E-10"), "mpl010");
  
  harness.check (((new BigDecimal("+1")).movePointLeft(-10).toString()).equals("10000000000"), "mpl020");
  harness.check (((new BigDecimal("+1")).movePointLeft(-5).toString()).equals("100000"), "mpl021");
  harness.check (((new BigDecimal("+1")).movePointLeft(-1).toString()).equals("10"), "mpl022");
  harness.check (((new BigDecimal("+1")).movePointLeft(0).toString()).equals("1"), "mpl023");
  harness.check (((new BigDecimal("+1")).movePointLeft(+1).toString()).equals("0.1"), "mpl024");
  harness.check (((new BigDecimal("+1")).movePointLeft(+5).toString()).equals("0.00001"), "mpl025");
  harness.check (((new BigDecimal("+1")).movePointLeft(+10).toString()).equals("1E-10"), "mpl026");
  
  harness.check (((new BigDecimal("0.5E+1")).movePointLeft(-10).toString()).equals("50000000000"), "mpl030");
  harness.check (((new BigDecimal("0.5E+1")).movePointLeft(-5).toString()).equals("500000"), "mpl031");
  harness.check (((new BigDecimal("0.5E+1")).movePointLeft(-1).toString()).equals("50"), "mpl032");
  harness.check (((new BigDecimal("0.5E+1")).movePointLeft(0).toString()).equals("5"), "mpl033");
  harness.check (((new BigDecimal("0.5E+1")).movePointLeft(+1).toString()).equals("0.5"), "mpl034");
  harness.check (((new BigDecimal("0.5E+1")).movePointLeft(+5).toString()).equals("0.00005"), "mpl035");
  harness.check (((new BigDecimal("0.5E+1")).movePointLeft(+10).toString()).equals("5E-10"), "mpl036");
  
  return;}
/* ----------------------------------------------------------------- */
 
 /** Test the {@link BigDecimal#movePointRight} method. */
 
 public void diagmovepointright(TestHarness harness){
  harness.check (((new BigDecimal("-1")).movePointRight(+10).toString()).equals("-10000000000"), "mpr001");
  harness.check (((new BigDecimal("-1")).movePointRight(+5).toString()).equals("-100000"), "mpr002");
  harness.check (((new BigDecimal("-1")).movePointRight(+1).toString()).equals("-10"), "mpr003");
  harness.check (((new BigDecimal("-1")).movePointRight(0).toString()).equals("-1"), "mpr004");
  harness.check (((new BigDecimal("-1")).movePointRight(-1).toString()).equals("-0.1"), "mpr005");
  harness.check (((new BigDecimal("-1")).movePointRight(-5).toString()).equals("-0.00001"), "mpr006");
  harness.check (((new BigDecimal("-1")).movePointRight(-10).toString()).equals("-1E-10"), "mpr007");
  
  harness.check (((new BigDecimal("0")).movePointRight(+10).toString()).equals("0"), "mpr010");
  harness.check (((new BigDecimal("0")).movePointRight(+5).toString()).equals("0"), "mpr011");
  harness.check (((new BigDecimal("0")).movePointRight(+1).toString()).equals("0"), "mpr012");
  harness.check (((new BigDecimal("0")).movePointRight(0).toString()).equals("0"), "mpr013");
  harness.check (((new BigDecimal("0")).movePointRight(-1).toString()).equals("0.0"), "mpr014");
  harness.check (((new BigDecimal("0")).movePointRight(-5).toString()).equals("0.00000"), "mpr015");
  harness.check (((new BigDecimal("0")).movePointRight(-10).toString()).equals("0E-10"), "mpr016");
  
  harness.check (((new BigDecimal("+1")).movePointRight(+10).toString()).equals("10000000000"), "mpr020");
  harness.check (((new BigDecimal("+1")).movePointRight(+5).toString()).equals("100000"), "mpr021");
  harness.check (((new BigDecimal("+1")).movePointRight(+1).toString()).equals("10"), "mpr022");
  harness.check (((new BigDecimal("+1")).movePointRight(0).toString()).equals("1"), "mpr023");
  harness.check (((new BigDecimal("+1")).movePointRight(-1).toString()).equals("0.1"), "mpr024");
  harness.check (((new BigDecimal("+1")).movePointRight(-5).toString()).equals("0.00001"), "mpr025");
  harness.check (((new BigDecimal("+1")).movePointRight(-10).toString()).equals("1E-10"), "mpr026");
  
  harness.check (((new BigDecimal("0.5E+1")).movePointRight(+10).toString()).equals("50000000000"), "mpr030");
  harness.check (((new BigDecimal("0.5E+1")).movePointRight(+5).toString()).equals("500000"), "mpr031");
  harness.check (((new BigDecimal("0.5E+1")).movePointRight(+1).toString()).equals("50"), "mpr032");
  harness.check (((new BigDecimal("0.5E+1")).movePointRight(0).toString()).equals("5"), "mpr033");
  harness.check (((new BigDecimal("0.5E+1")).movePointRight(-1).toString()).equals("0.5"), "mpr034");
  harness.check (((new BigDecimal("0.5E+1")).movePointRight(-5).toString()).equals("0.00005"), "mpr035");
  harness.check (((new BigDecimal("0.5E+1")).movePointRight(-10).toString()).equals("5E-10"), "mpr036");
  
  return;}
/* ----------------------------------------------------------------- */
 
 /** Test the {@link BigDecimal#scale} method. */
 
 public void diagscale(TestHarness harness){
  harness.check (((new BigDecimal("-1")).scale())==0, "sca001");
  harness.check (((new BigDecimal("-10")).scale())==0, "sca002");
  harness.check (((new BigDecimal("+1")).scale())==0, "sca003");
  harness.check (((new BigDecimal("+10")).scale())==0, "sca004");
  harness.check (((new BigDecimal("1E+10")).scale())==-10, "sca005");
  harness.check (((new BigDecimal("1E-10")).scale())==10, "sca006");
  harness.check (((new BigDecimal("0E-10")).scale())==10, "sca007");
  harness.check (((new BigDecimal("0.000")).scale())==3, "sca008");
  harness.check (((new BigDecimal("0.00")).scale())==2, "sca009");
  harness.check (((new BigDecimal("0.0")).scale())==1, "sca010");
  harness.check (((new BigDecimal("0.1")).scale())==1, "sca011");
  harness.check (((new BigDecimal("0.12")).scale())==2, "sca012");
  harness.check (((new BigDecimal("0.123")).scale())==3, "sca013");
  harness.check (((new BigDecimal("-0.0")).scale())==1, "sca014");
  harness.check (((new BigDecimal("-0.1")).scale())==1, "sca015");
  harness.check (((new BigDecimal("-0.12")).scale())==2, "sca016");
  harness.check (((new BigDecimal("-0.123")).scale())==3, "sca017");
  return;}

 /* ----------------------------------------------------------------- */
 
 /** Test the {@link BigDecimal#setScale} method. */
 
 public void diagsetscale(TestHarness harness){
  boolean flag=false;
  java.lang.RuntimeException e=null;
  
  harness.check (((new BigDecimal("-1")).setScale(0).toString()).equals("-1"), "ssc001");
  harness.check (((new BigDecimal("-1")).setScale(1).toString()).equals("-1.0"), "ssc002");
  harness.check (((new BigDecimal("-1")).setScale(2).toString()).equals("-1.00"), "ssc003");
  harness.check (((new BigDecimal("0")).setScale(0).toString()).equals("0"), "ssc004");
  harness.check (((new BigDecimal("0")).setScale(1).toString()).equals("0.0"), "ssc005");
  harness.check (((new BigDecimal("0")).setScale(2).toString()).equals("0.00"), "ssc006");
  harness.check (((new BigDecimal("+1")).setScale(0).toString()).equals("1"), "ssc007");
  harness.check (((new BigDecimal("+1")).setScale(1).toString()).equals("1.0"), "ssc008");
  harness.check (((new BigDecimal("+1")).setScale(2).toString()).equals("1.00"), "ssc009");
  
  try{checkscale:do{
   (new BigDecimal(1)).setScale(-8);
   flag=false;
  }while(false);}
  catch (java.lang.ArithmeticException xx117){e=xx117;
   flag=checkMessage(e, "Negative scale: -8");
  }/*checkscale*/
  harness.check (flag, "ssc100");
  try{checkrunn:do{
   (new BigDecimal(1.0001D)).setScale(3);
   flag=false;
  }while(false);}
  catch (java.lang.ArithmeticException xx118){e=xx118;
   flag=checkMessage(e, "Rounding necessary");
  }/*checkrunn*/
  harness.check (flag, "ssc101");
  try{checkrunn:do{
   (new BigDecimal(1E-8D)).setScale(3);
   flag=false;
  }while(false);}
  catch (java.lang.ArithmeticException xx119){e=xx119;
   flag=checkMessage(e, "Rounding necessary");
  }/*checkrunn*/
  harness.check (flag, "ssc102");
  return;}

 /* ----------------------------------------------------------------- */
 
 /** Test the <code>BigDecimal.shortValue()</code> method. */
 
 public void diagshortvalue(TestHarness harness){
  boolean flag=false;
  java.lang.String v=null;
  java.lang.ArithmeticException ae=null;
  java.lang.String badstrings[];
  int i=0;
  java.lang.String norm=null;
  
  harness.check ((((short)0))==((new BigDecimal("0")).shortValue()), "shv002");
  harness.check ((((short)1))==((new BigDecimal("1")).shortValue()), "shv003");
  harness.check ((((short)99))==((new BigDecimal("99")).shortValue()), "shv004");
  harness.check (((smax))==((new BigDecimal(smax)).shortValue()), "shv006");
  harness.check (((smin))==((new BigDecimal(smin)).shortValue()), "shv007");
  harness.check (((sneg))==((new BigDecimal(sneg)).shortValue()), "shv008");
  harness.check (((szer))==((new BigDecimal(szer)).shortValue()), "shv009");
  harness.check (((spos))==((new BigDecimal(spos)).shortValue()), "shv010");
  harness.check (((smin))==((new BigDecimal(smax+1)).shortValue()), "shv011");
  harness.check (((smax))==((new BigDecimal(smin-1)).shortValue()), "shv012");
  
  return;}

 /* ----------------------------------------------------------------- */
 
 /** Test the {@link BigDecimal#signum} method. */
 
 public void diagsignum(TestHarness harness){
  // necessarily checks some obscure constructions, too
  harness.check ((-1)==((new BigDecimal("-1")).signum()), "sig001");
  harness.check ((-1)==((new BigDecimal("-0.0010")).signum()), "sig002");
  harness.check ((-1)==((new BigDecimal("-0.001")).signum()), "sig003");
  harness.check (0==((new BigDecimal("-0.00")).signum()), "sig004");
  harness.check (0==((new BigDecimal("-0")).signum()), "sig005");
  harness.check (0==((new BigDecimal("0")).signum()), "sig006");
  harness.check (0==((new BigDecimal("00")).signum()), "sig007");
  harness.check (0==((new BigDecimal("00.0")).signum()), "sig008");
  harness.check (1==((new BigDecimal("00.01")).signum()), "sig009");
  harness.check (1==((new BigDecimal("00.01")).signum()), "sig010");
  harness.check (1==((new BigDecimal("00.010")).signum()), "sig011");
  harness.check (1==((new BigDecimal("01.01")).signum()), "sig012");
  harness.check (1==((new BigDecimal("+0.01")).signum()), "sig013");
  harness.check (1==((new BigDecimal("+0.001")).signum()), "sig014");
  harness.check (1==((new BigDecimal("1")).signum()), "sig015");
  harness.check (1==((new BigDecimal("1e+12")).signum()), "sig016");
  harness.check (0==((new BigDecimal("00e+12")).signum()), "sig017");
  return;}

 /* ----------------------------------------------------------------- */
 
 /** Test the {@link BigDecimal#toBigInteger} method. */
 
 public void diagtobiginteger(TestHarness harness){
  boolean flag=false;
  java.lang.String badstrings[];
  int i=0;
  harness.check (((new BigDecimal("-1")).toBigInteger().toString()).equals("-1"), "tbi001");
  harness.check (((new BigDecimal("0")).toBigInteger().toString()).equals("0"), "tbi002");
  harness.check (((new BigDecimal("+1")).toBigInteger().toString()).equals("1"), "tbi003");
  harness.check (((new BigDecimal("10")).toBigInteger().toString()).equals("10"), "tbi004");
  harness.check (((new BigDecimal("1000")).toBigInteger().toString()).equals("1000"), "tbi005");
  harness.check (((new BigDecimal("-1E+0")).toBigInteger().toString()).equals("-1"), "tbi006");
  harness.check (((new BigDecimal("0E+0")).toBigInteger().toString()).equals("0"), "tbi007");
  harness.check (((new BigDecimal("+1E+0")).toBigInteger().toString()).equals("1"), "tbi008");
  harness.check (((new BigDecimal("10E+0")).toBigInteger().toString()).equals("10"), "tbi009");
  harness.check (((new BigDecimal("1E+3")).toBigInteger().toString()).equals("1000"), "tbi010");
  harness.check (((new BigDecimal("0.00")).toBigInteger().toString()).equals("0"), "tbi011");
  harness.check (((new BigDecimal("0.01")).toBigInteger().toString()).equals("0"), "tbi012");
  harness.check (((new BigDecimal("0.0")).toBigInteger().toString()).equals("0"), "tbi013");
  harness.check (((new BigDecimal("0.1")).toBigInteger().toString()).equals("0"), "tbi014");
  harness.check (((new BigDecimal("-0.00")).toBigInteger().toString()).equals("0"), "tbi015");
  harness.check (((new BigDecimal("-0.01")).toBigInteger().toString()).equals("0"), "tbi016");
  harness.check (((new BigDecimal("-0.0")).toBigInteger().toString()).equals("0"), "tbi017");
  harness.check (((new BigDecimal("-0.1")).toBigInteger().toString()).equals("0"), "tbi018");
  harness.check (((new BigDecimal("1.00")).toBigInteger().toString()).equals("1"), "tbi019");
  harness.check (((new BigDecimal("1.01")).toBigInteger().toString()).equals("1"), "tbi020");
  harness.check (((new BigDecimal("1.0")).toBigInteger().toString()).equals("1"), "tbi021");
  harness.check (((new BigDecimal("1.1")).toBigInteger().toString()).equals("1"), "tbi022");
  harness.check (((new BigDecimal("-1.00")).toBigInteger().toString()).equals("-1"), "tbi023");
  harness.check (((new BigDecimal("-1.01")).toBigInteger().toString()).equals("-1"), "tbi024");
  harness.check (((new BigDecimal("-1.0")).toBigInteger().toString()).equals("-1"), "tbi025");
  harness.check (((new BigDecimal("-1.1")).toBigInteger().toString()).equals("-1"), "tbi026");
  harness.check (((new BigDecimal("-111.111")).toBigInteger().toString()).equals("-111"), "tbi027");
  harness.check (((new BigDecimal("+111.111")).toBigInteger().toString()).equals("111"), "tbi028");
  harness.check (((new BigDecimal("0.09")).toBigInteger().toString()).equals("0"), "tbi029");
  harness.check (((new BigDecimal("0.9")).toBigInteger().toString()).equals("0"), "tbi030");
  harness.check (((new BigDecimal("1.09")).toBigInteger().toString()).equals("1"), "tbi031");
  harness.check (((new BigDecimal("1.05")).toBigInteger().toString()).equals("1"), "tbi032");
  harness.check (((new BigDecimal("1.04")).toBigInteger().toString()).equals("1"), "tbi033");
  harness.check (((new BigDecimal("1.99")).toBigInteger().toString()).equals("1"), "tbi034");
  harness.check (((new BigDecimal("1.9")).toBigInteger().toString()).equals("1"), "tbi034");
  harness.check (((new BigDecimal("1.5")).toBigInteger().toString()).equals("1"), "tbi035");
  harness.check (((new BigDecimal("1.4")).toBigInteger().toString()).equals("1"), "tbi036");
  harness.check (((new BigDecimal("-1.09")).toBigInteger().toString()).equals("-1"), "tbi037");
  harness.check (((new BigDecimal("-1.05")).toBigInteger().toString()).equals("-1"), "tbi038");
  harness.check (((new BigDecimal("-1.04")).toBigInteger().toString()).equals("-1"), "tbi039");
  harness.check (((new BigDecimal("-1.99")).toBigInteger().toString()).equals("-1"), "tbi040");
  harness.check (((new BigDecimal("-1.9")).toBigInteger().toString()).equals("-1"), "tbi041");
  harness.check (((new BigDecimal("-1.5")).toBigInteger().toString()).equals("-1"), "tbi042");
  harness.check (((new BigDecimal("-1.4")).toBigInteger().toString()).equals("-1"), "tbi043");
  harness.check (((new BigDecimal("1E-1000")).toBigInteger().toString()).equals("0"), "tbi044");
  harness.check (((new BigDecimal("-1E-1000")).toBigInteger().toString()).equals("0"), "tbi045");
  
  return;}

 /* ----------------------------------------------------------------- */
 
 /** Test the {@link BigDecimal#toString} method. */
 
 public void diagtostring(TestHarness harness){
  java.lang.String str;
  char car[];
  BigDecimal d;
  char ca[];
  java.lang.String cs;
  // the function of this has been tested above, this is simply an
  // existence proof and type-check
  str="123.45";
  d=new BigDecimal(str);
  cs=d.toString();
  harness.check ((str.length())==(cs.length()), "tos002");
  harness.check (str.equals((java.lang.Object)cs), "tos004");
  harness.check ((cs instanceof java.lang.String), "tos005");
  harness.check ((d.toString() instanceof java.lang.String), "tos006");
  return;}

 /* ----------------------------------------------------------------- */
 
 /** Test the {@link BigDecimal#valueOf} method [long and double]. */
 
 public void diagvalueof(TestHarness harness){
  boolean flag=false;
  java.lang.NumberFormatException e=null;
  double dzer;
  double dpos;
  double dneg;
  double dpos5;
  double dneg5;
  double dmin;
  double dmax;
  double d;
  
  // valueOf(long [,scale]) --
  
  harness.check ((BigDecimal.valueOf((long)((byte)-2)).toString()).equals("-2"), "val001");
  harness.check ((BigDecimal.valueOf((long)((byte)-1)).toString()).equals("-1"), "val002");
  harness.check ((BigDecimal.valueOf((long)((byte)-0)).toString()).equals("0"), "val003");
  harness.check ((BigDecimal.valueOf((long)((byte)+1)).toString()).equals("1"), "val004");
  harness.check ((BigDecimal.valueOf((long)((byte)+2)).toString()).equals("2"), "val005");
  harness.check ((BigDecimal.valueOf((long)((byte)10)).toString()).equals("10"), "val006");
  harness.check ((BigDecimal.valueOf((long)((byte)11)).toString()).equals("11"), "val007");
  harness.check ((BigDecimal.valueOf(lmin).toString()).equals("-9223372036854775808"), "val008");
  harness.check ((BigDecimal.valueOf(lmax).toString()).equals("9223372036854775807"), "val009");
  harness.check ((BigDecimal.valueOf(lneg).toString()).equals("-1"), "val010");
  harness.check ((BigDecimal.valueOf(lzer).toString()).equals("0"), "val011");
  harness.check ((BigDecimal.valueOf(lpos).toString()).equals("1"), "val012");
  harness.check ((BigDecimal.valueOf(lmin,0).toString()).equals("-9223372036854775808"), "val013");
  harness.check ((BigDecimal.valueOf(lmax,0).toString()).equals("9223372036854775807"), "val014");
  harness.check ((BigDecimal.valueOf(lneg,0).toString()).equals("-1"), "val015");
  harness.check ((BigDecimal.valueOf(lpos,0).toString()).equals("1"), "val016");
  
  harness.check ((BigDecimal.valueOf(lzer,0).toString()).equals("0"), "val017");
  harness.check ((BigDecimal.valueOf(lzer,1).toString()).equals("0.0"), "val018");
  harness.check ((BigDecimal.valueOf(lzer,2).toString()).equals("0.00"), "val019");
  harness.check ((BigDecimal.valueOf(lzer,3).toString()).equals("0.000"), "val020");
  harness.check ((BigDecimal.valueOf(lzer,10).toString()).equals("0E-10"), "val021");
  
  harness.check ((BigDecimal.valueOf(lmin,7).toString()).equals("-922337203685.4775808"), "val022");
  harness.check ((BigDecimal.valueOf(lmax,11).toString()).equals("92233720.36854775807"), "val023");

  return;}

 
 /* ----------------------------------------------------------------- */
 /* right - Utility to do a 'right' on a Java String                  */
 /* ----------------------------------------------------------------- */
 /* Arg1 is string to right-justify */
 /* Arg2 is desired length */
 
 private static java.lang.String right(java.lang.String s,int len){
  int slen;
  slen=s.length();
  if (slen==len) 
   return s; // length just right
  if (slen>len) 
   return s.substring(slen-len); // truncate on left
  // too short
  return (new java.lang.String(new char[len-slen])).replace('\000',' ').concat(s);
  }

 /* ----------------------------------------------------------------- */
 /* left - Utility to do a 'left' on a Java String                    */
 /* ----------------------------------------------------------------- */
 /* Arg1 is string to left-justify */
 /* Arg2 is desired length */
 
 private static java.lang.String left(java.lang.String s,int len){
  int slen;
  slen=s.length();
  if (slen==len) 
   return s; // length just right
  if (slen>len) 
   return s.substring(0,len); // truncate on right
  // too short
  return s.concat((new java.lang.String(new char[len-slen])).replace('\000',' '));
  }

 /* ----------------------------------------------------------------- */
 /* say - Utility to do a display                                     */
 /* ----------------------------------------------------------------- */
 /* Arg1 is string to display, omitted if none */
 /*         [null or omitted gives blank line] */
 // this version doesn't heed continuation final character
 
 private void say(TestHarness harness){
   say((java.lang.String)null, harness);return;
  }
 private void say(java.lang.String s, TestHarness harness){
  if (s==null) 
   s="  ";
  harness.verbose(s);
  return;
 }

 private boolean checkMessage(Throwable ex, String msg){
   return !CHECK_EXCEPTION_MESSAGES || ex.getMessage().equals(msg);
 }

 public void test (TestHarness harness){
  diagrun (harness);
 }
}
