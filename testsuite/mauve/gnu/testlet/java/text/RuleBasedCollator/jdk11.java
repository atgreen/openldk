/*************************************************************************
/* Tests for java.text.RuleBasedCollator
/*
/* Copyright (c) 2003 Stephen C. Crawley (crawley@dstc.edu.au)
/*
/* This program is free software; you can redistribute it and/or modify
/* it under the terms of the GNU General Public License as published 
/* by the Free Software Foundation, either version 2 of the License, or
/* (at your option) any later version.
/*
/* This program is distributed in the hope that it will be useful, but
/* WITHOUT ANY WARRANTY; without even the implied warranty of
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
/* GNU General Public License for more details.
/*
/* You should have received a copy of the GNU General Public License
/* along with this program; if not, write to the Free Software Foundation
/* Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307 USA
/*************************************************************************/

// Tags: JDK1.1

package gnu.testlet.java.text.RuleBasedCollator;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.text.RuleBasedCollator;
import java.text.Collator;
import java.text.ParseException;

public class jdk11 implements Testlet
{
  // These are the rule strings returned by calling getRules() on the
  // collators for various JDK 1.4.0 Locales
  private final String EN_US_RULES = 
    "='\u200b'=\u200c=\u200d=\u200e=\u200f=\u0000=\u0001=\u0002=\u0003" +
    "=\u0004=\u0005=\u0006=\u0007=\u0008='\t'='\u000b'=\u000e" +
    "=\u000f='\u0010'=\u0011=\u0012=\u0013=\u0014=\u0015=\u0016" +
    "=\u0017=\u0018=\u0019=\u001a=\u001b=\u001c=\u001d=\u001e=\u001f" +
    "=\u007f=\u0080=\u0081=\u0082=\u0083=\u0084=\u0085=\u0086=\u0087" +
    "=\u0088=\u0089=\u008a=\u008b=\u008c=\u008d=\u008e=\u008f=\u0090" +
    "=\u0091=\u0092=\u0093=\u0094=\u0095=\u0096=\u0097=\u0098=\u0099" +
    "=\u009a=\u009b=\u009c=\u009d=\u009e=\u009f;' ';'\u00a0';'\u2000'" +
    ";'\u2001';'\u2002';'\u2003';'\u2004';'\u2005';'\u2006';'\u2007'" +
    ";'\u2008';'\u2009';'\u200a';'\u3000';'\ufeff';'\r';'\t'" +
    ";'\n';'\f';'\u000b';\u0301;\u0300;\u0306;\u0302;\u030c;\u030a" +
    ";\u030d;\u0308;\u030b;\u0303;\u0307;\u0304;\u0337;\u0327;\u0328" +
    ";\u0323;\u0332;\u0305;\u0309;\u030e;\u030f;\u0310;\u0311;\u0312" +
    ";\u0313;\u0314;\u0315;\u0316;\u0317;\u0318;\u0319;\u031a;\u031b" +
    ";\u031c;\u031d;\u031e;\u031f;\u0320;\u0321;\u0322;\u0324;\u0325" +
    ";\u0326;\u0329;\u032a;\u032b;\u032c;\u032d;\u032e;\u032f;\u0330" +
    ";\u0331;\u0333;\u0334;\u0335;\u0336;\u0338;\u0339;\u033a;\u033b" +
    ";\u033c;\u033d;\u033e;\u033f;\u0342;\u0344;\u0345;\u0360;\u0361" +
    ";\u0483;\u0484;\u0485;\u0486;\u20d0;\u20d1;\u20d2;\u20d3;\u20d4" +
    ";\u20d5;\u20d6;\u20d7;\u20d8;\u20d9;\u20da;\u20db;\u20dc;\u20dd" +
    ";\u20de;\u20df;\u20e0;\u20e1,'-';\u00ad;\u2010;\u2011;\u2012;\u2013" +
    ";\u2014;\u2015;\u2212<'_'<\u00af<','<';'<':'<'!'<\u00a1<'?'<\u00bf" +
    "<'/'<'.'<\u00b4<'`'<'^'<\u00a8<'~'<\u00b7<\u00b8<'''<'\"'<\u00ab" +
    "<\u00bb<'('<')'<'['<']'<'{'<'}'<\u00a7<\u00b6<\u00a9<\u00ae<'@'" +
    "<\u00a4<\u0e3f<\u00a2<\u20a1<\u20a2<'$'<\u20ab<\u20ac<\u20a3<\u20a4" +
    "<\u20a5<\u20a6<\u20a7<\u00a3<\u20a8<\u20aa<\u20a9<\u00a5<'*'<'\\'<'&'" +
    "<'#'<'%'<'+'<\u00b1<\u00f7<\u00d7<'<'<'='<'>'<\u00ac<'|'<\u00a6" +
    "<\u00b0<\u00b5<0<1<2<3<4<5<6<7<8<9<\u00bc<\u00bd<\u00be<a,A<b,B<c,C" +
    "<d,D<\u00f0,\u00d0<e,E<f,F<g,G<h,H<i,I<j,J<k,K<l,L<m,M<n,N<o,O<p,P" +
    "<q,Q<r,R<s, S & SS,\u00df<t,T& TH, \u00de &TH, \u00fe <u,U<v,V<w,W" +
    "<x,X<y,Y<z,Z&AE,\u00c6&AE,\u00e6&OE,\u0152&OE,\u0153";

  private final String FR_CA_RULES = 
    "='\u200b'=\u200c=\u200d=\u200e=\u200f=\u0000=\u0001=\u0002=\u0003" +
    "=\u0004=\u0005=\u0006=\u0007=\u0008='\t'='\u000b'=\u000e=\u000f" +
    "='\u0010'=\u0011=\u0012=\u0013=\u0014=\u0015=\u0016=\u0017=\u0018" +
    "=\u0019=\u001a=\u001b=\u001c=\u001d=\u001e=\u001f=\u007f=\u0080=\u0081" +
    "=\u0082=\u0083=\u0084=\u0085=\u0086=\u0087=\u0088=\u0089=\u008a=\u008b" +
    "=\u008c=\u008d=\u008e=\u008f=\u0090=\u0091=\u0092=\u0093=\u0094=\u0095" +
    "=\u0096=\u0097=\u0098=\u0099=\u009a=\u009b=\u009c=\u009d=\u009e=\u009f" +
    ";' ';'\u00a0';'\u2000';'\u2001';'\u2002';'\u2003';'\u2004';'\u2005'" +
    ";'\u2006';'\u2007';'\u2008';'\u2009';'\u200a';'\u3000';'\ufeff';'\r'" +
    ";'\t';'\n';'\f';'\u000b';\u0301;\u0300;\u0306;\u0302;\u030c;\u030a" +
    ";\u030d;\u0308;\u030b;\u0303;\u0307;\u0304;\u0337;\u0327;\u0328" +
    ";\u0323;\u0332;\u0305;\u0309;\u030e;\u030f;\u0310;\u0311;\u0312" +
    ";\u0313;\u0314;\u0315;\u0316;\u0317;\u0318;\u0319;\u031a;\u031b" +
    ";\u031c;\u031d;\u031e;\u031f;\u0320;\u0321;\u0322;\u0324;\u0325" +
    ";\u0326;\u0329;\u032a;\u032b;\u032c;\u032d;\u032e;\u032f;\u0330" +
    ";\u0331;\u0333;\u0334;\u0335;\u0336;\u0338;\u0339;\u033a;\u033b" +
    ";\u033c;\u033d;\u033e;\u033f;\u0342;\u0344;\u0345;\u0360;\u0361" +
    ";\u0483;\u0484;\u0485;\u0486;\u20d0;\u20d1;\u20d2;\u20d3;\u20d4" +
    ";\u20d5;\u20d6;\u20d7;\u20d8;\u20d9;\u20da;\u20db;\u20dc;\u20dd" +
    ";\u20de;\u20df;\u20e0;\u20e1,'-';\u00ad;\u2010;\u2011;\u2012;\u2013" +
    ";\u2014;\u2015;\u2212<'_'<\u00af<','<';'<':'<'!'<\u00a1<'?'<\u00bf" +
    "<'/'<'.'<\u00b4<'`'<'^'<\u00a8<'~'<\u00b7<\u00b8<'''<'\"'<\u00ab" +
    "<\u00bb<'('<')'<'['<']'<'{'<'}'<\u00a7<\u00b6<\u00a9<\u00ae<'@'" +
    "<\u00a4<\u0e3f<\u00a2<\u20a1<\u20a2<'$'<\u20ab<\u20ac<\u20a3<\u20a4" +
    "<\u20a5<\u20a6<\u20a7<\u00a3<\u20a8<\u20aa<\u20a9<\u00a5<'*'<'\\'<'&'" +
    "<'#'<'%'<'+'<\u00b1<\u00f7<\u00d7<'<'<'='<'>'<\u00ac<'|'<\u00a6" +
    "<\u00b0<\u00b5<0<1<2<3<4<5<6<7<8<9<\u00bc<\u00bd<\u00be<a,A<b,B<c,C" +
    "<d,D<\u00f0,\u00d0<e,E<f,F<g,G<h,H<i,I<j,J<k,K<l,L<m,M<n,N<o,O<p,P" +
    "<q,Q<r,R<s, S & SS,\u00df<t,T& TH, \u00de &TH, \u00fe <u,U<v,V<w,W" +
    "<x,X<y,Y<z,Z&AE,\u00c6&AE,\u00e6&OE,\u0152&OE,\u0153@";


  private TestHarness harness;

  private void constructorTests()
  {
    harness.checkPoint("constructor rule parsing");
    RuleBasedCollator r;
    final String[] GOOD_RULES = {
      // Examples from the Sun javadocs
      "< a < b < c < d",
      ("< a,A< b,B< c,C< d,D< e,E< f,F< g,G< h,H< i,I< j,J < k,K< l,L< m,M" +
       "< n,N< o,O< p,P< q,Q< r,R< s,S< t,T < u,U< v,V< w,W< x,X< y,Y< z,Z " +
       "< \u00E5=a\u030A,\u00C5=A\u030A ;aa,AA< \u00E6,\u00C6< \u00F8,\u00D8"),
      ("=\u0301;\u0300;\u0302;\u0308;\u0327;\u0303;\u0304;\u0305" +
       ";\u0306;\u0307;\u0309;\u030A;\u030B;\u030C;\u030D;\u030E" +
       ";\u030F;\u0310;\u0311;\u0312< a , A ; ae, AE ; \u00e6 , \u00c6" +
       "< b , B < c, C < e, E & C < d, D & \u0300 ; \u0308 ; \u0302"),
      // Real collation rules
      EN_US_RULES, FR_CA_RULES,
      // Cases involving non-significant white-space
      "=A ", "=A\t", "=A\n", 
      "=A B", "=A\tB", "=A\nB", 
      "= A", "=\tA", "=\nA", 
      " =A", "\t=A", "\n=A",
      // Dodgy cases that JDKs accept
      " ",
      "='\n''\n'",
      "='\n'\n'\n'",
      // Dodgy cases with unbalanced quotes.  JDKs allow these (though a 
      // couple result in IndexOutOfBoundsExceptions).  However, the spec
      // does not say what they mean.
      "='", /* <- JDK 1.4.0 exception */ "=' ", "='=A", "='=A'", 
      "=''", "='' ","=''=A", "=''=A'", 
      "=''''", /* <- JDK 1.4.0 exception */ "=''''=A", "=''''=A'", 
    };
    
    for (int i = 0; i < GOOD_RULES.length; i++) {
      try {
	r = new RuleBasedCollator(GOOD_RULES[i]);
	harness.check(true);
      }
      catch (ParseException ex) {
	harness.debug(ex);
	harness.debug("unexpected ParseException (offset is " +
		      ex.getErrorOffset() + ")");
	harness.check(false);
      }
      catch (Throwable ex) {
	harness.debug(ex);
	harness.check(false);
      }
    }

    try {
      r = new RuleBasedCollator(null);
      harness.check(false);
    }
    catch (ParseException ex) {
      harness.check(false);
    }
    catch (NullPointerException ex) {
      harness.check(true);
    }
    
    harness.checkPoint("constructor rule parsing errors");
    final String[] BAD_RULES = {
      // Empty rule list
      "", 
      // No relation
      "A",
      // No text following relation
      "=", "<", ";", ",", 
      // Special chars should be quoted
      "=\n", "=#", "==",
    };

    for (int i = 0; i < BAD_RULES.length; i++) {
      try {
	r = new RuleBasedCollator(BAD_RULES[i]);
	harness.check(false);
      }
      catch (ParseException ex) {
	harness.check(true);
      }
      catch (Throwable ex) {
	harness.debug(ex);
	harness.check(false);
      }
    }
  }

  private void doComparisons(RuleBasedCollator r, String[][] tests) 
  {
    for (int i = 0; i < tests.length; i++) {
      int res = r.compare(tests[i][0], tests[i][1]);
      if (res < 0) {
	harness.check(tests[i][2].equals("<"));
      }
      else if (res == 0) {
	harness.check(tests[i][2].equals("="));
      }
      else {
	harness.check(tests[i][2].equals(">"));
      }
    }
  }

  private void ignoreTests() 
  {
    harness.checkPoint("ignorable characters");
    final String TEST_RULES = "=Z<a,A<b,B<c,C";
    final String[][] TESTS = {
      {"abc", "ABC", "<"},
      {"abc", "abc", "="},
      {"Abc", "abc", ">"},
      {"aZbZc", "abc", "="},
      {"aZbZc", "aZbZc", "="},
      {"abc", "aZbZc", "="},
      {"aZbZc", "ABC", "<"},
      {"Z", "Z", "="},
      {"Abc", "aZbZc", ">"},
    };

    try {
      RuleBasedCollator r = new RuleBasedCollator(TEST_RULES);
      doComparisons(r, TESTS);
    }
    catch (ParseException ex) {
      harness.debug(ex);
      harness.fail("ignorable characters: ParseException (offset is " +
		   ex.getErrorOffset() + ")");
    }
  }

  private void oneCharTests() 
  {
    checkStrengths();
    harness.checkPoint("single character ordering");
    final String TEST_RULES = "<a;A=0<b,B=1<c;C,d=2";
    final String[][][] TESTS = {
      { // PRIMARY
	{"", "", "="},
	{"abc", "abc", "="},
	{"abc", "ab", ">"},
	{"ab", "abc", "<"},
	{"abc", "Abc", "="},
	{"abc", "aBc", "="},
	{"abc", "abd", "="},
	{"abc", "abC", "="},
	{"abC", "abd", "="},
	{"Abc", "abc", "="},
	{"aBc", "abc", "="},
	{"abd", "abc", "="},
	{"abC", "abc", "="},
	{"abd", "abC", "="},
	{"abc", "012", "="},
	{"ABd", "012", "="},
	{"abc", "xyz", "<"}, 
	{"xyz", "abc", ">"},
	{"pqr", "xyz", "<"}, /* While the Sun Javadoc simply says that
				unmentioned characters appear at the end
				of the collation, the Sun JDK impl'ns 
				appears to order them by raw char value. */
				
      },
      { // SECONDARY
	{"", "", "="},
	{"abc", "abc", "="},
	{"abc", "ab", ">"},
	{"ab", "abc", "<"},
	{"abc", "Abc", "<"},
	{"abc", "aBc", "="},
	{"abc", "abd", "<"},
	{"abc", "abC", "<"},
	{"abC", "abd", "="},
	{"Abc", "abc", ">"},
	{"aBc", "abc", "="},
	{"abd", "abc", ">"},
	{"abC", "abc", ">"},
	{"abd", "abC", "="},
	{"abc", "012", "<"},
	{"ABd", "012", "="},
	{"abc", "xyz", "<"},
	{"xyz", "abc", ">"},
	{"pqr", "xyz", "<"},
      },
      { // TERTIARY
	{"", "", "="},
	{"abc", "abc", "="},
	{"abc", "ab", ">"},
	{"ab", "abc", "<"},
	{"abc", "Abc", "<"},
	{"abc", "aBc", "<"},
	{"abc", "abd", "<"},
	{"abc", "abC", "<"},
	{"abC", "abd", "<"},
	{"Abc", "abc", ">"},
	{"aBc", "abc", ">"},
	{"abd", "abc", ">"},
	{"abC", "abc", ">"},
	{"abd", "abC", ">"},
	{"abc", "012", "<"},
	{"ABd", "012", "="},
	{"abc", "xyz", "<"},
	{"xyz", "abc", ">"},
	{"pqr", "xyz", "<"},
      },
      { // IDENTICAL
	{"", "", "="},
	{"abc", "abc", "="},
	{"abc", "ab", ">"},
	{"ab", "abc", "<"},
	{"abc", "Abc", "<"},
	{"abc", "aBc", "<"},
	{"abc", "abd", "<"},
	{"abc", "abC", "<"},
	{"abC", "abd", "<"},
	{"Abc", "abc", ">"},
	{"aBc", "abc", ">"},
	{"abd", "abc", ">"},	
	{"abC", "abc", ">"},
	{"abd", "abC", ">"},
	{"abc", "012", "<"},
	{"ABd", "012", ">"},  /* It appears that Sun JDKs fall back on the
				 raw character values when characters 
				 are defined as equivalent by the rules. */
	{"abc", "xyz", "<"},
	{"xyz", "abc", ">"},
	{"pqr", "xyz", "<"},
      },
    };

    try {
      RuleBasedCollator r = new RuleBasedCollator(TEST_RULES);
      for (int i = 0; i < TESTS.length; i++) {
	r.setStrength(i);
	doComparisons(r, TESTS[i]);
      }
    }
    catch (ParseException ex) {
      harness.debug(ex);
      harness.fail("single character ordering: ParseException (offset is " +
		   ex.getErrorOffset() + ")");
    }
  }

  private void contractionTests() 
  {
    checkStrengths();
    harness.checkPoint("contraction ordering");
    final String OLD_SPANISH_RULES = "<c,C<ch,cH,Ch,CH<d,D";
    final String[][][] TESTS = {
      {
	// PRIMARY
	{"cat", "cat", "="},
	{"cat", "Cat", "="},
	{"cat", "chat", "<"},
	{"cot", "chat", "<"},
	{"chat", "chit", "<"},
	{"chat", "dog", "<"},
      },
      {
	// SECONDARY
	{"cat", "cat", "="},
	{"cat", "Cat", "="},
	{"cat", "chat", "<"},
	{"cot", "chat", "<"},
	{"chat", "chit", "<"},
	{"chat", "dog", "<"},
      },
      {
	// TERTIARY
	{"cat", "cat", "="},
	{"cat", "Cat", "<"},
	{"cat", "chat", "<"},
	{"cot", "chat", "<"},
	{"chat", "chit", "<"},
	{"chat", "dog", "<"},
      },
      {
	// IDENTICAL
	{"cat", "cat", "="},
	{"cat", "Cat", "<"},
	{"cat", "chat", "<"},
	{"cot", "chat", "<"},
	{"chat", "chit", "<"},
	{"chat", "dog", "<"},
      },
    };
    
    try {
      RuleBasedCollator r = new RuleBasedCollator(OLD_SPANISH_RULES);
      for (int i = 0; i < TESTS.length; i++) {
	r.setStrength(i);
	doComparisons(r, TESTS[i]);
      }
    }
    catch (ParseException ex) {
      harness.debug(ex);
      harness.fail("contraction ordering: ParseException (offset is " +
		   ex.getErrorOffset() + ")");
    }
  }

  private void expansionTests() 
  {
    checkStrengths();
    harness.checkPoint("expansion ordering");
    final String OLD_ENGLISH_RULES = ("<a,A<b,B<c,C<d,D<e,E<f,F" +
				      " &AE,'\u00e6' &AE,'\u00c6'");
    final String[][][] TESTS = {
      {
	// PRIMARY
	{"ae", "\u00e6", "="},
	{"AE", "\u00e6", "="},
	{"ae", "\u00c6", "="},
	{"AE", "\u00c6", "="},
	{"cat", "cat", "="},
	{"cat", "Cat", "="},
	{"caet", "caet", "="},
	{"caet", "c\u00e6t", "="},
	{"c\u00e6t", "caet", "="},
	{"c\u00e6t", "c\u00e6t", "="},
	{"caet", "c\u00c6t", "="},         
	{"c\u00c6t", "caet", "="},
	{"c\u00c6t", "c\u00c6t", "="},	
	{"c\u00c6t", "c\u00e6t", "="},
	{"caet", "cat", "<"},
	{"c\u00e6t", "cat", "<"},
	{"C\u00c6T", "CAT", "<"},
	{"caet", "cab", ">"},
	{"c\u00e6t", "cab", ">"},
	{"C\u00c6T", "CAB", ">"},
      },
      {
	// SECONDARY
	{"ae", "\u00e6", "="},
	{"AE", "\u00e6", "="},
	{"ae", "\u00c6", "="},
	{"AE", "\u00c6", "="},
	{"cat", "cat", "="},
	{"cat", "Cat", "="},
	{"caet", "caet", "="},
	{"caet", "c\u00e6t", "="},
	{"c\u00e6t", "caet", "="},
	{"c\u00e6t", "c\u00e6t", "="},
	{"caet", "c\u00c6t", "="},        
	{"c\u00c6t", "caet", "="},
	{"c\u00c6t", "c\u00c6t", "="},	
	{"c\u00c6t", "c\u00e6t", "="},
	{"caet", "cat", "<"},
	{"c\u00e6t", "cat", "<"},
	{"C\u00c6T", "CAT", "<"},
	{"caet", "cab", ">"},
	{"c\u00e6t", "cab", ">"},
	{"C\u00c6T", "CAB", ">"},
      },
      {
	// TERTIARY
	{"ae", "\u00e6", "<"},
	{"AE", "\u00e6", "<"},            
	{"ae", "\u00c6", "<"},
	{"AE", "\u00c6", "<"},
	{"cat", "cat", "="},
	{"cat", "Cat", "<"},
	{"caet", "caet", "="},
	{"caet", "c\u00e6t", "<"},
	{"c\u00e6t", "caet", ">"},
	{"c\u00e6t", "c\u00e6t", "="},
	{"caet", "c\u00c6t", "<"},
	{"c\u00c6t", "caet", ">"},    
	{"c\u00c6t", "c\u00c6t", "="},	
	{"c\u00c6t", "c\u00e6t", "<"},
	{"caet", "cat", "<"},
	{"c\u00e6t", "cat", ">"}, // JDK is buggy. It fails here.
	{"C\u00c6T", "CAT", ">"}, // JDK is buggy. It fails here.
	{"caet", "cab", ">"},
	{"c\u00e6t", "cab", ">"},
	{"C\u00c6T", "CAB", ">"},
      },
      {
	// IDENTICAL
	{"ae", "\u00e6", "<"},
	{"AE", "\u00e6", "<"},   
	{"ae", "\u00c6", "<"},
	{"AE", "\u00c6", "<"},
	{"cat", "cat", "="},
	{"cat", "Cat", "<"},
	{"caet", "caet", "="},
	{"caet", "c\u00e6t", "<"},
	{"c\u00e6t", "caet", ">"},
	{"c\u00e6t", "c\u00e6t", "="},
	{"caet", "c\u00c6t", "<"},
	{"c\u00c6t", "caet", ">"},    
	{"c\u00c6t", "c\u00c6t", "="},	
	{"c\u00c6t", "c\u00e6t", "<"},
	{"caet", "cat", "<"},
	{"c\u00e6t", "cat", ">"}, // JDK is buggy. It fails here.
	{"C\u00c6T", "CAT", ">"}, // JDK is buggy. It fails here.
	{"caet", "cab", ">"},
	{"c\u00e6t", "cab", ">"},
	{"C\u00c6T", "CAB", ">"},
      },
    };
    
    try {
      RuleBasedCollator r = new RuleBasedCollator(OLD_ENGLISH_RULES);
      for (int i = 0; i < TESTS.length; i++) {
	r.setStrength(i);
	doComparisons(r, TESTS[i]);
      }
    }
    catch (ParseException ex) {
      harness.debug(ex);
      harness.fail("expansion ordering: ParseException (offset is " +
		   ex.getErrorOffset() + ")");
    }
  }

  private void checkStrengths() 
  {
    harness.checkPoint("collator strengths");
    harness.check(Collator.PRIMARY == 0);
    harness.check(Collator.SECONDARY == 1);
    harness.check(Collator.TERTIARY == 2);
    harness.check(Collator.IDENTICAL == 3);
  }

  public void test(TestHarness harness)
  {
    this.harness = harness;
    constructorTests();
    ignoreTests();
    oneCharTests();
    contractionTests();
    expansionTests();
    // More tests in the pipeline
  }
  
} // class jdk11
