/*************************************************************************
/* jdk11.java -- Test JDK 1.1 features in java.text.CollationElementIterator
/*
/* Copyright (c) 2003 C. Brian Jones (cbj@gnu.org)
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

// We use CollationElementIterator.setText, a 1.2 invention.
// Tags: JDK1.2

package gnu.testlet.java.text.CollationElementIterator;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.text.RuleBasedCollator;
import java.text.CollationElementIterator;

public class jdk11 implements Testlet
{
// JDK 1.4 collation rule string for locale English (United States)
  String JDK_1_4_EN_US_RULES =
    "='\u200B'=\u200C=\u200D=\u200E=\u200F=\000 =\001 =\002 =\003 =\004" +
    "=\005 =\006 =\007 =\b ='\t'='\013' =\016=\017 ='\020' =\021 =\022 " +
    "=\023=\024 =\025 =\026 =\027 =\030=\031 =\032 =\033 =\034 =\035=\036" +
    " =\037 =\177=\u0080 =\u0081 =\u0082 =\u0083 =\u0084 =\u0085=\u0086" +
    " =\u0087 =\u0088 =\u0089 =\u008A =\u008B=\u008C =\u008D =\u008E =\u008F" +
    " =\u0090 =\u0091=\u0092 =\u0093 =\u0094 =\u0095 =\u0096 =\u0097=\u0098" +
    " =\u0099 =\u009A =\u009B =\u009C =\u009D=\u009E =\u009F;' ';'\u00A0" +
    "';'\u2000';'\u2001';'\u2002';'\u2003';'\u2004';'\u2005';'\u2006';'" +
    "\u2007';'\u2008';'\u2009';'\u200A';'\u3000';'\uFEFF';'\r' ;'\t' ;'" +
    "\n';'\f';'\013';\u0301;\u0300;\u0306;\u0302;\u030C;\u030A;\u030D;\u0308" +
    ";\u030B;\u0303;\u0307;\u0304;\u0337;\u0327;\u0328;\u0323;\u0332;\u0305" +
    ";\u0309;\u030E;\u030F;\u0310;\u0311;\u0312;\u0313;\u0314;\u0315;\u0316" +
    ";\u0317;\u0318;\u0319;\u031A;\u031B;\u031C;\u031D;\u031E;\u031F;\u0320" +
    ";\u0321;\u0322;\u0324;\u0325;\u0326;\u0329;\u032A;\u032B;\u032C;\u032D" +
    ";\u032E;\u032F;\u0330;\u0331;\u0333;\u0334;\u0335;\u0336;\u0338;\u0339" +
    ";\u033A;\u033B;\u033C;\u033D;\u033E;\u033F;\u0342;\u0344;\u0345;\u0360" +
    ";\u0361;\u0483;\u0484;\u0485;\u0486;\u20D0;\u20D1;\u20D2;\u20D3;\u20D4" +
    ";\u20D5;\u20D6;\u20D7;\u20D8;\u20D9;\u20DA;\u20DB;\u20DC;\u20DD;\u20DE" +
    ";\u20DF;\u20E0;\u20E1,'-';\u00AD;\u2010;\u2011;\u2012;\u2013;\u2014" +
    ";\u2015;\u2212<'_'<\u00AF<','<';'<':'<'!'<\u00A1<'?'<\u00BF<'/'<'." +
    "'<\u00B4<'`'<'^'<\u00A8<'~'<\u00B7<\u00B8<'''<'\"'<\u00AB<\u00BB<'" +
    "('<')'<'['<']'<'{'<'}'<\u00A7<\u00B6<\u00A9<\u00AE<'@'<\u00A4<\u0E3F" +
    "<\u00A2<\u20A1<\u20A2<'$'<\u20AB<\u20AC<\u20A3<\u20A4<\u20A5<\u20A6" +
    "<\u20A7<\u00A3<\u20A8<\u20AA<\u20A9<\u00A5<'*'<'\\'<'&'<'#'<'%'<'+" +
    "'<\u00B1<\u00F7<\u00D7<'<'<'='<'>'<\u00AC<'|'<\u00A6<\u00B0<\u00B5" +
    "<0<1<2<3<4<5<6<7<8<9<\u00BC<\u00BD<\u00BE<a,A<b,B<c,C<d,D<\u00F0,\u00D0" +
    "<e,E<f,F<g,G<h,H<i,I<j,J<k,K<l,L<m,M<n,N<o,O<p,P<q,Q<r,R<s, S & SS" +
    ",\u00DF<t,T& TH, \u00DE &TH, \u00FE <u,U<v,V<w,W<x,X<y,Y<z,Z&AE,\u00C6" +
    "&AE,\u00E6&OE,\u0152&OE,\u0153";

  private static final int PRIMARY = 1;
  private static final int SECONDARY = 2;
  private static final int TERTIARY = 3;
  private static final int NONE = 4;

  private static TestHarness harness = null;

  static void checkOrder (CollationElementIterator iterator, 
			  int count, int order, String tag)
  {
    try
    {
      // next()
      int key = iterator.next();
      int[] prev = {key, 
		    CollationElementIterator.primaryOrder(key),
		    CollationElementIterator.secondaryOrder(key),
		    CollationElementIterator.tertiaryOrder(key)
      };
      harness.debug("first = {" + prev[0] + ", " + prev[1] + ", " +
		    prev[2] + ", " + prev[3] + "}");
      harness.check(key != CollationElementIterator.NULLORDER, "first " + tag);
      
      int i = 1;
      while ((key = iterator.next()) != CollationElementIterator.NULLORDER) {
        i++;
	int[] next = {key, 
		      CollationElementIterator.primaryOrder(key),
		      CollationElementIterator.secondaryOrder(key),
		      CollationElementIterator.tertiaryOrder(key)
	};

	harness.debug("next (" + i + ") = {" + 
		      next[0] + ", " + next[1] + ", " +
		      next[2] + ", " + next[3] + "}");
	
        harness.check(next[0] > prev[0], 
		      "next() " + i + " " + tag);
	if (order == PRIMARY) {
	  harness.check(next[1] > prev[1],
			"no primary difference " + i + " " + tag);
	}
	else if (order == SECONDARY) {
	  harness.check((next[1] > prev[1]) || 
			(next[1] == prev[1] && next[2] > prev[2]),
			"no secondary difference" + i + " " + tag);
	}
	else if (order == TERTIARY) {
	  harness.check((next[1] > prev[1]) || 
			(next[1] == prev[1] && next[2] > prev[2]) ||
			(next[1] == prev[1] && next[2] == prev[2] && 
			 next[3] > prev[3]),
			"no tertiary difference" + i + " " + tag);
	}	
	prev = next;
      }
      if (count != i) {
	harness.debug("count is " + count + ", i is " + i);
      }
      harness.check(count == i, "wrong number of keys " + tag);
    } 
    catch (Throwable t)
    {
      harness.debug (t);
      harness.fail ("CollationElementIterator with localeName");
    }
  }


  static void checkEquiv (CollationElementIterator iterator, 
			  String[] sets, int order, String tag)
  {
    try
    {
      for (int i = 0; i < sets.length; i++) {
	iterator.setText(sets[i]);
	int key = iterator.next();
	int[] prev = {key, 
		      CollationElementIterator.primaryOrder(key),
		      CollationElementIterator.secondaryOrder(key),
		      CollationElementIterator.tertiaryOrder(key)
	};
	harness.debug("first = {" + prev[0] + ", " + prev[1] + ", " +
		      prev[2] + ", " + prev[3] + "}");
	harness.check(key != CollationElementIterator.NULLORDER, 
		      "first " + tag);
	
	int j = 1;
	while ((key = iterator.next()) != CollationElementIterator.NULLORDER) {
	  j++;
	  int[] next = {key, 
			CollationElementIterator.primaryOrder(key),
			CollationElementIterator.secondaryOrder(key),
			CollationElementIterator.tertiaryOrder(key)
	  };
	  
	  harness.debug("next (" + i + ", " + j + ") = {" + 
			next[0] + ", " + next[1] + ", " +
			next[2] + ", " + next[3] + "}");
	  
	  if (order == PRIMARY) {
	    harness.check(next[1] > prev[1],
			  "not primary ordered " + i + ", " + j + " " + tag);
	  }
	  else if (order == SECONDARY) {
	    harness.check(next[1] == prev[1] && next[2] > prev[2],
			  "not secondary ordered" + i + ", " + j + " " + tag);
	  }
	  else if (order == TERTIARY) {
	    harness.check(next[1] == prev[1] && next[2] == prev[2] && 
			  next[3] > prev[3],
			  "not tertiary ordered" + i + ", " + j + " " + tag);
	  }
	  else if (order == NONE) {
	    harness.check(next[1] == prev[1], "keys not equal");
	  }
	  prev = next;
	}
	if (sets[i].length() != j) {
	  harness.debug("length[" + i + "] is " + sets[i].length() +
			", j is " + j);
	}
	harness.check(sets[i].length() == j, 
		      "wrong number of keys (" + j + ") " + tag);
      }
    } 
    catch (Throwable t)
      {
	harness.debug (t);
	harness.fail ("CollationElementIterator with localeName");
      }
  }


  public void test(TestHarness harness)
  {
    // FIXME ... add more test strings for the en_US locale
    // FIXME ... add tests for characters that compare equal
    // FIXME ... add tests for other locales
    final String[] TEST_STRINGS = {
      "X",
      "12",
      "abcdefghijklmnopqrstuvwxyz",
      "0123456789",
      " _,;:!?/.`^~'\"()[]{}@$*\\&#%+<=>|A",
      "aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ",
    };
    final int[] TEST_ORDERS = {
      PRIMARY,
      PRIMARY,
      PRIMARY, 
      PRIMARY,
      PRIMARY,
      TERTIARY,
    };

    final String[][] TEST_STRINGS2 = {
      {"aA", "bB", "cC", "dD", "eE", "fF", "gG", "hH", "iI", "jJ", "kK", 
       "lL", "mM", "nN", "oO", "pP", "qQ", "rR", "sS", "tT", "uU", "vV", 
       "wW", "xX", "yY", "zZ"},
    };
    final int[] TEST_ORDERS2 = {
      TERTIARY,
    };
    
    jdk11.harness = harness;
    try
    {
      // -------- constants -------- 
      harness.check(CollationElementIterator.NULLORDER, -1, "NULLORDER");
  
//       RuleBasedCollator en_USCollator = (RuleBasedCollator) 
// 	Collator.getInstance(new Locale("en", "US", ""));

      // Used to get the collator as above, but this assumes that the
      // en_US locale's collation rules are reasonably complete.
      // Since the point of this class is test the iterator, it is
      // better to use a collator with hard-wired collation rules of
      // known quality.
      RuleBasedCollator en_USCollator = 
        new RuleBasedCollator(JDK_1_4_EN_US_RULES);

      CollationElementIterator iterator = 
        en_USCollator.getCollationElementIterator("abcdefg");

      // -------- methods -------- 
      checkOrder(iterator, 7, PRIMARY, "initial test");

      // reset()
      harness.checkPoint("reset()");
      iterator.reset();
      checkOrder(iterator, 7, PRIMARY, "initial test after reset()");

      // ------- check empty string --------
      iterator = en_USCollator.getCollationElementIterator("");
      harness.check (iterator.next(), CollationElementIterator.NULLORDER, 
		     "next()");

      // ------- detailed checks of collation orders -------
      for (int i = 0; i < TEST_STRINGS.length; i++) {
	iterator = en_USCollator.getCollationElementIterator(TEST_STRINGS[i]);
	checkOrder(iterator, TEST_STRINGS[i].length(), TEST_ORDERS[i], 
		   "test string #" + i);
      }

      // ------- detailed checks of collation equivalences -------
      for (int i = 0; i < TEST_STRINGS2.length; i++) {
	checkEquiv(iterator, TEST_STRINGS2[i], TEST_ORDERS2[i], 
		   "test set #" + i);
      }
    } 
    catch (Throwable t)
    {
      harness.debug(t);
      harness.fail("CollationElementIterator");
    }
  }
 

} // class jdk11

