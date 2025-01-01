/*************************************************************************
/* VeryBasic.java -- Very basic tests of java.text.RuleBasedCollator
/*
/* Copyright (c) 1999 Aaron M. Renn (arenn@urbanophile.com)
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
import java.text.Collator;
import java.util.Locale;

public class VeryBasic implements Testlet
{

  public void test(TestHarness harness)
  {
    // This should be an instance of RuleBasedCollator
    // It should also be set to TERIARY strength and decomp doesn't matter
    // for good ol' English
    Collator col = Collator.getInstance(Locale.US);
    
    harness.check(col.compare("foo", "bar") > 0, "foo and bar");
    harness.check(col.compare("bar", "baz") < 0, "bar and baz");
    harness.check(col.compare("FOO", "FOO") == 0, "FOO and FOO");
    harness.check(col.compare("foo", "foobar") < 0, "foo and foobar");
    
    col.setStrength(Collator.SECONDARY); // Ignore case
    harness.check(col.compare("Foo", "foo") == 0, "Foo and foo");
  }
  
} // class VeryBasic

