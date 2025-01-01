/*************************************************************************
/* Constants.java -- Test class constants in java.text.Collator
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

package gnu.testlet.java.text.Collator;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.text.Collator;

public class Constants implements Testlet
{

public void 
test(TestHarness harness)
{
  harness.check(Collator.PRIMARY, 0, "PRIMARY");
  harness.check(Collator.SECONDARY, 1, "SECONDARY");
  harness.check(Collator.TERTIARY, 2, "TERTIARY");
  harness.check(Collator.IDENTICAL, 3, "IDENTICAL");
  harness.check(Collator.NO_DECOMPOSITION, 0, "NO_DECOMPOSITION");
  harness.check(Collator.CANONICAL_DECOMPOSITION, 1, 
                "CANONICAL_DECOMPOSITION");
  harness.check(Collator.FULL_DECOMPOSITION, 2, "FULL_DECOMPOSITION");
}

} // class Constants

