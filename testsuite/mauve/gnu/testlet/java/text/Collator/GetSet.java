/*************************************************************************
/* GetSet.java -- Test get/set methods in java.text.Collator
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
import java.util.Locale;

public class GetSet implements Testlet
{

public void 
test(TestHarness harness)
{
  Collator col = Collator.getInstance(Locale.US);

  harness.check(col.getStrength(), Collator.TERTIARY, "default strength");
  harness.check(col.getDecomposition(), Collator.NO_DECOMPOSITION, 
                "default decomposition");

  col.setStrength(Collator.PRIMARY);
  harness.check(col.getStrength(), Collator.PRIMARY, "set/get strength");

  col.setDecomposition(Collator.NO_DECOMPOSITION);
  harness.check(col.getDecomposition(), Collator.NO_DECOMPOSITION, 
                "set/get decomposition");

  try 
    {
      col.setStrength(999);
      harness.check(false, "invalid strength value");
    }
  catch (Exception e)
    {
      harness.check(true, "invalid strength value");
    }

  try
    {
      col.setDecomposition(999);
      harness.check(false, "invalid decomposition value");
    }
  catch (Exception e)
    {
      harness.check(true, "invalid decomposition value");
    }

  Collator col2 = (Collator)col.clone();
  col2.setStrength(Collator.SECONDARY); 
  harness.check(!col.equals(col2), "equals false");

  harness.check(col.equals(col), "equals true");
}

} // class GetSet

