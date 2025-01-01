// Test of shiftRight and shiftLeft methods of BigInteger.

/*************************************************************************
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

package gnu.testlet.java.math.BigInteger;

import java.math.BigInteger;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class shift implements Testlet
{
  public void test (TestHarness harness)
  {
    harness.checkPoint ("shift");
    BigInteger x =
      new BigInteger ("-50123044517898350982301255831878893568", 10);
    harness.check (x.shiftRight(64).toString(), "-2717175687894652269");
    harness.check (x.shiftLeft(-64).toString(), "-2717175687894652269");
    harness.check (x.shiftRight(1).toString(),
      "-25061522258949175491150627915939446784");
    harness.check (x.shiftLeft(1).toString(), 
      "-100246089035796701964602511663757787136");

    harness.check (x.shiftRight(0).toString(), x.toString());
    harness.check (x.shiftLeft(0).toString(), x.toString());

    x = new BigInteger ("50123044517898350982301255831878893568", 10);
    harness.check (x.shiftRight(64).toString(), "2717175687894652268");
    harness.check (x.shiftLeft(-64).toString(), "2717175687894652268");
    harness.check (x.shiftRight(1).toString(),
      "25061522258949175491150627915939446784");
    harness.check (x.shiftLeft(1).toString(), 
      "100246089035796701964602511663757787136");
  }
}
