// serial.java -- Checks that object can bee serialized and deserialized.
//
// Copyright (c) 2003 Mark J. Wielaard (mark@klomp.org)
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published 
// by the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software Foundation
// Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307 USA

// Tags: JDK1.4

package gnu.testlet.java.text.DecimalFormatSymbols;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.text.DecimalFormatSymbols;
import java.util.Locale;

public class getCurrency implements Testlet
{
  public void test(TestHarness harness)
  {
    try
      {
        DecimalFormatSymbols dfs = new DecimalFormatSymbols(new Locale("foobar"));
        harness.check(dfs.getCurrency().toString().equals("XXX"));
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.check(false);
      }
  }  
}
