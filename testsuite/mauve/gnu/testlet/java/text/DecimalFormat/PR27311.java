// Tags: JDK1.2

// Copyright (C) 2006 Andrew John Hughes <gnu_andrew@member.fsf.org>

// This file is part of Mauve.

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version. 

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.text.DecimalFormat;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Locale;

/*
 * This test is based on PR27311, where
 * formatting 0.0E00 using the format 0.0#####E0
 * produced 0.0E--9223372036854775808.  This was down
 * to incorrect use of the logarithm function in 
 * calculating the exponent.  The log of 0 is negative
 * infinity, which explains the bizarre output.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 */
public class PR27311
  implements Testlet
{
  public void test(TestHarness harness)
  {
    DecimalFormatSymbols dfs = new DecimalFormatSymbols(Locale.ENGLISH);
    DecimalFormat nf = new DecimalFormat("0.0#####E00", dfs);
    nf.setGroupingUsed(false);
    String result = nf.format(0.0E00);
    harness.check(result.equals("0.0E00"),result);
  }
}

