// Tags: JDK1.0

// Copyright (C) 1998 Cygnus Solutions

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

package gnu.testlet.java.lang.Long;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.util.Properties;

public class getLong implements Testlet
{
  public void test (TestHarness harness)
    {
      // Augment the System properties with the following.
      // Overwriting is bad because println needs the
      // platform-dependent line.separator property.
      Properties p = System.getProperties();
      p.put("e1", Long.toString(Long.MIN_VALUE));
      p.put("e2", Long.toString(Long.MAX_VALUE));
      p.put("e3", "0" + Long.toOctalString(Long.MIN_VALUE));
      p.put("e4", "0" + Long.toOctalString(Long.MAX_VALUE));
      p.put("e5", "0x" + Long.toHexString(Long.MIN_VALUE));
      p.put("e6", "0x" + Long.toHexString(Long.MAX_VALUE));
      p.put("e7", "0" + Long.toString(Long.MAX_VALUE, 8));
      p.put("e8", "#" + Long.toString(Long.MAX_VALUE, 16));
      p.put("e9", "");
      p.put("e10", " ");
      p.put("e11", "foo");

      try {
        harness.check (Long.getLong("e1").toString(), "-9223372036854775808");
      } catch (NullPointerException npe) { harness.check(false); }
      try {
        harness.check (Long.getLong("e2").toString(), "9223372036854775807");
      } catch (NullPointerException npe) { harness.check(false); }
      try {
        harness.check (Long.getLong("e3"), null);
      } catch (NullPointerException npe) { harness.check(false); }
      try {
        harness.check (Long.getLong("e4").toString(), "9223372036854775807");
      } catch (NullPointerException npe) { harness.check(false); }
      try {
        harness.check (Long.getLong("e5", 12345L).toString(), "12345");
      } catch (NullPointerException npe) { harness.check(false); }
      try {
        harness.check (Long.getLong("e6", new Long(56789L)).toString(),
		     "9223372036854775807");
      } catch (NullPointerException npe) { harness.check(false); }
      try {
        harness.check (Long.getLong("e7", null).toString(),
		     "9223372036854775807");
      } catch (NullPointerException npe) { harness.check(false); }
      try {
        harness.check (Long.getLong("e8", 12345).toString(),
		     "9223372036854775807");
      } catch (NullPointerException npe) { harness.check(false); }
      try {
        harness.check (Long.getLong("e9", new Long(56789L)).toString(), "56789");
      } catch (NullPointerException npe) { harness.check(false); }
      try {
        harness.check (Long.getLong("e10", null), null);
      } catch (NullPointerException npe) { harness.check(false); }
      try {
        harness.check (Long.getLong("e11"), null);
      } catch (NullPointerException npe) { harness.check(false); }
      try {
        harness.check (Long.getLong("junk", 12345L).toString(), "12345");
      } catch (NullPointerException npe) { harness.check(false); }
      try {
        harness.check (Long.getLong("junk", new Long(56789L)).toString(),
		     "56789");
      } catch (NullPointerException npe) { harness.check(false); }
      try {
        harness.check (Long.getLong("junk", null), null);
      } catch (NullPointerException npe) { harness.check(false); }
      try {
        harness.check (Long.getLong("junk"), null);
      } catch (NullPointerException npe) { harness.check(false); }
    }
}
