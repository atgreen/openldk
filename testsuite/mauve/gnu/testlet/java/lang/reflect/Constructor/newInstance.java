// Tags: JDK1.1

// Copyright (C) 2000, 2001 Cygnus Solutions

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

package gnu.testlet.java.lang.reflect.Constructor;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

public class newInstance implements Testlet
{
  public int dot;

  private newInstance (int z)
  {
    dot = z;
  }

  private newInstance (char z)
  {
    dot = (int) z;
  }

  public newInstance (String q, String z)
  {
    throw new NullPointerException ();
  }

  public newInstance ()
  {
    dot = 7;
  }

  public Class getClass (String name)
  {
    Class k = null;
    try
      {
	k = Class.forName (name);
      }
    catch (Throwable _)
      {
	// Nothing.
      }
    return k;
  }

  public Constructor getCons (Class k, Class[] ptypes)
  {
    Constructor c = null;
    try
      {
	c = k.getDeclaredConstructor(ptypes);
      }
    catch (Throwable _)
      {
	// Nothing.
      }
    return c;
  }

  public Object callNew (Constructor cons, Object[] args)
  {
    try
      {
	return cons.newInstance(args);
      }
    catch (Throwable _)
      {
	return _;
      }
  }

  public void test (TestHarness harness)
  {
    Class ni_class = getClass ("gnu.testlet.java.lang.reflect.Constructor.newInstance");
    Class S_class = getClass ("java.lang.String");
    Class i_class = Integer.TYPE;

    Class[] args0 = new Class[0];
    Class[] args1 = new Class[1];
    args1[0] = i_class;
    Class[] args2 = new Class[2];
    args2[0] = args2[1] = S_class;
    Class[] argsc = new Class[1];
    argsc[0] = Character.TYPE;

    harness.checkPoint ("no args");
    Constructor c0 = getCons (ni_class, args0);
    Object r = callNew (c0, new Object[0]);
    harness.check(r instanceof newInstance);
    harness.check(((newInstance) r).dot == 7);

    // null argument list is ok because constructor has no arguments.
    r = callNew (c0, null);
    harness.check(r instanceof newInstance);
    harness.check(((newInstance) r).dot == 7);

    harness.checkPoint ("int arg");
    Constructor c1 = getCons (ni_class, args1);
    Object[] a1 = new Object[1];
    a1[0] = new Integer (23);
    r = callNew (c1, a1);
    harness.check(r instanceof newInstance);
    harness.check(((newInstance) r).dot == 23);
    // Check that promotion works.
    a1[0] = new Short ((short) 24);
    r = callNew (c1, a1);
    harness.check(r instanceof newInstance);
    harness.check(((newInstance) r).dot == 24);
    // Check that demotion doesn't work.
    a1[0] = new Long (25);
    r = callNew (c1, a1);
    harness.check(r instanceof IllegalArgumentException);

    harness.checkPoint ("character arg");
    Constructor c2 = getCons (ni_class, argsc);
    a1[0] = new Character ('j');
    r = callNew (c2, a1);
    harness.check(r instanceof newInstance);
    harness.check(((newInstance) r).dot == (int) 'j');
    // Byte does not widen to Character.
    a1[0] = new Byte ((byte) 93);
    r = callNew (c2, a1);
    harness.check(r instanceof IllegalArgumentException);

    harness.checkPoint ("String args");
    Constructor c3 = getCons (ni_class, args2);
    Object[] a3 = new Object[2];
    // Check that arg types must match.
    a3[0] = new Integer (5);
    a3[1] = "has spoken";
    r = callNew (c3, a3);
    harness.check(r instanceof IllegalArgumentException);
    a3[0] = "zardoz";
    r = callNew (c3, a3);
    harness.check(r instanceof InvocationTargetException);
    harness.debug(r + "");
    harness.check(((InvocationTargetException) r).getTargetException()
		  instanceof NullPointerException);
  }
}
