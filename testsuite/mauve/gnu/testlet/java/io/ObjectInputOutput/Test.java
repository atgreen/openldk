// Tags: not-a-test

/* Test.java -- Classes used to test Object Input/Output

   Copyright (c) 1999, 2004 by Free Software Foundation, Inc.
   Written by Geoff Berry <gcb@gnu.org>, Guilhem Lavaux <guilhem@kaffe.org>.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation, version 2. (see COPYING)

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation
   Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307 USA */

package gnu.testlet.java.io.ObjectInputOutput;

import java.io.*;
import java.lang.reflect.*;

public abstract class Test
{
  public static void main (String[] args) throws IOException
  {
    Test[] tests = Test.getValidTests ();
    for (int i = 0; i < tests.length; ++ i)
      writeRefData (tests[i], false);

    tests = Test.getErrorTests ();
    for (int i = 0; i < tests.length; ++ i)
      writeRefData (tests[i], true);
  }

  static void writeRefData (Test t, boolean throwsOSE) throws IOException
  {
    String file = t.getClass ().getName ();
    int idx = file.lastIndexOf ('.');
    if (idx != -1)
      file = file.substring (idx + 1);
    file += ".data";

    ObjectOutputStream oos
      = new ObjectOutputStream (new FileOutputStream (file));

    Object[] objs = t.getTestObjs ();
    for (int i = 0; i < objs.length; ++ i)
      writeData (oos, objs[i], throwsOSE);

    oos.close ();
  }
  
  static void writeData (ObjectOutputStream oos, Object obj, boolean throwsOSE)
    throws IOException
  {
    try
    {
      oos.writeObject (obj);
    }
    catch (ObjectStreamException nse)
    {
      if (!throwsOSE)
	throw nse;
    }
  }
  
  static Test[] getValidTests ()
  {
    return new Test[] {new CallDefault (),
			 new Extern (),
			 new NoCallDefault (),
			 new HairyGraph (),
			 new GetPutField (),
                         new FinalField ()};
  }

  static Test[] getErrorTests ()
  {
    return new Test[] {new NotSerial (), new BadField ()};
  }

  Test () {}
  
  abstract Object[] getTestObjs ();
  
  public String toString ()
  {
    try
    {
      Class clazz = getClass ();
      StringBuffer buf = new StringBuffer (clazz.getName ());
      buf.append (" (");
      Field[] fields = clazz.getDeclaredFields ();
      for (int i = 0; i < fields.length; ++ i)
      {
	Field f = fields[i];
	buf.append (f.getName ());
	buf.append (" = ");
	Class f_type = f.getType ();

	if (f_type == boolean.class)
	  buf.append (f.getBoolean (this));
	else if (f_type == byte.class)
	  buf.append (f.getByte (this));
	else if (f_type == char.class)
	  buf.append (f.getChar (this));
	else if (f_type == double.class)
	  buf.append (f.getDouble (this));
	else if (f_type == float.class)
	  buf.append (f.getFloat (this));
	else if (f_type == long.class)
	  buf.append (f.getLong (this));
	else if (f_type == short.class)
	  buf.append (f.getShort (this));
	else if (f_type == String.class)
	{
	  String s = (String)f.get (this);
	  if (s != null)
	    buf.append ('"');
	  buf.append (s);
	  if (s != null)
	    buf.append ('"');	  
	}
	else
	  buf.append (f.get (this));

	if (i != fields.length - 1)
	  buf.append (", ");
      }
      buf.append (')');
      return buf.toString ();
    }
    catch (IllegalAccessException iae)
    {
      return super.toString ();
    }
  }
  
  static class CallDefault extends Test implements Serializable
  {
    CallDefault () {}

    CallDefault (int X, double Y, String S)
    {
      x = X;
      y = Y;
      s = S;
    }
    
    public boolean equals (Object o)
    {
      CallDefault oo = (CallDefault)o;
      return oo.x == x
	&& oo.y == y
	&& oo.s.equals (s);
    }
    
    Object[] getTestObjs ()
    {
      return new Object[] {new CallDefault (1, 3.14, "test")};
    }

    private void writeObject (ObjectOutputStream oos) throws IOException
    {
      oos.defaultWriteObject ();
    }
    
    private void readObject (ObjectInputStream ois)
      throws ClassNotFoundException, IOException
    {
      ois.defaultReadObject ();
    }
    
    int x;
    double y;
    String s;
  }

  static class Extern extends NoCallDefault implements Externalizable
  {
    public Extern () {}
    
    Extern (int X, String S, boolean B)
    {
      super (X, S, B);
    }

    public void writeExternal (ObjectOutput oo) throws IOException
    {
      oo.writeInt (x);
      oo.writeObject (s);
      oo.writeBoolean (b);
    }
    
    public void readExternal (ObjectInput oi)
      throws ClassNotFoundException, IOException
    {
      x = oi.readInt ();
      s = (String)oi.readObject ();
      b = oi.readBoolean ();
    }
    
    public boolean equals (Object o)
    {
      Extern e = (Extern)o;
      return e.x == x
	&& e.s.equals (s)
	&& e.b == b;
    }  

    Object[] getTestObjs ()
    {
      return new Object[] {new Extern (-1, "", true)};
    }
  }
  
  static class NoCallDefault extends Test implements Serializable
  {
    NoCallDefault () {}
    
    NoCallDefault (int X, String S, boolean B)
    {
      x = X;
      s = S;
      b = B;
    }
    
    public boolean equals (Object o)
    {
      NoCallDefault oo = (NoCallDefault)o;
      return oo.x == x
	&& oo.b == b
	&& oo.s.equals (s);
    }
    
    Object[] getTestObjs ()
    {
      return new Object[] {new NoCallDefault (17, "no\ncalldefaults", false)};
    }

    private void writeObject (ObjectOutputStream oos) throws IOException
    {
      oos.writeInt (x);
      oos.writeObject (s);
      oos.writeBoolean (b);
    }
    
    private void readObject (ObjectInputStream ois)
      throws ClassNotFoundException, IOException
    {
      x = ois.readInt ();
      s = (String)ois.readObject ();
      b = ois.readBoolean ();
    }
    
    int x;
    String s;
    boolean b;    
  }
  
  static class GraphNode implements Serializable
  {
    GraphNode (String s)
    {
      this.s = s;
    }
    
    public String toString ()
    {
      return this.s;
    }
    
    String s;
    GraphNode a;
    GraphNode b;
    GraphNode c;
    GraphNode d;
  }
  
  static class HairyGraph extends Test implements Serializable
  {
    HairyGraph ()
    {
      A = new GraphNode ("A");
      B = new GraphNode ("B");
      C = new GraphNode ("C");
      D = new GraphNode ("D");
      
      A.a = B;
      A.b = C;
      A.c = D;
      A.d = A;
      
      B.a = C;
      B.b = D;
      B.c = A;
      B.d = B;
      
      C.a = D;
      C.b = A;
      C.c = B;
      C.d = C;
      
      D.a = A;
      D.b = B;
      D.c = C;
      D.d = D;
    }
    
    public boolean equals (Object o)
    {
      HairyGraph hg = (HairyGraph)o;
      
      return (A.a == B.d) && (A.a == C.c) && (A.a == D.b)
	&& (A.b == B.a) && (A.b == C.d) && (A.b == D.c)
	&& (A.c == B.b) && (A.c == C.a) && (A.c == D.d)
	&& (A.d == B.c) && (A.d == C.b) && (A.d == D.a);
    }
    
    Object[] getTestObjs ()
    {
      return new Object[] {new HairyGraph ()};
    }

    void printOneLevel (GraphNode gn)
    {
      System.out.println ("GraphNode( " + gn + ": " + gn.a + ", " + gn.b
			  + ", " + gn.c + ", " + gn.d + " )");
    }

    GraphNode A;
    GraphNode B;
    GraphNode C;
    GraphNode D;
  }

  static class GetPutField extends Test implements Serializable
  {
    Object[] getTestObjs ()
    {
      // Don't make a test with WRONG_STR_VAL or WRONG_X_VAL
      return new Object[] {new GetPutField ("test123", 123),
			   new GetPutField ("", 0),
			   new GetPutField (null, -1)};
    }

    GetPutField () {}
    
    GetPutField (String str, int x)
    {
      this.str = str;
      this.x = x;
    }
    
    public boolean equals (Object o)
    {
      if (!(o instanceof GetPutField))
	return false;

      GetPutField other = (GetPutField)o;
      
      return (other.str == str || other.str.equals (str))
	&& other.x == x;
    }

    public String toString ()
    {
      return "test(str=" + str + ", x=" + x + ")";
    }
    
    private void writeObject (ObjectOutputStream oo) throws IOException
    {
      ObjectOutputStream.PutField pf = oo.putFields ();
      pf.put ("str", str);
      pf.put ("x", x);
      oo.writeFields ();
    }

    private void readObject (ObjectInputStream oi)
      throws ClassNotFoundException, IOException
    {
      ObjectInputStream.GetField gf = oi.readFields ();
      str = (String)gf.get ("str", WRONG_STR_VAL);
      x = gf.get ("x", WRONG_X_VAL);
    }

    private static final String WRONG_STR_VAL = "wrong-o";
    private static final int WRONG_X_VAL = -17;
    private String str;
    private int x;
  }
  
  static class NotSerial extends Test
  {
    Object[] getTestObjs ()
    {
      return new Object[] {new NotSerial ()};
    }
  }

  static class BadField extends Test implements Serializable
  {
    BadField (int X, int Y, NotSerial O)
    {
      x = X;
      y = Y;
      o = O;
    }

    BadField () {}
    
    Object[] getTestObjs ()
    {
      return new Object[] {new BadField (1, 2, new NotSerial ())};
    }

    int x;
    int y;
    NotSerial o;    
  }

  static class FinalField extends Test implements Serializable
  {
    final int a;
    final String s;

    FinalField() 
    {
      s = "C";
      a = 2;
    }
    
    Object[] getTestObjs ()
    {
      return new Object[] { new FinalField () };
    }
    
    public boolean equals (Object o)
    {
      FinalField oo = (FinalField)o;
      return oo.a == a
	&& oo.s.equals (s);
    }
  }
}
