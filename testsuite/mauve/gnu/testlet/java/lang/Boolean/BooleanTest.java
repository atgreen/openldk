/* Copyright (C) 1999  Hewlett-Packard Company

   This file is part of Mauve.

   Mauve is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Mauve is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Mauve; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
*/

// Tags: JDK1.5

package gnu.testlet.java.lang.Boolean;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class BooleanTest implements Testlet
{
  public void test_Basics(TestHarness harness)
  {
    harness.checkPoint ("Basics");

    harness.check (Boolean.TRUE.equals(new Boolean(true)) 
		   && Boolean.FALSE.equals(new Boolean(false)));
      
    Boolean b1 = new Boolean( true );
    Boolean b2 = new Boolean( false );
    
    harness.check (b1.booleanValue() == true && b2.booleanValue() == false);
      
    Boolean bs1 = new Boolean ("True");
    Boolean bs2 = new Boolean ("False");
    Boolean bs3 = new Boolean ("true");
    Boolean bs4 = new Boolean ("hi");
    Boolean bs5 = new Boolean ("");

    harness.check (bs1.booleanValue() == true 
		   && bs2.booleanValue() == false
		   && bs3.booleanValue() == true
		   && bs4.booleanValue() == false
		   && bs5.booleanValue() == false );
      
    harness.check (bs1.toString().equals("true")
		   && bs2.toString().equals("false"));
  }
  
  public void test_equals (TestHarness harness)
    {
      harness.checkPoint ("equals");

      Boolean b1 = new Boolean(true);
      Boolean b2 = new Boolean(false);
      
      harness.check (! b1.equals(new Integer(4)));

      harness.check (! b1.equals(null));
      
      harness.check (! b1.equals( b2 ));
      
      harness.check (b1.equals( new Boolean(true) ));
    }
  
  public void test_hashCode(TestHarness harness)
    {
      harness.checkPoint ("hashCode");

      Boolean b1 = new Boolean(true);
      Boolean b2 = new Boolean(false);
      
      harness.check ( b1.hashCode() == 1231 
		      && b2.hashCode() == 1237 );
    }
  
  public void test_booleanValue(TestHarness harness)
    {
      harness.checkPoint ("booleanValue");

      Boolean b1 = new Boolean(true);
      Boolean b2 = new Boolean(false);
      
      harness.check ( b1.booleanValue() == true 
		      && b2.booleanValue() == false );
    }
  
  public void test_valueOf(TestHarness harness)
    {
      harness.checkPoint ("valueOf");

      harness.check (Boolean.valueOf("True").booleanValue() 
		     && Boolean.valueOf("true").booleanValue()
		     && !Boolean.valueOf("anc").booleanValue());
    }
  
  public void test_getBoolean(TestHarness harness)
    {
      harness.checkPoint ("getBoolean");

      java.util.Properties prop = System.getProperties();
      prop.put("booleankey1" , "true" );
      prop.put("booleankey2" , "false" );
      prop.put("booleankey3" , "hi" );
      
      System.setProperties(prop);

      harness.check ( Boolean.getBoolean("booleankey1") == true 
		      && Boolean.getBoolean("booleankey2") == false 
		      && Boolean.getBoolean("booleankey3") == false );
    }

  public void test (TestHarness harness)
    {
      test_Basics (harness);
      test_equals (harness);
      test_hashCode (harness);
      test_booleanValue (harness);
      test_valueOf (harness);
      test_getBoolean (harness);
    }
}
