/* Copyright (C) 1999 Hewlett-Packard Company

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

// Tags: JDK1.0

package gnu.testlet.java.lang.Short;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class ShortTest implements Testlet
{

  protected static TestHarness harness;
	public void test_Basics()
	{
		harness.check(!( Short.MIN_VALUE != -32768 || 
			 Short.MAX_VALUE != 32767 ), 
			"test_Basics - 1" );

		Short i1 = new Short((short)100);

		harness.check(!( i1.shortValue() != 100 ), 
			"test_Basics - 2" );

		try {
		harness.check(!( (new Short("234")).shortValue() != 234 ), 
			"test_Basics - 3" );
		}
		catch ( NumberFormatException e )
		{
			harness.fail("test_Basics - 3" );
		}

		try {
		harness.check(!( (new Short("-FF")).shortValue() != -255 ), 
			"test_Basics - 4" );
		}
		catch ( NumberFormatException e )
		{
		}

		try {
		    new Short("babu");
			harness.fail("test_Basics - 5" );
		}
		catch ( NumberFormatException e )
		{
		}
		harness.check(!( Short.decode( "123").shortValue() != 123 ), 
			"test_Basics - 6" );
		harness.check(!( Short.decode( "32767").shortValue() != 32767 ), 
			"test_Basics - 7" );

	}

	public void test_toString()
	{
		harness.check(!( !( new Short((short)123)).toString().equals("123")), 
			"test_toString - 1" );
		harness.check(!( !( new Short((short)-44)).toString().equals("-44")), 
			"test_toString - 2" );

		harness.check(!( !Short.toString((short) 234 ).equals ("234" )), 
			"test_toString - 3" );
		harness.check(!( !Short.toString((short) -34 ).equals ("-34" )), 
			"test_toString - 4" );
		harness.check(!( !Short.toString((short) -34 ).equals ("-34" )), 
			"test_toString - 5" );

	}

	public void test_equals()
	{
		Short i1 = new Short((short)23);
		Short i2 = new Short((short)-23);

		harness.check(!( !i1.equals( new Short((short)23))), 
			"test_equals - 1" );
		harness.check(!( !i2.equals( new Short((short)-23))), 
			"test_equals - 2" );

		
		harness.check(!( i1.equals( i2 )), 
			"test_equals - 3" );

		harness.check(!( i1.equals(null)), 
			"test_equals - 4" );
	}

	public void test_hashCode( )
	{
		Short b1 = new Short((short)3439);
		Short b2 = new Short((short)-3439);

		harness.check(!( b1.hashCode() != 3439 || b2.hashCode() != -3439 ), 
			"test_hashCode" );
	}

	public void test_intValue( )
	{
		Short b1 = new Short((short)32767);
		Short b2 = new Short((short)-32767);

		harness.check(!( b1.intValue() != 32767 ),  
			"test_intValue - 1" );

		harness.check(!( b2.intValue() != -32767 ),  
			"test_intValue - 2" );
	}

	public void test_longValue( )
	{
		Short b1 = new Short((short)3767);
		Short b2 = new Short((short)-3767);

		harness.check(!( b1.longValue() != (long)3767 ),  
			"test_longValue - 1" );

		harness.check(!( b2.longValue() != -3767 ),  
			"test_longValue - 2" );
	}
	public void test_floatValue( )
	{
		Short b1 = new Short((short)3276);
		Short b2 = new Short((short)-3276);

		harness.check(!( b1.floatValue() != 3276.0f ),  
			"test_floatValue - 1" );

		harness.check(!( b2.floatValue() != -3276.0f ),  
			"test_floatValue - 2" );
	}
	public void test_doubleValue( )
	{
		Short b1 = new Short((short)0);
		Short b2 = new Short((short)30);

		harness.check(!( b1.doubleValue() != 0.0 ),  
			"test_doubleValue - 1" );

		harness.check(!( b2.doubleValue() != 30.0 ),  
			"test_doubleValue - 2" );
	}

	public void test_shortbyteValue( )
	{
		Short b1 = new Short((short)0);
		Short b2 = new Short((short)300);

		harness.check(!( b1.byteValue() != 0 ),  
			"test_shortbyteValue - 1" );

		harness.check(!( b2.byteValue() != (byte)300 ),  
			"test_shortbyteValue - 2" );
		harness.check(!( b1.shortValue() != 0 ),  
			"test_shortbyteValue - 3" );

		harness.check(!( b2.shortValue() != (short)300 ),  
			"test_shortbyteValue - 4" );
		harness.check(!( ((Number)b1).shortValue() != 0 ),  
			"test_shortbyteValue - 5" );

		harness.check(!( ((Number)b2).byteValue() != (byte)300 ),  
			"test_shortbyteValue - 6" );
	}

	public void test_parseShort()
	{
		harness.check(!( Short.parseShort("473") != Short.parseShort("473" , 10 )), 
			"test_parseInt - 1" );	

		harness.check(!( Short.parseShort("0" , 10 ) != 0 ),  
			"test_parseInt - 2" );	

		harness.check(!( Short.parseShort("473" , 10 ) != 473 ),  
			"test_parseInt - 3" );	
		harness.check(!( Short.parseShort("-0" , 10 ) != 0 ),  
			"test_parseInt - 4" );	
		harness.check(!( Short.parseShort("-FF" , 16 ) != -255 ),  
			"test_parseInt - 5" );	
		harness.check(!( Short.parseShort("1100110" , 2 ) != 102 ),  
			"test_parseInt - 6" );	
		try {
			Short.parseShort("99" , 8 );
			harness.fail("test_parseInt - 10" );	
		}catch ( NumberFormatException e ){}
		try {
			Short.parseShort("kona" , 10 );
			harness.fail("test_parseInt - 11" );	
		}catch ( NumberFormatException e ){}
	}

	public void test_valueOf( )
	{
		harness.check(!( Short.valueOf("21234").intValue() != Short.parseShort("21234")), 
			"test_valueOf - 1" );	
	}

	public void testall()
	{
		test_Basics();
		test_toString();
		test_equals();
		test_hashCode();
		test_intValue();
		test_longValue();
		test_floatValue();
		test_doubleValue();
		test_shortbyteValue();
		test_parseShort();
		test_valueOf();
	}

  public void test (TestHarness the_harness)
  {
    harness = the_harness;
    testall ();
  }

}
