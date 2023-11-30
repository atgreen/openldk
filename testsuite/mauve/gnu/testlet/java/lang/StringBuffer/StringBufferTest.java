/* Copyright (C) 1999 Hewlett-Packard Company
   Copyright (C) 2002 Free Software Foundation, Inc.

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

package gnu.testlet.java.lang.StringBuffer;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class StringBufferTest implements Testlet
{
  protected static TestHarness harness;
	public void test_Basics()
	{
		try {
			StringBuffer str0 = new StringBuffer(-10);
			harness.fail("test_Basics - 0");
		}catch ( NegativeArraySizeException e ){}

		StringBuffer str1 = new StringBuffer();
		harness.check(!( str1.length() != 0 || str1.capacity() != 16 ),  
			"test_Basics - 1");
		harness.check(!( !str1.toString().equals("")), 
			"test_Basics - 2");

		StringBuffer str2 = new StringBuffer( "testing" );
		harness.check(!( str2.length() != 7 ),  
			"test_Basics - 3");
		harness.check(!( !str2.toString().equals("testing")), 
			"test_Basics - 4");

		
		try {
			String str = null;
			StringBuffer str3 = new StringBuffer(str);
			/*
			  CYGNUS: nowhere does it say that we should
			  handle a null argument in StringBuffer(str).
			  In fact, the JCL implies that we should not.
			  But this leads to an asymmetry: `null + ""'
			  will fail, while `"" + null' will work.  For
			  thaht reason, this test is commented out
			  here: it's not a failure.

			harness.fail("test_Basics - 5");

			*/
		}
		catch ( NullPointerException e ){}	

		StringBuffer str4 =  new StringBuffer("hi there");
		harness.check(!( str4.length () != 8 || str4.capacity () != 24 ),  
			"test_Basics - 6");
		harness.check(!( !str4.toString().equals("hi there")), 
			"test_Basics - 7");

		StringBuffer strbuf = new StringBuffer(0);
		harness.check(!( ! strbuf.append("hiii").toString().equals("hiii")), 
			"test_Basics - 8");

		strbuf = new StringBuffer(10);
		harness.check(!( strbuf.capacity() != 10 ), 
			"test_Basics - 9");


	}

	public void test_toString()
	{
		StringBuffer str1 = new StringBuffer("218943289");

		harness.check(!( !str1.toString().equals("218943289")), 
			"test_toString - 1");

		str1 = new StringBuffer();
		harness.check(!( !str1.toString().equals("")), 
			"test_toString - 2");
	}

	public void test_length()
	{
		StringBuffer buf1 = new StringBuffer("");
		StringBuffer buf2 = new StringBuffer("pentium");

		harness.check(!( buf1.length() != 0 ),  
			"test_length - 1");
		
		harness.check(!( buf2.length() != 7 ),  
			"test_length - 2");
	}

	public void test_capacity()
	{
		StringBuffer buf1 = new StringBuffer("");
		StringBuffer buf2 = new StringBuffer("pentiumpentiumpentium");
		
		harness.check(!( buf1.capacity() != 16 ),  
			"test_capacity - 1");

		int cap = buf2.capacity();
		harness.check(!( cap != 37 ), 
			"test_capacity - 2");


		buf1.ensureCapacity( 17);
		
		// CYGNUS: This is a test for JDK 1.2 conformance
		harness.check(!( buf1.capacity() != 34 ), 
			"test_capacity - 3");
	}

	public void test_setLength()
	{
		StringBuffer strbuf = new StringBuffer("ba");

		try {
			strbuf.setLength( -10);
			harness.fail("test_setLength - 1");
		}
		catch ( IndexOutOfBoundsException e ){}

		strbuf.setLength( 4 );
		harness.check(!(strbuf.length() != 4 ), 
			"test_setLength - 2");

		harness.check(!( strbuf.charAt(0 ) != 'b' || strbuf.charAt(1 ) != 'a' ||
			  strbuf.charAt(2 ) != '\u0000' || strbuf.charAt(3 ) != '\u0000' ), 
			"test_setLength - 3");
	}

	public void test_charAt()
	{
		harness.check(!( (new StringBuffer("abcd")).charAt(0) != 'a' || 
			 (new StringBuffer("abcd")).charAt(1) != 'b' ||
			 (new StringBuffer("abcd")).charAt(2) != 'c' || 
			 (new StringBuffer("abcd")).charAt(3) != 'd'	), 
			
			"test_charAt - 1");

		try {
			char ch = (new StringBuffer("abcd")).charAt(4);
			harness.fail("test_charAt - 2");
		}
		catch ( IndexOutOfBoundsException e ){}

		try {
			char ch = (new StringBuffer("abcd")).charAt(-1);
			harness.fail("test_charAt - 3");
		}
		catch ( IndexOutOfBoundsException e ){}
	}

	public void test_getChars()
	{
		StringBuffer str = new StringBuffer("abcdefghijklmn");

		try {
			str.getChars(0 , 3 , null , 1 );
			harness.fail("test_getChars - 1");
		}catch ( NullPointerException e ){}

		char dst[] = new char[5];
		
		try {
			str.getChars(-1 , 3 , dst , 1 );
			harness.fail("test_getChars - 2");
		}catch ( IndexOutOfBoundsException e ){}

		try {
			str.getChars(4 , 3 , dst , 3 );
			// CYGNUS: This is a test for JDK 1.2 conformance
			harness.fail("test_getChars - 3");
		}catch ( IndexOutOfBoundsException e ){}

		try {
			str.getChars(1 , 15 , dst , 1 );
			harness.fail("test_getChars - 4");
		}catch ( IndexOutOfBoundsException e ){}

		try {
			str.getChars(1 , 5 , dst , -1 );
			harness.fail("test_getChars - 5");
		}catch ( IndexOutOfBoundsException e ){}

		try {
			str.getChars(1 , 10 , dst , 1 );
			harness.fail("test_getChars - 6");
		}catch ( IndexOutOfBoundsException e ){}

		str.getChars(0,5,dst, 0 );
		harness.check(!( dst[0] != 'a' || dst[1] != 'b' || dst[2] != 'c' ||
			 				  dst[3] != 'd' || dst[4] != 'e' ), 
			"test_getChars - 7");

		dst[0] = dst[1] = dst[2] = dst[3] = dst[4] = ' ';
		str.getChars(0,0,dst, 0 );
		harness.check(!( dst[0] != ' ' || dst[1] != ' ' || dst[2] != ' ' ||
			 				  dst[3] != ' ' || dst[4] != ' ' ), 
			"test_getChars - 9");

		dst[0] = dst[1] = dst[2] = dst[3] = dst[4] = ' ';
		str.getChars(0,1,dst, 0 );
		harness.check(!( dst[0] != 'a' || dst[1] != ' ' || dst[2] != ' ' ||
			 				  dst[3] != ' ' || dst[4] != ' ' ), 
			"test_getChars - 10");

		dst = new char[25];
		str.getChars(3,14,dst,12);
		harness.check(dst[12], 'd', "getChars starting src offset 3");
		harness.check(dst[22], 'n', "getChars ending dst offset 22");
	}

	public void test_append( )
	{
		StringBuffer str = new StringBuffer();
		Object nullobj = null;
		harness.check(!( !str.append( nullobj ).toString().equals("null")), 
			"test_append - 1");

		harness.check(!( !str.append( new Integer(100) ).toString().equals("null100")), 
			"test_append - 2");

		StringBuffer str1 = new StringBuffer("hi");
		str1 = str1.append( " there" );
		str1 = str1.append( " buddy");

		harness.check(!( !str1.toString().equals("hi there buddy")), 
			"test_append - 2");

		StringBuffer str2 = new StringBuffer();
		str2 = str2.append("sdljfksdjfklsdjflksdjflkjsdlkfjlsdkjflksdjfklsd");
		harness.check(!( !str2.toString().equals(
							"sdljfksdjfklsdjflksdjflkjsdlkfjlsdkjflksdjfklsd")), 
			"test_append - 3");

		char carr[] = null;
		StringBuffer str3 = new StringBuffer();

		try {
			str3 = str3.append( carr );
			harness.fail("test_append - 4");
		}
		catch ( NullPointerException e ){}

		char carr1[] = {'h','i','t','h','e','r'};
		StringBuffer  str4 = new StringBuffer("!");
		harness.check(!( !str4.append( carr1 ).toString().equals("!hither")), 
			"test_append - 5");


		try {
			str3 = str3.append( carr , 0 , 3);
			harness.fail("test_append - 6");
		}
		catch ( NullPointerException e ){}
		str3 = new StringBuffer();
		try {
			str3 = str3.append( carr1 , -1 , 3);
			harness.fail("test_append - 6");
		}
		catch ( IndexOutOfBoundsException e ){}
		try {
			str3 = str3.append ( carr1 , 0 , -3);
			harness.fail("test_append - 6");
		}
		catch ( IndexOutOfBoundsException e ){}


		StringBuffer str5 = new StringBuffer("!");
		str5 = str5.append(carr1 , 2 , 3 );
		harness.check(!( !str5.toString().equals("!the")), 
			"test_append - 7");

		str5 = new StringBuffer();
		str5 = str5.append( true );
		harness.check(!( !str5.toString().equals("true")), 
			"test_append - 8");

		str5 = str5.append( false );
		harness.check(!( !str5.toString().equals("truefalse")), 
			"test_append - 9");

		str5 = str5.append( 20);
		harness.check(!( !str5.toString().equals("truefalse20")), 
			"test_append - 10");

		str5 = new StringBuffer();
		str5 = str5.append( 2034L );
		harness.check(!( !str5.toString().equals("2034")), 
			"test_append - 11");

		str5 = new StringBuffer();
		str5 = str5.append( 34.45f );
		harness.check(!( !str5.toString().equals("34.45")), 
			"test_append - 12");

		str5 = new StringBuffer();
		str5 = str5.append( 34.46 );
		harness.check(!( !str5.toString().equals("34.46")), 
			"test_append - 13");
	}

	public void test_insert()
	{
		StringBuffer buf = new StringBuffer("1234567");
		Object obj = null;
		buf = buf.insert(5 , obj);
		harness.check(!( !buf.toString().equals("12345null67")), 
			"test_insert - 1");

		try {
			buf = buf.insert(-1 , new Object());
			harness.fail("test_insert - 2");

		}catch ( IndexOutOfBoundsException e ){}

		buf = new StringBuffer("1234567");
		try {
			buf = buf.insert(8 , new Object() );
			harness.fail("test_insert - 3");
		}catch ( IndexOutOfBoundsException e ){}
		
		buf = new StringBuffer("1234567");
		buf = buf.insert(4 , "inserted" );
		harness.check(!( !buf.toString().equals("1234inserted567")), 
			"test_insert - 4");


		buf = new StringBuffer("1234567");
		char cdata[] = null;
		try {
			buf = buf.insert(4 , cdata );
			harness.fail("test_insert - 5");
		}catch ( NullPointerException e ) {}

		cdata = new char[2];
		try {
			buf = buf.insert(-1 , cdata );
			harness.fail("test_insert - 6");

		}catch ( IndexOutOfBoundsException e ){}

		try {
			buf = buf.insert(8 , cdata );
			harness.fail("test_insert - 7");
		}catch ( IndexOutOfBoundsException e ){}

		buf = new StringBuffer("1234567");
		char cdata1[] = {'h','e','l','l','o'};
		buf = buf.insert(4 , cdata1 );
		harness.check(!( !buf.toString().equals("1234hello567")), 
			"test_insert - 8");


		buf = new StringBuffer("1234567");
		buf = buf.insert(0 , true );
		harness.check(!( !buf.toString().equals("true1234567")), 
			"test_insert - 9");

		buf = new StringBuffer("1234567");
		buf = buf.insert(7 , false );
		harness.check(!( !buf.toString().equals("1234567false")), 
			"test_insert - 10");


		buf = new StringBuffer("1234567");
		buf = buf.insert(0 , 'c' );
		harness.check(!( !buf.toString().equals("c1234567")), 
			"test_insert - 11");

		buf = new StringBuffer("1234567");
		buf = buf.insert(7 , 'b' );
		harness.check(!( !buf.toString().equals("1234567b")), 
			"test_insert - 12");
			
		buf = new StringBuffer("1234567");
		buf = buf.insert(7 , 999 );
		harness.check(!( !buf.toString().equals("1234567999")), 
			"test_insert - 13");

		buf = new StringBuffer("1234567");
		buf = buf.insert(0, 99.9f );
		harness.check(!( !buf.toString().equals("99.91234567")), 
			"test_insert - 14");

		buf = new StringBuffer("1234567");
		buf = buf.insert(3, 34.46 );
		harness.check(!( !buf.toString().equals("12334.464567")), 
			"test_insert - 15 "
			   + buf.toString());
		buf = new StringBuffer("1234567");
		buf = buf.insert(3, (long)1230 );
		harness.check(!( !buf.toString().equals("12312304567")), 
			"test_insert - 16 "
			   + buf.toString());

	}

	public void test_reverse()
	{
		StringBuffer buff = new StringBuffer();
		harness.check(!( !buff.reverse().toString().equals("")), 
			"test_reverse - 1");

		buff = new StringBuffer("babu");
		harness.check(!( !buff.reverse().toString().equals("ubab")), 
			"test_reverse - 2");

		buff = new StringBuffer("malayalam");
		harness.check(!( !buff.reverse().toString().equals("malayalam")), 
			"test_reverse - 3");

		buff = new StringBuffer("cnbcbnc");
		harness.check(!( !buff.reverse().toString().equals("cnbcbnc")), 
			"test_reverse - 4");

		buff = new StringBuffer("vinod");
		harness.check(!( !buff.reverse().toString().equals("doniv")), 
			"test_reverse - 5");
	}

	public void testall()
	{
		test_Basics();
		test_toString();
		test_length();
		test_capacity();
		test_setLength();
		test_charAt();
		test_getChars();
		test_append();
		test_insert();
		test_reverse();
	}


  public void test (TestHarness the_harness)
  {
    harness = the_harness;
    testall ();
  }

}
