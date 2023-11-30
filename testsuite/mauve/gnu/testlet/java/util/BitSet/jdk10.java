// Tags: JDK1.0

/* Copyright (C) 1999 Artur Biesiadowski

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
   Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.util.BitSet;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.util.BitSet;

public class jdk10 implements Testlet
{
	TestHarness h;
	public void test ( TestHarness harness )
	{
		h = harness;
		BitSet b1, b2, b3, b4, b5;

		h.checkPoint("Clone/Equals");
		b1 = new BitSet();
		b2 = (BitSet)b1.clone();
		h.check( trulyEquals(b1,b2) );
		b1 = new BitSet(100);
		h.check( trulyEquals(b1,b2) );
		b1.set(5);
		h.check( !trulyEquals(b1,b2) );
		b2 = (BitSet)b1.clone();
		h.check( trulyEquals(b1,b2));
		h.check(!b2.equals(null));

		h.checkPoint("NegativeSize");
		try {
			b1 = new BitSet(-1);
			h.check(false);
		} catch ( NegativeArraySizeException e )
			{
				h.check(true);
			}

		h.checkPoint("Set/Clear/Get");
		b1 = new BitSet();
		b1.set(1);
		b1.set(200);
		b1.set(0);
		h.check(b1.get(0));
		h.check(b1.get(1));
		h.check(!b1.get(2));
		h.check(b1.get(200));
		b1.clear(0);
		h.check(!b1.get(0));

		h.checkPoint("Set/Clear/Get negative index");
		try {
			b1.set(-1);
			h.check(false);
		} catch ( IndexOutOfBoundsException e )
		{
			h.check(true);
		}
		
		try {
			b1.get(-1);
			h.check(false);
		} catch ( IndexOutOfBoundsException e )
		{
			h.check(true);
		}
		
		try {
			b1.clear(-1);
			h.check(false);
		} catch ( IndexOutOfBoundsException e )
		{
			h.check(true);
		}

		h.checkPoint("toString");
		h.check(b1.toString().equals("{1, 200}"));
		b1.set(2);
		b1.set(11);
		h.check(b1.toString().equals("{1, 2, 11, 200}"));
		b2 = new BitSet(100);
		h.check(b2.toString().equals("{}"));
		b2.set(1);
		h.check(b2.toString().equals("{1}"));

		h.checkPoint("Hashcode");
		h.check(b1.hashCode() == 2260);
		b3 = new BitSet();
		h.check(b3.hashCode() == 1234);

		h.checkPoint("And/Or/Xor");
		b2.set(1);
		b2.set(3);
		b2.set(200);
		b2.set(300);
		b2.and(b1);
		h.check( b2.toString().equals("{1, 200}") );
		b1.set(17);
		b2.set(15);
		b2.or(b1);
		h.check( b2.toString().equals("{1, 2, 11, 15, 17, 200}") );
		b2.xor(b2);
		h.check( b2.toString().equals("{}") );
		b2.xor(b1);
		b3.or(b1);
		h.check( trulyEquals(b2,b3) );
		
		h.checkPoint("NullPointerExceptions");
		try {
			b1.and(null);
			h.check(false);
		} catch ( NullPointerException e )
			{
				h.check(true);
			}
		try {
			b1.or(null);
			h.check(false);
		} catch ( NullPointerException e )
			{
				h.check(true);
			}
			
		try {
			b1.xor(null);
			h.check(false);
		} catch ( NullPointerException e )
			{
				h.check(true);
			}			
		
	}
	
	private boolean trulyEquals( BitSet b1, BitSet b2 )
	{
		boolean e1 = b1.equals(b2);
		boolean e2 = true;
		for ( int i = 0; i < 300; i++ )
		{
			if ( b1.get(i) !=  b2.get(i) )
				e2 = false;
		}
		h.check (e1 == e2);
		return e2;	
	}
}
