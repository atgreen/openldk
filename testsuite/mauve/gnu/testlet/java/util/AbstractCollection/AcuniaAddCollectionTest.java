/* Copyright (C) 2001 ACUNIA

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

// Tags: JDK1.2

package gnu.testlet.java.util.AbstractCollection;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.util.*;

/**
*  Written by ACUNIA. <br>
*                        <br>
*  this file contains tests for java.util.AbstractCollection   <br>
*
*/
public class AcuniaAddCollectionTest extends AbstractCollection
                                     implements Testlet
{
  protected TestHarness th;

  public void test (TestHarness harness)
    {
       th = harness;
       test_add();
    }

/**
*  implemented. <br>
*
*/
  public void test_add(){
    th.checkPoint("add(java.lang.Object)boolean");
    AcuniaAddCollectionTest eac = new AcuniaAddCollectionTest();
    try {
    	eac.add(this);
    	th.fail("should throw an UnsupportedOperationException");
    	}
    catch (UnsupportedOperationException uoe) { th.check(true);}
  }

// The following fields and methods are needed to use this class as a
// AbstractCollection implementation.

        public Vector v;


        public AcuniaAddCollectionTest(){
                super();
                v=new Vector();
        }

        public int size() {
                return v.size();
        }

        public Iterator iterator() {
                return v.iterator();
        }
}
