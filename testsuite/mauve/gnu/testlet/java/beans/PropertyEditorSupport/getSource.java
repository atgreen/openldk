//Tags: JDK1.5

//Copyright (C) 2004 Robert Schuster <theBohemian@gmx.net>

//This file is part of Mauve.

//Mauve is free software; you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation; either version 2, or (at your option)
//any later version.
//Mauve is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.
//You should have received a copy of the GNU General Public License
//along with Mauve; see the file COPYING.  If not, write to
//the Free Software Foundation, 59 Temple Place - Suite 330,
//Boston, MA 02111-1307, USA.package gnu.testlet.java.beans.PropertyEditorSupport;

package gnu.testlet.java.beans.PropertyEditorSupport;

import java.beans.PropertyEditorSupport;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Very simple tests that check whether the (event) source of a
 * PropertyEditorSupport instance is set correctly.
 *  
 * @author Robert Schuster
 */
public class getSource implements Testlet
{
  public void test(TestHarness harness)
  {
    PropertyEditorSupport pes;
    
    pes = new PropertyEditorSupport();

    // pes-non argument: using the non-argument form of
    // PropertyEditorSupport the event source should be the 
    // PropertyEditorSupport instance itself
    harness.check(pes.getSource(), pes, "pes-non argument");
    
    // pes-single argument: using the single argument constructor of
    // PropertyEditorSupport the event source should be set to the given
    // Object instance.
    Object eventSource = new Object();
    pes = new PropertyEditorSupport(eventSource);

    harness.check(pes.getSource(), eventSource, "pes-single argument");
  }
}
