// Tags: not-a-test

// Copyright (C) 2007 Andrew John Hughes <gnu_andrew@member.fsf.org>

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
// Boston, MA 02111-1307, USA.

package gnu.testlet.javax.management.MBeanServerInvocationHandler;

import javax.management.ObjectName;

import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;

/**
 * Test {@link javax.management.MXBean} for proxying.
 *
 * @author <a href="mailto:gnu_andrew@member.fsf.org">Andrew John Hughes</a>
 */
public interface TestCMXBean
{
  
  int getId();

  void setId(int id);

  Integer getSize();

  void setSize(Integer size);

  ObjectName getName();

  void setName(ObjectName name);
  
  float[] getWeights();

  void setWeights(float[] weights);

  String[] getNames();

  void setNames(String[] names);

  Set<Integer> getAges();

  void setAges(Set<Integer> ages);

  SortedSet<String> getBiscuits();

  void setBiscuits(SortedSet<String> biscuits);

  Colour getColour();

  void setColour(Colour colour);

  Map<String,Integer> getPhoneNumbers();

  void setPhoneNumbers(Map<String,Integer> numbers);

  SortedMap<String, Integer> getSortedPhoneNumbers();

  void setSortedPhoneNumbers(SortedMap<String, Integer> numbers);

  ChildMXBean getChild();

  void setChild(ChildMXBean bean);

}
