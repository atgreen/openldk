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
 * Test {@link javax.management.MXBean} implementation for proxying.
 *
 * @author <a href="mailto:gnu_andrew@member.fsf.org">Andrew John Hughes</a>
 */
public class TestC
  implements TestCMXBean
{
  
  private int id;

  private Integer size;

  private ObjectName name;

  private float[] weights;

  private String[] names;

  private Set<Integer> ages;

  private SortedSet<String> biscuits;

  private Colour colour;

  private Map<String,Integer> numbers;

  private SortedMap<String, Integer> sortedNumbers;

  private ChildMXBean child;

  public int getId()
  {
    return id;
  }

  public void setId(int id)
  {
    this.id = id;
  }

  public Integer getSize()
  {
    return size;
  }

  public void setSize(Integer size)
  {
    this.size = size;
  }

  public ObjectName getName()
  {
    return name;
  }

  public void setName(ObjectName name)
  {
    this.name = name;
  }
  
  public float[] getWeights()
  {
    return weights;
  }

  public void setWeights(float[] weights)
  {
    this.weights = weights;
  }

  public String[] getNames()
  {
    return names;
  }

  public void setNames(String[] names)
  {
    this.names = names;
  }

  public Set<Integer> getAges()
  {
    return ages;
  }

  public void setAges(Set<Integer> ages)
  {
    this.ages = ages;
  }

  public SortedSet<String> getBiscuits()
  {
    return biscuits;
  }

  public void setBiscuits(SortedSet<String> biscuits)
  {
    this.biscuits = biscuits;
  }

  public Colour getColour()
  {
    return colour;
  }

  public void setColour(Colour colour)
  {
    this.colour = colour;
  }

  public Map<String,Integer> getPhoneNumbers()
  {
    return numbers;
  }

  public void setPhoneNumbers(Map<String,Integer> numbers)
  {
    this.numbers = numbers;
  }

  public SortedMap<String,Integer> getSortedPhoneNumbers()
  {
    return sortedNumbers;
  }

  public void setSortedPhoneNumbers(SortedMap<String,Integer> numbers)
  {
    sortedNumbers = numbers;
  }

  public ChildMXBean getChild()
  {
    return child;
  }

  public void setChild(ChildMXBean child)
  {
    this.child = child;
  }

}
