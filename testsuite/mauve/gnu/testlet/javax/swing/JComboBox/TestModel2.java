//Tags: not-a-test

//Copyright (C) 2004 Robert Schuster <thebohemian@gmx.net>

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
//Boston, MA 02111-1307, USA.

package gnu.testlet.javax.swing.JComboBox;

import javax.swing.MutableComboBoxModel;
import javax.swing.AbstractListModel;
import java.util.LinkedList;

/** A mutable model class for JComboBox for use in a model
 * modification test. The model supports unselection.
 * 
 * <p>The test is aware of the fact that this model contains 10 items
 * when created.</p>
 * 
 * @author Robert Schuster
 */
public class TestModel2
    extends AbstractListModel
    implements MutableComboBoxModel
{

    private LinkedList stuff = new LinkedList();

    private String selected;

    public TestModel2()
    {

        for (int i = 0; i < 10; i++)
        {
            stuff.add(String.valueOf(i));
        }

        selected = (String) stuff.get(0);
    }

    public void setSelectedItem(Object o)
    {
        if (o == null)
        {
            selected = null;
            return;
        }

        if (!(o instanceof String))
            return;

        String str = (String) o;

        int index = stuff.indexOf(o);
        if (index != -1)
        {
            selected = (String) stuff.get(index);
        }
    }

    public Object getSelectedItem()
    {
        return selected;
    }

    public Object getElementAt(int index)
    {
        return stuff.get(index);
    }

    public int getSize()
    {
        return stuff.size();
    }

    public void addElement(Object obj)
    {
        stuff.add(obj);
        int index = stuff.size() - 1;
        fireIntervalAdded(obj, index, index);
    }

    public void removeElement(Object obj)
    {
        int index = stuff.indexOf(obj);
        if (index != -1)
        {
            stuff.remove(obj);
            fireIntervalRemoved(obj, index, index);
        }

    }

    public void insertElementAt(Object obj, int index)
    {
        stuff.add(index, obj);
        fireIntervalAdded(obj, index, index);
    }

    public void removeElementAt(int index)
    {
        fireIntervalRemoved(stuff.remove(index), index, index);
    }

}
