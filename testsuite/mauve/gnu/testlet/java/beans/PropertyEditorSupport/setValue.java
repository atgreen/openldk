//Tags: JDK1.4

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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyEditorSupport;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/** This test checks whether PropertyEditorSupport.setValue() and its event notification
 * works as specified.  
 * 
 * These tests have been written as a reaction on classpath's bug #10799
 * (http://savannah.gnu.org/bugs/index.php?func=detailitem&item_id=10799).
 * 
 * @author Robert Schuster
 */
public class setValue implements Testlet
{

    private boolean nonNullPropertyChanged;

    private boolean nullPropertyChanged;

    // declaration is needed for anonymous class access without using 'final'
    private PropertyEditorSupport pes;

    public void test(final TestHarness harness)
    {
        // for 1.4 compatibility it is needed to subclass PropertyEditorSupport because the constructors are
        // 'protected' (they are 'public' in 1.5)
        pes = new PropertyEditorSupport()
        {};

        final Object newValue = "new value";

        pes.addPropertyChangeListener(new PropertyChangeListener()
        {
            public void propertyChange(PropertyChangeEvent event)
            {
                nonNullPropertyChanged = true;

                // the PropertyEditorSupport instance should be the event source 
                // (according to documentation of its zero argument constructor)
                harness.check(
                    event.getSource(),
                    pes,
                    "pes1-event-event source");

                // the property name for an PropertyEditorSupport object is always null
                harness.check(
                    event.getPropertyName(),
                    null,
                    "pes1-event-property name");

                // according to documentation of PropertyChangeEvent the old and new value should be
                // null if the property name is null.
                harness.check(
                    event.getOldValue(),
                    null,
                    "pes1-event-old value");
                harness.check(
                    event.getNewValue(),
                    null,
                    "pes1-event-new value");

                // at this point the PropertyEditorSupport instance should have been updated to the new value already
                harness.check(pes.getValue(), newValue, "pes1-new value");
            }

        });

        // this method should trigger the calling of anonymous PropertyChangeListener above
        pes.setValue(newValue);

        harness.check(
            nonNullPropertyChanged,
            "pes1-PropertyChangeListener call");

        // creates another PropertyEditorSupport instance for a slightly different test
        pes = new PropertyEditorSupport()
        {};

        pes.addPropertyChangeListener(new PropertyChangeListener()
        {

            public void propertyChange(PropertyChangeEvent event)
            {
                nullPropertyChanged = true;

                // the same as in pes1 tests
                harness.check(
                    event.getSource(),
                    pes,
                    "pes2-event-event source");
                    
                harness.check(
                    event.getPropertyName(),
                    null,
                    "pes2-event-property name");
                    
                harness.check(
                    event.getOldValue(),
                    null,
                    "pes2-event-old value");
                    
                harness.check(
                    event.getNewValue(),
                    null,
                    "pes2-event-new value");

                // the new value should be null, too
                harness.check(pes.getValue(), null, "pes2-new value");

            }

        });

        // changing the PropertyEditorSupport value (which is null at this moment) to null should
        // cause the PropertyChangeListener to get notified
        pes.setValue(null);

        harness.check(nullPropertyChanged, "pes2-PropertyChangeListener call");
    }

}
