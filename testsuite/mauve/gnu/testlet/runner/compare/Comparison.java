// Copyright (c) 2008 Fabien DUMINY (fduminy@jnode.org)

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.runner.compare;

import gnu.testlet.runner.Result;

import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;

/**
 * Abstract class for the result of the comparison of 2 {@link Result}s
 * 
 * @author fabien
 *
 */
public abstract class Comparison implements Comparable {
    private final String name;
    private final Set children = new TreeSet();
    
    Comparison(Result result) {
        this.name = result.getName();
    }
    
    public final String getName() {
        return name;
    }
    
    public final Comparison get(String name) {
        Comparison result = null;
        
        for (Iterator it = children.iterator(); it.hasNext(); ) {
            Comparison r = (Comparison) it.next(); 
            if (r.getName().equals(name)) {
                result = r;
                break;
            }
        }
        
        return result;
    }
    
    public abstract void accept(ComparisonVisitor visitor);
    
    public int getProgression() {
        int progression = 0;
        
        for (Iterator it = children.iterator(); it.hasNext(); ) {
            Comparison r = (Comparison) it.next(); 
            progression += r.getProgression();
        }
        
        return progression;
    }
    
    public final void add(Comparison child) {
        children.add(child);
    }
    
    /**
     * @param comparison an instance of Comparison 
     */
    public final int compareTo(Object comparison) {
        Comparison c = (Comparison) comparison;
        
        // regressions have negative progression
        // we sort from bigger regression to bigger progression
        int result = getProgression() - c.getProgression();
        
        if (result == 0) {
            result = getName().compareTo(c.getName());
        }
        
        return result;
    }

    protected final void acceptChildren(ComparisonVisitor visitor) {
        for (Iterator it = children.iterator(); it.hasNext(); ) {
            Comparison cmp = (Comparison) it.next(); 
            cmp.accept(visitor);
        }
    }    
}
