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

class EvolutionType {
    public static final EvolutionType REGRESSION = new EvolutionType("REGRESSION", 0);
    public static final EvolutionType PROGRESSION = new EvolutionType("PROGRESSION", 1);
    public static final EvolutionType STAGNATION = new EvolutionType("STAGNATION", 2);
    
    private static final EvolutionType[] VALUES = new EvolutionType[]{REGRESSION, PROGRESSION, STAGNATION};
    
    public static final EvolutionType[] values() {
        return VALUES;
    }
    
    private final int ordinal;
    private final String name;
    
    private EvolutionType(final String name, final int ordinal) {
        this.name = name;
        this.ordinal = ordinal;
    }
    
    public final int ordinal() {
        return ordinal;
    }
    
    public String toString() {
        return name;
    }
}
