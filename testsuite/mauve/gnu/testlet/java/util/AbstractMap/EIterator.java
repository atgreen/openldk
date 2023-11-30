// Tags: not-a-test

package gnu.testlet.java.util.AbstractMap;

import java.util.*;

class EIterator implements Iterator {
        int pos=0;
        int status=0;

	private AcuniaAbstractMapTest map;

        public EIterator(AcuniaAbstractMapTest map) {
		this.map = map;
	}

        public  boolean hasNext() {
                return  pos < map.size();
        }

        public Object next() {
                status = 1;
                if (pos>= map.size()) throw new NoSuchElementException("no elements left");
                pos++;
                return new Entry(map.keys.get(pos-1), map.values.get(pos-1));                   
        }

        public void remove() {
                if (status != 1 ) throw new IllegalStateException("do a next() operation before remove()");
                map.deleteInAM(map.keys.get(pos-1));
                pos--;
                status=-1;
        }
}

