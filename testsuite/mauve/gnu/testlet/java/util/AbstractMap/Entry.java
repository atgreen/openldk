// Tags: not-a-test

package gnu.testlet.java.util.AbstractMap;

import java.util.*;

class Entry implements Map.Entry {

    private Object key;
    private Object value;

    public Entry(Object k, Object v) {
            key = k;
            value = v;
    }

    public Object getKey() {
            return key;
    }

    public Object getValue() {
            return value;
    }

    public Object setValue(Object nv) {
            Object ov = value;
            value = nv;
            return ov;
    }

    public boolean equals(Object o) {

            if (!(o instanceof Map.Entry))return false;
            Map.Entry e = (Map.Entry)o;
            if (  e == null ) return false;
            return ( (key == null ? e.getKey()==null : key.equals(e.getKey())) &&
              (value == null ? e.getValue()==null : key.equals(e.getValue())));
    }
}
