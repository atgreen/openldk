// Minimal reproducer: force clojure.lang.RT.<clinit> to run under OpenLDK.
// Class.forName(..., initialize=true) triggers the static initializer that
// fails to JIT-compile in OpenLDK on JDK 25.
public class RTInit {
    public static void main(String[] args) throws Exception {
        Class.forName("clojure.lang.RT");
        System.out.println("clojure.lang.RT initialized OK");
    }
}
