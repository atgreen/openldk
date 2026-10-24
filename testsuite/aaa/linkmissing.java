/* { dg-output "OK" } */

package aaa;

import java.io.File;
import java.util.*;
import java.util.function.*;

// A JVM resolves a class an instruction names only when that instruction
// runs: code behind a ClassUtils.isPresent check may name a class that
// isn't there (Spring Data's SpringDataWebConfiguration and Jackson 2).
// Each instruction kind gets its own method, as the JIT compiles them so.
public class linkmissing
{
    static boolean present = false;

    static Object newInstance() { return present ? new linkmissingGone() : "ok"; }
    static Object checkcast(Object o) { return present ? (linkmissingGone) o : "ok"; }
    static Object instanceOf(Object o) { return present ? (Object) (o instanceof linkmissingGone) : "ok"; }
    static Object newArray() { return present ? new linkmissingGone[1] : "ok"; }
    static Object getStatic() { return present ? linkmissingGone.FIELD : "ok"; }
    static Object putStatic() { if (present) linkmissingGone.FIELD = "x"; return "ok"; }
    static Object invokeStatic() { return present ? linkmissingGone.make() : "ok"; }
    static Object invokeVirtual(Object o) { return present ? ((linkmissingGone) o).name() : "ok"; }
    static Object catchType()
    {
        try {
            if (present)
                throw new IllegalStateException();
            return "ok";
        } catch (linkmissingGoneException e) {
            return "caught";
        }
    }
    static Object lambda()
    {
        if (!present)
            return "ok";
        Supplier<linkmissingGone> s = linkmissingGone::new;
        return s.get();
    }

    // A present class whose members' types are missing (Micrometer's
    // AbstractTimer and LatencyUtils).
    linkmissingGone gone;
    static linkmissingGone takeGone(linkmissingGone g) { return g; }
    Object fieldOfMissingType() { if (present) gone = (linkmissingGone) null; return present ? gone : "ok"; }
    static Object paramOfMissingType() { return present ? takeGone(null) : "ok"; }
    static Object constructorOfMissingType() { return present ? new linkmissingHolder(null) : "ok"; }
    // The lambda's body, a method of this class, returns a missing type.
    static Object lambdaOfMissingType()
    {
        if (!present)
            return "ok";
        Function<Object, linkmissingGone> f = o -> (linkmissingGone) o;
        return f.apply(null);
    }

    // A lambda whose interface is missing (LatencyUtils' PauseDetectorListener).
    static Object lambdaOfMissingInterface()
    {
        if (!present)
            return "ok";
        linkmissingGoneListener l = (a, b) -> { };
        return l;
    }

    interface Probe { Object run() throws Throwable; }

    public static void main(String[] args) throws Exception
    {
        for (String name : List.of("linkmissingGone.class", "linkmissingGoneException.class",
                                   "linkmissingGoneListener.class"))
            new File(linkmissing.class.getResource(name).toURI()).delete();
        Map<String, Probe> probes = new LinkedHashMap<>();
        probes.put("new", linkmissing::newInstance);
        probes.put("checkcast", () -> checkcast(null));
        probes.put("instanceof", () -> instanceOf(null));
        probes.put("anewarray", linkmissing::newArray);
        probes.put("getstatic", linkmissing::getStatic);
        probes.put("putstatic", linkmissing::putStatic);
        probes.put("invokestatic", linkmissing::invokeStatic);
        probes.put("invokevirtual", () -> invokeVirtual(null));
        probes.put("catch", linkmissing::catchType);
        probes.put("lambda", linkmissing::lambda);
        probes.put("field", () -> new linkmissing().fieldOfMissingType());
        probes.put("param", linkmissing::paramOfMissingType);
        probes.put("constructor", linkmissing::constructorOfMissingType);
        probes.put("lambda-body", linkmissing::lambdaOfMissingType);
        probes.put("lambda-interface", linkmissing::lambdaOfMissingInterface);

        List<String> got = new ArrayList<>();
        for (Map.Entry<String, Probe> p : probes.entrySet()) {
            try {
                Object r = p.getValue().run();
                if (!"ok".equals(r))
                    got.add(p.getKey() + "=" + r);
            } catch (Throwable t) {
                got.add(p.getKey() + " threw " + t);
            }
        }
        present = true;
        for (String name : List.of("new", "anewarray", "getstatic", "invokestatic", "lambda", "lambda-interface")) {
            try {
                probes.get(name).run();
                got.add(name + " ran");
            } catch (NoClassDefFoundError e) {
            } catch (Throwable t) {
                got.add(name + " threw " + t);
            }
        }
        System.out.println(got.isEmpty() ? "OK" : got.toString());
    }
}

class linkmissingGone
{
    static Object FIELD = "f";
    static linkmissingGone make() { return new linkmissingGone(); }
    String name() { return "gone"; }
}

class linkmissingGoneException extends RuntimeException
{
}

interface linkmissingGoneListener
{
    void handle(long a, long b);
}

class linkmissingHolder
{
    linkmissingGone gone;
    linkmissingHolder(linkmissingGone gone) { this.gone = gone; }
}
