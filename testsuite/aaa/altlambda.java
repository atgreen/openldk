/* { dg-output "OK" } */

package aaa;

import java.io.Serializable;
import java.util.*;
import java.util.function.Function;

// Serializable lambdas go through LambdaMetafactory.altMetafactory, as in
// the JDK's own Comparator.comparing (Groovy's MetaClassImpl uses it).
public class altlambda
{
    public static void main(String[] args)
    {
        List<String> words = new ArrayList<>(List.of("ccc", "a", "bb"));
        words.sort(Comparator.comparing(String::length));
        Function<Integer, Integer> twice = (Function<Integer, Integer> & Serializable) x -> x * 2;
        String result = words + " " + twice.apply(21);
        System.out.println("[a, bb, ccc] 42".equals(result) ? "OK" : "FAIL: " + result);
    }
}
