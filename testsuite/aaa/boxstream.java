/* { dg-output "OK" } */

package aaa;

import java.util.*;
import java.util.stream.LongStream;

// A method reference whose functional-interface method takes a primitive but
// whose target method takes a reference.  The lambda adapter must box the
// primitive argument, exactly as javac's generated adapter does.
//
//   LongStream.forEach(list::add)         // accept(long) -> List.add(Object)
//
// OpenLDK named the adapted argument through without boxing, so the list held
// bare longs, not Longs.  A later Long::longValue method reference -- whose
// receiver comes straight from the collection, with no intervening virtual
// call to auto-box it -- then found no method on the primitive.  This is the
// shape of Lucene's ScorerUtil.costWithMinShouldMatch, which broke every
// multi-clause BooleanQuery.
public class boxstream
{
    public static void main(String[] args)
    {
        List<Long> list = new ArrayList<>();
        LongStream.of(3L, 1L, 2L).forEach(list::add);   // box long -> Long here
        boolean boxed = list.get(0) instanceof Long;
        long sum = list.stream().mapToLong(Long::longValue).sum();   // longValue() per element
        String result = boxed + " " + sum;
        System.out.println("true 6".equals(result) ? "OK" : "FAIL: " + result);
    }
}
