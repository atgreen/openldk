public class EvaluationOrder
 {
     private static int first (int x, int y)
     {
         return x;
     }

     public static void main (String[] args)
     {
         int l = args.length;

         /* This should print:
 5
 5
 6
         */
         System.out.println (l);
         System.out.println (first (l, ++l));
         System.out.println (l);
     }
 }

