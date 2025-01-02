/* { dg-output "^Hello!" } */

package aaa;

import java.util.Date;

public class ni {

    public static int x = 0;

		public static void main(String[] args) {

      try {
         // date object
         Date d = new Date();
         Class cls = d.getClass();
         System.out.println("Time = " + d.toString());

        try {
            x = 10 / x ;
        } catch (Exception e) {
            System.out.println ("OK!");
        }
        System.out.println ("Done.");


         /* creates a new instance of the class represented by this
            Class object cls */
         Object obj = cls.newInstance();
         System.out.println("Time = " + obj);
      } catch(InstantiationException e) {
         System.out.println(e.toString());
      } catch(IllegalAccessException e) {
         System.out.println(e.toString());
      }
   }
}
