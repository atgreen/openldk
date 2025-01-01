// Tags: not-a-test

package gnu.testlet.java.io.ObjectStreamClass;

abstract class B extends A
{
  private B (int[] ar) {}
  public B () {}
  public static void foo () {}
  public abstract void absfoo ();

  private static String s;
  public int[] a;

  static
  {
    s = "hello";
  }
}
