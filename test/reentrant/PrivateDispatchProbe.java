// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
public class PrivateDispatchProbe {
  static int parentCalls, childCalls;
  static class Parent {
    Parent() { initialize(); }
    private void initialize() { parentCalls++; }
    int call() { return value(); }
    private int value() { return 17; }
    private synchronized long combine(int a, long b, int c) {
      if (!Thread.holdsLock(this)) throw new AssertionError("Private synchronized method lost monitor");
      return a * 1000L + b * 10L + c;
    }
    int virtualValue() { return 5; }
  }
  static class Child extends Parent {
    Child() { initialize(); }
    private void initialize() { childCalls++; }
    private int value() { return 29; }
    @Override int virtualValue() { return 7; }
  }
  interface WithPrivate {
    private int hidden() { return 31; }
    default int throughDefault() { return hidden(); }
  }
  static class Implementation implements WithPrivate {
    public int hidden() { return 41; }
  }
  public static void main(String[] args) {
    Child child = new Child();
    if (parentCalls != 1 || childCalls != 1 || child.call() != 17)
      throw new AssertionError("private dispatch: parent=" + parentCalls + " child=" + childCalls + " value=" + child.call());
    Parent parent = child;
    if (parent.value() != 17 || child.value() != 29 || parent.virtualValue() != 7)
      throw new AssertionError("Nestmate private call or ordinary virtual dispatch failed");
    if (parent.combine(3, 4_000_000_000L, 9) != 40_000_003_009L)
      throw new AssertionError("Private synchronized arguments or wide result failed");
    Implementation implementation = new Implementation();
    if (implementation.throughDefault() != 31 || implementation.hidden() != 41)
      throw new AssertionError("Private interface method was overridden");
    Parent missing = null;
    try { missing.value(); throw new AssertionError("Private null receiver accepted"); }
    catch (NullPointerException expected) { }
    System.out.println("Private class, nestmate, and interface calls preserve declaring methods");
  }
}
