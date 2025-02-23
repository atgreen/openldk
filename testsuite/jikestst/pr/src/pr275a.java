// $Id: pr275a.java,v 1.2 1999/02/03 22:18:10 shields Exp $
interface A { void m(Object a); }
interface B { void m(String a); }
interface C extends A, B { }

public class Main implements C {
	public static void main(String argv[]) {
		C C = new Main();
		((B) C).m("A String");
		C.m("A String");
	}
	public void m(Object o) { }
	public void m(String o) { }
}
