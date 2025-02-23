// $Id: pr199h.java,v 1.2 1999/02/03 22:18:09 shields Exp $
public class Example1 {
	public static void main(String [] args) {
		int k =0;
		for (final int a;;) {
			k++;
			a=k;
			System.out.println("a="+a);
			if (k>3)
				return;
		}
	}
}

public class Example2 {
	final int k;

	Example2() {
		k = 1;
	}
	Example2(Object whatever) {
		this();
		k = 2;
	}
	static public void main(String[] args) {
		Example2 t = new Example2(null);
		System.out.println("k is " + t.k);
	}
}
