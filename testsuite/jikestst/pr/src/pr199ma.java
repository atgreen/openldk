// $Id: pr199ma.java,v 1.2 1999/02/03 22:18:09 shields Exp $
import pack1.P1;
public class Cmain {
	public static void main(String [] args) {
		P1 p1 = new P1();
		// the access to field 'i' below are
		// ill3gal, as the type of 'p2', the
		// class type 'pack1.P2', is not accessible.
		p1.p2.i = 3;
		System.out.println(p1.p2.i);
	}
}
