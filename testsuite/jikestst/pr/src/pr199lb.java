// $Id: pr199lb.java,v 1.4 1999/06/08 19:01:22 shields Exp $
public class InitTest2 {
	public int j = method();
	public int i = 0;

	public int method() {
		i = 100;
		return 200;
	}

	public static void main(String[] args) {
		InitTest2 test = new InitTest2();
		System.out.println(test.i);
	}
}
