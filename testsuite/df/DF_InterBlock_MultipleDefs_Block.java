// Inter-block test: multiple reaching definitions at merge must prevent propagation.
// If propagation mistakenly picks one literal for both paths, results will be wrong.

public class DF_InterBlock_MultipleDefs_Block {
    static int g(int x) {
        int y;
        if (x > 0) {
            y = 1;
        } else {
            y = 2;
        }
        // Use after merge
        return y;
    }

    public static void main(String[] args) {
        int a = g(5);    // expect 1
        int b = g(-5);   // expect 2
        if (a != 1 || b != 2) {
            System.err.println("FAIL: DF_InterBlock_MultipleDefs_Block expected 1,2 got " + a + "," + b);
            System.exit(1);
        }
        System.out.println("PASS");
    }
}

