/**
 * Following program contains indentifiers in russian,
 * in KOI8-R encoding.
 * It should be compiled with '-encoding koi8-r' flag.
 * 
 * This code test following cases:
 * 
 * 1. Local variable name in koi8
 * 2. Method name in koi8
 * 3. Formal parameter name in koi8
 * 4. String constant in koi8
 * 5. Match between identified declared in koi8 and referenced as unicode escape.
 * 
 * When executed it rints 10 lines of russuan text, nubererd 0-9 and
 * returns 0 upon sucessul execution.
 * 
 * Vadim Zaliva <lord@crocodile.org>
 */
public class test0
{
    public static void main(String []args)
    {
        int сумма=0;
        for(int и=0;и<10;и++)
        {
            сумма++;
            напечататьInt(и);
        }
        System.exit(сумма==10?0:1);
    }
    
    static void напечататьInt(int \u041F)
    {
        write(П+" >Строка<");
    }

    public static void write(String s)
    {
        StringBuffer sb=new StringBuffer();
        for(int i=0;i<s.length();i++)
                sb.append(" "+(int)s.charAt(i));
        sb.append("\n");
        System.out.println(sb.toString());
    }

};


