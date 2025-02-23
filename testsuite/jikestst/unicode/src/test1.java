/**
 * Following program contains indentifiers in russian,
 * in KOI8-R encoding.
 * It should be compiled with '-encoding koi8-r' flag.
 * 
 * This code test following cases:
 * 
 * 1. Inner class name in koi8
 * 2. instance variable name in koi8
 * 
 * When executed it rints 10 lines of russuan text, nubererd 0-9 and
 * returns 0 upon sucessul execution.
 * 
 * Vadim Zaliva <lord@crocodile.org>
 */
public class test1
{
    private class ���������������
    {
        private int ��������;

        public ���������������(int ��������)
            {
                this.��������=��������;
            }
        
        int ����������()
            {
                write("�������� :"+��������);
                return ��������;
            }
    };
    
    public static void main(String []args)
    {
        new test1();
    }
    
    public test1()
    {
        ��������������� �=new ���������������(23);
        int res=�.����������();
        System.exit(res==23?0:1);
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


