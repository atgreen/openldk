import java.io.*;

public class big5
{
    public static void main(String argv[])
    {
            write("�رd������\">�U�ϥΪ��ɮ׺޲z�@�ɳn��C");
    }

    public static void write(String s)
    {
        StringBuffer sb=new StringBuffer();
        for(int i=0;i<s.length();i++)
                sb.append(" "+(int)s.charAt(i));
        sb.append("\n");
        System.out.println(sb.toString());
    }
}
