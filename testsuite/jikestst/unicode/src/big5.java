import java.io.*;

public class big5
{
    public static void main(String argv[])
    {
            write("華康中黑體\">下使用的檔案管理共享軟體。");
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
