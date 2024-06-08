
public class Jelly {
    public static void main(String[] args){
    int j_main = 0;
      int a_main = 6;
      int b_main = 3;
      String c_main = "test";
      if (a_main > b_main) {
    j_main = j_main + 1;
    
    };
      System.out.println(j_main);
      System.out.println(c_main);
     
    }
    static int m1 (int a_m1, int b_m1) {
    while (a_m1 != 0){
    if (a_m1 < b_m1) {
    b_m1 = b_m1 - a_m1;
    
    } else{
    a_m1 = a_m1 - b_m1;
    
    };
    
    };
     return b_m1;
    
    }
     static int gcd (int a_gcd, int b_gcd) {
    int i_gcd = 0;
     while (i_gcd < 5){
    i_gcd = i_gcd + 5;
     i_gcd = i_gcd + 1;
    
    };
     int c_gcd = 0;
     while (c_gcd < 5){
    c_gcd = a_gcd + 5;
     c_gcd = c_gcd + 1;
    
    };
     return c_gcd;
    
    }
    
    }