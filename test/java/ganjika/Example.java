package ganjika;

public class Example {
    final String name;

    public Example(String name) {
        this.name = name;
    }

    public String sayHello() {
        return String.format("Hi, %s!", name);
    }

    public String sayHello(int emotionLevel) {
        String exclamations = "";
        for(int i = 0; i < emotionLevel; i++) {
            exclamations += "!";
        }
        return String.format("Hi, %s%s", name, exclamations);
    }

    public String coerciveSum(Integer a, long b) {
        return String.format("sum is %s", a + b);
    }

    public String coerciveSum(String a, String b) {
        return String.format("sum is %s", Integer.parseInt(a) + Integer.parseInt(b));
    }

    public static int addTwoNumbers(int x, int y) {
        return x + y;
    }
}
