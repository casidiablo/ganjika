package ganjika;

import java.util.List;

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

    public String goodBye(String[] list) {
        String names = "";
        for (String name : list) {
            names += " " + name;
        }
        return String.format("Bye%s", names);
    }

    public int varArgsInt(int... args) {
        int sum = 0;
        for (int x : args) {
            sum += x;
        }
        return sum;
    }

    public String varArgsChar(char... args) {
        String sum = "";
        for (char x : args) {
            sum += x;
        }
        return sum;
    }

    public long varArgsLong(long... args) {
        long sum = 0;
        for (long x : args) {
            sum += x;
        }
        return sum;
    }

    public short varArgsShort(short... args) {
        short sum = 0;
        for (short x : args) {
            sum += x;
        }
        return sum;
    }

    public float varArgsFloat(float... args) {
        float sum = 0;
        for (float x : args) {
            sum += x;
        }
        return sum;
    }

    public double varArgsDouble(double... args) {
        double sum = 0;
        for (double x : args) {
            sum += x;
        }
        return sum;
    }

    public boolean varArgsBoolean(boolean... args) {
        boolean sum = false;
        for (boolean x : args) {
            sum = sum || x;
        }
        return sum;
    }

    public byte varArgsByte(byte... args) {
        byte sum = 0;
        for (byte x : args) {
            sum |= x;
        }
        return sum;
    }

    public long square(Integer i) {
        return i * i;
    }
}
