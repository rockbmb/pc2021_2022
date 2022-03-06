class Adder extends Thread {
    Counter c;
    int countTo;

    public Adder(Counter c, int upTo) {
        this.c = c;
        this.countTo = upTo;
    }

    public void run() {
        for (int k = 1; k <= this.countTo; k++) {
            c.count++;
        }
    }
}

class Counter {
    public int count = 0;
}

class Main {
    public static void main(String[] args) throws InterruptedException {
        final int N = Integer.parseInt(args[0]);
        final int I = Integer.parseInt(args[1]);
        final Thread[] ts = new Thread[N];

        Counter c = new Counter();

        for (int i = 0; i < N; ++i) {
            ts[i] = new Adder(c, I);
        }
        for (Thread t : ts) {
            t.start();
        }
        for (Thread t : ts) {
            t.join();
        }
        System.out.println("Main");
        System.out.println(c.count);
    }
}
