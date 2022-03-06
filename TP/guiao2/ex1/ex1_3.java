/*
Sincronização em objeto no acesso à v.i., com encapsulamento.
*/

class Adder extends Thread {
    Counter c;
    int countTo;

    public Adder(Counter c, int upTo) {
        this.c = c;
        this.countTo = upTo;
    }

    public void run() {
        for (int k = 1; k <= this.countTo; k++) {
            synchronized (c) {
                this.c.increment();
            }
        }
    }
}

class Counter {
    private int count = 0;

    public int getCount() {
        return this.count;
    }

    public void increment() {
        this.count++;
    }
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
        System.out.println(c.getCount());
    }
}