import java.util.concurrent.ThreadLocalRandom;

class Barreira {
    private final int N;
    private boolean waiting_for_previous_group;
    private int c = 0;

    Barreira (int N) {
        this.N = N;
        waiting_for_previous_group = false;
    }

    public synchronized void await() throws InterruptedException {
        while (waiting_for_previous_group) {
                this.wait();
        }

        c += 1;

        if (c == N) {
            waiting_for_previous_group = true;
            this.notifyAll();
        } else {
            while (!waiting_for_previous_group) {
                this.wait();
            }
        }

        c -= 1;

        if (c == 0) {
            waiting_for_previous_group = false;
            this.notifyAll();
        }
     }

}

class Runner extends Thread {
    Barreira barrier;

    public Runner(Barreira b) {
        this.barrier = b;
    }

    public void run() {
        int low = 1000;
        int high = 5000;
        boolean bool = true;

        while (bool) {
            Integer elem = ThreadLocalRandom.current().nextInt(low, high);
            try {
                System.out.println("Runner R-" + Thread.currentThread().getName() + " start");
                sleep(elem);
                System.out.println("Runner R-" + Thread.currentThread().getName() + " worked for " + elem + "millis, await()");
                this.barrier.await();
                System.out.println("Runner R-" + Thread.currentThread().getName() + ", await() done");
            } catch (InterruptedException e) {
                System.out.println("Runner interrompido!");
                e.printStackTrace();
            }

            //bool = false;
        }
    }
}

class Main {
    public static void main(String[] args) throws InterruptedException {
        final int NumRunners = Integer.parseInt(args[0]);

        Barreira b = new Barreira(NumRunners);

        final Runner[] rs = new Runner[NumRunners];

        for (int i = 0; i < NumRunners; i++) {
            rs[i] = new Runner(b);
        }

        for (Runner r : rs) {
            r.start();
        }

        for (Runner r : rs) {
            r.join();
        }
    }
}