import java.util.concurrent.Semaphore;
import java.util.concurrent.ThreadLocalRandom;

class Barreira {
    private final int N;
    private int first_gate;

    private final Semaphore mut = new Semaphore(1);
    private final Semaphore sem1 = new Semaphore(0);
    private final Semaphore sem2 = new Semaphore(1);

    Barreira (int N) {
        this.N = N;
        this.first_gate = 0;
    }

    void await() throws InterruptedException {
        // Think in terms of turnstiles.
        mut.acquire();
        first_gate += 1;
        if (first_gate == N) {
            sem2.acquire();
            sem1.release();
        }
        mut.release();

        sem1.acquire();
        sem1.release();

        mut.acquire();
            first_gate -= 1;
            if (first_gate == 0) {
                sem1.acquire();
                sem2.release();
            }
        mut.release();

        sem2.acquire();
        sem2.release();
        }
}

class Runner extends Thread {
    Barreira barrier;

    public Runner(Barreira b) {
        this.barrier = b;
    }

    public void run() {
        int low = 100;
        int high = 1000;

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