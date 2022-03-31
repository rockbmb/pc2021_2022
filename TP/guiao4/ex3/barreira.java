import java.util.concurrent.Semaphore;
import java.util.concurrent.ThreadLocalRandom;

class Barreira {
    private final int N;
    private int c = 0;
    private Semaphore mut = new Semaphore(1);
    private Semaphore sem = new Semaphore(0);

    Barreira (int N) {
        c = 0;
        this.N = N;
    }

    void await() throws InterruptedException {
        int temp;

        mut.acquire();
        c += 1;
        temp = c;
        mut.release();

        if (temp == N) {
            sem.release(N - 1);
        } else {
            sem.acquire();
        }
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

            bool = false;
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