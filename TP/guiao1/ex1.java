class MyThread extends Thread {
    private int state;

    public MyThread(int i) {
        this.state = i;
    }

    public void run() {
        for (int k = 1; k <= this.state; k++) {
            long id = Thread.currentThread().getId();
            System.out.println("Thread id: " + id + "; num: " + k);
        }
        try {
            sleep(500);
        } catch (InterruptedException ignored) {}
    }
}

class MyRunnable implements Runnable {
    public void run() {
        System.out.println("Runnable");
    }
}

class Main {
    public static void main(String[] args) throws InterruptedException {
        final int N = Integer.parseInt(args[0]);
        final int I = Integer.parseInt(args[1]);
        final Thread[] ts = new Thread[N];
        for (int i = 0; i < N; ++i) {
            ts[i] = new MyThread(I);
        }
        for (Thread t : ts) {
            t.start();
        }
        for (Thread t : ts) {
            t.join();
        }
        System.out.println("Main");
    }
}
