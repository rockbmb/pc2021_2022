import java.lang.reflect.Array;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

class BoundedBuffer<T> {
    private T[] buffer;
    private int itake;
    private int iput;
    private int size;
    private int numElems;

    /**
     * Locks e variáveis de condição.
     */
    private final Lock lock = new ReentrantLock();
    private final Condition notFull  = lock.newCondition(); 
    private final Condition notEmpty = lock.newCondition(); 

    public BoundedBuffer(Class<T> clazz, int N) {
        this.buffer   = (T[]) Array.newInstance(clazz, N);
        this.itake    = 0;
        this.iput     = 0;
        this.size     = N;
        this.numElems = 0;
    }

    public T get() throws InterruptedException {
        lock.lock();
        try {
            while (numElems == 0) {
                notEmpty.await();
            }
            T elem = this.buffer[itake];
            itake = (itake + 1) % size;
            numElems -= 1;
            /*
            Na versão anterior:
            
            Ineficiente, usar uma só variável de condição faz com que todas threads tenham que
            acordar, quer a variável de condição tenha o valor que precisam ou não.
            Lição - não misturar threads com motivos diferentes na mesma variável de condição,
            com o uso de notify().

            Nesta versão:
            Como há variáveis de condição diferentes para situações diferentes, já não
            é preciso usar notifyAll, bastando signal() para acordar apenas uma das threads
            à espera de uma variável de condição específica.
            */
            notFull.signal();
            return elem;

        } finally {
        lock.unlock();}
    }

    public void put(T x) throws InterruptedException {
        lock.lock();
        try {
            while (numElems == this.size) {
                notFull.await();
            }
            this.buffer[iput] = x;
            iput = (iput + 1) % size;
            numElems += 1;
            notEmpty.signal();
        } finally {
            lock.unlock();
        }
    }
}

class Consumer<T> extends Thread {
    BoundedBuffer<T> buf;
    int tempo_consumo;

    public Consumer(BoundedBuffer<T> b, int tc) {
        this.buf = b;
        this.tempo_consumo = tc;
    }

    public void run() {
        while (true) {
            T elem;
            try {
                sleep(tempo_consumo);
                elem = this.buf.get();
                System.out.println("Sou consumidor C-" + Thread.currentThread().getName() + ", obtive " + elem);
            } catch (InterruptedException e) {
                System.out.println("Consumidor interrompido!");
                e.printStackTrace();
            }
        }
    }
}

class Producer extends Thread {
    BoundedBuffer<Integer> buf;
    int tempo_producao;

    public Producer(BoundedBuffer<Integer> b, int tp) {
        this.buf = b;
        this.tempo_producao = tp;
    }

    public void run() {
        int low = 0;
        int high = 1000000;

        while (true) {
            
            Integer elem = ThreadLocalRandom.current().nextInt(low, high);
            try {
                sleep(tempo_producao);
                this.buf.put(elem);
                System.out.println("Sou o produtor P-" + Thread.currentThread().getName() + ", inseri " + elem);
            } catch (InterruptedException e) {
                System.out.println("Produtor interrompido!");
                e.printStackTrace();
            }
        }
    }
}

class Main {
    public static void main(String[] args) throws InterruptedException {
        final int NumProdutores = Integer.parseInt(args[0]);
        final int NumConsumidores = Integer.parseInt(args[1]);
        // Em milisegundos.
        final int TempoProdução = Integer.parseInt(args[2]);
        // Em milisegundos.
        final int TempoConsumo = Integer.parseInt(args[3]);
        final int BoundedBufferSize = Integer.parseInt(args[4]);

        BoundedBuffer<Integer> b = new BoundedBuffer<Integer>(Integer.class, BoundedBufferSize);
        final Producer[] ps = new Producer[NumProdutores];
        final Consumer<Integer>[] cs = new Consumer[NumConsumidores];

        for (int i = 0; i < NumProdutores; i++) {
            ps[i] = new Producer(b, TempoProdução);
        }

        for (int i = 0; i < NumConsumidores; i++) {
            cs[i] = new Consumer<Integer>(b, TempoConsumo);
        }

        for (Producer p : ps) {
            p.start();
        }

        for (Consumer<Integer> c : cs) {
            c.start();
        }
    }
}