import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import java.util.concurrent.locks.Condition;

public class RWLock {
    private int readers;
    private boolean writers = false;

    /**
     * Locks e variáveis de condição.
     */
    private final Lock lock = new ReentrantLock();

    private final Condition readersCanRead  = lock.newCondition(); 
    private final Condition writersCanWrite = lock.newCondition(); 

    public void readLock() throws InterruptedException {
        lock.lock();

        try {
            while(writers) {
                readersCanRead.await();
            }
            readers += 1;
            readersCanRead.signal();
        } finally {
            lock.unlock();
        }
    }

    public void readUnlock() {
        lock.lock();
        try {
            readers -= 1;
            if (readers == 0) {
                writersCanWrite.signal();
            }
        } finally {
            lock.unlock();
        }
    }

    public void writeLock() throws InterruptedException {
        lock.lock();

        try {
            while (writers || readers > 0) {
                writersCanWrite.await();
            }
            writers = true;
        } finally {
            lock.unlock();
        }
    }

    public void writeUnlock() {
        lock.lock();

        try {
            writers = false;
            readersCanRead.signal();
            writersCanWrite.signal();
        } finally {
            lock.unlock();
        }
    }

}