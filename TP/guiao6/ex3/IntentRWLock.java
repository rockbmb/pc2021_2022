import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import java.util.concurrent.locks.Condition;

/**
 * RWLock com declaração de intenção, e turnos para evitar starvation.
 */
public class IntentRWLock {
    private int readers;
    private int writers;
    private int requesting_read;
    private int requesting_write;

    /**
     * To avoid starvation.
     * true -> it's the writers' turn
     * false -> it's the readers' turn
     */
    private boolean is_writer_turn = false;

    /**
     * Locks e variáveis de condição.
     */
    private final Lock lock = new ReentrantLock();

    private final Condition readersCanRead  = lock.newCondition(); 
    private final Condition writersCanWrite = lock.newCondition(); 

    public void readLock() throws InterruptedException {
        lock.lock();

        try {
            requesting_read += 1;
            while(writers > 0 || (is_writer_turn && requesting_write > 0)) {
                readersCanRead.await();
            }
            requesting_read -= 1;
            readers += 1;
            /**
             * Só é necessario sinalizar outras threads à espera de fazer leituras
             * se existirem.
             */
            if (requesting_read > 0) {
                /**
                 * Como pode haver várias leituras ao mesmo tempo, aqui
                 * poder-se-ia fazer signalAll em vez de signal.
                 * No entanto, isso diminuiria a concorrência.
                 */
                readersCanRead.signal();
            } else {
                is_writer_turn = true;
            }
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
            requesting_write += 1;
            while (writers > 0 || readers > 0) {
                writersCanWrite.await();
            }
            requesting_write -= 1;
            writers += 1;
        } finally {
            lock.unlock();
        }
    }

    public void writeUnlock() {
        lock.lock();

        try {
            writers -= 1;
            is_writer_turn = false;
            if (requesting_read > 0) {
                readersCanRead.signal();
            }
        } finally {
            lock.unlock();
        }
    }

}