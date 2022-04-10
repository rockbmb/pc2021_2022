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
            if (requesting_read > 0) {
                /**
                 * Como pode haver várias leituras ao mesmo tempo, aqui
                 * poder-se-ia fazer signalAll em vez de signal.
                 * No entanto, isso diminuiria a cocorrência.
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

/**
 * Banco com número de contas variável, com locks reentrantes.
 */

class NotEnoughFunds extends Exception {}
class InvalidAccount extends Exception {}

class Bank {
    class Account {
        private int balance;
        final ReentrantLock accLock = new ReentrantLock();

        public Account() {
            this.balance = 0;
        }
        public int balance() { return balance; }
        public void deposit(int val) {balance += val;}
        public void withdraw(int val) throws NotEnoughFunds {
            if (balance < val) {
                throw new NotEnoughFunds();
            }
            balance -= val;
        }
    }

    HashMap<Integer, Account> accounts;

    /**
     * Default initial balance for newly created accounts.
     */
    public static final int saldoInicial = 1000;

    /**
     * This variable records the total amount of funds that belonged to deleted accounts.
     */
    public static int closedAccountFunds = 0;
    /**
     * Keeps track of how many accounts were added since the bank's creation, exluding those
     * present when the Bank(int numContas) constructor is used.
     */
    public static int createdAccounts = 0;


    public int lastId;
    private final RWLock rwl = new RWLock();

    public Bank() {
        this.accounts = new HashMap<>();
        this.lastId = 0;
    }

    public Bank(int numContas) throws InterruptedException {
        this.accounts = new HashMap<>();
        this.lastId = 0;
        for (int i = 0; i < numContas; i++) {
            createAccount(Bank.saldoInicial);
        }

        /**
         * Important - otherwise Observer gets wrong results. Accounts present during
         * Bank creation don't count towards this variable.
        */
        Bank.createdAccounts = 0;
    }

    int createAccount(int balance) throws InterruptedException {
        Account c = new Account();
        c.deposit(balance);
        rwl.writeLock();
        try {
            lastId += 1;
            accounts.put(lastId, c);
        /**
         * O unlock do banco *tem* de estar depois da variável de classe
         * Bank.createdAccount ser modificada, porque senão haverá race-condition
         * nesta instrução.
         */
            Bank.createdAccounts++;
            return lastId;
        } finally {
            rwl.writeUnlock();
        }
    }

    public Account getAccount(int id) throws InvalidAccount {
        Account acc = this.accounts.get(id);
        if (acc == null) throw new InvalidAccount();
        return acc;
    }

    int closeAccount(int id) throws InvalidAccount, InterruptedException {
        rwl.writeLock();

        Account acc;

        try {
            acc = getAccount(id);
        } catch (InvalidAccount e) {
            rwl.writeUnlock();
            throw e;
        }

        int res;
        try {
            acc.accLock.lock();
            res = acc.balance();
            Bank.closedAccountFunds += res;
            accounts.remove(id);
        
        /**
         * O unlock do banco *tem* de estar depois da variável de classe
         * Bank.closedAccountFunds ser modificada, porque senão haverá race-condition
         * nesta instrução.
         */

        } finally {
            rwl.writeUnlock();
        }

        try {} finally {
            acc.accLock.unlock();
        }

        return res;
    }

    public void deposit(int id, int val) throws InvalidAccount, InterruptedException {
        rwl.readLock();
        Account acc;
        try{
            acc = this.getAccount(id);
        } catch (InvalidAccount e) {
            rwl.readUnlock();
            throw e;
        }
        try {
            acc.accLock.lock();
        } finally {
            rwl.readUnlock();
        }
        try {
            acc.deposit(val);
        } finally {
            acc.accLock.unlock();
        }
    }

    public void withdraw(int id, int val) throws InvalidAccount, NotEnoughFunds, InterruptedException {
        rwl.readLock();
        try {
            Account acc = this.getAccount(id);
            try {
                acc.accLock.lock();
                acc.withdraw(val);
            } finally {
                acc.accLock.unlock();
            }
        } finally {
            rwl.readUnlock();
        }
    }

    public int totalBalance(int accounts[]) throws InvalidAccount, InterruptedException {
        int res = 0;
        int accs[] = accounts.clone();

        // Necessário para garantir ordem dos locks.
        Arrays.sort(accs);

        rwl.readLock();
        List<Account> accsL = new ArrayList<Account>();

        try {
            for (int ix : accs) {
                Account a = this.getAccount(ix);
                accsL.add(a);
            }
            // Poderia esta fora do block try.
            for (Account ac : accsL) {
                ac.accLock.lock();
            }
        } finally {
            rwl.readUnlock();
        }

        try {
            for (int acc : accs) {
                Account a = this.getAccount(acc);
                a.accLock.lock();
            }
        } finally {
            rwl.readUnlock();
        }

        for (Account acc : accsL) {
            try {
                res += acc.balance();
            } finally {
                acc.accLock.unlock();
            }
        }

        return res;
    }

    public int totalBalance() throws InvalidAccount, InterruptedException {

        /*
        ArrayList<Integer> intKeys = new ArrayList<Integer>(accounts.keySet());
        return this.totalBalance(intKeys.stream().mapToInt(i -> i).toArray());
        */

        rwl.readLock();

        List<Entry<Integer, Account>> idsAccs = new ArrayList<Entry<Integer, Account>>(this.accounts.entrySet());
        idsAccs.sort(Entry.comparingByKey());

        int res = 0;
        for (Entry<Integer, Account> ixAcc : idsAccs) {
                // Não lança exceção pois as chaves vêm diretamente do keySet.
                ixAcc.getValue().accLock.lock();
        }

        for (Entry<Integer, Account> ixAcc : idsAccs) {
            // Não lança exceção pois as chaves vêm diretamente do keySet.
            res += ixAcc.getValue().balance();
            ixAcc.getValue().accLock.unlock();
        }
        rwl.readUnlock();

        return res;
    }

    /**
     * Nesta versão da classe Bank, o método só necessita de um readLock ao Banco,
     * já que é possível ocorrer mais que uma transferência em simultâneo.
     * @throws InterruptedException
     */
    public void transfer(int from, int to, int amount) throws InvalidAccount, NotEnoughFunds, InterruptedException {
        rwl.readLock();
        Account fromAcc;
        Account toAcc;
        
        try {
            fromAcc = getAccount(from);
            toAcc = getAccount(to);
        } catch (InvalidAccount e) {
            rwl.readUnlock();
            throw e;
        }

        Account lowerAcc, higherAcc;

        if (from < to) {
            lowerAcc = fromAcc;
            higherAcc = toAcc;
        } else {
            lowerAcc = toAcc;
            higherAcc = fromAcc;
        }

        /**
         * Let's assume locks dont throw exceptions!
         */
        lowerAcc.accLock.lock();
        higherAcc.accLock.lock();
        rwl.readUnlock();

        try {
            fromAcc.withdraw(amount);
            toAcc.deposit(amount);
        } finally {
            lowerAcc.accLock.unlock();
            higherAcc.accLock.unlock();
        }
    }
}

class Transferer extends Thread {
    int iterations;
    Bank b;

    public Transferer(Bank b, int its) {
        this.b = b;
        this.iterations = its;
    }

    public void run() {
        List<Integer> keysList = new ArrayList<Integer>(b.accounts.keySet());
        int low = 0;
        int high = keysList.size() - 1;
        for (int k = 0; k < this.iterations; k++) {
            int from = ThreadLocalRandom.current().nextInt(low, high);
            int to = ThreadLocalRandom.current().nextInt(low, high);
            try {
                this.b.transfer(keysList.get(from), keysList.get(to), 1);
            } catch (InvalidAccount e) {
                System.out.println("Account has been deleted just before the transfer!");
            } catch (NotEnoughFunds e) {
                System.out.println("Account with id " + from + " has insufficient funds!");
            } catch (InterruptedException e) {
                System.out.println("TRANSFERER INTERRUPTED");
            }
        }
    }
}

class Creator extends Thread {
    int iterations;
    Bank b;

    public Creator(Bank b, int its) {
        this.b = b;
        this.iterations = its;
    }

    public void run() {
        for (int k = 0; k < iterations; k++) {
            try {
                b.createAccount(Bank.saldoInicial);
            } catch (InterruptedException e) {
                System.out.println("CREATOR INTERRUPTED");
            }
        }
    }
}

class Remover extends Thread {
    int iterations;
    Bank b;

    public Remover(Bank b, int its) {
        this.b = b;
        this.iterations = its;
    }

    public void run() {
        List<Integer> keysList = new ArrayList<Integer>(b.accounts.keySet());
        int low = 0;
        int high = keysList.size() - 1;
        for (int k = 0; k < this.iterations; k++) {
            int ix = ThreadLocalRandom.current().nextInt(low, high);
            try {
                this.b.closeAccount(ix);
            } catch (InvalidAccount e) {
                System.out.println("Account has already been deleted!");
            } catch (InterruptedException e) {
                System.out.println("REMOVER INTERRUPTED");
            }
        }
    }
}

class Observer extends Thread {
    final int iterations;
    final int numContas;
    final Bank b;

    public Observer(Bank b, int its, int numCs) {
        this.b = b;
        this.iterations = its;
        this.numContas = numCs;
    }

    public void run() {
        try {
            for (int i = 0; i < iterations; i++) {
                int totalBalance = b.totalBalance() + Bank.closedAccountFunds;
                /**
                 * Aqui sabe-se que a classe Creator utiliza sempre Bank.saldoInicial quando cria uma conta,
                 * e que Bank.createdAccounts representa o número de contas criado *após* a inicialização do banco.
                 */
                int expectedBalance = numContas * Bank.saldoInicial + Bank.createdAccounts * Bank.saldoInicial;
                
                if (totalBalance != expectedBalance) {
                    System.out.println("expected: " + expectedBalance + "; actual: " + totalBalance);
                }
            }
        } catch (Exception e) {}
    }
}

class Main {
    public static void main(String[] args) throws InterruptedException, InvalidAccount {
        final int NumContas = Integer.parseInt(args[0]);
        final int NumTransferers = Integer.parseInt(args[1]);
        final int transfers = Integer.parseInt(args[2]);
        final int observerIterations = Integer.parseInt(args[3]);
        final int accountDeletions = Integer.parseInt(args[4]);
        final int accountCreations = Integer.parseInt(args[5]);

        Bank b = new Bank(NumContas);
        final Transferer[] ts = new Transferer[NumTransferers];
        final Observer obs = new Observer(b, observerIterations, NumContas);
        final Remover rem = new Remover(b, accountDeletions);
        final Creator creat = new Creator(b, accountCreations);

        for (int i = 0; i < NumTransferers; i++) {
            ts[i] = new Transferer(b, transfers);
        }

        for (Transferer t : ts) {
            t.start();
        }
        obs.start();
        rem.start();
        creat.start();

        for (Transferer t : ts) {
            t.join();
        }
        obs.join();
        rem.join();
        creat.join();

        /*for (Integer k : b.accounts.keySet()) {
            System.out.println("Conta " + k + " tem saldo: " + b.getAccount(k).balance());
        }*/

        System.out.println("O banco tem " + b.accounts.size() + " contas no final.");
    }
}