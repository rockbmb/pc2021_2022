/**
 * Banco com sincronização a nível de contas individuais, utilizando synchronized.
 */

import java.util.concurrent.ThreadLocalRandom;

class NotEnoughFunds extends Exception {}
class InvalidAccount extends Exception {}

class Bank {
    class Account {
        private int balance;

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

    public Account[] accounts;

    public Bank(int accNumber) {
        this.accounts = new Account[accNumber];
        for(int i = 0; i < accNumber; i++) {
            this.accounts[i] = new Account();
        }
    }

    public Bank(int accNumber, int initBalance) {
        this.accounts = new Account[accNumber];
        for(int i = 0; i < accNumber; i++) {
            this.accounts[i] = new Account();
            this.accounts[i].deposit(initBalance);
        }
    }

    private Account get(int id) throws InvalidAccount {
        if (id < 0 || id >= this.accounts.length) {
            throw new InvalidAccount();
        }
        return this.accounts[id];
    }

    /**
     * Estes métodos não necessitam ser synchronized se o método transfer o for.
     * Nesta versão do banco, usam exclusão mútua a nível das contas individuais.
     */
    public void deposit(int id, int val) throws InvalidAccount {
        Account acc = this.get(id);
        acc.deposit(val);
    }

    public void withdraw(int id, int val) throws InvalidAccount, NotEnoughFunds {
        Account acc = this.get(id);
        acc.withdraw(val);
    }

    public synchronized int totalBalance(int accounts[]) throws InvalidAccount {
        int res = 0;
        for (int id : accounts) {
            res += this.get(id).balance();
        }
        return res;
    }

    public synchronized int totalBalance() throws InvalidAccount {
        int res = 0;
        for (Account acc : accounts) {
            res += acc.balance();
        }
        return res;
    }

    /**
     * 
     */
    public void transfer(int from, int to, int amount) throws InvalidAccount, NotEnoughFunds {
        Account fromAcc = this.get(from);
        Account toAcc = this.get(to);
        Account lowerAcc, higherAcc;
        if (from < to) {
            lowerAcc = fromAcc;
            higherAcc = toAcc;
        } else {
            lowerAcc = toAcc;
            higherAcc = fromAcc;
        }
        synchronized (lowerAcc) {
            synchronized (higherAcc) {
                fromAcc.withdraw(amount);
                toAcc.deposit(amount);
            }
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
        int low = 0;
        int high = this.b.accounts.length;
        for (int k = 0; k < this.iterations; k++) {
            int from = ThreadLocalRandom.current().nextInt(low, high);
            int to = ThreadLocalRandom.current().nextInt(low, high);
            try {
                this.b.transfer(from, to, 1);
            } catch (InvalidAccount | NotEnoughFunds e) {}
        }
    }
}

class Main {
    public static void main(String[] args) throws InterruptedException, InvalidAccount {
        final int NumContas = Integer.parseInt(args[0]);
        final int NumThreads = Integer.parseInt(args[1]);
        final int iterations = Integer.parseInt(args[2]);
        Bank b = new Bank(NumContas, 1000);
        final Transferer[] ts = new Transferer[NumThreads];

        for (int i = 0; i < NumThreads; i++) {
            ts[i] = new Transferer(b, iterations);
        }

        int todasContas[] = new int[NumContas];

        for (int i = 0; i < NumContas; i++) {
            todasContas[i] = i;
        }

        System.out.println("Initial total balance is: " + b.totalBalance());

        for (Transferer t : ts) {
            t.start();
        }
        for (Transferer t : ts) {
            t.join();
        }

        for (int j = 0; j < NumContas; j++) {
            System.out.println("Conta " + (j + 1) + " tem saldo: " + b.accounts[j].balance());
        }

        System.out.println("Final total balance is: " + b.totalBalance());
    }
}