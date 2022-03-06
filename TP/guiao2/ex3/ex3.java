/**
 * Banco com sincronização a nível do objeto Bank.
 */

import java.util.concurrent.ThreadLocalRandom;

class NotEnoughFunds extends Exception {}
class InvalidAccount extends Exception {}

class Bank {
    public Account[] accounts;

    class Account {
        public Account() {
            this.balance = 0;
        }

        private int balance;
        public int balance() { return balance; }
        public void deposit(int val) {balance += val;}
        public void withdraw(int val) throws NotEnoughFunds {
            if (balance < val) {
                throw new NotEnoughFunds();
            }
            balance -= val;
        }
    }

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
     */
    public synchronized void deposit(int id, int val) throws InvalidAccount {
        this.get(id).deposit(val);
    }

    public synchronized void withdraw(int id, int val) throws InvalidAccount, NotEnoughFunds {
        Account ac = this.get(id);
        ac.withdraw(val);
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
     * Se se tirar o synchronized desta função passa a ser possível uma thread adquirir
     * lock sobre o banco, fazer levantamento, libertar lock, e depois outra thread
     * adquirir o lock antes de a primeira poder fazer o depósito.
     */
    public synchronized void transfer(int from, int to, int amount) throws InvalidAccount, NotEnoughFunds {
        this.get(from).withdraw(amount);
        this.get(to).deposit(amount);
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