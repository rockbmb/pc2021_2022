/**
 * Banco sem sincronização.
 */

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

    private Account get(int id) throws InvalidAccount {
        if (id < 0 || id >= this.accounts.length) {
            throw new InvalidAccount();
        }
        return this.accounts[id];
    }

    public void deposit(int id, int val) throws InvalidAccount {
        this.get(id).deposit(val);
    }

    public void withdraw(int id, int val) throws InvalidAccount, NotEnoughFunds {
        this.get(id).withdraw(val);
    }

    public int totalBalance(int accounts[]) throws InvalidAccount {
        int res = 0;
        for (int id : accounts) {
            res += this.get(id).balance();
        }
        return res;
    }
}

class Depositor extends Thread {
    int iterations;
    Bank b;

    public Depositor(Bank b, int its) {
        this.b = b;
        this.iterations = its;
    }

    public void run() {
        int idx = this.b.accounts.length;
        for (int k = 0; k < this.iterations; k++) {
            try {
                this.b.deposit(k % idx, 1);
            } catch (InvalidAccount e) {

            }
        }
    }
}

class Main {
    public static void main(String[] args) throws InterruptedException {
        final int NC = Integer.parseInt(args[0]);
        final int ND = Integer.parseInt(args[1]);
        final int iterations = Integer.parseInt(args[2]);
        Bank b = new Bank(NC);
        final Depositor[] ts = new Depositor[ND];

        for (int i = 0; i < ND; i++) {
            ts[i] = new Depositor(b, iterations);
        }

        int todasContas[] = new int[NC];

        for (int i = 0; i < NC; i++) {
            todasContas[i] = i;
        }

        for (Depositor t : ts) {
            t.start();
        }
        for (Depositor t : ts) {
            t.join();
        }

        for (int j = 0; j < NC; j++) {
            System.out.println("Conta " + (j + 1) + " tem saldo: " + b.accounts[j].balance());
        }
    }
}