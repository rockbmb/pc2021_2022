import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

class Warehouse {
    private class ItemInfo {
        String item_name;
        int quantity;
        final Condition item_has_stock = lock.newCondition();
    
        ItemInfo(String name) {
            this.item_name = name;
            this.quantity = 0;
        }
    }

    private final Lock lock = new ReentrantLock();
    private HashMap<String, ItemInfo> items = new HashMap<>();

    public ItemInfo get(String name) {
        ItemInfo i = this.items.get(name);
        if (i == null) {
            i = new ItemInfo(name);
            items.put(name, i);
        }
        return i;
    }

    public void supply(String name, int quantity) throws InterruptedException {
        lock.lock();

        try {
            ItemInfo i = this.get(name);
            i.quantity = quantity;
            if (i.quantity > 0) {
                i.item_has_stock.signalAll();
            }
        } finally {
            lock.unlock();
        }
    }

    public void consume(String[] names) throws InterruptedException {
        lock.lock();

        try {
            for (String name : names) {
                ItemInfo i = this.get(name);
                while (i.quantity == 0) {
                    i.item_has_stock.await();
                }
                i.quantity--;
            }
        } finally {
            lock.unlock();
        }
    }
}

class Consumer extends Thread {
    Warehouse wh;
    int tempo_consumo;
    Set<String> item_names;

    public Consumer(Warehouse w, int tc, Set<String> names) {
        this.wh = w;
        this.tempo_consumo = tc;
        this.item_names = names;
    }

    public void run() {
        int low = 1;
        int high = item_names.size();
        while (true) {
            try {
                sleep(tempo_consumo);

                List<String> list = new LinkedList<String>(item_names);
                Collections.shuffle(list);
                Integer elem = ThreadLocalRandom.current().nextInt(low, high);
                Set<String> randomSet = new HashSet<String>(list.subList(0, elem / 3));

                /*
                Map<String, Integer> items_quants = new HashMap<>();
                for (String s : randomSet) {
                    Integer elem = ThreadLocalRandom.current().nextInt(low, high);
                    items_quants.put(s, elem);
                }*/

                String[] ns = randomSet.stream().toArray(String[]::new);

                this.wh.consume(ns);

                System.out.println("Sou consumidor C-" + Thread.currentThread().getName() + ", obtive:");
                for(String s : randomSet) {
                    System.out.println("\t- item: " + s);
                }
            } catch (InterruptedException e) {
                System.out.println("Consumidor interrompido!");
                e.printStackTrace();
            }
        }
    }
}

class Supplier extends Thread {
    Warehouse w;
    int tempo_producao;
    Set<String> item_names;

    public Supplier(Warehouse w, int tp, Set<String> names) {
        this.w = w;
        this.tempo_producao = tp;
        this.item_names = names;
    }

    public void run() {
        int low = 1;
        int high = 2;

        while (true) {
            
            Integer elem = ThreadLocalRandom.current().nextInt(low, high);

            List<String> list = new LinkedList<String>(item_names);
            Collections.shuffle(list);
            Set<String> randomSet = new HashSet<String>(list.subList(0, item_names.size() / 3));

            
            Map<String, Integer> items_quants = new HashMap<>();
            for (String s : randomSet) {
                elem = ThreadLocalRandom.current().nextInt(low, high);
                items_quants.put(s, elem);
            }

            try {
                sleep(tempo_producao);
                System.out.println("Sou o produtor P-" + Thread.currentThread().getName() + "e inseri:");
                for (Entry<String, Integer> e : items_quants.entrySet()) {
                    this.w.supply(e.getKey(), e.getValue());
                    System.out.println("\t- Nome: " + e.getKey() + "; Qt: " + e.getValue());
                }

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

        Set<String> itemNames = new HashSet<>(
            Arrays.asList(
                "Martelo",
                "Prego",
                "Chave de Fendas",
                "Alicate",
                "Porcas",
                "Camarões",
                "Corda")
            );
        Warehouse wh = new Warehouse();
        final Supplier[] ss = new Supplier[NumProdutores];
        final Consumer[] cs = new Consumer[NumConsumidores];

        for (int i = 0; i < NumProdutores; i++) {
            ss[i] = new Supplier(wh, TempoProdução, itemNames);
        }

        for (int i = 0; i < NumConsumidores; i++) {
            cs[i] = new Consumer(wh, TempoConsumo, itemNames);
        }

        for (Supplier p : ss) {
            p.start();
        }

        for (Consumer c : cs) {
            c.start();
        }
    }
}