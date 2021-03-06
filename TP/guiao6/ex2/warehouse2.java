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

class Warehouse2 {
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
            boolean done = false;
            while(!done) {
                done = true;
                for (String name : names) {
                    ItemInfo i = this.get(name);
                    if (i.quantity == 0) {
                        i.item_has_stock.await();
                        done = false;
                        break;
                    }
                }
            }

            for (String name : names) {
                ItemInfo i = this.get(name);
                i.quantity--;
            }
        } finally {
            lock.unlock();
        }
    }
}

class Consumer extends Thread {
    Warehouse2 wh;
    int tempo_consumo;
    Set<String> item_names;

    public Consumer(Warehouse2 w, int tc, Set<String> names) {
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

                StringBuilder sb = new StringBuilder();
                sb.append("Sou o \033[38;2;255;0;0mconsumidor C-");
                sb.append(Thread.currentThread().getName());
                sb.append("\033[39m\033[49m e obtive:\n");
                for(String s : randomSet) {
                    sb.append("\t- item: ");
                    sb.append(s);
                    sb.append("\n");
                    System.out.print(sb.toString());
                }
            } catch (InterruptedException e) {
                System.out.println("Consumidor interrompido!");
                e.printStackTrace();
            }
        }
    }
}

class Supplier extends Thread {
    Warehouse2 w;
    int tempo_producao;
    Set<String> item_names;

    public Supplier(Warehouse2 w, int tp, Set<String> names) {
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
                StringBuilder sb = new StringBuilder();
                sb.append("Sou o \033[38;2;0;255;0mprodutor P-");
                sb.append(Thread.currentThread().getName());
                sb.append("\033[39m\033[49m e inseri:\n");
                for (Entry<String, Integer> e : items_quants.entrySet()) {
                    this.w.supply(e.getKey(), e.getValue());
                    sb.append("\t- Nome: ");
                    sb.append(e.getKey());
                    sb.append("; Qt: ");
                    sb.append(e.getValue());
                    sb.append("\n");
                    System.out.print(sb.toString());
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
        final int TempoProdu????o = Integer.parseInt(args[2]);
        // Em milisegundos.
        final int TempoConsumo = Integer.parseInt(args[3]);

        Set<String> itemNames = new HashSet<>(
            Arrays.asList(
                "Martelo",
                "Prego",
                "Chave de Fendas",
                "Alicate",
                "Porcas",
                "Camar??es",
                "Corda")
            );
        Warehouse2 wh = new Warehouse2();
        final Supplier[] ss = new Supplier[NumProdutores];
        final Consumer[] cs = new Consumer[NumConsumidores];

        for (int i = 0; i < NumProdutores; i++) {
            ss[i] = new Supplier(wh, TempoProdu????o, itemNames);
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