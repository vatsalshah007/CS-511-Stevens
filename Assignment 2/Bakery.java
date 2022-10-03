// Vatsal Shah 10474245
import java.text.DecimalFormat;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Semaphore;

public class Bakery implements Runnable {
    private static final int TOTAL_CUSTOMERS = 200;
    private static final int CAPACITY = 50;
    private static final int FULL_BREAD = 20;
    private Map<BreadType, Integer> availableBread;
    private ExecutorService executor;
    private float sales = 0;

    // TODO

    public Semaphore shelves = new Semaphore(3); // 3 shelves
    public Semaphore checkoutRegisters = new Semaphore(4); // 4 checkout registers
    public Semaphore cashier = new Semaphore(1); // 1 person per cashier
    public Map<BreadType, Semaphore> breadShelves; // 1 person per bread shelf
    
    // Shelf Semaphore
    public Semaphore ryeShelf = new Semaphore(1);
    public Semaphore sourdoughShelf = new Semaphore(1);
    public Semaphore wonderShelf = new Semaphore(1);

    /**
     * Remove a loaf from the available breads and restock if necessary
     */
    public void takeBread(BreadType bread) {
        int breadLeft = availableBread.get(bread);
        if (breadLeft > 0) {
            availableBread.put(bread, breadLeft - 1);
        } else {
            System.out.println("No " + bread.toString() + " bread left! Restocking...");
            // restock by preventing access to the bread stand for some time
            try {
                Thread.sleep(1000);
            } catch (InterruptedException ie) {
                ie.printStackTrace();
            }
            availableBread.put(bread, FULL_BREAD - 1);
        }
    }

    /**
     * Add to the total sales
     */
    public void addSales(float value) {
        sales += value;
    }

    /**
     * Run all customers in a fixed thread pool
     */
    public void run() {
        availableBread = new ConcurrentHashMap<BreadType, Integer>();
        availableBread.put(BreadType.RYE, FULL_BREAD);
        availableBread.put(BreadType.SOURDOUGH, FULL_BREAD);
        availableBread.put(BreadType.WONDER, FULL_BREAD);

        // TODO
        breadShelves = new ConcurrentHashMap<BreadType, Semaphore>();
        breadShelves.put(BreadType.RYE, ryeShelf);
        breadShelves.put(BreadType.SOURDOUGH, sourdoughShelf);
        breadShelves.put(BreadType.WONDER, wonderShelf);

        executor = Executors.newFixedThreadPool(CAPACITY);
        int i = 0;
        while (i < TOTAL_CUSTOMERS) {
            executor.execute(new Customer(this));
            i++;
        }
        executor.shutdown();

        try {
            executor.awaitTermination(Long.MAX_VALUE, java.util.concurrent.TimeUnit.NANOSECONDS);
            DecimalFormat df = new DecimalFormat("0.##");
            System.out.println("Total sales: $" + df.format(sales));
        } catch (Exception e) {
            // TODO: handle exception
            e.printStackTrace();
        }
    }
}
