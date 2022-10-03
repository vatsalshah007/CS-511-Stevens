// Vatsal Shah 10474245
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.ArrayList;

public class Customer implements Runnable {
    private Bakery bakery;
    private Random rnd;
    private List<BreadType> shoppingCart;
    private int shopTime;
    private int checkoutTime;

    /**
     * Initialize a customer object and randomize its shopping cart
     */
    public Customer(Bakery bakery) {
        // TODO
        this.bakery = bakery;
        this.rnd = new Random();
        this.shoppingCart = new ArrayList<BreadType>();
        this.shopTime = rnd.nextInt(250);
        this.checkoutTime = rnd.nextInt(250);
    }

    /**
     * Run tasks for the customer
     */
    public void run() {
        // TODO
        try {
            // fill shopping cart
            this.fillShoppingCart();

            // shop
            this.bakery.shelves.acquire(); 
            // get bread from shelves
            for (BreadType bread : shoppingCart) {
                this.bakery.breadShelves.get(bread).acquire();
                // Thread.sleep(shopTime / shoppingCart.size());
                this.bakery.takeBread(bread);
                this.bakery.breadShelves.get(bread).release();
            }

            System.out.println(this + " is shopping for " + shopTime + "ms");
            Thread.sleep(shopTime);

            // checkout
            this.bakery.checkoutRegisters.acquire();
            this.bakery.shelves.release();
            this.bakery.cashier.acquire();
            System.out.println(this + " is checking out for " + checkoutTime + "ms");
            Thread.sleep(checkoutTime);

            // pay
            System.out.println(this + " is paying for " + Arrays.toString(shoppingCart.toArray()));
            bakery.addSales(this.getItemsValue());
            this.bakery.cashier.release();
            this.bakery.checkoutRegisters.release(); // Customer leaves
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Return a string representation of the customer
     */
    public String toString() {
        return "Customer " + hashCode() + ": shoppingCart=" + Arrays.toString(shoppingCart.toArray()) + ", shopTime=" + shopTime + ", checkoutTime=" + checkoutTime;
    }

    /**
     * Add a bread item to the customer's shopping cart
     */
    private boolean addItem(BreadType bread) {
        // do not allow more than 3 items, chooseItems() does not call more than 3 times
        if (shoppingCart.size() >= 3) {
            return false;
        }
        shoppingCart.add(bread);
        return true;
    }

    /**
     * Fill the customer's shopping cart with 1 to 3 random breads
     */
    private void fillShoppingCart() {
        int itemCnt = 1 + rnd.nextInt(3);
        while (itemCnt > 0) {
            addItem(BreadType.values()[rnd.nextInt(BreadType.values().length)]);
            itemCnt--;
        }
    }

    /**
     * Calculate the total value of the items in the customer's shopping cart
     */
    private float getItemsValue() {
        float value = 0;
        for (BreadType bread : shoppingCart) {
            value += bread.getPrice();
        }
        return value;
    }
}