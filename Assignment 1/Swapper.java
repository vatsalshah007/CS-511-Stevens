// Vatsal Shah 10474245

public class Swapper implements Runnable {
    private int offset;
    private Interval interval;
    private String content;
    private char[] buffer;

    public Swapper(Interval interval, String content, char[] buffer, int offset) {
        this.offset = offset;
        this.interval = interval;
        this.content = content;
        this.buffer = buffer;
    }

    @Override
    public void run() {
        // TODO: Implement me!
        
        int temp = this.offset;
        for (int i = this.interval.getX();i <= this.interval.getY(); i++) {
            this.buffer[i] = this.content.charAt(temp);
            temp++;
        }

    }
}