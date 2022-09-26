// Vatsal Shah 10474245

import java.io.*;
import java.util.*;

public class TextSwap {

    private static String readFile(String filename, int chunkSize) throws Exception {
        String line;
        StringBuilder buffer = new StringBuilder();
        File file = new File(filename);
	// The "-1" below is because of this:
	// https://stackoverflow.com/questions/729692/why-should-text-files-end-with-a-newline
	if ((file.length()-1) % chunkSize!=0)
	    { throw new Exception("File size not multiple of chunk size"); };
        BufferedReader br = new BufferedReader(new FileReader(file));
        while ((line = br.readLine()) != null){
            buffer.append(line);
        }
        br.close();
        return buffer.toString();
    }

    private static Interval[] getIntervals(int numChunks, int chunkSize) {
        // TODO: Implement me!
        Interval[] myIntervals = new Interval[numChunks];
        int i = 0;
        while (i < numChunks) {
            Interval interval = new Interval(i * chunkSize, ((i+1) * chunkSize) - 1);
            myIntervals[i] = interval;
            i++;
        }
        return myIntervals;
    }

    private static List<Character> getLabels(int numChunks) {
        Scanner scanner = new Scanner(System.in);
        List<Character> labels = new ArrayList<Character>();
        int endChar = numChunks == 0 ? 'a' : 'a' + numChunks - 1;
        System.out.printf("Input %d character(s) (\'%c\' - \'%c\') for the pattern.\n", numChunks, 'a', endChar);
        for (int i = 0; i < numChunks; i++) {
            labels.add(scanner.next().charAt(0));
        }
        scanner.close();
        // System.out.println(labels);
        return labels;
    }

    private static char[] runSwapper(String content, int chunkSize, int numChunks) {
        List<Character> labels = getLabels(numChunks);
        Interval[] intervals = getIntervals(numChunks, chunkSize);
        int[] offsets = new int[numChunks];
        for (int i = 0; i < numChunks; i++) {
            offsets[i] = content.indexOf(Character.toUpperCase(labels.get(i)));
        }
        // TODO: Order the intervals properly, then run the Swapper instances.
        char[] buffer = new char[content.length()];
        ArrayList<Thread> swappers = new ArrayList<Thread>();
        for (int i = 0; i < numChunks; i++) {
            // int offset = labels.indexOf((char)('a' + i));
            System.out.println(offsets[i]);
            Thread t = new Thread(new Swapper(intervals[i], content, buffer, offsets[i]));
            swappers.add(t);
        }
        for (Thread swapper : swappers) {
            swapper.start();
        }
        for (Thread swapper : swappers) {
            try {
                swapper.join();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
        return buffer;
    }

    private static void writeToFile(String contents, int chunkSize, int numChunks) throws Exception {
        char[] buff = runSwapper(contents, chunkSize, contents.length() / chunkSize);
        PrintWriter writer = new PrintWriter("output.txt", "UTF-8");
        writer.print(buff);
        writer.close();
    }

     public static void main(String[] args) {
        
        if (args.length != 2) {
            System.out.println("Usage: java TextSwap <chunk size> <filename>");
            return;
        }
        String contents = "";
        int chunkSize = Integer.parseInt(args[0]);
        try {
            contents = readFile(args[1],chunkSize);
            if (contents.length() / chunkSize > 26) {
                System.out.println("Maximum number of chunks is 26. Please provide a bigger chunk size.");
                return;
            }    
            writeToFile(contents, chunkSize, contents.length() / chunkSize);
        } catch (Exception e) {
            System.out.println(e.getMessage());
            return;
        }
    }
}
