package parser;

import java.util.ArrayList;
import java.util.List;

public class TokenStream {
    private List<Token> operations = new ArrayList<>();
    private int index = 0;

    public void reset() {
        index = 0;
    }

    public boolean hasNext() {
        return  index < operations.size();
    }

    public Token next() {
        return operations.get(index++);
    }

    public void prev() {
        --index;
    }

    public int size() {
        return operations.size();
    }

    public void add(Token t) {
        operations.add(t);
    }
}
