package parser;

import java.io.IOException;
import parser.Token.tokens;
import tree.Tree;

public class Parser {
  private static TokenStream stream = new TokenStream();

  public static Tree parse(TokenStream input) throws IOException {
    stream = input;
    Tree ans = expression();
    if (stream.next().get() != Token.tokens.END) {
      throw new IOException("Wrong input");
    }
    return ans;
  }

  private static Tree expression() {
    Tree ans;
    Token token = stream.next();
    if (token.get() == tokens.LAMBDA) {
      ans = new Tree("lambda", "\\" + token.getName() + '.');
      ans.addSon(expression());
    } else {
      stream.prev();
      ans = atom();
    }
    return ans;
  }

  //(\a.(\b.((((a b) c) (\d.(e (\f.g)))) h)))

  private static Tree atom() {
    Tree ans = new Tree ("atom", "list");
    while (true) {
      Token token = stream.next();
      if (token.get() == tokens.L_BRACE) {
        ans.addSon(expression());
        stream.next();
      } else if (token.get() == tokens.VARIABLE) {
        ans.addSon(new Tree("variable", token.getName()));
      } else if (token.get() == tokens.LAMBDA) {
        stream.prev();
        ans.addSon(expression());
      } else if (token.get() == tokens.END || token.get() == tokens.R_BRACE) {
        stream.prev();
        break;
      }
    }
    return ans;
  }
}
