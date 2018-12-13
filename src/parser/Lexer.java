package parser;

import java.io.IOException;

public class Lexer {

  public static TokenStream lexer(String in) throws IOException {
    TokenStream ans = new TokenStream();
    StringBuilder name = new StringBuilder();
    boolean lambda = false;

    for (int ind = 0; ind < in.length(); ind++) {
      char ch = in.charAt(ind);

      if (ch == ')') {
        if (name.length() != 0) {
          ans.add(new Token("var", name.toString()));
          name = new StringBuilder();
        }
        ans.add(new Token(")", ")"));
      } else if (ch == '(') {
        if (name.length() != 0) {
          ans.add(new Token("var", name.toString()));
          name = new StringBuilder();
        }
        ans.add(new Token("(", "("));
      } else if (ch == '\\') {
        lambda = true;
        if (name.length() != 0) {
          ans.add(new Token("var", name.toString()));
          name = new StringBuilder();
        }
      } else if (ch == '.') {
        lambda = false;
        ans.add(new Token("\\", name.toString()));
        name = new StringBuilder();
      } else if ((ch >= 'a' && ch <= 'z') || Character.isDigit(ch) || ch == '\'') {
        name.append(ch);
      } else if (Character.isWhitespace(ch)) {
        if (lambda) {
          continue;
        }
        if (name.length() != 0) {
          ans.add(new Token("var", name.toString()));
          name = new StringBuilder();
        }
      } else {
        throw new IOException("Bad symbol: " + ch);
      }
    }

    ans.add(new Token("#", "#"));
    return ans;
  }

}
