import java.io.IOException;
import java.util.Scanner;
import parser.Lexer;
import parser.Parser;
import tree.Tree;

public class HW1 {

  public static void main(String[] args) throws IOException {
    Scanner in = new Scanner(System.in);
    StringBuilder input = new StringBuilder();
    while (in.hasNext()) {
      input.append(in.nextLine());
      input.append(" ");
    }

    Tree ans = Parser.parse(Lexer.lexer(input.toString()));
    System.out.println(ans.toString());
  }
}