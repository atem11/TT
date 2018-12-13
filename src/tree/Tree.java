package tree;

import java.util.ArrayList;
import java.util.List;

public class Tree {

  private String name;

  private String value;

  private List<Tree> children = new ArrayList<>();

  public Tree(String name, String value) {
    this.name = name;
    this.value = value;
  }

  public void addSon(Tree son) {
    children.add(son);
  }

  @Override
  public String toString() {
    StringBuilder res = new StringBuilder();
    if (name.equals("atom")) {
      for (int i = 0; i < children.size() - 1; i++) {
        res.append("(");
      }
      res.append(children.get(0).toString());
      for (int i = 1; i < children.size(); i++) {
        res.append(" ").append(children.get(i).toString()).append(")");
      }
    } else if (name.equals("lambda")) {
      res.append("(")
          .append(value)
          .append(children.get(0).toString())
          .append(")");
    } else {
      res.append(value);
    }
    return res.toString();
  }
}
