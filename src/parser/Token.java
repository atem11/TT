package parser;

public class Token {
  enum tokens {
    L_BRACE, R_BRACE, VARIABLE,
    LAMBDA, END
  }

  private tokens token;
  private String name;

  Token(String token, String name) {
    this.name = name;

    switch (token) {
      case "#":
        this.token = tokens.END;
        break;
      case "(":
        this.token = tokens.L_BRACE;
        break;
      case ")":
        this.token = tokens.R_BRACE;
        break;
      case "\\":
        this.token = tokens.LAMBDA;
        break;
      default:
        this.token = tokens.VARIABLE;
    }
  }

  public tokens get() {
    return token;
  }

  public String getName() {
    return name;
  }
}
