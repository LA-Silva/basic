<?php

/**
 * C INTERPRETER IN PHP 8.0+
 *
 * A foundational interpreter for a subset of the C language.
 *
 * Components:
 * 1. Lexer: Converts source string into tokens.
 * 2. Parser: Converts tokens into an Abstract Syntax Tree (AST).
 * 3. Interpreter: Traverses the AST and executes logic.
 */

// -----------------------------------------------------------------------------
// 1. TOKEN TYPES & TOKEN CLASS
// -----------------------------------------------------------------------------

class TokenType {
    // Data Types
    public const TYPE_INT = 'TYPE_INT';
    public const RETURN = 'RETURN';

    // Literals
    public const INTEGER_CONST = 'INTEGER_CONST';
    public const STRING_LITERAL = 'STRING_LITERAL';

    // Operators
    public const PLUS    = 'PLUS';
    public const MINUS   = 'MINUS';
    public const MUL     = 'MUL';
    public const DIV     = 'DIV';
    public const ASSIGN  = 'ASSIGN';

    // Punctuation
    public const LPAREN  = 'LPAREN';
    public const RPAREN  = 'RPAREN';
    public const LBRACE  = 'LBRACE'; // {
    public const RBRACE  = 'RBRACE'; // }
    public const SEMI    = 'SEMI';
    public const COMMA   = 'COMMA';

    // Identifier
    public const ID      = 'ID';

    // End of File
    public const EOF     = 'EOF';
}

class Token {
    public function __construct(
        public string $type,
        public mixed $value
    ) {}

    public function __toString(): string {
        return "Token({$this->type}, {$this->value})";
    }
}

// -----------------------------------------------------------------------------
// 2. LEXER (TOKENIZER)
// -----------------------------------------------------------------------------

class Lexer {
    private string $text;
    private int $pos = 0;
    private ?string $currentChar;

    private const RESERVED_KEYWORDS = [
        'int' => TokenType::TYPE_INT,
        'return' => TokenType::RETURN,
    ];

    public function __construct(string $text) {
        $this->text = $text;
        $this->currentChar = $this->text[$this->pos] ?? null;
    }

    private function error(): void {
        throw new Exception("Lexer Error: Invalid character '{$this->currentChar}' at position {$this->pos}");
    }

    private function advance(): void {
        $this->pos++;
        $this->currentChar = ($this->pos > strlen($this->text) - 1) ? null : $this->text[$this->pos];
    }

    private function skipWhitespace(): void {
        while ($this->currentChar !== null && ctype_space($this->currentChar)) {
            $this->advance();
        }
    }

    private function skipComment(): void {
        // Skips /* ... */ style comments
        $this->advance(); // Skip '*'
        while ($this->currentChar !== null) {
            if ($this->currentChar === '*' && ($this->text[$this->pos + 1] ?? null) === '/') {
                $this->advance(); // Skip '*'
                $this->advance(); // Skip '/'
                return;
            }
            $this->advance();
        }
    }

    private function number(): int {
        $result = '';
        while ($this->currentChar !== null && ctype_digit($this->currentChar)) {
            $result .= $this->currentChar;
            $this->advance();
        }
        return (int)$result;
    }

    private function stringLiteral(): string {
        $result = '';
        $this->advance(); // Skip opening "
        while ($this->currentChar !== null && $this->currentChar !== '"') {
            // Basic escape sequence handling
            if ($this->currentChar === '\\') {
                $this->advance();
                switch ($this->currentChar) {
                    case 'n': $result .= "\n"; break;
                    case 't': $result .= "\t"; break;
                    case '\\': $result .= "\\"; break;
                    case '"': $result .= '"'; break;
                    default: $result .= '\\' . $this->currentChar; break;
                }
            } else {
                $result .= $this->currentChar;
            }
            $this->advance();
        }
        $this->advance(); // Skip closing "
        return $result;
    }

    private function _id(): Token {
        $result = '';
        while ($this->currentChar !== null && (ctype_alnum($this->currentChar) || $this->currentChar === '_')) {
            $result .= $this->currentChar;
            $this->advance();
        }
        $type = self::RESERVED_KEYWORDS[$result] ?? TokenType::ID;
        return new Token($type, $result);
    }

    public function getNextToken(): Token {
        while ($this->currentChar !== null) {
            if (ctype_space($this->currentChar)) {
                $this->skipWhitespace();
                continue;
            }

            if ($this->currentChar === '/' && ($this->text[$this->pos + 1] ?? null) === '*') {
                $this->advance();
                $this->skipComment();
                continue;
            }

            if (ctype_alpha($this->currentChar) || $this->currentChar === '_') {
                return $this->_id();
            }

            if (ctype_digit($this->currentChar)) {
                return new Token(TokenType::INTEGER_CONST, $this->number());
            }

            if ($this->currentChar === '"') {
                return new Token(TokenType::STRING_LITERAL, $this->stringLiteral());
            }

            $singleCharTokens = [
                '=' => TokenType::ASSIGN,
                '+' => TokenType::PLUS,
                '-' => TokenType::MINUS,
                '*' => TokenType::MUL,
                '/' => TokenType::DIV,
                '(' => TokenType::LPAREN,
                ')' => TokenType::RPAREN,
                '{' => TokenType::LBRACE,
                '}' => TokenType::RBRACE,
                ';' => TokenType::SEMI,
                ',' => TokenType::COMMA,
            ];

            if (isset($singleCharTokens[$this->currentChar])) {
                $token = new Token($singleCharTokens[$this->currentChar], $this->currentChar);
                $this->advance();
                return $token;
            }

            $this->error();
        }

        return new Token(TokenType::EOF, null);
    }
}

// -----------------------------------------------------------------------------
// 3. ABSTRACT SYNTAX TREE (AST) NODES
// -----------------------------------------------------------------------------

abstract class AST {}

class Program extends AST {
    /** @param AST[] $declarations */
    public function __construct(public array $declarations) {}
}

class FuncDecl extends AST {
    public function __construct(
        public string $funcName,
        public Block $block
    ) {}
}

class Block extends AST {
    /** @param AST[] $declarations */
    /** @param AST[] $statements */
    public function __construct(
        public array $declarations,
        public array $statements
    ) {}
}

class VarDecl extends AST {
    public function __construct(
        public VarNode $varNode,
        public Type $typeNode
    ) {}
}

class Assign extends AST {
    public function __construct(
        public VarNode $left,
        public AST $right
    ) {}
}

class ReturnNode extends AST {
    public function __construct(public AST $expr) {
    }
}

class FuncCall extends AST {
    /** @param AST[] $args */
    public function __construct(
        public string $funcName,
        public array $args
    ) {}
}

class BinOp extends AST {
    public function __construct(
        public AST $left,
        public Token $op,
        public AST $right
    ) {}
}

class Num extends AST {
    public function __construct(public Token $token) {}
}

class StringLiteral extends AST {
    public function __construct(public Token $token) {}
}

class VarNode extends AST {
    public function __construct(public Token $token) {}
}

class Type extends AST {
    public function __construct(public Token $token) {}
}

// -----------------------------------------------------------------------------
// 4. PARSER
// -----------------------------------------------------------------------------

class Parser {
    private Token $currentToken;
    private ?Token $peekToken = null;

    public function __construct(private Lexer $lexer) {
        $this->currentToken = $this->lexer->getNextToken();
        $this->peekToken = $this->lexer->getNextToken();
    }

    private function error(string $message = "Invalid Syntax"): void {
        throw new Exception("Parser Error: $message. Unexpected token: {$this->currentToken->type}({$this->currentToken->value})");
    }

    private function eat(string $tokenType): void {
        if ($this->currentToken->type === $tokenType) {
            $this->currentToken = $this->peekToken;
            $this->peekToken = $this->lexer->getNextToken();
        } else {
            $this->error("Expected token $tokenType");
        }
    }

    private function program(): Program {
        // program : (variable_declaration | function_declaration)+
        $declarations = [];
        while ($this->currentToken->type !== TokenType::EOF) {
            // Simplified: assumes all top-level constructs are functions for now
            $declarations[] = $this->functionDeclaration();
        }
        return new Program($declarations);
    }

    private function functionDeclaration(): FuncDecl {
        // function_declaration : type_spec ID LPAREN RPAREN block
        $this->eat(TokenType::TYPE_INT); // Simplified: only int functions
        $funcName = $this->currentToken->value;
        $this->eat(TokenType::ID);
        $this->eat(TokenType::LPAREN);
        // Parameter parsing would go here
        $this->eat(TokenType::RPAREN);
        $blockNode = $this->block();
        return new FuncDecl($funcName, $blockNode);
    }

    private function block(): Block {
        $this->eat(TokenType::LBRACE);
        $declarations = [];
        $statements = [];

        // Declarations must come before statements in C
        while ($this->currentToken->type === TokenType::TYPE_INT) {
            $decls = $this->variableDeclaration();
            $declarations = array_merge($declarations, $decls);
            $this->eat(TokenType::SEMI);
        }

        while ($this->currentToken->type !== TokenType::RBRACE) {
            $statements[] = $this->statement();
        }

        $this->eat(TokenType::RBRACE);
        return new Block($declarations, $statements);
    }

    private function variableDeclaration(): array {
        // variable_declaration : type_spec ID (COMMA ID)*
        $this->eat(TokenType::TYPE_INT);
        $varNodes = [new VarNode($this->currentToken)];
        $this->eat(TokenType::ID);

        while ($this->currentToken->type === TokenType::COMMA) {
            $this->eat(TokenType::COMMA);
            $varNodes[] = new VarNode($this->currentToken);
            $this->eat(TokenType::ID);
        }

        $typeNode = new Type(new Token(TokenType::TYPE_INT, 'int'));
        $declarations = [];
        foreach ($varNodes as $varNode) {
            $declarations[] = new VarDecl($varNode, $typeNode);
        }
        return $declarations;
    }

    private function statement(): AST {
        // statement : expression_statement | block | return_statement | empty
        if ($this->currentToken->type === TokenType::ID) {
            $node = $this->expressionStatement();
            $this->eat(TokenType::SEMI);
            return $node;
        }
        if ($this->currentToken->type === TokenType::LBRACE) {
            return $this->block();
        }
        if ($this->currentToken->type === TokenType::RETURN) {
            return $this->returnStatement();
        }
        $this->eat(TokenType::SEMI); // Empty statement
        return new AST(); // Represents NoOp
    }

    private function expressionStatement(): AST {
        // Check for assignment
        if ($this->currentToken->type === TokenType::ID && $this->peekToken->type === TokenType::ASSIGN) {
            $left = new VarNode($this->currentToken);
            $this->eat(TokenType::ID); // Consume the variable name
            $this->eat(TokenType::ASSIGN); // Consume the '='
            $right = $this->expr();
            return new Assign($left, $right);
        }
        return $this->expr();
    }

    private function returnStatement(): AST {
        $this->eat(TokenType::RETURN);
        $expr = $this->expr();
        $this->eat(TokenType::SEMI);
        return new ReturnNode($expr);
    }

    private function expr(): AST {
        return $this->additiveExpr();
    }

    private function additiveExpr(): AST {
        // additive_expr : multiplicative_expr ((PLUS | MINUS) multiplicative_expr)*
        $node = $this->multiplicativeExpr();
        while (in_array($this->currentToken->type, [TokenType::PLUS, TokenType::MINUS])) {
            $token = $this->currentToken;
            $this->eat($token->type);
            $node = new BinOp($node, $token, $this->multiplicativeExpr());
        }
        return $node;
    }

    private function multiplicativeExpr(): AST {
        // multiplicative_expr : primary ((MUL | DIV) primary)*
        $node = $this->primary();
        while (in_array($this->currentToken->type, [TokenType::MUL, TokenType::DIV])) {
            $token = $this->currentToken;
            $this->eat($token->type);
            $node = new BinOp($node, $token, $this->primary());
        }
        return $node;
    }

    private function primary(): AST {
        // primary : INTEGER_CONST | STRING_LITERAL | ID | LPAREN expr RPAREN | function_call
        $token = $this->currentToken;
        if ($token->type === TokenType::INTEGER_CONST) {
            $this->eat(TokenType::INTEGER_CONST);
            return new Num($token);
        }
        if ($token->type === TokenType::STRING_LITERAL) {
            $this->eat(TokenType::STRING_LITERAL);
            return new StringLiteral($token);
        }
        if ($token->type === TokenType::ID) {
            // Check for function call
            $idToken = $token;
            $this->eat(TokenType::ID);
            if ($this->currentToken->type === TokenType::LPAREN) {
                $this->eat(TokenType::LPAREN);
                $args = [];
                if ($this->currentToken->type !== TokenType::RPAREN) {
                    $args[] = $this->expr();
                    while ($this->currentToken->type === TokenType::COMMA) {
                        $this->eat(TokenType::COMMA);
                        $args[] = $this->expr();
                    }
                }
                $this->eat(TokenType::RPAREN);
                return new FuncCall($idToken->value, $args);
            }
            // It's a variable
            return new VarNode($idToken);
        }
        if ($token->type === TokenType::LPAREN) {
            $this->eat(TokenType::LPAREN);
            $node = $this->expr();
            $this->eat(TokenType::RPAREN);
            return $node;
        }
        $this->error("Unexpected primary expression token");
    }

    public function parse(): AST {
        $node = $this->program();
        if ($this->currentToken->type !== TokenType::EOF) {
            $this->error("Expected EOF");
        }
        return $node;
    }
}

// -----------------------------------------------------------------------------
// 5. INTERPRETER
// -----------------------------------------------------------------------------

class Interpreter {
    private array $callStack = [];

    public function __construct(private Parser $parser) { }

    public function visit(AST $node): mixed {
        $methodName = 'visit' . (new ReflectionClass($node))->getShortName();
        if (method_exists($this, $methodName)) {
            return $this->$methodName($node);
        }
        // For NoOp (empty statements)
        if ($node instanceof AST && (new ReflectionClass($node))->getShortName() === 'AST') {
            return null;
        }
        throw new Exception("No visit method defined for " . get_class($node));
    }

    private function &currentScope(): array {
        return $this->callStack[count($this->callStack) - 1];
    }

    private function visitProgram(Program $node): void {
        // Find and call main
        foreach ($node->declarations as $decl) {
            if ($decl instanceof FuncDecl && $decl->funcName === 'main') {
                $this->visit($decl);
                return;
            }
        }
        throw new Exception("Runtime Error: 'main' function not found.");
    }

    private function visitFuncDecl(FuncDecl $node): void {
        $this->callStack[] = []; // Push new scope for the function
        $this->visit($node->block);
        array_pop($this->callStack); // Pop scope
    }

    private function visitBlock(Block $node): void {
        foreach ($node->declarations as $declaration) {
            $this->visit($declaration);
        }
        foreach ($node->statements as $statement) {
            $this->visit($statement);
        }
    }

    private function visitVarDecl(VarDecl $node): void {
        $varName = $node->varNode->token->value;
        $this->currentScope()[$varName] = 0; // C initializes local variables to garbage, but we'll use 0.
    }

    private function visitAssign(Assign $node): void {
        $varName = $node->left->token->value;
        if (!array_key_exists($varName, $this->currentScope())) {
            throw new Exception("Runtime Error: Variable '{$varName}' not declared in this scope.");
        }
        $value = $this->visit($node->right);
        $this->currentScope()[$varName] = $value;
    }

    private function visitFuncCall(FuncCall $node): mixed {
        if ($node->funcName === 'printf') {
            // Simplified printf
            $formatString = $this->visit($node->args[0]);
            $argValues = array_map(fn($arg) => $this->visit($arg), array_slice($node->args, 1));
            echo vsprintf($formatString, $argValues);
            return null;
        }
        throw new Exception("Runtime Error: Call to undefined function '{$node->funcName}'");
    }

    private function visitBinOp(BinOp $node): mixed {
        $leftVal = $this->visit($node->left);
        $rightVal = $this->visit($node->right);
        return match ($node->op->type) {
            TokenType::PLUS  => $leftVal + $rightVal,
            TokenType::MINUS => $leftVal - $rightVal,
            TokenType::MUL   => $leftVal * $rightVal,
            TokenType::DIV   => intdiv($leftVal, $rightVal),
            default => throw new Exception("Unknown operator"),
        };
    }

    private function visitNum(Num $node): int {
        return (int)$node->token->value;
    }

    private function visitStringLiteral(StringLiteral $node): string {
        return (string)$node->token->value;
    }

    private function visitVarNode(VarNode $node): mixed {
        $varName = $node->token->value;
        if (!array_key_exists($varName, $this->currentScope())) {
            throw new Exception("Runtime Error: Use of undeclared variable '{$varName}'.");
        }
        return $this->currentScope()[$varName];
    }

    private function visitReturnNode(ReturnNode $node): void {
        // In this simple interpreter, we can just visit the expression
        // but we don't do anything with the value.
        $this->visit($node->expr);
    }

    public function interpret(): void {
        $tree = $this->parser->parse();
        $this->visit($tree);
    }
}

// -----------------------------------------------------------------------------
// MAIN EXECUTION
// -----------------------------------------------------------------------------

$sourceCode = <<<'C'
/*
 * Simple C Program
 * Demonstrates variables, arithmetic, and printf.
 */
int main() {
    int a, b, result;

    a = 10;
    b = 5;

    result = (a + b) * 2;

    printf("The value of a is: %d\n", a);
    printf("The result is: %d\n", result);

    return 0; /* Return value is ignored in this interpreter */
}
C;

try {
    echo "--- Source Code ---\n";
    echo $sourceCode . "\n";
    echo "--- Output ---\n";

    $lexer = new Lexer($sourceCode);
    $parser = new Parser($lexer);
    $interpreter = new Interpreter($parser);
    $interpreter->interpret();

} catch (Exception $e) {
    echo "Error: " . $e->getMessage() . "\n";
}

?>
