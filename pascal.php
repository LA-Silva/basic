<?php

/**
 * PASCAL INTERPRETER IN PHP 8.0+
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
    public const INTEGER = 'INTEGER';
    public const PLUS    = 'PLUS';
    public const MINUS   = 'MINUS';
    public const MUL     = 'MUL';
    public const DIV     = 'DIV'; // Integer division for this specific implementation
    public const LPAREN  = 'LPAREN';
    public const RPAREN  = 'RPAREN';
    public const ID      = 'ID';
    public const ASSIGN  = 'ASSIGN';
    public const BEGIN   = 'BEGIN';
    public const END     = 'END';
    public const SEMI    = 'SEMI';
    public const DOT     = 'DOT';
    public const PROGRAM = 'PROGRAM';
    public const VAR     = 'VAR';
    public const COLON   = 'COLON';
    public const COMMA   = 'COMMA';
    public const INTEGER_CONST = 'INTEGER_CONST'; // The literal number
    public const WRITELN = 'WRITELN';
    public const IF      = 'IF';
    public const THEN    = 'THEN';
    public const ELSE    = 'ELSE';
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
        'PROGRAM' => TokenType::PROGRAM,
        'VAR'     => TokenType::VAR,
        'DIV'     => TokenType::DIV,
        'INTEGER' => TokenType::INTEGER,
        'BEGIN'   => TokenType::BEGIN,
        'END'     => TokenType::END,
        'WRITELN' => TokenType::WRITELN,
        'IF'      => TokenType::IF,
        'THEN'    => TokenType::THEN,
        'ELSE'    => TokenType::ELSE,
    ];

    public function __construct(string $text) {
        $this->text = $text;
        $this->currentChar = $this->text[$this->pos] ?? null;
    }

    private function error(): void {
        throw new Exception("Invalid character: '{$this->currentChar}' at position {$this->pos}");
    }

    private function advance(): void {
        $this->pos++;
        $this->currentChar = ($this->pos > strlen($this->text) - 1) ? null : $this->text[$this->pos];
    }

    private function peek(): ?string {
        $peekPos = $this->pos + 1;
        return ($peekPos > strlen($this->text) - 1) ? null : $this->text[$peekPos];
    }

    private function skipWhitespace(): void {
        while ($this->currentChar !== null && ctype_space($this->currentChar)) {
            $this->advance();
        }
    }

    private function skipComment(): void {
        while ($this->currentChar !== '}') {
            $this->advance();
        }
        $this->advance(); // Skip the closing '}'
    }

    private function integer(): int {
        $result = '';
        while ($this->currentChar !== null && ctype_digit($this->currentChar)) {
            $result .= $this->currentChar;
            $this->advance();
        }
        return (int)$result;
    }

    private function _id(): Token {
        $result = '';
        while ($this->currentChar !== null && (ctype_alnum($this->currentChar) || $this->currentChar === '_')) {
            $result .= $this->currentChar;
            $this->advance();
        }
        
        // Pascal is case-insensitive
        $upperResult = strtoupper($result);
        $type = self::RESERVED_KEYWORDS[$upperResult] ?? TokenType::ID;
        
        return new Token($type, ($type === TokenType::ID) ? $result : $upperResult);
    }

    public function getNextToken(): Token {
        while ($this->currentChar !== null) {
            if (ctype_space($this->currentChar)) {
                $this->skipWhitespace();
                continue;
            }

            if ($this->currentChar === '{') {
                $this->skipComment();
                continue;
            }

            if (ctype_alpha($this->currentChar) || $this->currentChar === '_') {
                return $this->_id();
            }

            if (ctype_digit($this->currentChar)) {
                return new Token(TokenType::INTEGER_CONST, $this->integer());
            }

            if ($this->currentChar === ':' && $this->peek() === '=') {
                $this->advance();
                $this->advance();
                return new Token(TokenType::ASSIGN, ':=');
            }

            if ($this->currentChar === ':') {
                $this->advance();
                return new Token(TokenType::COLON, ':');
            }
            
            if ($this->currentChar === ',') {
                $this->advance();
                return new Token(TokenType::COMMA, ',');
            }

            if ($this->currentChar === ';') {
                $this->advance();
                return new Token(TokenType::SEMI, ';');
            }

            if ($this->currentChar === '.') {
                $this->advance();
                return new Token(TokenType::DOT, '.');
            }

            if ($this->currentChar === '+') {
                $this->advance();
                return new Token(TokenType::PLUS, '+');
            }

            if ($this->currentChar === '-') {
                $this->advance();
                return new Token(TokenType::MINUS, '-');
            }

            if ($this->currentChar === '*') {
                $this->advance();
                return new Token(TokenType::MUL, '*');
            }

            if ($this->currentChar === '/') {
                $this->advance();
                return new Token(TokenType::DIV, '/');
            }

            if ($this->currentChar === '(') {
                $this->advance();
                return new Token(TokenType::LPAREN, '(');
            }

            if ($this->currentChar === ')') {
                $this->advance();
                return new Token(TokenType::RPAREN, ')');
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

class BinOp extends AST {
    public function __construct(
        public AST $left,
        public Token $op,
        public AST $right
    ) {}
}

class UnaryOp extends AST {
    public function __construct(
        public Token $op,
        public AST $expr
    ) {}
}

class Num extends AST {
    public function __construct(
        public Token $token
    ) {}
}

class VarNode extends AST {
    public function __construct(
        public Token $token
    ) {}
}

class Assign extends AST {
    public function __construct(
        public VarNode $left,
        public Token $op,
        public AST $right
    ) {}
}

class Compound extends AST {
    /** @param AST[] $children */
    public function __construct(
        public array $children
    ) {}
}

class NoOp extends AST {}

class Program extends AST {
    public function __construct(
        public string $name,
        public Block $block
    ) {}
}

class Block extends AST {
    /** @param VarDecl[] $declarations */
    public function __construct(
        public array $declarations,
        public Compound $compoundStatement
    ) {}
}

class VarDecl extends AST {
    public function __construct(
        public VarNode $varNode,
        public Type $typeNode
    ) {}
}

class Type extends AST {
    public function __construct(
        public Token $token
    ) {}
}

class Writeln extends AST {
    public function __construct(
        public AST $expr
    ) {}
}

class IfNode extends AST {
    public function __construct(
        public AST $condition,
        public AST $thenBranch,
        public ?AST $elseBranch
    ) {}
}

// -----------------------------------------------------------------------------
// 4. PARSER
// -----------------------------------------------------------------------------

class Parser {
    private Token $currentToken;

    public function __construct(
        private Lexer $lexer
    ) {
        $this->currentToken = $this->lexer->getNextToken();
    }

    private function error(): void {
        throw new Exception("Invalid Syntax. Unexpected token: " . $this->currentToken->type);
    }

    private function eat(string $tokenType): void {
        if ($this->currentToken->type === $tokenType) {
            $this->currentToken = $this->lexer->getNextToken();
        } else {
            $this->error();
        }
    }

    public function program(): AST {
        // program : PROGRAM variable SEMI block DOT
        $this->eat(TokenType::PROGRAM);
        $varNode = $this->variable();
        $progName = $varNode->token->value;
        $this->eat(TokenType::SEMI);
        $blockNode = $this->block();
        $this->eat(TokenType::DOT);
        return new Program($progName, $blockNode);
    }

    private function block(): Block {
        // block : declarations compound_statement
        $declarations = $this->declarations();
        $compoundStatement = $this->compoundStatement();
        return new Block($declarations, $compoundStatement);
    }

    private function declarations(): array {
        // declarations : VAR (variable_declaration SEMI)+ | empty
        $declarations = [];
        if ($this->currentToken->type === TokenType::VAR) {
            $this->eat(TokenType::VAR);
            while ($this->currentToken->type === TokenType::ID) {
                $varDecls = $this->variableDeclaration();
                foreach ($varDecls as $varDecl) {
                    $declarations[] = $varDecl;
                }
                $this->eat(TokenType::SEMI);
            }
        }
        return $declarations;
    }

    private function variableDeclaration(): array {
        // variable_declaration : ID (COMMA ID)* COLON type_spec
        $varNodes = [new VarNode($this->currentToken)];
        $this->eat(TokenType::ID);

        while ($this->currentToken->type === TokenType::COMMA) {
            $this->eat(TokenType::COMMA);
            $varNodes[] = new VarNode($this->currentToken);
            $this->eat(TokenType::ID);
        }

        $this->eat(TokenType::COLON);
        $typeNode = $this->typeSpec();
        
        $varDeclarations = [];
        foreach ($varNodes as $varNode) {
            $varDeclarations[] = new VarDecl($varNode, $typeNode);
        }
        return $varDeclarations;
    }

    private function typeSpec(): Type {
        // type_spec : INTEGER
        $token = $this->currentToken;
        $this->eat(TokenType::INTEGER);
        return new Type($token);
    }

    private function compoundStatement(): Compound {
        // compound_statement : BEGIN statement_list END
        $this->eat(TokenType::BEGIN);
        $nodes = $this->statementList();
        $this->eat(TokenType::END);
        return new Compound($nodes);
    }

    private function statementList(): array {
        // statement_list : statement | statement SEMI statement_list
        $node = $this->statement();
        $results = [$node];

        while ($this->currentToken->type === TokenType::SEMI) {
            $this->eat(TokenType::SEMI);
            $results[] = $this->statement();
        }

        // Handle error recovery for stray semicolons before END
        if ($this->currentToken->type === TokenType::ID) {
            $this->error();
        }

        return $results;
    }

    private function statement(): AST {
        /*
        statement : compound_statement
                  | assignment_statement
                  | writeln_statement
                  | if_statement
                  | empty
        */
        if ($this->currentToken->type === TokenType::BEGIN) {
            return $this->compoundStatement();
        }
        
        if ($this->currentToken->type === TokenType::ID) {
            return $this->assignmentStatement();
        }

        if ($this->currentToken->type === TokenType::WRITELN) {
            return $this->writelnStatement();
        }

        if ($this->currentToken->type === TokenType::IF) {
            return $this->ifStatement();
        }

        return $this->empty();
    }

    private function assignmentStatement(): Assign {
        // assignment_statement : variable ASSIGN expr
        $left = $this->variable();
        $token = $this->currentToken;
        $this->eat(TokenType::ASSIGN);
        $right = $this->expr();
        return new Assign($left, $token, $right);
    }

    private function writelnStatement(): Writeln {
        // writeln_statement : WRITELN LPAREN expr RPAREN
        $this->eat(TokenType::WRITELN);
        $this->eat(TokenType::LPAREN);
        $expr = $this->expr();
        $this->eat(TokenType::RPAREN);
        return new Writeln($expr);
    }

    private function ifStatement(): IfNode {
        // if_statement : IF expr THEN statement (ELSE statement)?
        $this->eat(TokenType::IF);
        $condition = $this->expr();
        $this->eat(TokenType::THEN);
        $thenBranch = $this->statement();
        
        $elseBranch = null;
        if ($this->currentToken->type === TokenType::ELSE) {
            $this->eat(TokenType::ELSE);
            $elseBranch = $this->statement();
        }

        return new IfNode($condition, $thenBranch, $elseBranch);
    }

    private function variable(): VarNode {
        $node = new VarNode($this->currentToken);
        $this->eat(TokenType::ID);
        return $node;
    }

    private function empty(): NoOp {
        return new NoOp();
    }

    private function expr(): AST {
        // expr : term ((PLUS | MINUS) term)*
        $node = $this->term();

        while (in_array($this->currentToken->type, [TokenType::PLUS, TokenType::MINUS])) {
            $token = $this->currentToken;
            if ($token->type === TokenType::PLUS) {
                $this->eat(TokenType::PLUS);
            } elseif ($token->type === TokenType::MINUS) {
                $this->eat(TokenType::MINUS);
            }
            $node = new BinOp($node, $token, $this->term());
        }
        return $node;
    }

    private function term(): AST {
        // term : factor ((MUL | DIV) factor)*
        $node = $this->factor();

        while (in_array($this->currentToken->type, [TokenType::MUL, TokenType::DIV])) {
            $token = $this->currentToken;
            if ($token->type === TokenType::MUL) {
                $this->eat(TokenType::MUL);
            } elseif ($token->type === TokenType::DIV) {
                $this->eat(TokenType::DIV);
            }
            $node = new BinOp($node, $token, $this->factor());
        }
        return $node;
    }

    private function factor(): AST {
        // factor : PLUS factor | MINUS factor | INTEGER_CONST | LPAREN expr RPAREN | variable
        $token = $this->currentToken;

        if ($token->type === TokenType::PLUS) {
            $this->eat(TokenType::PLUS);
            return new UnaryOp($token, $this->factor());
        }
        if ($token->type === TokenType::MINUS) {
            $this->eat(TokenType::MINUS);
            return new UnaryOp($token, $this->factor());
        }
        if ($token->type === TokenType::INTEGER_CONST) {
            $this->eat(TokenType::INTEGER_CONST);
            return new Num($token);
        }
        if ($token->type === TokenType::LPAREN) {
            $this->eat(TokenType::LPAREN);
            $node = $this->expr();
            $this->eat(TokenType::RPAREN);
            return $node;
        }

        return $this->variable();
    }

    public function parse(): AST {
        $node = $this->program();
        if ($this->currentToken->type !== TokenType::EOF) {
            $this->error();
        }
        return $node;
    }
}

// -----------------------------------------------------------------------------
// 5. INTERPRETER
// -----------------------------------------------------------------------------

class Interpreter {
    // Symbol table to store variable values.
    // Pascal variables are case-insensitive, we normalize keys to lowercase.
    private array $GLOBAL_SCOPE = [];

    public function __construct(
        private Parser $parser
    ) {}

    public function visit(AST $node): mixed {
        $methodName = 'visit' . (new ReflectionClass($node))->getShortName();
        if (method_exists($this, $methodName)) {
            return $this->$methodName($node);
        }
        throw new Exception("No visit method defined for " . get_class($node));
    }

    private function visitProgram(Program $node): void {
        $this->visit($node->block);
    }

    private function visitBlock(Block $node): void {
        foreach ($node->declarations as $declaration) {
            $this->visit($declaration);
        }
        $this->visit($node->compoundStatement);
    }

    private function visitVarDecl(VarDecl $node): void {
        // Initialize variables to 0 (default in this interpreter)
        // Store keys as lowercase to handle case-insensitivity
        $varName = strtolower($node->varNode->token->value);
        $this->GLOBAL_SCOPE[$varName] = 0; 
    }

    private function visitType(Type $node): void {
        // No-op for now
    }

    private function visitCompound(Compound $node): void {
        foreach ($node->children as $child) {
            $this->visit($child);
        }
    }

    private function visitNoOp(NoOp $node): void {
        // Do nothing
    }

    private function visitBinOp(BinOp $node): mixed {
        $leftVal = $this->visit($node->left);
        $rightVal = $this->visit($node->right);

        return match ($node->op->type) {
            TokenType::PLUS  => $leftVal + $rightVal,
            TokenType::MINUS => $leftVal - $rightVal,
            TokenType::MUL   => $leftVal * $rightVal,
            TokenType::DIV   => intdiv($leftVal, $rightVal), // Pascal DIV is integer division
            default => throw new Exception("Unknown operator"),
        };
    }

    private function visitUnaryOp(UnaryOp $node): mixed {
        $val = $this->visit($node->expr);
        if ($node->op->type === TokenType::PLUS) {
            return +$val;
        } elseif ($node->op->type === TokenType::MINUS) {
            return -$val;
        }
        return $val;
    }

    private function visitNum(Num $node): int {
        return (int)$node->token->value;
    }

    private function visitVarNode(VarNode $node): mixed {
        $varName = strtolower($node->token->value);
        if (!array_key_exists($varName, $this->GLOBAL_SCOPE)) {
            throw new Exception("Variable '{$varName}' not declared.");
        }
        return $this->GLOBAL_SCOPE[$varName];
    }

    private function visitAssign(Assign $node): void {
        $varName = strtolower($node->left->token->value);
        if (!array_key_exists($varName, $this->GLOBAL_SCOPE)) {
            throw new Exception("Variable '{$varName}' not declared.");
        }
        $value = $this->visit($node->right);
        $this->GLOBAL_SCOPE[$varName] = $value;
    }

    private function visitWriteln(Writeln $node): void {
        $val = $this->visit($node->expr);
        echo $val . PHP_EOL;
    }

    private function visitIfNode(IfNode $node): void {
        // In this simple Pascal subset, we treat any non-zero value as true
        $conditionValue = $this->visit($node->condition);
        
        if ($conditionValue != 0) {
            $this->visit($node->thenBranch);
        } elseif ($node->elseBranch !== null) {
            $this->visit($node->elseBranch);
        }
    }

    public function interpret(): void {
        $tree = $this->parser->parse();
        $this->visit($tree);
    }
}

// -----------------------------------------------------------------------------
// MAIN EXECUTION
// -----------------------------------------------------------------------------

// Sample Pascal Code
// Calculates Factorial of 5 and tests basic math/if logic
$sourceCode = <<<'PASCAL'
PROGRAM TestPascal;
VAR
    number, result, i : INTEGER;
    x : INTEGER;
BEGIN
    number := 5;
    result := 1;
    i := number;

    x := (10 + 2) * 3;
    writeln(x);

    IF x - 36 THEN
        writeln(1)
    ELSE
        writeln(0);

    { Factorial Loop Simulation using recursion not available, unrolled manual or assume loop }
    { Since the prompt asked for WHILE *OR* IF, we implemented IF. 
      Let's do a simple calculation sequence to demonstrate logic. }
    
    result := result * 5;
    result := result * 4;
    result := result * 3;
    result := result * 2;
    
    writeln(result)
END.
PASCAL;

try {
    echo "--- Source Code ---" . PHP_EOL;
    echo $sourceCode . PHP_EOL;
    echo "--- Output ---" . PHP_EOL;

    $lexer = new Lexer($sourceCode);
    $parser = new Parser($lexer);
    $interpreter = new Interpreter($parser);
    $interpreter->interpret();

} catch (Exception $e) {
    echo "Error: " . $e->getMessage() . PHP_EOL;
}
