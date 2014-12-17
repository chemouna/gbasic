
package main
/**
  An interpreter for Basic written in go (in a single file)
**/

import(
    "path"
    "os"
    "flag"
    "fmt"
    "io/ioutil"
    "strings"
    "unicode"
	"bufio"
	"strconv"
)

func usage() {
    fileName := path.Base(os.Args[0])
    fmt.Println("Usage of", fileName, ":")
    fmt.Println(" Where <script> is a relative path to .gasic script to run.")
    fmt.Println()
    fmt.Println("Flags:")
    flag.PrintDefaults()
}


//--------------------- Gbasic interpreter ----------------------
type Gbasic struct {
    variables map[string]Value 
    labels map[string]int
	currentStatement int
	lineInScanner *bufio.Scanner
}

func NewGbasic() *Gbasic {
    g := new(Gbasic)
    g.variables = make(map[string]Value)
    g.labels = make(map[string]int)
	g.lineInScanner = bufio.NewScanner(os.Stdin)
    return g
}

var gBasic *Gbasic

func (g *Gbasic) interpret(src string) {
    //Tokens []*token.Token tokenize(src)
    tokens := tokenize(src)

    parser := Parser{tokens, 0}

	statements := parser.parse(g.labels);

	//Interpret all statement until we're done
	g.currentStatement = 0
	//fmt.Printf(" in interpret after parsing statements are of size = %d ", len(statements))
	for g.currentStatement < len(statements) {
		thisStatement := g.currentStatement
		g.currentStatement++
		statements[thisStatement].execute()
	}
	//fmt.Println(" end of interpret execute statement loop ")
}


func main() {
    //TODO: show usage and quiet if no basic script to interpret is provided 
    flag.Usage = usage
    flag.Parse()

    if(flag.NArg() != 1) {
        flag.Usage()
        return
    }

    src, err := ioutil.ReadFile(flag.Arg(0))    
    if err != nil {
        fmt.Fprintln(os.Stderr, err)
        return
    }

    // shld i have this method in a gasic class that gets instantiated here ?   
    gBasic= NewGbasic() 
    gBasic.interpret(string(src))
    
}

/**
 * tokenize takes a script as a string of characters and chunks it into
 * a sequence of tokens. Each token is a meaningful unit of program, like a
 * variable name, a number, a string, or an operator.
 */
func tokenize(source string) []Token {
    var tokens []Token 

    state := DEFAULT_STATE;
    token := ""
    // Many tokens are a single character, like operators and ().
    charTokens := "\n=+-*/<>()"
    tokenTypes := [...]TokenType{LINE, EQUALS,
        OPERATOR, OPERATOR, OPERATOR,
        OPERATOR, OPERATOR, OPERATOR,
        LEFT_PAREN, RIGHT_PAREN}        

	for i := 0; i < len(source); i++ {
		c := rune(source[i])
		//fmt.Printf(" in source range for loop current i = %d \n", i)
        switch (state) {
            case DEFAULT_STATE:
                //check if c is in charTokens   
                if strings.ContainsRune(charTokens, c) {
                    tokens = append(tokens, Token{string(c), 
                                        tokenTypes[strings.IndexRune(charTokens, c)]})  
                } else if unicode.IsLetter(c) {
                    token += string(c)
                    state = WORD_STATE
                } else if unicode.IsDigit(c) {
                    token += string(c)
                    state = NUMBER_STATE
                } else if c == '"' { //a start of a string since we have a quote
                    state = STRING_STATE
                } else if c == '\'' { // a comment starts with '
                    state = COMMENT_STATE       
                }
                break
            case WORD_STATE:
                 _LETTER_OR_DIGIT := []*unicode.RangeTable{
                    unicode.Letter,
                    unicode.Digit,
                }
                if unicode.IsOneOf(_LETTER_OR_DIGIT, c) {
                    token += string(c)
                } else if c == ':' {
                    tokens = append(tokens, Token{token, LABEL})
                    token = ""  
                    state = DEFAULT_STATE
                } else { //another token that can be added to a word -> marks end of word 
                    tokens = append(tokens, Token{token, WORD})
                    token = ""
                    state = DEFAULT_STATE
                    i--
					//fmt.Printf("decrementing i -> range shld go back one step behind i = %d \n ", i)
                }
                break
            case NUMBER_STATE:
                // HACK: Negative numbers and floating points aren't supported.
                // To get a negative number, just do 0 - <your number>.
                // To get a floating point, divide.
                //TODO: improve this by adding them 
                if unicode.IsDigit(c) {
                    token += string(c)  
                } else {
                    tokens = append(tokens, Token{token, NUMBER})
                    token = ""
                    state = DEFAULT_STATE
                    i--; // Reprocess this character in the default state.
                }
                break
            case STRING_STATE:
                if c == '"' { //end of string
                    tokens = append(tokens, Token{token, STRING})
                    token = ""
                    state = DEFAULT_STATE
                } else {
                    token += string(c)
                }
                break
            case COMMENT_STATE:
                if c == '\n' { //end of comment 
                    state = DEFAULT_STATE
                }   
                break               
        }
    }
	//fmt.Println(" end of tokenize loop ")
    return tokens
}

//------------------------- Token data -------------------------------

type Token struct {
    text string
    tokenType TokenType
}

/**
 * This defines the different kinds of tokens or meaningful chunks of code
 * that the parser knows how to consume. These let us distinguish, for
 * example, between a string "foo" and a variable named "foo".
 * 
 * HACK: A typical tokenizer would actually have unique token types for
 * each keyword (print, goto, etc.) so that the parser doesn't have to look
 * at the names, but gasic is a little more crude.-> TODO: improve this later
 */
type TokenType int
const (
    WORD TokenType = iota
    NUMBER 
    STRING
    LABEL
    LINE 
    EQUALS
    OPERATOR 
    LEFT_PAREN
    RIGHT_PAREN 
    EOF 
)


type TokenizeState int
const (
     DEFAULT_STATE TokenizeState = iota 
     WORD_STATE
     NUMBER_STATE
     STRING_STATE
     COMMENT_STATE
)


//---------------------- Parsing ------------------------
/**
This defines our parse, which takes a sequence of tokens and generate an AST.
It's a recursive descent parser .. Also we store line numbers for each label 
in the program to be able to go to it 
**/
type Parser struct {
    tokens []Token 
    position int
}


 /**
 * The top-level function to start parsing. This will keep consuming
 * tokens and routing to the other parse functions for the different
 * grammar syntax until we run out of code to parse.
 *
 */
func (p *Parser) parse(labels map[string]int) []Statement {
    var statements []Statement
    for { //adding i temp here
        for p.matchByType(LINE) {} // Ignore empty lines. //not sure this is the construct i shld be using

        if p.matchByType(LABEL) {
			//fmt.Printf(" parsing a label \n")
			labels[p.last(1).text] = len(statements) //mark the index of the statement after the label
        } else if p.matchByMultipleTypes(WORD, EQUALS) {
			//fmt.Printf(" parsing a word & equals \n")
            name := p.last(2).text 
            value := p.expression()
            statements = append(statements, AssignmentStatement{name, value})
        } else if p.matchByName("print") {
			//fmt.Printf(" parsing a print \n")
			statements = append(statements, PrintStatement{p.expression()})
		} else if p.matchByName("input") {
			//fmt.Printf(" parsing input \n")
			statements = append(statements, InputStatement{p.consumeByType(WORD).text})
		} else if p.matchByName("goto") {
			//fmt.Printf(" parsing goto \n ")
			statements = append(statements, GotoStatement{p.consumeByType(WORD).text})
		} else if p.matchByName("if") {
			//fmt.Printf(" parsing if \n")
			condition := p.expression()
			p.consumeByName("then")
			label := p.consumeByType(WORD).text
			statements = append(statements, IfThenStatement{condition, label})
		} else {
			//fmt.Println(" shld break now out of parse's for loop")
			break
		}
    }
	//fmt.Println(" statements at the end of parse : ")
	//fmt.Println(statements)
	return statements
}

/**
 * Consumes the next token if it's the given type. If not, throws an
 * exception. This is for cases where the parser demands a token of a
 * certain type in a certain position, for example a matching ) after
 * an opening (.
 */
func (p *Parser) consumeByType(type1 TokenType) Token {
	if p.get(0).tokenType != type1 {
		//throw new Error("Expected " + type + ".")
		panic(" Expected "+ string(type1) + ".")
	}
	p.position++
	return p.tokens[p.position]
}

func (p Parser) consumeByName(name string) Token {
	if !p.matchByName(name) {
		panic("Expected " + name + ".")
	}
	return p.last(1)
}

/**
 * Consumes the next token if it's a word token with the given name.
 */
func (p *Parser) matchByName(name string) bool {
    if p.get(0).tokenType != WORD {
        return false
    }
    if p.get(0).text != name {
        return false 
    }
    p.position++
    return true
}

 /**
 * Consumes the next two tokens if they are the given type (in order).
 * Consumes no tokens if either check fais.
 */
 func (p *Parser) matchByMultipleTypes(type1 TokenType, type2 TokenType) bool {
    if p.get(0).tokenType != type1 {
        return false
    }
    if p.get(1).tokenType != type2 {
        return false
    }
    p.position += 2 
    return true
 }

/**
 * Consumes the next token if it's the given type.
 **/
func (p *Parser) matchByType(type1 TokenType) bool {
    if p.get(0).tokenType != type1 {
        return false
    }
    p.position++
    return true
}

 /**
 * Gets a previously consumed token, indexing backwards. last(1) will
 * be the token just consumed, last(2) the one before that, etc.
 */
 func (p Parser) last(offset int) Token {
	 /*fmt.Printf(" calling last with offset %d , and current position is %d "+
			  " tokens size : %d + tokens : \n", offset, p.position, len(p.tokens));
	 fmt.Println(p.tokens)*/
	 if p.position - offset >= 0 {
	 	return p.tokens[p.position - offset]
	 }
	 return p.tokens[0]
 }  

/**
 * Gets an unconsumed token, indexing forward. get(0) will be the next
 * token to be consumed, get(1) the one after that, etc.
 */
func (p Parser) get(offset int) Token {
    if p.position + offset >= len(p.tokens) {
        return Token{"", EOF}
    }
    return p.tokens[p.position + offset]
}

// these functions each represent one grammatical part of the language. 
func (p Parser) expression() Expression {
    return p.operator()
}

 /**
 * Parses a series of binary operator expressions into a single
 * expression. In Jasic, all operators have the same predecence and
 * associate left-to-right. That means it will interpret:
 *    1 + 2 * 3 - 4 / 5
 * like:
 *    ((((1 + 2) * 3) - 4) / 5)
 * 
 * It works by building the expression tree one at a time. So, given
 * this code: 1 + 2 * 3, this will:
 * 
 * 1. Parse (1) as an atomic expression.
 * 2. See the (+) and start a new operator expression.
 * 3. Parse (2) as an atomic expression.
 * 4. Build a (1 + 2) expression and replace (1) with it.
 * 5. See the (*) and start a new operator expression.
 * 6. Parse (3) as an atomic expression.
 * 7. Build a ((1 + 2) * 3) expression and replace (1 + 2) with it.
 * 8. Return the last expression built.
 **/
func (p Parser) operator() Expression {
    expression := p.atomic()
	//fmt.Println(" operator exp ", expression.evaluate())
    // Keep building operator expressions as long as we have operators.
    for p.matchByType(OPERATOR) || p.matchByType(EQUALS) {
		operator := rune(p.last(1).text[1]) //not sure that this will get me equivalent to charAt(0)
		right := p.atomic()
		expression = OperatorExpression{expression, operator, right}
	}
	return expression
}

 /**
 * Parses an "atomic" expression. This is the highest level of
 * precedence and contains single literal tokens like 123 and "foo", as
 * well as parenthesized expressions. 
 **/
func (p Parser) atomic() Expression {
	if p.matchByType(WORD) {
		return VariableExpression{p.last(1).text}
	} else if p.matchByType(NUMBER) {
		if val, err := strconv.ParseFloat(p.last(1).text, 64); err == nil {
			return NumberValue{val}
		}
	} else if p.matchByType(STRING) {
		return StringValue{p.last(1).text}
	} else if p.matchByType(LEFT_PAREN) {
		expression := p.expression()
		p.consumeByType(RIGHT_PAREN)
		return expression
	}
	panic(" Couldn't parse this expression ")
}

/**
 * An operator expression evaluates two expressions and performs an arithmetic
 * operationon the results
 */
type OperatorExpression struct {
	left Expression
	operator rune
	right Expression
}

func (oe OperatorExpression) evaluate() Value {
	leftVal := oe.left.evaluate()
	rightVal := oe.right.evaluate()

	switch (oe.operator){
	case '=':
		if _, ok := leftVal.(NumberValue); ok {
			if leftVal.toNumber() == rightVal.toNumber() {
				return NumberValue{1}
			} else {
				return NumberValue{0}
			}
		}
	case '+':
		// Addition if the left argument is a number, otherwise do
		// string concatenation.
		if _, ok := leftVal.(NumberValue); ok {
			return NumberValue{leftVal.toNumber() + rightVal.toNumber()}
		} else {
			return StringValue{leftVal.toString() + rightVal.toString()}
		}
	case '-':
		return NumberValue{leftVal.toNumber() - rightVal.toNumber()}
	case '*':
		return NumberValue{leftVal.toNumber() * rightVal.toNumber()}
	case '/':
		return NumberValue{leftVal.toNumber() / rightVal.toNumber()}
	case '<':
		//compare depending on the type
		if _, ok := leftVal.(NumberValue); ok {
			if leftVal.toNumber() < rightVal.toNumber() {
				return NumberValue{1}
			} else {
				return NumberValue{0}
			}
		} else {
			if leftVal.toString() < rightVal.toString() {
				return NumberValue{1}
			} else {
				return NumberValue{0}
			}
		}
	case '>':
	 	//coerce on left's arg type, then compare
		if _, ok := leftVal.(NumberValue); ok {
			if leftVal.toNumber() > rightVal.toNumber() {
				return NumberValue{1}
			} else {
				return NumberValue{0}
			}
		} else {
			if leftVal.toString() > rightVal.toString() {
				return NumberValue{1}
			} else {
				return NumberValue{0}
			}
		}
	}
	panic(" Unknown operator")
}

//----

 /**
 * An assignment statement evaluates an expression and stores the result in
 * a variable.
 */
type AssignmentStatement struct {
    name string
    value Expression
}   

func (as AssignmentStatement) execute() {
    gBasic.variables[as.name] = as.value.evaluate()
}

type PrintStatement struct {
	expression Expression
}

func (ps PrintStatement) execute() {
	//fmt.Println(" print statement execuate called and shld print this : %s "+ ps.expression.evaluate().toString())
	fmt.Println(ps.expression.evaluate().toString())
}

type InputStatement struct {
	name string
}

type GotoStatement struct {
	label string
}

type IfThenStatement struct {
	condition Expression
	label string
}

func (gs GotoStatement) execute() {
	if val, ok := gBasic.labels[gs.label]; ok {
		gBasic.currentStatement = val
	}
}

func (its IfThenStatement) execute() {
	if val, ok := gBasic.labels[its.label]; ok {
		fval := its.condition.evaluate().toNumber()
		if fval != 0 {
			gBasic.currentStatement = val
		}
	}
}

func (is InputStatement) execute() {
	//maybe using a scanner is better suited ?
	gBasic.lineInScanner.Scan()
	input := gBasic.lineInScanner.Text()
	value, err := strconv.ParseFloat(input, 64)

	if err == nil {
		gBasic.variables[is.name] = NumberValue{value}
	} else {
		gBasic.variables[is.name] = StringValue{input}
	}
}

type NumberValue struct {
	value float64
}

func (nv NumberValue) toString() string {
	//return strconv.FormatFloat(nv.value, 'f', -1, 64)
	return fmt.Sprintf("%f", nv.value)
}

func (nv NumberValue) toNumber() float64 {
	return nv.value
}

//maybe here we shld pass receiver by ref (pointer)
func (nv NumberValue) evaluate() Value {
	return nv
}

type StringValue struct {
	value string
}

func (sv StringValue) toString() string {
	return sv.value
}

func (sv StringValue) toNumber() float64 {
	v, _ := strconv.ParseFloat(sv.value, 64)
	return v
}

func (sv StringValue) evaluate() Value {
	return sv
}

type VariableExpression struct {
	name string
}

func (ve VariableExpression) evaluate() Value {
	if  val, ok := gBasic.variables[ve.name]; ok {
		return val
	}
	return NumberValue{0} //not sure to by default return nb 0 ?
}

type Statement interface {
    execute()
}

type Value interface {
    toString() string 
    toNumber() float64 
}

type Expression interface {
    evaluate() Value
}




//TODO: do a test with some code early to verify already written code 


