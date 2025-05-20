// C语言词法分析器
#include <cstdio>
#include <cstring>
#include <iostream>
#include <map>
#include <stdexcept>
#include <string>
#include <fstream>
#include <sstream>
#include <vector>
#include <unordered_set>
using namespace std;
/* 不要修改这个标准输入函数 */
void read_prog(string& prog)
{
	char c;
	while(scanf("%c",&c)!=EOF){
		prog += c;
	}
}
/* 你可以添加其他函数 */

enum class SymbolType{
    TERMINAL,
    NONTERMINAL,

    EPSILON,
    ENDMARK
};

class Symbol {
public:
    SymbolType type;
    string value;

    static const unordered_set<string> terminalSymbols;

    Symbol() : type(SymbolType::NONTERMINAL), value("") {}

    explicit Symbol(SymbolType t, const string& v) : type(t), value(v) {}

    explicit Symbol(const string& v) : value(v) {
        if (v == "E") {
            type = SymbolType::EPSILON;
        } else if (v == "$") {
            type = SymbolType::ENDMARK;
        } else if (terminalSymbols.count(v)) {
            type = SymbolType::TERMINAL;
        } else {
            type = SymbolType::NONTERMINAL;
        }
    }

    ~Symbol() {}

    bool isTerminal() const { return type == SymbolType::TERMINAL; }
    bool isNonTerminal() const { return type == SymbolType::NONTERMINAL; }
    bool isEpsilon() const { return type == SymbolType::EPSILON; }
    bool isEndMark() const { return type == SymbolType::ENDMARK; }

    bool operator==(const Symbol& other) const {
        return type == other.type && value == other.value;
    }

    bool operator!=(const Symbol& other) const {
        return !(*this == other);
    }
};

const unordered_set<string> Symbol::terminalSymbols = {
    "{", "}", "(", ")", "if", "then", "else", "while",
    "ID", "=", "<", ">", "<=", ">=", "==",
    "+", "-", "*", "/", ";", "NUM"
};

class Production{
public:
    Symbol left;
    vector<Symbol> right;

    Production(const string& leftStr, const vector<string>& rightStrs) {
        left = Symbol(leftStr);
        for (const auto& str : rightStrs) {
            right.emplace_back(str);
        }
    }

    Production(string rule){
        istringstream iss(rule);
        string leftSymbol, arrow;
        iss >> leftSymbol >> arrow;

        if (arrow != "->") {
            throw runtime_error("Invalid production missing '->'");
        }

        left = Symbol(leftSymbol);

        string symbol;
        while (iss >> symbol) {
            right.emplace_back(symbol);
        }
    }

    friend ostream& operator<<(ostream& os, const Production& p) {
        os << p.left.value << " -> ";
        for (const auto& sym : p.right) {
            os << sym.value << " ";
        }
        return os;
    }
};

vector<Production> parseProductions(const string& line){
    auto barPos = line.find("|");
    if (barPos == string::npos) {
        return { Production(line) };
    }

    vector<Production> productions;

    auto arrowPos = line.find("->");
    if (arrowPos == string::npos) {
        throw runtime_error("Invalid production missing '->'");
    }

    string leftStr = line.substr(0, arrowPos);
    string rightStr = line.substr(arrowPos + 2);

    string leftTrimmed;
    istringstream lss(leftStr);
    lss >> leftTrimmed;

    istringstream rss(rightStr);
    string token;
    vector<string> currentRight;
    while (rss >> token) {
        if (token == "|") {
            productions.emplace_back(leftTrimmed, currentRight);
            currentRight.clear();
        } else {
            currentRight.push_back(token);
        }
    }

    if (!currentRight.empty()) {
        productions.emplace_back(leftTrimmed, currentRight);
    }

    return productions;
}

class Grammar{
public:
    vector<Production> productions;

    Grammar(string& grammar){
        istringstream iss(grammar);
        string line;
        while (getline(iss, line)) {
            auto prods = parseProductions(line);
            productions.insert(productions.end(), prods.begin(), prods.end());
        }
    }

    friend ostream& operator<<(ostream& os, const Grammar& g) {
        for (const auto& prod : g.productions) {
            os << prod << endl;
        }
        return os;
    }
};

class LL1Grammar : public Grammar {
public:

    unordered_map<string, unordered_set<string>> firstSets;
    unordered_map<string, unordered_set<string>> followSets;
    unordered_map<string, unordered_map<string, Production>> parseTable;


    LL1Grammar(string& grammar) : Grammar(grammar) {
        computeFirstSets();
        computeFollowSets();
        computeParseTable();
    }

    void computeFirstSets() {
        for (const auto& prod : productions) {
            firstSets[prod.left.value].insert(prod.right[0].value);
        }

        bool changed;
        do {
            changed = false;
            for (const auto& prod : productions) {
                const auto& left = prod.left.value;
                for (const auto& symbol : prod.right) {
                    if (symbol.isTerminal()) {
                        if (firstSets[left].insert(symbol.value).second) {
                            changed = true;
                        }
                        break;
                    } else if (symbol.isEpsilon()) {
                        break;
                    } else {
                        const auto& firstSet = firstSets[symbol.value];
                        size_t oldSize = firstSets[left].size();
                        firstSets[left].insert(firstSet.begin(), firstSet.end());
                        if (firstSets[left].size() > oldSize) {
                            changed = true;
                        }
                    }
                }
            }
        } while (changed);
    }

    void computeFollowSets() {
        followSets[productions[0].left.value].insert("$");

        bool changed;
        do {
            changed = false;
            for (const auto& prod : productions) {
                const auto& left = prod.left.value;
                for (size_t i = 0; i < prod.right.size(); ++i) {
                    const auto& symbol = prod.right[i];
                    if (symbol.isNonTerminal()) {
                        if (i + 1 < prod.right.size()) {
                            const auto& nextSymbol = prod.right[i + 1];
                            if (nextSymbol.isTerminal()) {
                                if (followSets[symbol.value].insert(nextSymbol.value).second) {
                                    changed = true;
                                }
                            } else {
                                const auto& firstSet = firstSets[nextSymbol.value];
                                size_t oldSize = followSets[symbol.value].size();
                                followSets[symbol.value].insert(firstSet.begin(), firstSet.end());
                                if (followSets[symbol.value].size() > oldSize) {
                                    changed = true;
                                }
                                if (firstSet.count("E")) {
                                    if (followSets[symbol.value].insert(followSets[left].begin(), followSets[left].end()).second) {
                                        changed = true;
                                    }
                                }
                            }
                        } else {
                            size_t oldSize = followSets[symbol.value].size();
                            followSets[symbol.value].insert(followSets[left].begin(), followSets[left].end());
                            if (followSets[symbol.value].size() > oldSize) {
                                changed = true;
                            }
                        }
                    }
                }
            }
        } while (changed);
    }

    void computeParseTable() {
        for (const auto& prod : productions) {
            const auto& left = prod.left.value;
            for (const auto& symbol : prod.right) {
                if (symbol.isTerminal()) {
                    parseTable[left][symbol.value] = prod;
                    break;
                } else if (symbol.isEpsilon()) {
                    for (const auto& followSymbol : followSets[left]) {
                        parseTable[left][followSymbol] = prod;
                    }
                    break;
                } else {
                    const auto& firstSet = firstSets[symbol.value];
                    for (const auto& terminal : firstSet) {
                        parseTable[left][terminal] = prod;
                    }
                    if (firstSet.count("E")) {
                        for (const auto& followSymbol : followSets[left]) {
                            parseTable[left][followSymbol] = prod;
                        }
                    }
                }
            }
        }
    }
    friend ostream& operator<<(ostream& os, const LL1Grammar& g) {
        os << "First Sets:" << endl;
        for (const auto& pair : g.firstSets) {
            os << pair.first << ": ";
            for (const auto& symbol : pair.second) {
                os << symbol << " ";
            }
            os << endl;
        }

        os << "Follow Sets:" << endl;
        for (const auto& pair : g.followSets) {
            os << pair.first << ": ";
            for (const auto& symbol : pair.second) {
                os << symbol << " ";
            }
            os << endl;
        }

        os << "Parse Table:" << endl;
        for (const auto& pair : g.parseTable) {
            os << pair.first << ": ";
            for (const auto& innerPair : pair.second) {
                os << innerPair.first << " -> " << innerPair.second.left.value << endl;
            }
        }

        return os;
    }

};

string exp_gramar = R"(program -> compoundstmt
stmt ->  ifstmt  |  whilestmt  |  assgstmt  |  compoundstmt
compoundstmt ->  { stmts }
stmts ->  stmt stmts   |   E
ifstmt ->  if ( boolexpr ) then stmt else stmt
whilestmt ->  while ( boolexpr ) stmt
assgstmt ->  ID = arithexpr ;
boolexpr  ->  arithexpr boolop arithexpr
boolop ->   <  |  >  |  <=  |  >=  | ==
arithexpr  ->  multexpr arithexprprime
arithexprprime ->  + multexpr arithexprprime  |  - multexpr arithexprprime  |   E
multexpr ->  simpleexpr  multexprprime
multexprprime ->  * simpleexpr multexprprime  |  / simpleexpr multexprprime  |   E
simpleexpr ->  ID  |  NUM  |  ( arithexpr ))";

void Analysis()
{
	string prog;
	read_prog(prog);
	/* 骚年们 请开始你们的表演 */
    /********* Begin *********/
    Grammar grammar(exp_gramar);
    
    

    /********* End *********/
	
}

int main()
{
    Analysis();
    return 0;
}