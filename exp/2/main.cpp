// C语言词法分析器
#include <cstddef>
#include <cstdio>
#include <cstring>
#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_set>
#include <vector>
using namespace std;
/* 不要修改这个标准输入函数 */
void read_prog(string &prog) {
    char c;
    while (scanf("%c", &c) != EOF) {
        prog += c;
    }
}
/* 你可以添加其他函数 */

enum class SymbolType {
    TERMINAL,
    NONTERMINAL,

    EPSILON,
    ENDMARK
};

class Symbol {
  public:
    SymbolType type;
    string     value;

    static const unordered_set<string> terminalSymbols;

    Symbol() : type(SymbolType::NONTERMINAL) {}

    explicit Symbol(SymbolType t) : type(t) {
        if (t == SymbolType::EPSILON) {
            value = "E";
        } else if (t == SymbolType::ENDMARK) {
            value = "$";
        } else {
            throw runtime_error("Cannot create Symbol without a value for TERMINAL or NONTERMINAL type");
        }
    }

    explicit Symbol(const string &v) : value(v) {
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

    ~Symbol() = default;

    bool isTerminal() const { return type == SymbolType::TERMINAL; }
    bool isNonTerminal() const { return type == SymbolType::NONTERMINAL; }
    bool isEpsilon() const { return type == SymbolType::EPSILON; }
    bool isEndMark() const { return type == SymbolType::ENDMARK; }

    Symbol &operator=(const Symbol &other) {
        if (this != &other) {
            type  = other.type;
            value = other.value;
        }
        return *this;
    }

    bool operator==(const Symbol &other) const {
        return type == other.type && value == other.value;
    }

    bool operator!=(const Symbol &other) const { return !(*this == other); }

    size_t hash() const {
        return std::hash<string>()(value) ^ (std::hash<int>()(static_cast<int>(type)) << 1);
    }

    friend ostream &operator<<(ostream &os, const Symbol &s) {
        if (s.isEpsilon()) {
            os << "E";
        } else if (s.isEndMark()) {
            os << "$";
        } else {
            os << s.value;
        }
        return os;
    }
};

template<>
struct std::hash<Symbol> {
    size_t operator()(const Symbol& s) const noexcept {
        return s.hash();
    }
};

const unordered_set<string> Symbol::terminalSymbols = {
    "{",
    "}",
    "(",
    ")",
    "if",
    "then",
    "else",
    "while",
    "ID",
    "=",
    "<",
    ">",
    "<=",
    ">=",
    "==",
    "+",
    "-",
    "*",
    "/",
    ";",
    "NUM"};

class Production {
  public:
    Symbol         left;
    vector<Symbol> right;

    Production() = default;

    Production(const string &leftStr, const vector<string> &rightStrs) {
        left = Symbol(leftStr);
        for (const auto &str : rightStrs) {
            right.emplace_back(str);
        }
    }

    explicit Production(const string& rule) {
        istringstream iss(rule);
        string        leftSymbol, arrow;
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

    Production &operator=(const Production &other) {
        if (this != &other) {
            left  = other.left;
            right = other.right;
        }
        return *this;
    }

    friend ostream &operator<<(ostream &os, const Production &p) {
        os << p.left.value << " -> ";
        for (const auto &sym : p.right) {
            os << sym.value << " ";
        }
        return os;
    }
};

vector<Production> parseProductions(const string &line) {
    auto barPos = line.find('|');
    if (barPos == string::npos) {
        return {Production(line)};
    }

    vector<Production> productions;

    auto arrowPos = line.find("->");
    if (arrowPos == string::npos) {
        throw runtime_error("Invalid production missing '->'");
    }

    string leftStr  = line.substr(0, arrowPos);
    string rightStr = line.substr(arrowPos + 2);

    string        leftTrimmed;
    istringstream lss(leftStr);
    lss >> leftTrimmed;

    istringstream  rss(rightStr);
    string         token;
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

class Grammar {
  public:
    Symbol startSymbol;
    unordered_set<Symbol> terminals;
    unordered_set<Symbol> nonTerminals;
    unordered_map<Symbol, unordered_set<Symbol>> firstSets;
    unordered_map<Symbol, unordered_set<Symbol>> followSets;

    vector<Production> productions;

    explicit Grammar(const string &grammar) {
        istringstream iss(grammar);
        string        line;
        while (getline(iss, line)) {
            auto prods = parseProductions(line);
            productions.insert(productions.end(), prods.begin(), prods.end());
        }

        if (productions.empty()) {
            throw runtime_error("No productions found in the grammar.");
        }

        startSymbol = productions[0].left;
        for (const auto &prod : productions) {
            nonTerminals.insert(prod.left);
            for (const auto &sym : prod.right) {
                if (sym.isNonTerminal()) {
                    nonTerminals.insert(sym);
                }
                if (sym.isTerminal()) {
                    terminals.insert(sym);
                }
            }
        }

        computeFirstSets();
        computeFollowSets();
    }

    unordered_set<Symbol> computeFirstSet(const Symbol &symbol) {
        if (symbol.isTerminal() || symbol.isEpsilon()) {
            return {symbol};
        }

        if (firstSets.count(symbol) && !firstSets[symbol].empty()) {
            return firstSets[symbol];
        }

        unordered_set<Symbol> result;
        for (const auto &prod : productions) {
            if (prod.left.value != symbol.value)
                continue;

            bool allHaveEpsilon = true;
            for (const auto &rightSymbol : prod.right) {
                if (rightSymbol.isEpsilon() || rightSymbol.isTerminal()) {
                    result.insert(rightSymbol);
                    allHaveEpsilon = false;
                    break;
                } else if (rightSymbol.isNonTerminal()) {
                    auto subFirstSet = computeFirstSet(rightSymbol);
                    bool hasEpsilon = false;
                    for (const auto &sym : subFirstSet) {
                        if (!sym.isEpsilon()) {
                            result.insert(sym);
                        } else {
                            hasEpsilon = true;
                        }
                    }

                    if (!hasEpsilon) {
                        allHaveEpsilon = false;
                        break;
                    }
                }
            }

            if (allHaveEpsilon) {
                result.insert(Symbol(SymbolType::EPSILON));
            }
        }

        firstSets[symbol].insert(result.begin(), result.end());
        return result;
    }

    unordered_set<Symbol> computeFirstSetForSequence(const vector<Symbol> &sequence, size_t index = 0) {
        if (sequence.size() - index == 1){
            return computeFirstSet(sequence.front());
        }

        if (index >= sequence.size()) {
            throw std::runtime_error("Index out of range for sequence.");
        }
        
        std::unordered_set<Symbol> result;
        for (size_t i = index; i < sequence.size(); ++i) {
            const Symbol& sym = sequence[i];
            if (sym.isTerminal()) {
                result.insert(sym);
                break;
            }
            if (sym.isNonTerminal()) {
                auto firstSet = computeFirstSet(sym);
                bool hasEpsilon = false;
                for (const auto& s : firstSet) {
                    if (s.isEpsilon()) {
                        hasEpsilon = true;
                    } else {
                        result.insert(s);
                    }
                }
                if (!hasEpsilon) {
                    break;
                }
                if (i == sequence.size() - 1) {
                    result.insert(Symbol(SymbolType::EPSILON));
                }
            }
        }
        
        return result;

    }

    void computeFirstSets() {
        firstSets.clear();
        for (const auto &prod : productions) {
            computeFirstSet(prod.left);
        }
    }

    unordered_set<Symbol> computeFollowSet(const Symbol &symbol) {
        if (followSets.count(symbol) && !followSets[symbol].empty()) {
            return followSets[symbol];
        }

        unordered_set<Symbol>& result = followSets[symbol];

        for (const auto &prod : productions) {
            for (auto it = prod.right.begin(); it != prod.right.end(); ++it) {
                if (it->isNonTerminal() && it->value == symbol.value) {
                    auto beta = it + 1;
                    if (beta == prod.right.end()) {
                        if (prod.left.value != symbol.value) {
                            auto leftFollowSet = computeFollowSet(prod.left);
                            result.insert(leftFollowSet.begin(), leftFollowSet.end());
                        }
                    } else {
                        if (beta->isTerminal()) {
                            result.insert(*beta);
                        } else if (beta->isNonTerminal()) {
                            auto& firstSet = firstSets[*beta];
                            // size_t index = it - prod.right.begin();
                            // auto firstSet = computeFirstSetForSequence(prod.right, index);
                            bool hasEpsilon = false;
                            for (const auto &sym : firstSet) {
                                if (!sym.isEpsilon()) {
                                    result.insert(sym);
                                } else {
                                    hasEpsilon = true;
                                }
                            }

                            if (hasEpsilon && prod.left.value != symbol.value) {
                                auto leftFollowSet = computeFollowSet(prod.left);
                                result.insert(leftFollowSet.begin(), leftFollowSet.end());
                            }
                        }
                    }
                }
            }
        }

        followSets[symbol] = result;
        return result;
    }

    void computeFollowSets() {
        if (!productions.empty()) {
            followSets[startSymbol].insert(Symbol(SymbolType::ENDMARK));
        }

        for (const auto &nt : nonTerminals) {
            computeFollowSet(nt);
        }
    }

    friend ostream &operator<<(ostream &os, const Grammar &g) {
        os << "Productions:" << endl;
        for (const auto &prod : g.productions) {
            os << prod << endl;
        }

        os << "------------------------" << endl;

        os << "First Sets:" << endl;
        for (const auto &pair : g.firstSets) {
            os << pair.first << ": ";
            for (const auto &symbol : pair.second) {
                os << symbol << " ";
            }
            os << endl;
        }

        os << "------------------------" << endl;

        os << "Follow Sets:" << endl;
        for (const auto& pair : g.followSets) {
            os << pair.first << ": ";
            for (const auto& symbol : pair.second) {
                os << symbol << " ";
            }
            os << endl;
        }
        return os;
    }
};

class LL1Grammar : public Grammar {
  public:
    unordered_map<Symbol, unordered_map<Symbol, Production>> parseTable;

    explicit LL1Grammar(const string &grammar) : Grammar(grammar) { computeParseTable(); }

    void computeParseTable() {
        for (const auto &prod : productions) {
            const auto& left = prod.left;
            auto rightFirstSet = computeFirstSetForSequence(prod.right);
            bool rightHasEpsilon = false;
            for (const auto& symbol : rightFirstSet) {
                if (symbol.isEpsilon()) {
                    rightHasEpsilon = true;
                } else if (symbol.isTerminal()) {
                    parseTable[left][symbol] = prod;
                }
            }
            if (rightHasEpsilon) {
                for (const auto& followSymbol : followSets[left]) {
                    if (followSymbol.isTerminal()) {
                        parseTable[left][followSymbol] = prod;
                    }
                    if (followSymbol.isEndMark()) {
                        parseTable[left][followSymbol] = prod;
                    }
                }
            }
        }
    }

    friend ostream &operator<<(ostream &os, const LL1Grammar &g) {
        os << "Parse Table:" << endl;
        for (const auto &pair : g.parseTable) {
            os << pair.first << ": " << endl;
            for (const auto &prod : pair.second) {
                os << pair.first << " -> " << prod.first << endl;
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

void Analysis() {
    string prog;
    // read_prog(prog);
    /* 骚年们 请开始你们的表演 */
    /********* Begin *********/
    LL1Grammar grammar(exp_gramar);
    cout << grammar;

    /********* End *********/
}

int main() {
    Analysis();
    return 0;
}