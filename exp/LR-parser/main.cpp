// C语言词法分析器
#include <algorithm>
#include <cstddef>
#include <cstdio>
#include <fstream>
#include <iostream>
#include <map>
#include <memory>
#include <sstream>
#include <stack>
#include <stdexcept>
#include <string>
#include <unordered_map>
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
    string value;

    static const unordered_set<string> terminalSymbols;

    Symbol() : type(SymbolType::NONTERMINAL) {}

    explicit Symbol(SymbolType t) : type(t) {
        if (t == SymbolType::EPSILON) {
            value = "E";
        } else if (t == SymbolType::ENDMARK) {
            value = "$";
        } else {
            throw runtime_error(
                "Cannot create Symbol without a value for TERMINAL "
                "or NONTERMINAL type");
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

    [[nodiscard]] bool isTerminal() const {
        return type == SymbolType::TERMINAL;
    }
    [[nodiscard]] bool isNonTerminal() const {
        return type == SymbolType::NONTERMINAL;
    }
    [[nodiscard]] bool isEpsilon() const { return type == SymbolType::EPSILON; }
    [[nodiscard]] bool isEndMark() const { return type == SymbolType::ENDMARK; }

    Symbol &operator=(const Symbol &other) {
        if (this != &other) {
            type = other.type;
            value = other.value;
        }
        return *this;
    }

    bool operator==(const Symbol &other) const {
        return type == other.type && value == other.value;
    }

    bool operator!=(const Symbol &other) const { return !(*this == other); }

    [[nodiscard]] size_t hash() const {
        return std::hash<string>()(value) ^
               (std::hash<int>()(static_cast<int>(type)) << 1);
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

template <> struct std::hash<Symbol> {
    size_t operator()(const Symbol &s) const noexcept { return s.hash(); }
};

const unordered_set<string> Symbol::terminalSymbols = {
    "{", "}",  "(",  ")",  "if", "then", "else", "while", "ID", "=",  "<",
    ">", "<=", ">=", "==", "+",  "-",    "*",    "/",     ";",  "NUM"};

class Production {
  public:
    int id{-1};
    static int nextId;
    Symbol left;
    vector<Symbol> right;

    Production() = default;

    Production(const string &leftStr, const vector<string> &rightStrs, const int id = -1) {
        left = Symbol(leftStr);
        for (const auto &str : rightStrs) {
            right.emplace_back(str);
        }
        if (id != -1) {
            this->id = id;
        } else {
            this->id = nextId++;
        }
    }

    explicit Production(const string &rule, const int id = -1) {
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
        if (id != -1) {
            this->id = id;
        } else {
            this->id = nextId++;
        }
    }

    Production &operator=(const Production &other) {
        if (this != &other) {
            left = other.left;
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

    [[nodiscard]] string toString() const {
        ostringstream oss;
        oss << left.value << " -> ";
        for (const auto &sym : right) {
            oss << sym.value << " ";
        }
        return oss.str();
    }
};

int Production::nextId = 1;

template <> struct std::hash<Production> {
    size_t operator()(const Production &prod) const noexcept {
        return hash<string>()(prod.toString());
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

vector<pair<Symbol, int>> tokenize(const string &input) {
    vector<pair<Symbol, int>> tokens;
    istringstream iss(input);
    string line;
    int lineNumber = 1;

    while (getline(iss, line)) {
        istringstream lineStream(line);
        string token;
        bool hasTokensInLine = false;

        while (lineStream >> token) {
            tokens.emplace_back(Symbol(token), lineNumber);
            hasTokensInLine = true;
        }

        if (hasTokensInLine || !line.empty()) {
            lineNumber++;
        }
    }

    tokens.emplace_back(Symbol(SymbolType::ENDMARK), lineNumber - 1);
    return tokens;
}

struct ParseTreeNode {
    Symbol symbol;
    vector<shared_ptr<ParseTreeNode>> children;

    explicit ParseTreeNode(const Symbol &sym) : symbol(sym) {}

    void addChild(shared_ptr<ParseTreeNode> &child) {
        children.push_back(child);
    }
};

void printParseTree(const shared_ptr<ParseTreeNode> &node, const int depth = 0) {
    if (!node)
        return;
    for (int i = 0; i < depth; ++i) {
        cout << "\t";
    }
    cout << node->symbol.value << endl;
    for (const auto &child : node->children) {
        printParseTree(child, depth + 1);
    }
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
        string line;
        while (getline(iss, line)) {
            if (line.empty())
                continue;
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
        // FIXME Cannot handle left recursion
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

    unordered_set<Symbol>
    computeFirstSetForSequence(const vector<Symbol> &sequence,
                               size_t index = 0) {
        if (sequence.size() - index == 1) {
            return computeFirstSet(sequence.front());
        }

        if (index >= sequence.size()) {
            throw std::runtime_error("Index out of range for sequence.");
        }

        std::unordered_set<Symbol> result;
        for (size_t i = index; i < sequence.size(); ++i) {
            const Symbol &sym = sequence[i];
            if (sym.isTerminal()) {
                result.insert(sym);
                break;
            }
            if (sym.isNonTerminal()) {
                auto firstSet = computeFirstSet(sym);
                bool hasEpsilon = false;
                for (const auto &s : firstSet) {
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

    unordered_set<Symbol> computeFollowSet(const Symbol &symbol,
                                           unordered_set<Symbol> &visited) {
        if (visited.count(symbol))
            return followSets[symbol];
        visited.insert(symbol);

        unordered_set<Symbol> &result = followSets[symbol];

        for (const auto &prod : productions) {
            for (auto it = prod.right.begin(); it != prod.right.end(); ++it) {
                if (it->isNonTerminal() && it->value == symbol.value) {
                    auto beta = it + 1;
                    if (beta == prod.right.end()) {
                        if (prod.left.value != symbol.value) {
                            auto leftFollowSet =
                                computeFollowSet(prod.left, visited);
                            result.insert(leftFollowSet.begin(),
                                          leftFollowSet.end());
                        }
                    } else {
                        if (beta->isTerminal()) {
                            result.insert(*beta);
                        } else if (beta->isNonTerminal()) {
                            bool hasEpsilon = false;
                            auto current = beta;
                            while (current != prod.right.end()) {
                                auto &firstSet = firstSets[*current];
                                for (const auto &sym : firstSet) {
                                    if (!sym.isEpsilon()) {
                                        result.insert(sym);
                                    } else {
                                        hasEpsilon = true;
                                    }
                                }
                                if (!hasEpsilon) {
                                    break;
                                }
                                ++current;
                            }

                            if (hasEpsilon && prod.left.value != symbol.value) {
                                auto leftFollowSet =
                                    computeFollowSet(prod.left, visited);
                                result.insert(leftFollowSet.begin(),
                                              leftFollowSet.end());
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
            unordered_set<Symbol> visited;
            computeFollowSet(nt, visited);
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
        for (const auto &pair : g.followSets) {
            os << pair.first << ": ";
            for (const auto &symbol : pair.second) {
                os << symbol << " ";
            }
            os << endl;
        }
        return os;
    }
};

enum ActionType { SHIFT, REDUCE, ACCEPT, ERROR };

class Action {
  public:
    ActionType type;
    union {
        int state;
        int productionId;
    };

    Action() : type(ERROR), state(-1) {}

    explicit Action(const ActionType t, const int num = -1){
        if (t == SHIFT || t == ACCEPT) {
            type = t;
            state = num;
        } else if (t == REDUCE) {
            type = t;
            productionId = num;
        } else {
            type = ERROR;
            state = -1;
        }
    }

    [[nodiscard]] bool isShift() const { return type == SHIFT; }
    [[nodiscard]] bool isReduce() const { return type == REDUCE; }
    [[nodiscard]] bool isAccept() const { return type == ACCEPT; }
    [[nodiscard]] bool isError() const { return type == ERROR; }

    Action &operator=(const Action &other) {
        if (this != &other) {
            type = other.type;
            state = other.state;
        }
        return *this;
    }
};

class Item {
  public:
    Production production;
    int dotPos;

    Item(const Production &prod, int pos) : production(prod), dotPos(pos) {}

    Item &operator=(const Item &other) {
        if (this != &other) {
            production = other.production;
            dotPos = other.dotPos;
        }
        return *this;
    }

    bool operator==(const Item &other) const {
        return production.left == other.production.left &&
               production.right == other.production.right &&
               dotPos == other.dotPos;
    }

    bool operator!=(const Item &other) const { return !(*this == other); }

    [[nodiscard]] bool isComplete() const {
        return dotPos >= static_cast<int>(production.right.size());
    }

    [[nodiscard]] Symbol nextSymbol() const {
        if (dotPos < static_cast<int>(production.right.size())) {
            return production.right[dotPos];
        }
        return Symbol(SymbolType::EPSILON);
    }

    [[nodiscard]] Item advance() const {
        if (isComplete()) {
            throw runtime_error("Cannot advance a complete item.");
        }
        return {production, dotPos + 1};
    }
};

template <> struct std::hash<Item> {
    size_t operator()(const Item &item) const noexcept {
        return hash<Production>()(item.production) ^ hash<int>()(item.dotPos);
    }
};

template <> struct std::hash<std::pair<int, Symbol>> {
    size_t operator()(const std::pair<int, Symbol>& p) const noexcept {
        return hash<int>()(p.first) ^ (hash<Symbol>()(p.second) << 1);
    }
};

class ItemSet {
  public:
    unordered_set<Item> items;
    int state;

    explicit ItemSet(const int id = -1) : state(id) {}

    bool operator==(const ItemSet &other) const { return items == other.items; }

    void add(const Item &item) { items.insert(item); }

    ItemSet &operator=(const ItemSet &other) {
        if (this != &other) {
            items = other.items;
            state = other.state;
        }
        return *this;
    }
};

class SLRGrammar : public Grammar {

    vector<ItemSet> itemSets;
    unordered_map<int, unordered_map<Symbol, int>> transitions;

    ItemSet closure(const ItemSet &itemSet) const {
        ItemSet closureSet = itemSet;
        bool changed;
        do {
            changed = false;
            for (const auto &item : closureSet.items) {
                if (item.isComplete())
                    continue;

                Symbol nextSym = item.nextSymbol();
                if (!nextSym.isNonTerminal())
                    continue;

                for (const auto &prod : productions) {
                    if (prod.left == nextSym) {
                        Item newItem(prod, 0);
                        if (closureSet.items.insert(newItem).second) {
                            changed = true;
                        }
                    }
                }
            }
        } while (changed);
        return closureSet;
    }

    ItemSet gotoSet(const ItemSet &itemSet, const Symbol &symbol) const {
        ItemSet gotoSet;
        for (const auto &item : itemSet.items) {
            if (item.isComplete())
                continue;

            if (item.nextSymbol() == symbol) {
                gotoSet.add(item.advance());
            }
        }
        return closure(gotoSet);
    }

    void buildItemSets() {
        ItemSet initialSet(0);
        for (const auto &prod : productions) {
            if (prod.left == startSymbol) {
                initialSet.add(Item(prod, 0));
            }
        }
        initialSet = closure(initialSet);
        initialSet.add(Item(Production(startSymbol.value + "'", {startSymbol.value}), 0));
        itemSets.push_back(initialSet);

        unordered_set<pair<int, Symbol>> processedTransitions;
        bool changed;
        do {
            changed = false;
            for (int i = 0; i < itemSets.size(); ++i) {

                unordered_set<Symbol> symbols;
                for (const auto &item : itemSets[i].items) {
                    if (!item.isComplete()) {
                        Symbol nextSym = item.nextSymbol();
                        if (!nextSym.isEpsilon()) {
                            symbols.insert(nextSym);
                        }
                    }
                }

                for (const Symbol &symbol : symbols) {
                    pair<int, Symbol> transition = {i, symbol};
                    if (processedTransitions.count(transition)) continue;
                    processedTransitions.insert(transition);

                    ItemSet nextSet = gotoSet(itemSets[i], symbol);
                    if (nextSet.items.empty()) continue;

                    auto it = find(itemSets.begin(), itemSets.end(), nextSet);
                    if (it == itemSets.end()) {
                        nextSet.state = static_cast<int>(itemSets.size());
                        itemSets.push_back(nextSet);
                        transitions[i][symbol] = nextSet.state;
                        changed = true;
                    } else {
                        transitions[i][symbol] = it - itemSets.begin();
                    }
                }
            }
        } while (changed);
    }

  public:
    unordered_map<int, unordered_map<Symbol, Action>> actionTable;
    unordered_map<int, unordered_map<Symbol, int>> gotoTable;

    explicit SLRGrammar(const string &grammar) : Grammar(grammar) {
        buildItemSets();
        computeTables();
    }

    void computeTables() {
        for (int i = 0; i < itemSets.size(); i++) {
            const ItemSet& state = itemSets[i];
            for (const auto& item : state.items) {
                if (item.isComplete()) {
                    if (item.production.left == startSymbol) {
                        actionTable[i][Symbol(SymbolType::ENDMARK)] = Action(ActionType::ACCEPT);
                    } else {
                        const auto& followSet = followSets.find(item.production.left);
                        if (followSet != followSets.end()) {
                            for (const auto& symbol : followSet->second) {
                                if (symbol.isTerminal() || symbol.isEndMark()) {
                                    actionTable[i][symbol] = Action(ActionType::REDUCE, item.production.id);
                                }
                            }
                        }
                    }
                } else {
                    Symbol nextSymbol = item.nextSymbol();
                    auto transitionIt = transitions.find(i);
                    if (transitionIt != transitions.end()) {
                        auto symbolTransition = transitionIt->second.find(nextSymbol);
                        if (symbolTransition != transitionIt->second.end()) {
                            if (nextSymbol.isTerminal()) {
                                actionTable[i][nextSymbol] = Action(ActionType::SHIFT, symbolTransition->second);
                            } else if (nextSymbol.isNonTerminal()) {
                                gotoTable[i][nextSymbol] = symbolTransition->second;
                            }
                        }
                    }
                }
            }
        }
    }

    friend ostream &operator<<(ostream &os, const SLRGrammar &g) {
        os << "Item Sets:" << endl;
        for (const auto &itemSet : g.itemSets) {
            os << "State " << itemSet.state << ": ";
            for (const auto &item : itemSet.items) {
                os << item.production.left.value << " -> ";
                for (size_t i = 0; i < item.production.right.size(); ++i) {
                    if (i == item.dotPos) {
                        os << ".";
                    }
                    os << item.production.right[i].value << " ";
                }
                if (item.dotPos == item.production.right.size()) {
                    os << ".";
                }
                os << " | ";
            }
            os << endl;
        }

        os << "------------------------" << endl;

        os << "Action Table:" << endl;
        for (const auto &pair : g.actionTable) {
            int state = pair.first;
            os << "State " << state << ": ";
            for (const auto &actionPair : pair.second) {
                const Symbol &symbol = actionPair.first;
                const Action &action = actionPair.second;
                if (action.isShift()) {
                    os << symbol.value << " -> SHIFT " << action.state << ", ";
                } else if (action.isReduce()) {
                    os << symbol.value << " -> REDUCE " << action.productionId << ", ";
                } else if (action.isAccept()) {
                    os << symbol.value << " -> ACCEPT, ";
                } else if (action.isError()) {
                    os << symbol.value << " -> ERROR, ";
                }
            }
            os << endl;
        }

        os << "------------------------" << endl;
        os << "Goto Table:" << endl;
        for (const auto &pair : g.gotoTable) {
            int state = pair.first;
            os << "State " << state << ": ";
            for (const auto &gotoPair : pair.second) {
                const Symbol &symbol = gotoPair.first;
                int nextState = gotoPair.second;
                os << symbol.value << " -> GOTO " << nextState << ", ";
            }
            os << endl;
        }

        return os;
    }
};

string exp_grammar = R"(
program -> compoundstmt
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
simpleexpr ->  ID  |  NUM  |  ( arithexpr )
)";

string test_grammar = R"(
S -> L = R | R
L -> * R | id
R -> L
)";

string test_grammar1 = R"(
E' -> E' + T | T
T -> T * F | F
F -> ( E' ) | id
)";

void Analysis() {
    string prog;
    //    read_prog(prog);
    /* 骚年们 请开始你们的表演 */
    /********* Begin *********/
    SLRGrammar grammar(test_grammar);
    cout << grammar << endl;

    /********* End *********/
}

int main() {
    Analysis();
    return 0;
}