// C语言词法分析器
#include <cstdio>
#include <cstring>
#include <iostream>
#include <string>
#include <fstream>
#include <unordered_map>
#include <vector>
#include <algorithm>
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

unordered_map<string, int> element_table;

void load_element_table() {
    ifstream file("/Users/wuzekai/repo/compiler-cpp/exp/lexical-analysis/c_keys.txt", ios::in);
    if (!file.is_open()) {
        cerr << "Error opening file for reading." << endl;
        return;
    }

    while (file) {
        string key;
        file >> key;
        if (key.empty()) break;
        int value;
        file >> value;
        if (!file.fail()) {
            element_table[key] = value;
        }
    }
    file.close();
}

enum class TokenType {
    IDENTIFIER,
    CONSTANT,
    KEYWORD,
    DELIMITER,
    OPERATOR,

    COMMENT,
    LITERAL
};

vector<int> delimiter = {44, 45, 48, 49, 52, 53, 55, 56, 59, 63, 78};

TokenType get_token_type(int serial) {
    if (serial >= 1 && serial <= 32) {
        return TokenType::KEYWORD;
    } else if (serial == 79) {
        return TokenType::COMMENT;
    } else if (serial == 80) {
        return TokenType::CONSTANT;
    } else if (serial == 81) {
        return TokenType::IDENTIFIER;
    } else if (find(delimiter.begin(), delimiter.end(), serial) != delimiter.end()) {
        return TokenType::DELIMITER;
    } else if (serial == 83) {
        return TokenType::LITERAL;
    }
    return TokenType::OPERATOR;
}

struct Token {
    int serial;
    TokenType type;
    string value;

    Token() = default;
    Token(int serial, TokenType t, const string &v)
        : serial(serial), type(t), value(v) {}
};

vector<Token> tokenize(const string& prog) {
    vector<Token> tokens;

    int current = 0;
    while (true) {
        if (current >= prog.size())
            break;
        char c = prog[current];
        if (isspace(c)) {
            ++current;
            continue;
        }
        if (isalpha(c)) {
            string identifier;
            while (isalnum(c)) {
                identifier += c;
                ++current;
                c = prog[current];
            }
            if (element_table.find(identifier) != element_table.end()) {
                tokens.emplace_back(element_table[identifier], get_token_type(element_table[identifier]), identifier);
            } else {
                tokens.emplace_back(81, TokenType::IDENTIFIER, identifier);
            }

            continue;
        } else if (isdigit(c)) {
            string number;
            while (isdigit(c)) {
                number += c;
                ++current;
                c = prog[current];
            }
            tokens.emplace_back(80, TokenType::CONSTANT, number);

            continue;
        } else if (c == '/' && prog[current + 1] == '/') {
            string comment;
            while (c != '\n') {
                comment += c;
                ++current;
                c = prog[current];
            }
            tokens.emplace_back(79, TokenType::COMMENT, comment);

            ++current;
        } else if (c == '/' && prog[current + 1] == '*') {
            string comment;
            while (!(prog[current - 1] == '*' && prog[current] == '/')) {
                comment += c;
                ++current;
                c = prog[current];
            }
            comment += c;
            tokens.emplace_back(79, TokenType::COMMENT, comment);

            ++current;
            continue;
        } else if (c == '\"') {
            tokens.emplace_back(78, TokenType::DELIMITER, string(1, c));
            string str_literal;
            ++current;
            c = prog[current];
            while (c != '\"') {
                str_literal += c;
                ++current;
                c = prog[current];
            }
            tokens.emplace_back(81, TokenType::LITERAL, str_literal);
            tokens.emplace_back(78, TokenType::DELIMITER, string(1, c));

            ++current;
            continue;
        } else {
            string op3, op2, op1;
            if (current + 2 < prog.size())
                op3 = prog.substr(current, 3);
            if (current + 1 < prog.size())
                op2 = prog.substr(current, 2);
            op1 = prog.substr(current, 1);

            if (!op3.empty() && element_table.find(op3) != element_table.end()) {
                tokens.emplace_back(element_table[op3], get_token_type(element_table[op3]), op3);
                current += 3;
                continue;
            } else if (!op2.empty() && element_table.find(op2) != element_table.end()) {
                tokens.emplace_back(element_table[op2], get_token_type(element_table[op2]), op2);
                current += 2;
                continue;
            } else if (element_table.find(op1) != element_table.end()) {
                tokens.emplace_back(element_table[op1], get_token_type(element_table[op1]), op1);
                ++current;
                continue;
            } else {
                ++current;
                continue;
            }
        }
    }

    return tokens;
}

void Analysis()
{
	string prog;
	read_prog(prog);
	/* 骚年们 请开始你们的表演 */
    /********* Begin *********/
    load_element_table();
    auto tokens = tokenize(prog);
    for (int x = 0, idx = 1; x < tokens.size(); ++x, ++idx) {
        std::cout << idx << ": <" << tokens[x].value << "," << tokens[x].serial << ">" << std::endl;
    }

    /********* End *********/
}

int main() {
    Analysis();
    return 0;
}