//#include "stdafx.h"
#include <functional>
#include <algorithm>
#include <iostream>
#include <cstdlib>
#include <cstdlib>
#include <numeric>
#include <iomanip>
#include <stdio.h>
#include <cstring>
#include <string>
#include <cassert>
#include <vector>
#include <math.h>
#include <queue>
#include <stack>
#include <ctime>
#include <set>
#include <map>

using namespace std;

typedef long long ll;

const int N = (int)1e5 + 10;
const int INF = (int)1e9;
const int mod = (int)1e9 + 7;
const ll LLINF = (ll)1e18;
const int LOG = 22;
int count_of_tests = 1;

typedef unsigned int uint;
typedef unsigned long long ull;


struct Expression {

	string token, exp;
	vector <Expression*> args;

	Expression(string token) : token(token) {}
	Expression(string token, Expression* a) : token(token), args{ a } {}
	Expression(string token, Expression* a, Expression* b) : token(token), args{ a, b } {}

};

class Parser {
public:
	explicit Parser(const char* input) : input(input) {}
	Expression* parse();
private:
	string parse_token();
	Expression* parse_simple_expression();
	Expression* parse_binary_expression(int min_priority);

	const char* input;
};

bool isx(char x) {
	return (x == 'x' || x == 'e');
}

string Parser::parse_token() {
	while (isspace(*input)) ++input;

	if (isdigit(*input) || isx(*input) || *input == '.') {
		string number;
		while (isdigit(*input) || isx(*input) || *input == '.') {
			number.push_back(*input++);
		}
		return number;
	}

	static const string tokens[] = { "+", "-", "**", "*", "/", "(", ")", "ln", "sin", "cos", "tg", "ctg", "arcsin", "arccos", "arctg", "arcctg" };

	for (auto& t : tokens) {
		if (strncmp(input, t.c_str(), t.size()) == 0) {
            input += t.size();
			return t;
		}
	}

	return "";
}

Expression* Parser::parse_simple_expression() {
	auto token = parse_token();
	if (token.empty()) exit(0);

	if (token == "(") {
		auto result = parse();
		if (parse_token() != ")") exit(0);
		return result;
	}

	if (isdigit(token[0]) || isx(token[0])) {
		return new Expression(token);
	}

	auto arg = parse_simple_expression();
	return new Expression(token, arg);
}

int get_priority(string token) {
	if (token == "+") return 1;
	if (token == "-") return 1;
	if (token == "*") return 2;
	if (token == "/") return 2;
	if (token == "**") return 3;
	return 0;
}

Expression* Parser::parse_binary_expression(int min_priority) {
	auto left_expression = parse_simple_expression();

	for (;;) {
		auto op = parse_token();
		auto priority = get_priority(op);
		if ((min_priority < 3 && priority <= min_priority) || (min_priority == 3 && priority < min_priority)) {
			input -= op.size();
			return left_expression;
		}

		auto right_expression = parse_binary_expression(priority);
		left_expression = new Expression(op, left_expression, right_expression);
	}
}

Expression* Parser::parse() {
	return parse_binary_expression(0);
}

bool haveX(string s) {
    for(auto t : s) {
        if(t == 'x') return true;
    }
    return false;
}

string derivative(Expression* e) {
	switch (e->args.size()) {
	case 2: {
		auto x = e->args[0];
		auto y = e->args[1];
		auto derX = derivative(x);
		auto derY = derivative(y);
		auto X = x->exp;
		auto Y = y->exp;
		if (e->token == "+") {
			return derX + " + " + derY;
		}
		if (e->token == "-") {
			return derX + " - " + derY;
		}
		if (e->token == "*") {
			return "(" + derX + ") * (" + Y + ") + (" + X + ") * (" + derY + ")";
		}
		if (e->token == "/") {
            return "((" + derX + ") * (" + Y + ") - (" + X + ") * (" + derY + ")) / ((" + Y + ") ** 2)";
		}
        if (e->token == "**") {
            if(haveX(Y)) return "((" + X + ") ** (" + Y + ")) * ((" + derX + ") * (" + Y + ") / (" + X + ") + (" + derY + ") * ln(" + X + "))";
            return "(" + Y + ")" + " * (" + X + ") ** (" + Y + " - 1)" + " * (" + derX + ")";
        }
	}
	case 1: {
        auto x = e->args[0];
        auto derX = derivative(x);
        auto X = x->exp;
        if(e->token == "+") {
                return " +" + derX;
        }
        if (e->token == "-") {
                return " -" + derX;
        }
        if (e->token == "ln") {
                return "(" + derX + ") / (" + X + ")";
        }
        if (e->token == "sin") {
                return "(" + derX + ") * cos(" + X + ")";
        }
        if (e->token == "cos") {
                return "-(" + derX + ") * sin(" + X + ")";
        }
        if (e->token == "tg") {
                return "(" + derX + ") / (cos(" + X + ") ** 2)";
        }
        if (e->token == "ctg") {
                return "-(" + derX + ") / (sin(" + X + ") ** 2)";
        }
        if (e->token == "arcsin") {
                return "(" + derX + ") / ((1 - (" + X + ") ** 2) ** (1/2))";
        }
        if (e->token == "arccos") {
                return "-(" + derX + ") / ((1 + (" + X + ") ** 2) ** (1/2))";
        }
        if (e->token == "arctg") {
                return "(" + derX + ") / (1 + (" + X + ") ** 2)";
        }
        if (e->token == "arcctg") {
                return "-(" + derX + ") / (1 + (" + X + ") ** 2)";
        }
	}
	case 0: {
        if(e->token == "x") return "1";
        else return "0";
    }
	}
}

void build(Expression* e) {
	int sz = e->args.size();
	if(sz == 2) {
		Expression* x = e->args[0];
		Expression* y = e->args[1];
		build(x); build(y);
		if (e->token == "+") e->exp = x->exp + " + " + y->exp;
		if (e->token == "-") e->exp = x->exp + " - " + y->exp;
		if (e->token == "*") e->exp = "(" + x->exp + ") * (" + y->exp + ")";
		if (e->token == "/") e->exp = "(" + x->exp + ") / (" + y->exp + ")";
		if (e->token == "**") e->exp = "(" + x->exp + ") ** (" + y->exp + ")";
	}
	if(sz == 1) {
		Expression* x = e->args[0];
		build(x);
		if (e->token == "+") e->exp = "+" + x->exp;
		if (e->token == "-") e->exp = "-" + x->exp;
		if (e->token == "ln") e->exp = "ln(" + x->exp + ")";
		if (e->token == "sin") e->exp = "sin(" + x->exp + ")";
		if (e->token == "cos") e->exp = "cos(" + x->exp + ")";
		if (e->token == "tg") e->exp = "tg(" + x->exp + ")";
		if (e->token == "ctg") e->exp = "ctg(" + x->exp + ")";
        if (e->token == "arcsin") e->exp = "arcsin(" + x->exp + ")";
		if (e->token == "arccos") e->exp = "arccos(" + x->exp + ")";
		if (e->token == "arctg") e->exp = "arctg(" + x->exp + ")";
		if (e->token == "arcctg") e->exp = "arcctg(" + x->exp + ")";
	}
	if(sz == 0) {
		e->exp = e->token;
	}
}

void print(Expression* x) {
    cout << x->args.size() << x->token << ' ' << x->exp << '\n';
    for(auto to : x->args) {
        print(to);
    }
}

void test(const char* input) {
	Parser p(input);
	Expression* Q = p.parse();
	build(Q);
	auto result = derivative(Q);
	cout << result << '\n';
}

int main() {

    freopen("deriv.in", "r", stdin);
    freopen("deriv.out", "w", stdout);

	while(!cin.eof()) {
		char s[256];
		cin.getline(s, sizeof(s));
		test(s);
	}

	return 0;
}
