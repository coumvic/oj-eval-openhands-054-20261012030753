#include <bits/stdc++.h>
using namespace std;

class Term {
public:
    int a, b, c, d;  // coefficient, power of x, power of sin, power of cos
    
    Term(int a = 0, int b = 0, int c = 0, int d = 0) : a(a), b(b), c(c), d(d) {}
    
    bool operator==(const Term& other) const {
        return b == other.b && c == other.c && d == other.d;
    }
    
    bool operator<(const Term& other) const {
        if (b != other.b) return b > other.b;
        if (c != other.c) return c > other.c;
        return d > other.d;
    }
};

class Poly {
public:
    vector<Term> terms;
    
    Poly() {}
    Poly(const vector<Term>& t) : terms(t) {}
    
    void simplify() {
        // Sort terms
        sort(terms.begin(), terms.end());
        
        // Combine like terms
        vector<Term> new_terms;
        for (const Term& t : terms) {
            if (t.a == 0) continue;
            
            if (!new_terms.empty() && new_terms.back() == t) {
                new_terms.back().a += t.a;
            } else {
                new_terms.push_back(t);
            }
        }
        
        // Remove zero coefficient terms
        terms.clear();
        for (const Term& t : new_terms) {
            if (t.a != 0) {
                terms.push_back(t);
            }
        }
    }
    
    Poly operator+(const Poly& other) const {
        Poly result;
        result.terms = terms;
        result.terms.insert(result.terms.end(), other.terms.begin(), other.terms.end());
        result.simplify();
        return result;
    }
    
    Poly operator-(const Poly& other) const {
        Poly result;
        result.terms = terms;
        for (Term t : other.terms) {
            t.a = -t.a;
            result.terms.push_back(t);
        }
        result.simplify();
        return result;
    }
    
    Poly operator*(const Poly& other) const {
        Poly result;
        for (const Term& t1 : terms) {
            for (const Term& t2 : other.terms) {
                Term new_term(t1.a * t2.a, t1.b + t2.b, t1.c + t2.c, t1.d + t2.d);
                result.terms.push_back(new_term);
            }
        }
        result.simplify();
        return result;
    }
    
    Poly derivative() const {
        Poly result;
        for (const Term& t : terms) {
            // Apply product rule: derivative of a * x^b * sin^c * cos^d
            // = a * [b*x^(b-1)*sin^c*cos^d + c*x^b*sin^(c-1)*cos^(d+1) - d*x^b*sin^(c+1)*cos^(d-1)]
            
            if (t.b > 0) {
                Term term1(t.a * t.b, t.b - 1, t.c, t.d);
                result.terms.push_back(term1);
            }
            
            if (t.c > 0) {
                Term term2(t.a * t.c, t.b, t.c - 1, t.d + 1);
                result.terms.push_back(term2);
            }
            
            if (t.d > 0) {
                Term term3(-t.a * t.d, t.b, t.c + 1, t.d - 1);
                result.terms.push_back(term3);
            }
        }
        result.simplify();
        return result;
    }
};

class Frac {
public:
    Poly p, q;
    
    Frac() {}
    Frac(const Poly& p, const Poly& q) : p(p), q(q) {}
    Frac(int a) {
        p = Poly({Term(a, 0, 0, 0)});
        q = Poly({Term(1, 0, 0, 0)});
    }
    Frac(const Term& t) {
        p = Poly({t});
        q = Poly({Term(1, 0, 0, 0)});
    }
    
    Frac operator+(const Frac& other) const {
        Frac result;
        result.p = p * other.q + other.p * q;
        result.q = q * other.q;
        return result;
    }
    
    Frac operator-(const Frac& other) const {
        Frac result;
        result.p = p * other.q - other.p * q;
        result.q = q * other.q;
        return result;
    }
    
    Frac operator*(const Frac& other) const {
        Frac result;
        result.p = p * other.p;
        result.q = q * other.q;
        return result;
    }
    
    Frac operator/(const Frac& other) const {
        Frac result;
        result.p = p * other.q;
        result.q = q * other.p;
        return result;
    }
    
    Frac derivative() const {
        Frac result;
        result.p = p.derivative() * q - q.derivative() * p;
        result.q = q * q;
        return result;
    }
};

string termToString(const Term& t) {
    string result;
    
    // Handle coefficient
    if (t.b == 0 && t.c == 0 && t.d == 0) {
        // Constant term
        return to_string(t.a);
    }
    
    // Handle sign and coefficient
    if (t.a == -1) {
        result += "-";
    } else if (t.a != 1) {
        result += to_string(t.a);
    }
    
    // Handle x^b
    if (t.b > 0) {
        result += "x";
        if (t.b > 1) {
            result += "^" + to_string(t.b);
        }
    }
    
    // Handle sin^c
    if (t.c > 0) {
        result += "sin";
        if (t.c > 1) {
            result += "^" + to_string(t.c);
        }
        result += "x";
    }
    
    // Handle cos^d
    if (t.d > 0) {
        result += "cos";
        if (t.d > 1) {
            result += "^" + to_string(t.d);
        }
        result += "x";
    }
    
    return result;
}

string polyToString(const Poly& poly) {
    if (poly.terms.empty()) {
        return "0";
    }
    
    string result;
    for (size_t i = 0; i < poly.terms.size(); i++) {
        const Term& t = poly.terms[i];
        
        if (i == 0) {
            // First term - no leading sign needed if positive
            if (t.a < 0) {
                result += termToString(t);
            } else {
                result += termToString(t);
            }
        } else {
            // Subsequent terms - always include sign
            if (t.a < 0) {
                result += termToString(t);
            } else {
                result += "+" + termToString(t);
            }
        }
    }
    
    return result;
}

string fracToString(const Frac& frac) {
    string numerator = polyToString(frac.p);
    string denominator = polyToString(frac.q);
    
    // Special cases
    if (numerator == "0") {
        return "0";
    }
    if (denominator == "1") {
        return numerator;
    }
    
    // Check if we need parentheses
    bool need_paren_num = (frac.p.terms.size() > 1);
    bool need_paren_den = (frac.q.terms.size() > 1);
    
    string result;
    if (need_paren_num) result += "(";
    result += numerator;
    if (need_paren_num) result += ")";
    
    result += "/";
    
    if (need_paren_den) result += "(";
    result += denominator;
    if (need_paren_den) result += ")";
    
    return result;
}

// Parser functions
int getNumber(const string& s, int l, int r) {
    if (l >= r) return 1;
    
    bool negative = false;
    if (s[l] == '-') {
        negative = true;
        l++;
    }
    
    if (l >= r || !isdigit(s[l])) {
        return negative ? -1 : 1;
    }
    
    int result = 0;
    while (l < r && isdigit(s[l])) {
        result = result * 10 + (s[l] - '0');
        l++;
    }
    
    return negative ? -result : result;
}

Term getTerm(const string& s, int l, int r) {
    int coeff = 1, power_x = 0, power_sin = 0, power_cos = 0;
    
    int i = l;
    // Get coefficient
    while (i < r && (isdigit(s[i]) || s[i] == '-')) {
        i++;
    }
    if (i > l) {
        coeff = getNumber(s, l, i);
    }
    
    // Parse the rest
    while (i < r) {
        if (s[i] == 'x' && (i + 1 >= r || s[i + 1] != 'n')) {
            // x power
            power_x = 1;
            i++;
            if (i < r && s[i] == '^') {
                i++;
                int start = i;
                while (i < r && isdigit(s[i])) i++;
                power_x = stoi(s.substr(start, i - start));
            }
        } else if (s.substr(i, 3) == "sin") {
            // sin power
            power_sin = 1;
            i += 3;
            if (i < r && s[i] == '^') {
                i++;
                int start = i;
                while (i < r && isdigit(s[i])) i++;
                power_sin = stoi(s.substr(start, i - start));
            }
            if (i < r && s[i] == 'x') i++;
        } else if (s.substr(i, 3) == "cos") {
            // cos power
            power_cos = 1;
            i += 3;
            if (i < r && s[i] == '^') {
                i++;
                int start = i;
                while (i < r && isdigit(s[i])) i++;
                power_cos = stoi(s.substr(start, i - start));
            }
            if (i < r && s[i] == 'x') i++;
        } else {
            i++;
        }
    }
    
    return Term(coeff, power_x, power_sin, power_cos);
}

// Recursive descent parser
Frac dfs(const string& s, int l, int r) {
    // Handle parentheses
    if (s[l] == '(' && s[r - 1] == ')') {
        int balance = 1;
        int i = l + 1;
        while (i < r - 1 && balance > 0) {
            if (s[i] == '(') balance++;
            else if (s[i] == ')') balance--;
            i++;
        }
        if (balance == 1) {
            return dfs(s, l + 1, r - 1);
        }
    }
    
    // Find addition/subtraction operators (lowest precedence)
    int add_pos = -1;
    int balance = 0;
    for (int i = l; i < r; i++) {
        if (s[i] == '(') balance++;
        else if (s[i] == ')') balance--;
        else if (balance == 0 && (s[i] == '+' || s[i] == '-')) {
            // Skip if this is a negative sign at the beginning or after another operator
            if (i == l || s[i-1] == '+' || s[i-1] == '-' || s[i-1] == '*' || s[i-1] == '/') {
                continue;
            }
            add_pos = i;
        }
    }
    
    if (add_pos != -1) {
        Frac left = dfs(s, l, add_pos);
        Frac right = dfs(s, add_pos + 1, r);
        if (s[add_pos] == '-') {
            return left - right;
        } else {
            return left + right;
        }
    }
    
    // Find multiplication/division operators (higher precedence)
    int mul_pos = -1, div_pos = -1;
    balance = 0;
    for (int i = l; i < r; i++) {
        if (s[i] == '(') balance++;
        else if (s[i] == ')') balance--;
        else if (balance == 0) {
            if (s[i] == '*') {
                mul_pos = i;
            } else if (s[i] == '/') {
                div_pos = i;
            }
        }
    }
    
    // Process multiplication before division (left to right)
    if (mul_pos != -1 && (div_pos == -1 || mul_pos < div_pos)) {
        Frac left = dfs(s, l, mul_pos);
        Frac right = dfs(s, mul_pos + 1, r);
        return left * right;
    }
    
    if (div_pos != -1) {
        Frac left = dfs(s, l, div_pos);
        Frac right = dfs(s, div_pos + 1, r);
        return left / right;
    }
    
    // Base case: single term
    Term term = getTerm(s, l, r);
    return Frac(term);
}

void solve(const string& s) {
    Frac expr = dfs(s, 0, s.length());
    Frac deriv = expr.derivative();
    
    cout << fracToString(expr) << endl;
    cout << fracToString(deriv) << endl;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    
    string expr;
    getline(cin, expr);
    solve(expr);
    
    return 0;
}