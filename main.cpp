#include <algorithm>
#include <cctype>
#include <iomanip>
#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <vector>
using namespace std;

struct User { string id, password, name; int privilege = 0; };
struct Book { string isbn, name, author, keyword_raw; vector<string> keywords; double price = 0; long long quantity = 0; };
struct Session { string user_id, selected_isbn; };
struct FinanceEntry { double income = 0, expense = 0; };

static string trim_spaces(const string &s) {
    size_t l = 0, r = s.size();
    while (l < r && s[l] == ' ') ++l;
    while (r > l && s[r - 1] == ' ') --r;
    return s.substr(l, r - l);
}

static bool is_visible_ascii_no_quote(const string &s, size_t max_len) {
    if (s.empty() || s.size() > max_len) return false;
    for (unsigned char c : s) if (c < 32 || c > 126 || c == '"') return false;
    return true;
}
static bool is_identifier(const string &s, size_t max_len = 30) {
    if (s.empty() || s.size() > max_len) return false;
    for (unsigned char c : s) if (!(isalnum(c) || c == '_')) return false;
    return true;
}
static bool is_nonnegative_integer_token(const string &s, size_t max_len = 10) {
    if (s.empty() || s.size() > max_len) return false;
    for (unsigned char c : s) if (!isdigit(c)) return false;
    return true;
}
static bool parse_positive_int(const string &s, long long &value) {
    if (!is_nonnegative_integer_token(s)) return false;
    try { value = stoll(s); } catch (...) { return false; }
    return value > 0 && value <= 2147483647LL;
}
static bool parse_nonnegative_int(const string &s, long long &value) {
    if (!is_nonnegative_integer_token(s)) return false;
    try { value = stoll(s); } catch (...) { return false; }
    return value <= 2147483647LL;
}
static bool parse_money(const string &s, double &value, bool must_positive) {
    if (s.empty() || s.size() > 13) return false;
    int dots = 0;
    for (unsigned char c : s) {
        if (c == '.') ++dots;
        else if (!isdigit(c)) return false;
    }
    if (dots > 1) return false;
    try { value = stod(s); } catch (...) { return false; }
    return must_positive ? value > 0 : value >= 0;
}
static string money2(double x) {
    ostringstream os; os.setf(ios::fixed); os << setprecision(2) << x; return os.str();
}
static vector<string> tokenize(const string &line, bool &ok) {
    vector<string> tokens; ok = true;
    for (unsigned char c : line) if (c < 32 && c != ' ') { ok = false; return {}; }
    size_t i = 0, n = line.size();
    while (i < n) {
        while (i < n && line[i] == ' ') ++i;
        if (i >= n) break;
        string cur; bool in_quote = false;
        while (i < n) {
            char c = line[i];
            if (!in_quote && c == ' ') break;
            if (c == '"') in_quote = !in_quote;
            cur.push_back(c); ++i;
        }
        if (in_quote) { ok = false; return {}; }
        tokens.push_back(cur);
    }
    return tokens;
}
static bool quoted_payload(const string &s, const string &prefix, string &out, size_t max_len) {
    if (s.rfind(prefix, 0) != 0) return false;
    string rem = s.substr(prefix.size());
    if (rem.size() < 2 || rem.front() != '"' || rem.back() != '"') return false;
    out = rem.substr(1, rem.size() - 2);
    return is_visible_ascii_no_quote(out, max_len);
}
static vector<string> split_keywords(const string &raw, bool &ok) {
    vector<string> res; ok = true; if (raw.empty()) { ok = false; return {}; }
    set<string> seen; size_t start = 0;
    while (true) {
        size_t pos = raw.find('|', start);
        string part = raw.substr(start, pos == string::npos ? string::npos : pos - start);
        if (part.empty() || !is_visible_ascii_no_quote(part, 60) || seen.count(part)) { ok = false; return {}; }
        seen.insert(part); res.push_back(part);
        if (pos == string::npos) break;
        start = pos + 1;
    }
    return res;
}

class Bookstore {
    map<string, User> users;
    map<string, Book> books;
    vector<Session> sessions;
    vector<FinanceEntry> finance;

    int cur_priv() const { return sessions.empty() ? 0 : users.at(sessions.back().user_id).privilege; }
    Session *cur_session() { return sessions.empty() ? nullptr : &sessions.back(); }
    bool need_priv(int p) const { return cur_priv() >= p; }
    static void invalid() { cout << "Invalid\n"; }
    static void print_book(const Book &b) {
        cout << b.isbn << '\t' << b.name << '\t' << b.author << '\t' << b.keyword_raw << '\t' << money2(b.price) << '\t' << b.quantity << '\n';
    }

    bool cmd_su(const vector<string> &t) {
        if (t.size() != 2 && t.size() != 3) return false;
        if (!is_identifier(t[1])) return false;
        auto it = users.find(t[1]); if (it == users.end()) return false;
        if (t.size() == 2) {
            if (cur_priv() <= it->second.privilege) return false;
        } else {
            if (!is_identifier(t[2]) || it->second.password != t[2]) return false;
        }
        sessions.push_back({t[1], ""}); return true;
    }
    bool cmd_logout(const vector<string> &t) {
        if (t.size() != 1 || !need_priv(1) || sessions.empty()) return false;
        sessions.pop_back(); return true;
    }
    bool cmd_register(const vector<string> &t) {
        if (t.size() != 4) return false;
        if (!is_identifier(t[1]) || !is_identifier(t[2]) || !is_visible_ascii_no_quote(t[3], 30) || users.count(t[1])) return false;
        users[t[1]] = {t[1], t[2], t[3], 1}; return true;
    }
    bool cmd_passwd(const vector<string> &t) {
        if (!need_priv(1) || (t.size() != 3 && t.size() != 4) || !is_identifier(t[1]) || !users.count(t[1])) return false;
        if (t.size() == 3) {
            if (cur_priv() != 7 || !is_identifier(t[2])) return false;
            users[t[1]].password = t[2]; return true;
        }
        if (!is_identifier(t[2]) || !is_identifier(t[3]) || users[t[1]].password != t[2]) return false;
        users[t[1]].password = t[3]; return true;
    }
    bool cmd_useradd(const vector<string> &t) {
        if (!need_priv(3) || t.size() != 5 || !is_identifier(t[1]) || !is_identifier(t[2]) || !is_visible_ascii_no_quote(t[4], 30)) return false;
        if (t[3].size() != 1 || (t[3] != "1" && t[3] != "3")) return false;
        int p = t[3][0] - '0'; if (p >= cur_priv() || users.count(t[1])) return false;
        users[t[1]] = {t[1], t[2], t[4], p}; return true;
    }
    bool cmd_delete(const vector<string> &t) {
        if (cur_priv() != 7 || t.size() != 2 || !is_identifier(t[1])) return false;
        auto it = users.find(t[1]); if (it == users.end()) return false;
        for (const auto &s : sessions) if (s.user_id == t[1]) return false;
        users.erase(it); return true;
    }
    bool show_finance(const vector<string> &t) {
        if (cur_priv() != 7) return false;
        if (t.size() == 2) {
            double in = 0, out = 0; for (const auto &e : finance) { in += e.income; out += e.expense; }
            cout << "+ " << money2(in) << " - " << money2(out) << '\n'; return true;
        }
        if (t.size() != 3) return false;
        long long cnt; if (!parse_nonnegative_int(t[2], cnt)) return false;
        if (cnt == 0) { cout << '\n'; return true; }
        if (cnt > (long long)finance.size()) return false;
        double in = 0, out = 0;
        for (long long i = (long long)finance.size() - cnt; i < (long long)finance.size(); ++i) { in += finance[i].income; out += finance[i].expense; }
        cout << "+ " << money2(in) << " - " << money2(out) << '\n'; return true;
    }
    bool show_filtered(const string &arg) {
        vector<const Book *> res;
        if (arg.rfind("-ISBN=", 0) == 0) {
            string isbn = arg.substr(6); if (!is_visible_ascii_no_quote(isbn, 20)) return false;
            auto it = books.find(isbn); if (it != books.end()) res.push_back(&it->second);
        } else if (arg.rfind("-name=", 0) == 0) {
            string v; if (!quoted_payload(arg, "-name=", v, 60) || v.empty()) return false;
            for (auto &kv : books) if (kv.second.name == v) res.push_back(&kv.second);
        } else if (arg.rfind("-author=", 0) == 0) {
            string v; if (!quoted_payload(arg, "-author=", v, 60) || v.empty()) return false;
            for (auto &kv : books) if (kv.second.author == v) res.push_back(&kv.second);
        } else if (arg.rfind("-keyword=", 0) == 0) {
            string v; if (!quoted_payload(arg, "-keyword=", v, 60) || v.empty() || v.find('|') != string::npos) return false;
            for (auto &kv : books) {
                for (auto &k : kv.second.keywords) if (k == v) { res.push_back(&kv.second); break; }
            }
        } else return false;
        if (res.empty()) { cout << '\n'; return true; }
        for (auto *b : res) print_book(*b);
        return true;
    }
    bool cmd_show(const vector<string> &t) {
        if (!need_priv(1)) return false;
        if (t.size() == 1) { for (auto &kv : books) print_book(kv.second); return true; }
        if (t[1] == "finance") return show_finance(t);
        if (t.size() != 2) return false;
        return show_filtered(t[1]);
    }
    bool cmd_buy(const vector<string> &t) {
        if (!need_priv(1) || t.size() != 3 || !is_visible_ascii_no_quote(t[1], 20)) return false;
        long long q; if (!parse_positive_int(t[2], q)) return false;
        auto it = books.find(t[1]); if (it == books.end() || it->second.quantity < q) return false;
        it->second.quantity -= q; double total = it->second.price * q; finance.push_back({total, 0}); cout << money2(total) << '\n'; return true;
    }
    bool cmd_select(const vector<string> &t) {
        if (!need_priv(3) || t.size() != 2 || !is_visible_ascii_no_quote(t[1], 20)) return false;
        Book &b = books[t[1]]; if (b.isbn.empty()) b.isbn = t[1];
        cur_session()->selected_isbn = t[1]; return true;
    }
    bool cmd_modify(const vector<string> &t) {
        if (!need_priv(3) || t.size() < 2) return false;
        Session *s = cur_session(); if (!s || s->selected_isbn.empty()) return false;
        auto it = books.find(s->selected_isbn); if (it == books.end()) return false;
        Book updated = it->second; string final_isbn = updated.isbn;
        bool seen_i = false, seen_n = false, seen_a = false, seen_k = false, seen_p = false;
        for (size_t i = 1; i < t.size(); ++i) {
            const string &arg = t[i];
            if (arg.rfind("-ISBN=", 0) == 0) {
                if (seen_i) return false; seen_i = true;
                string v = arg.substr(6); if (!is_visible_ascii_no_quote(v, 20) || v == updated.isbn) return false;
                auto found = books.find(v); if (found != books.end()) return false; final_isbn = v;
            } else if (arg.rfind("-name=", 0) == 0) {
                if (seen_n) return false; seen_n = true; string v; if (!quoted_payload(arg, "-name=", v, 60) || v.empty()) return false; updated.name = v;
            } else if (arg.rfind("-author=", 0) == 0) {
                if (seen_a) return false; seen_a = true; string v; if (!quoted_payload(arg, "-author=", v, 60) || v.empty()) return false; updated.author = v;
            } else if (arg.rfind("-keyword=", 0) == 0) {
                if (seen_k) return false; seen_k = true; string v; if (!quoted_payload(arg, "-keyword=", v, 60) || v.empty()) return false; bool ok; auto parts = split_keywords(v, ok); if (!ok) return false; updated.keyword_raw = v; updated.keywords = parts;
            } else if (arg.rfind("-price=", 0) == 0) {
                if (seen_p) return false; seen_p = true; double v; if (!parse_money(arg.substr(7), v, false)) return false; updated.price = v;
            } else return false;
        }
        if (final_isbn != updated.isbn) {
            books.erase(it); updated.isbn = final_isbn; books[final_isbn] = updated; s->selected_isbn = final_isbn;
        } else it->second = updated;
        return true;
    }
    bool cmd_import(const vector<string> &t) {
        if (!need_priv(3) || t.size() != 3) return false;
        Session *s = cur_session(); if (!s || s->selected_isbn.empty()) return false;
        auto it = books.find(s->selected_isbn); if (it == books.end()) return false;
        long long q; double cost; if (!parse_positive_int(t[1], q) || !parse_money(t[2], cost, true)) return false;
        it->second.quantity += q; finance.push_back({0, cost}); return true;
    }
    bool cmd_log(const vector<string> &t) { if (cur_priv() != 7 || t.size() != 1) return false; cout << "log\n"; return true; }
    bool cmd_report(const vector<string> &t) {
        if (cur_priv() != 7 || t.size() != 2) return false;
        if (t[1] == "finance") { cout << "report finance\n"; return true; }
        if (t[1] == "employee") { cout << "report employee\n"; return true; }
        return false;
    }

public:
    Bookstore() { users["root"] = {"root", "sjtu", "root", 7}; }
    void run() {
        string line;
        while (getline(cin, line)) {
            line = trim_spaces(line);
            if (line.empty()) continue;
            bool ok; auto t = tokenize(line, ok); if (!ok || t.empty()) { invalid(); continue; }
            if (t[0] == "quit" || t[0] == "exit") break;
            bool done = false;
            if (t[0] == "su") done = cmd_su(t);
            else if (t[0] == "logout") done = cmd_logout(t);
            else if (t[0] == "register") done = cmd_register(t);
            else if (t[0] == "passwd") done = cmd_passwd(t);
            else if (t[0] == "useradd") done = cmd_useradd(t);
            else if (t[0] == "delete") done = cmd_delete(t);
            else if (t[0] == "show") done = cmd_show(t);
            else if (t[0] == "buy") done = cmd_buy(t);
            else if (t[0] == "select") done = cmd_select(t);
            else if (t[0] == "modify") done = cmd_modify(t);
            else if (t[0] == "import") done = cmd_import(t);
            else if (t[0] == "log") done = cmd_log(t);
            else if (t[0] == "report") done = cmd_report(t);
            if (!done) invalid();
        }
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    Bookstore bookstore; bookstore.run();
    return 0;
}
