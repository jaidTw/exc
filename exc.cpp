#include <iostream>
#include <deque>
#include <string>

using namespace std;

extern int yyparse(void);
extern deque<string> IR;

int main(void) {
    yyparse();
    for(auto &s : IR) {
        cout << '[' << s << ']' << endl;
    }
}
