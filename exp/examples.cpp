#include <string>
using namespace std;

string test_grammar = R"(
S -> L = R | R
L -> * R | ID
R -> L
)";

string test_grammar1 = R"(
E' -> E' + T | T
T -> T * F | F
F -> ( E' ) | id
)";

string test_input = R"({

while ( ID == NUM )

{

ID = NUM

}

})";

string test_input1 = R"(id * id + id)";