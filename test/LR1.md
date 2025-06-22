1.
Input
```
{
    if ( ID > NUM ) then ID = NUM ; else ID = NUM ;
}
```

Output
```
pprogram => 
compoundstmt => 
{ stmts } => 
{ stmt stmts } => 
{ stmt } => 
{ ifstmt } => 
{ if ( boolexpr ) then stmt else stmt } => 
{ if ( boolexpr ) then stmt else assgstmt } => 
{ if ( boolexpr ) then stmt else ID = arithexpr ; } => 
{ if ( boolexpr ) then stmt else ID = multexpr arithexprprime ; } => 
{ if ( boolexpr ) then stmt else ID = multexpr ; } => 
{ if ( boolexpr ) then stmt else ID = simpleexpr multexprprime ; } => 
{ if ( boolexpr ) then stmt else ID = simpleexpr ; } => 
{ if ( boolexpr ) then stmt else ID = NUM ; } => 
{ if ( boolexpr ) then assgstmt else ID = NUM ; } => 
{ if ( boolexpr ) then ID = arithexpr ; else ID = NUM ; } => 
{ if ( boolexpr ) then ID = multexpr arithexprprime ; else ID = NUM ; } => 
{ if ( boolexpr ) then ID = multexpr ; else ID = NUM ; } => 
{ if ( boolexpr ) then ID = simpleexpr multexprprime ; else ID = NUM ; } => 
{ if ( boolexpr ) then ID = simpleexpr ; else ID = NUM ; } => 
{ if ( boolexpr ) then ID = NUM ; else ID = NUM ; } => 
{ if ( arithexpr boolop arithexpr ) then ID = NUM ; else ID = NUM ; } => 
{ if ( arithexpr boolop multexpr arithexprprime ) then ID = NUM ; else ID = NUM ; } => 
{ if ( arithexpr boolop multexpr ) then ID = NUM ; else ID = NUM ; } => 
{ if ( arithexpr boolop simpleexpr multexprprime ) then ID = NUM ; else ID = NUM ; } => 
{ if ( arithexpr boolop simpleexpr ) then ID = NUM ; else ID = NUM ; } => 
{ if ( arithexpr boolop NUM ) then ID = NUM ; else ID = NUM ; } => 
{ if ( arithexpr > NUM ) then ID = NUM ; else ID = NUM ; } => 
{ if ( multexpr arithexprprime > NUM ) then ID = NUM ; else ID = NUM ; } => 
{ if ( multexpr > NUM ) then ID = NUM ; else ID = NUM ; } => 
{ if ( simpleexpr multexprprime > NUM ) then ID = NUM ; else ID = NUM ; } => 
{ if ( simpleexpr > NUM ) then ID = NUM ; else ID = NUM ; } => 
{ if ( ID > NUM ) then ID = NUM ; else ID = NUM ; } 
```

2.
Input
```
{
    while ( ID <= NUM ) ID = ID + NUM ;
}
```

Output
```
program => 
compoundstmt => 
{ stmts } => 
{ stmt stmts } => 
{ stmt } => 
{ whilestmt } => 
{ while ( boolexpr ) stmt } => 
{ while ( boolexpr ) assgstmt } => 
{ while ( boolexpr ) ID = arithexpr ; } => 
{ while ( boolexpr ) ID = multexpr arithexprprime ; } => 
{ while ( boolexpr ) ID = multexpr + multexpr arithexprprime ; } => 
{ while ( boolexpr ) ID = multexpr + multexpr ; } => 
{ while ( boolexpr ) ID = multexpr + simpleexpr multexprprime ; } => 
{ while ( boolexpr ) ID = multexpr + simpleexpr ; } => 
{ while ( boolexpr ) ID = multexpr + NUM ; } => 
{ while ( boolexpr ) ID = simpleexpr multexprprime + NUM ; } => 
{ while ( boolexpr ) ID = simpleexpr + NUM ; } => 
{ while ( boolexpr ) ID = ID + NUM ; } => 
{ while ( arithexpr boolop arithexpr ) ID = ID + NUM ; } => 
{ while ( arithexpr boolop multexpr arithexprprime ) ID = ID + NUM ; } => 
{ while ( arithexpr boolop multexpr ) ID = ID + NUM ; } => 
{ while ( arithexpr boolop simpleexpr multexprprime ) ID = ID + NUM ; } => 
{ while ( arithexpr boolop simpleexpr ) ID = ID + NUM ; } => 
{ while ( arithexpr boolop NUM ) ID = ID + NUM ; } => 
{ while ( arithexpr <= NUM ) ID = ID + NUM ; } => 
{ while ( multexpr arithexprprime <= NUM ) ID = ID + NUM ; } => 
{ while ( multexpr <= NUM ) ID = ID + NUM ; } => 
{ while ( simpleexpr multexprprime <= NUM ) ID = ID + NUM ; } => 
{ while ( simpleexpr <= NUM ) ID = ID + NUM ; } => 
{ while ( ID <= NUM ) ID = ID + NUM ; } 
```

3.
Input
```
{
    if ( ID == NUM ) then {
        ID = NUM ;
        while ( ID < NUM ) {
            ID = ID + NUM ;
        }
    } else {
        ID = NUM ;
    }
}
```

Output
```
program => 
compoundstmt => 
{ stmts } => 
{ stmt stmts } => 
{ stmt } => 
{ ifstmt } => 
{ if ( boolexpr ) then stmt else stmt } => 
{ if ( boolexpr ) then stmt else compoundstmt } => 
{ if ( boolexpr ) then stmt else { stmts } } => 
{ if ( boolexpr ) then stmt else { stmt stmts } } => 
{ if ( boolexpr ) then stmt else { stmt } } => 
{ if ( boolexpr ) then stmt else { assgstmt } } => 
{ if ( boolexpr ) then stmt else { ID = arithexpr ; } } => 
{ if ( boolexpr ) then stmt else { ID = multexpr arithexprprime ; } } => 
{ if ( boolexpr ) then stmt else { ID = multexpr ; } } => 
{ if ( boolexpr ) then stmt else { ID = simpleexpr multexprprime ; } } => 
{ if ( boolexpr ) then stmt else { ID = simpleexpr ; } } => 
{ if ( boolexpr ) then stmt else { ID = NUM ; } } => 
{ if ( boolexpr ) then compoundstmt else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmts } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt stmts } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt stmt stmts } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt stmt } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt whilestmt } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt while ( boolexpr ) stmt } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt while ( boolexpr ) compoundstmt } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt while ( boolexpr ) { stmts } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt while ( boolexpr ) { stmt stmts } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt while ( boolexpr ) { stmt } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt while ( boolexpr ) { assgstmt } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt while ( boolexpr ) { ID = arithexpr ; } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt while ( boolexpr ) { ID = multexpr arithexprprime ; } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt while ( boolexpr ) { ID = multexpr + multexpr arithexprprime ; } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt while ( boolexpr ) { ID = multexpr + multexpr ; } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt while ( boolexpr ) { ID = multexpr + simpleexpr multexprprime ; } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt while ( boolexpr ) { ID = multexpr + simpleexpr ; } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt while ( boolexpr ) { ID = multexpr + NUM ; } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt while ( boolexpr ) { ID = simpleexpr multexprprime + NUM ; } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt while ( boolexpr ) { ID = simpleexpr + NUM ; } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt while ( boolexpr ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt while ( arithexpr boolop arithexpr ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt while ( arithexpr boolop multexpr arithexprprime ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt while ( arithexpr boolop multexpr ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt while ( arithexpr boolop simpleexpr multexprprime ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt while ( arithexpr boolop simpleexpr ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt while ( arithexpr boolop NUM ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt while ( arithexpr < NUM ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt while ( multexpr arithexprprime < NUM ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt while ( multexpr < NUM ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt while ( simpleexpr multexprprime < NUM ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt while ( simpleexpr < NUM ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { stmt while ( ID < NUM ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { assgstmt while ( ID < NUM ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { ID = arithexpr ; while ( ID < NUM ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { ID = multexpr arithexprprime ; while ( ID < NUM ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { ID = multexpr ; while ( ID < NUM ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { ID = simpleexpr multexprprime ; while ( ID < NUM ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { ID = simpleexpr ; while ( ID < NUM ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( boolexpr ) then { ID = NUM ; while ( ID < NUM ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( arithexpr boolop arithexpr ) then { ID = NUM ; while ( ID < NUM ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( arithexpr boolop multexpr arithexprprime ) then { ID = NUM ; while ( ID < NUM ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( arithexpr boolop multexpr ) then { ID = NUM ; while ( ID < NUM ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( arithexpr boolop simpleexpr multexprprime ) then { ID = NUM ; while ( ID < NUM ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( arithexpr boolop simpleexpr ) then { ID = NUM ; while ( ID < NUM ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( arithexpr boolop NUM ) then { ID = NUM ; while ( ID < NUM ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( arithexpr == NUM ) then { ID = NUM ; while ( ID < NUM ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( multexpr arithexprprime == NUM ) then { ID = NUM ; while ( ID < NUM ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( multexpr == NUM ) then { ID = NUM ; while ( ID < NUM ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( simpleexpr multexprprime == NUM ) then { ID = NUM ; while ( ID < NUM ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( simpleexpr == NUM ) then { ID = NUM ; while ( ID < NUM ) { ID = ID + NUM ; } } else { ID = NUM ; } } => 
{ if ( ID == NUM ) then { ID = NUM ; while ( ID < NUM ) { ID = ID + NUM ; } } else { ID = NUM ; } } ```
