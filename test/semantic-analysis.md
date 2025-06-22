1.
Input
```
int x = 4 ; int y = 2 ; real z = 10.0 ;

{
    if ( x > y ) then z = z / 2 ; else z = z / 4 ;
}
```

Output
```
x: 4
y: 2
z: 5
```

2.
Input
```
int a = 1 ; int b = 2 ; real c = 3.0 ;

{
    a = a + 1 ;
    b = b * a ;
    if ( a < b ) then c = c / 2 ; else c = c / 4 ;
}
```

Output
```
a: 2
b: 4
c: 1.5
```

3.
Input
```
int a = 1 ; int b = 2 ; real r = 6.0 ;

{
    a = a + b ;
    {
        r = r - 1.5 ;
        if ( a == b ) then r = r * 2 ; else r = r / 2 ;
    }
}
```

Output
```
a: 3
b: 2
r: 2.25
```