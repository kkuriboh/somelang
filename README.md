# weird language, mostly a haskell test drive

Almost able to parse all of this
```c
struct Point {
    int x;
    int y;
}

fun main(): int {
    int x = 1;
    int y = 2;

    Point p = .{ .x = x; .y = y };

    if x > y {
        printf("%d is greather than %d\n", p.x, p.y);
    } else {
        printf("%d is smaller than %d\n", p.x, p.y);
    }

    int[] arr = [1, 2, 3, 4];
    for val in arr {
        printf("%d\n", val);
    }

    while true {
        printf("suicide tendencies\n");
    }

    return 0;
}
```
