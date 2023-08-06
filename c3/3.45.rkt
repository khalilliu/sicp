1. Peter calls test-and-set!, (car cell) is false.

2. Paul *also* calls test-and-set!, *also* finds (car cell) to be false, since Peter's test-and-set! has not completed yet.

3. Peter sets (car cell) to true.

4. Paul *also* sets (car cell) to true.

Here, both Peter and Paul have acquired the mutex, which is bad.