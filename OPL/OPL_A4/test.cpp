#include <iostream>
#include <stdlib.h>

#include "tombstones.h" // Header file for your MyPointer class (tombstones implementation)

class testclass
{
public:
    int a;
    float b;
    testclass(int x, float y) : a(x), b(y) {}
};

// Driver program for the MyPointer class (tombstones implementation)
int main(int argc, char **argv)
{
    MyPointer<int> ptr(new int(12));
    MyPointer<int> tmp((int *)NULL);
    MyPointer<int> bar = tmp;

    if (ptr == 0)
    {
        std::cout << "Error 1: ptr should not be null!\n";
    }

    if (bar != 0)
    {
        std::cout << "Error 2: bar should be null!\n";
    }

    bar = new int(12);
    if (ptr == bar)
    {
        std::cout << "Error 3: ptr and bar should refer to distinct objects!\n";
    }

    if (*ptr != *bar)
    {
        std::cout << "Error 4: *ptr and *bar should have the same value!\n";
    }

    std::cout << "*ptr == " << *ptr << ", *bar == " << *bar << " (both should be 12)\n";

    free(ptr);
    free(bar);

    MyPointer<int> valCheck;
    valCheck = new int(12);
    MyPointer<int> copy(valCheck);

    if (copy == 0)
    {
        std::cout << "Error 5: copy should not be null!\n";
    }

    if (*copy != 12)
    {
        std::cout << "Error 6: copy is the wrong value!\n";
    }

    if (valCheck != copy)
    {
        std::cout << "Error 7: valcheck and copy should refer to the same object!\n";
    }

    if (*valCheck != *copy)
    {
        std::cout << "Error 8: *valcheck and *copy are supposed to have the same value!\n";
    }

    *valCheck = 15;
    if (*copy != 15)
    {
        std::cout << "Error 9: valcheck and copy should still match here!\n";
    }

    MyPointer<testclass> tc(new testclass(14, 3.14159));
    std::cout << "tc->a == " << tc->a << ", tc->b == " << tc->b << " (should be 14 and 3.14159)\n\n";

    std::cout << "About to use the free() friend function - beware of SEGFAULTs...\n";

    free(valCheck);
    free(copy);

    MyPointer<int> freeSegfaultTest(new int(13));
    MyPointer<int> freeSegfaultTestCopy(freeSegfaultTest);
    free(freeSegfaultTest);
    std::cout << "free() did not cause a SEGFAULT (which is good)!\n\n";

    std::cout << "About to dereference the freed MyPointer...\n"
              << "(this should cause an error message about a dangling MyPointer\n"
              << "or memory leak, and terminate this driver program!)\n\n";

    int i = *freeSegfaultTestCopy; // should cause a dangling MyPointer error message
                                   // and terminate this driver program

    std::cout << "Error 10: Tests complete, but dangling MyPointer was NOT detected!" << std::endl;
    return 0;
}